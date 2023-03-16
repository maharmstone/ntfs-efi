#include <efi.h>
#include <string.h>
#include <string_view>
#include <optional>
#include <uchar.h>
#include "ntfs.h"

#define UNUSED(x) (void)(x)
#define sector_align(n, a) ((n)&((a)-1)?(((n)+(a))&~((a)-1)):(n))

using namespace std;

struct mapping {
    LIST_ENTRY list_entry;
    uint64_t lcn;
    uint64_t vcn;
    uint64_t length;
};

struct volume {
    ~volume();

    EFI_SIMPLE_FILE_SYSTEM_PROTOCOL proto;
    NTFS_BOOT_SECTOR* boot_sector;
    EFI_HANDLE controller;
    EFI_BLOCK_IO_PROTOCOL* block;
    EFI_DISK_IO_PROTOCOL* disk_io;
    uint64_t file_record_size;
    LIST_ENTRY mft_mappings;
    char16_t upcase[0x10000];
};

struct inode {
    inode(volume& vol) : vol(vol) { }
    ~inode();

    EFI_FILE_PROTOCOL proto;
    uint64_t ino;
    volume& vol;
    bool inode_loaded;
    STANDARD_INFORMATION standard_info;
    uint64_t size;
    uint64_t phys_size;
    uint64_t vdl;
    uint64_t position;
    LIST_ENTRY index_mappings;
    index_root* index_root;
    LIST_ENTRY levels;
    bool is_dir;
    size_t name_len;
    char16_t* name;
    bool data_loaded;
    LIST_ENTRY data_mappings;
    uint8_t* data;
};

struct btree_level {
    LIST_ENTRY list_entry;
    const index_entry* ent;
    uint8_t data[];
};

static EFI_SYSTEM_TABLE* systable;
static EFI_BOOT_SERVICES* bs;
static EFI_DRIVER_BINDING_PROTOCOL drvbind;

static void populate_file_handle(EFI_FILE_PROTOCOL* h);
static EFI_STATUS load_inode(inode* ino);
static EFI_STATUS read_from_mappings(const volume& vol, const LIST_ENTRY* mappings, uint64_t offset,
                                     uint8_t* buf, uint64_t size);
static EFI_STATUS process_fixups(MULTI_SECTOR_HEADER* header, uint64_t length,
                                 unsigned int sector_size);
static EFI_STATUS read_mappings(const volume& vol, const ATTRIBUTE_RECORD_HEADER& att,
                                LIST_ENTRY* mappings);
static EFI_STATUS loop_through_atts(const volume& vol, uint64_t inode, const FILE_RECORD_SEGMENT_HEADER* file_record,
                                    invocable<const ATTRIBUTE_RECORD_HEADER&, string_view, u16string_view> auto func);

static void do_print_error(const char* func, EFI_STATUS Status) {
    UNUSED(func);
    UNUSED(Status);

    // FIXME
}

static EFI_STATUS drv_supported(EFI_DRIVER_BINDING_PROTOCOL* This, EFI_HANDLE ControllerHandle,
                                EFI_DEVICE_PATH_PROTOCOL* RemainingDevicePath) {
    EFI_STATUS Status;
    EFI_DISK_IO_PROTOCOL* disk_io;
    EFI_GUID guid_disk = EFI_DISK_IO_PROTOCOL_GUID;
    EFI_GUID guid_block = EFI_BLOCK_IO_PROTOCOL_GUID;

    UNUSED(RemainingDevicePath);

    Status = bs->OpenProtocol(ControllerHandle, &guid_disk, (void**)&disk_io, This->DriverBindingHandle,
                              ControllerHandle, EFI_OPEN_PROTOCOL_BY_DRIVER);

    if (EFI_ERROR(Status))
        return Status;

    bs->CloseProtocol(ControllerHandle, &guid_disk, This->DriverBindingHandle, ControllerHandle);

    return bs->OpenProtocol(ControllerHandle, &guid_block, NULL, This->DriverBindingHandle,
                            ControllerHandle, EFI_OPEN_PROTOCOL_TEST_PROTOCOL);
}

static int cmp_filenames(const char16_t* upcase, u16string_view fn1, u16string_view fn2) {
    // FIXME - what about directories with case-sensitivity flag set?

    while (!fn1.empty() && !fn2.empty()) {
        if (fn1.empty())
            return -1;

        if (fn2.empty())
            return 1;

        char16_t c1 = upcase[fn1[0]];
        char16_t c2 = upcase[fn2[0]];

        if (c1 < c2)
            return -1;
        else if (c1 > c2)
            return 1;

        fn1 = u16string_view(fn1.data() + 1, fn1.size() - 1);
        fn2 = u16string_view(fn2.data() + 1, fn2.size() - 1);
    }

    return 0;
}

static EFI_STATUS find_file_in_dir(const volume& vol, uint64_t dir, u16string_view name, uint64_t* inode) {
    EFI_STATUS Status, Status2;
    FILE_RECORD_SEGMENT_HEADER* file;
    index_root* ir = nullptr;
    LIST_ENTRY index_mappings;
    const index_entry* ent;
    uint8_t* scratch = nullptr;

    InitializeListHead(&index_mappings);

    Status = bs->AllocatePool(EfiBootServicesData, vol.file_record_size, (void**)&file);
    if (EFI_ERROR(Status)) {
        do_print_error("AllocatePool", Status);
        return Status;
    }

    Status = read_from_mappings(vol, &vol.mft_mappings, dir * vol.file_record_size,
                                (uint8_t*)file, vol.file_record_size);
    if (EFI_ERROR(Status)) {
        bs->FreePool(file);
        do_print_error("read_from_mappings", Status);
        return Status;
    }

    if (file->MultiSectorHeader.Signature != NTFS_FILE_SIGNATURE) {
        // FIXME - print error
        bs->FreePool(file);
        return EFI_INVALID_PARAMETER;
    }

    Status = process_fixups(&file->MultiSectorHeader, vol.file_record_size,
                            vol.boot_sector->BytesPerSector);

    if (EFI_ERROR(Status)) {
        do_print_error("process_fixups", Status);
        bs->FreePool(file);
        return Status;
    }

    Status2 = loop_through_atts(vol, dir, file, [&](const ATTRIBUTE_RECORD_HEADER& att, string_view res_data, u16string_view att_name) -> bool {
        switch (att.TypeCode) {
            case ntfs_attribute::INDEX_ALLOCATION:
                if (att_name == u"$I30" && att.FormCode == NTFS_ATTRIBUTE_FORM::NONRESIDENT_FORM) {
                    Status = read_mappings(vol, att, &index_mappings);
                    if (EFI_ERROR(Status)) {
                        do_print_error("read_mappings", Status);
                        return false;
                    }
                }
            break;

            case ntfs_attribute::INDEX_ROOT:
                if (att_name == u"$I30" && att.FormCode == NTFS_ATTRIBUTE_FORM::RESIDENT_FORM && !res_data.empty() && !ir) {
                    Status = bs->AllocatePool(EfiBootServicesData, res_data.size(), (void**)&ir);
                    if (EFI_ERROR(Status)) {
                        do_print_error("AllocatePool", Status);
                        return false;
                    }

                    memcpy(ir, res_data.data(), res_data.size());
                }
            break;

            default:
                break;
        }

        return true;
    });

    if (EFI_ERROR(Status2)) {
        do_print_error("loop_through_atts", Status2);
        Status = Status2;
    }

    if (EFI_ERROR(Status))
        goto end;

    if (!ir) {
        Status = EFI_NOT_FOUND;
        goto end;
    }

    ent = reinterpret_cast<const index_entry*>((uint8_t*)&ir->node_header + ir->node_header.first_entry);

    while (true) {
        string_view data((const char*)ent + sizeof(index_entry), ent->stream_length);

        if (data.size() >= offsetof(FILE_NAME, FileName)) {
            const auto& fn = *(FILE_NAME*)data.data();
            u16string_view ent_name(fn.FileName, fn.FileNameLength);

            auto cmp = cmp_filenames(vol.upcase, name, ent_name);

            if (cmp == 0) { // found
                *inode = ent->file_reference.SegmentNumber;
                Status = EFI_SUCCESS;
                goto end;
            } else if (cmp == 1) { // skip to next
                ent = reinterpret_cast<const index_entry*>((uint8_t*)ent + ent->entry_length);
                continue;
            }

            if (cmp == -1 && !(ent->flags & INDEX_ENTRY_SUBNODE)) {
                Status = EFI_NOT_FOUND;
                goto end;
            }
        }

        if (ent->flags & INDEX_ENTRY_SUBNODE) { // if subnode, descend
            uint64_t vcn = ((MFT_SEGMENT_REFERENCE*)((uint8_t*)ent + ent->entry_length - sizeof(uint64_t)))->SegmentNumber;

            if (ir->bytes_per_index_record < vol.boot_sector->BytesPerSector * vol.boot_sector->SectorsPerCluster)
                vcn *= vol.boot_sector->BytesPerSector;
            else
                vcn *= (uint64_t)vol.boot_sector->BytesPerSector * (uint64_t)vol.boot_sector->SectorsPerCluster;

            if (!scratch) {
                Status = bs->AllocatePool(EfiBootServicesData, ir->bytes_per_index_record,
                                         (void**)&scratch);
                if (EFI_ERROR(Status)) {
                    do_print_error("AllocatePool", Status);
                    goto end;
                }
            }

            Status = read_from_mappings(vol, &index_mappings, vcn, scratch, ir->bytes_per_index_record);
            if (EFI_ERROR(Status)) {
                do_print_error("read_from_mappings", Status);
                goto end;
            }

            auto rec = reinterpret_cast<index_record*>(scratch);

            if (rec->MultiSectorHeader.Signature != INDEX_RECORD_MAGIC) {
                // FIXME - print error
                Status = EFI_INVALID_PARAMETER;
                goto end;
            }

            Status = process_fixups(&rec->MultiSectorHeader, ir->bytes_per_index_record,
                                    vol.boot_sector->BytesPerSector);
            if (EFI_ERROR(Status)) {
                do_print_error("process_fixups", Status);
                goto end;
            }

            ent = reinterpret_cast<const index_entry*>((uint8_t*)&rec->header + rec->header.first_entry);

            continue;
        }

        if (ent->flags & INDEX_ENTRY_LAST) {
            Status = EFI_NOT_FOUND;
            goto end;
        }

        ent = reinterpret_cast<const index_entry*>((uint8_t*)ent + ent->entry_length);
    }

end:
    if (ir)
        bs->FreePool(ir);

    if (scratch)
        bs->FreePool(scratch);

    bs->FreePool(file);

    return Status;
}

static size_t count_path_parts(u16string_view v) {
    size_t num_parts = 0;

    while (!v.empty()) {
        num_parts++;

        if (auto bs = v.find(u'\\'); bs != u16string_view::npos)
            v = u16string_view(v.data() + bs + 1, v.size() - bs - 1);
        else
            break;
    }

    return num_parts;
}

static void extract_parts(u16string_view v, u16string_view*& p) {
    while (!v.empty()) {
        if (auto bs = v.find(u'\\'); bs != u16string_view::npos) {
            *p = u16string_view(v.data(), bs);
            p++;
            v = u16string_view(v.data() + bs + 1, v.size() - bs - 1);
        } else {
            *p = v;
            p++;
            break;
        }
    }
}

static EFI_STATUS normalize_path(u16string_view fn, u16string_view parent, char16_t*& name, size_t& name_len) {
    EFI_STATUS Status;
    bool from_root = false;
    size_t num_parts = 0;
    u16string_view* parts;
    bool first;

    if (fn.front() == '\\') {
        from_root = true;
        fn = u16string_view(fn.data() + 1, fn.size() - 1);
    }

    if (parent.empty())
        from_root = true;

    if (!from_root)
        num_parts = count_path_parts(parent);

    num_parts += count_path_parts(fn);

    if (num_parts == 0) {
        name = nullptr;
        name_len = 0;
        return EFI_SUCCESS;
    }

    Status = bs->AllocatePool(EfiBootServicesData, num_parts * sizeof(u16string_view), (void**)&parts);
    if (EFI_ERROR(Status)) {
        do_print_error("AllocatePool", Status);
        return Status;
    }

    {
        u16string_view* p = parts;

        if (!from_root)
            extract_parts(parent, p);

        extract_parts(fn, p);
    }

    for (size_t i = 0; i < num_parts; i++) {
        if (parts[i] == u".")
            parts[i] = u"";
        else if (parts[i] == u"..") {
            parts[i] = u"";

            if (i == 0) {
                bs->FreePool(parts);
                return EFI_INVALID_PARAMETER;
            }

            auto j = i - 1;
            while (true) {
                if (!parts[j].empty()) {
                    parts[j] = u"";
                    break;
                }

                if (j == 0) {
                    bs->FreePool(parts);
                    return EFI_INVALID_PARAMETER;
                }

                j--;
            }
        }
    }

    name_len = 0;
    first = true;
    for (size_t i = 0; i < num_parts; i++) {
        if (parts[i].empty())
            continue;

        if (!first)
            name_len++;

        name_len += parts[i].size();
        first = false;
    }

    if (name_len == 0) {
        bs->FreePool(parts);
        name = nullptr;
        return EFI_SUCCESS;
    }

    Status = bs->AllocatePool(EfiBootServicesData, name_len * sizeof(char16_t), (void**)&name);
    if (EFI_ERROR(Status)) {
        do_print_error("AllocatePool", Status);
        bs->FreePool(parts);
        return Status;
    }

    {
        char16_t* n = name;

        first = true;
        for (size_t i = 0; i < num_parts; i++) {
            if (parts[i].empty())
                continue;

            if (!first) {
                *n = u'\\';
                n++;
            }

            memcpy(n, parts[i].data(), parts[i].size() * sizeof(char16_t));
            n += parts[i].size();
            first = false;
        }
    }

    bs->FreePool(parts);

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI file_open(struct _EFI_FILE_HANDLE* File, struct _EFI_FILE_HANDLE** NewHandle, CHAR16* FileName,
                                   UINT64 OpenMode, UINT64 Attributes) {
    EFI_STATUS Status;
    inode* file = _CR(File, inode, proto);
    uint64_t inode_num;
    inode* ino;
    char16_t* name;
    size_t name_len;

    UNUSED(Attributes);

    if (OpenMode & EFI_FILE_MODE_CREATE)
        return EFI_UNSUPPORTED;

    if (FileName[0] == L'\\' && FileName[1] == 0) {
        inode_num = NTFS_ROOT_DIR_INODE;
        name = nullptr;
        name_len = 0;
    } else if (FileName[0] == L'.' && FileName[1] == 0) {
        inode_num = file->ino;

        if (file->name) {
            Status = bs->AllocatePool(EfiBootServicesData, file->name_len * sizeof(char16_t), (void**)&name);
            if (EFI_ERROR(Status)) {
                do_print_error("AllocatePool", Status);
                return Status;
            }

            memcpy(name, file->name, file->name_len * sizeof(char16_t));
            name_len = file->name_len;
        } else {
            name = nullptr;
            name_len = 0;
        }
    } else {
        u16string_view fn((char16_t*)FileName);

        if (fn.empty())
            return EFI_NOT_FOUND;

        Status = normalize_path(fn, u16string_view(file->name, file->name_len), name, name_len);
        if (EFI_ERROR(Status)) {
            do_print_error("normalize_path", Status);
            return Status;
        }

        fn = u16string_view(name, name_len);
        inode_num = NTFS_ROOT_DIR_INODE;

        if (!fn.empty()) {
            while (true) {
                u16string_view part;

                auto backslash = fn.find(u'\\');

                if (backslash != u16string_view::npos)
                    part = u16string_view(fn.data(), backslash);
                else
                    part = fn;

                Status = find_file_in_dir(file->vol, inode_num, part, &inode_num);

                if (Status == EFI_NOT_FOUND) {
                    if (name)
                        bs->FreePool(name);

                    return Status;
                }

                if (EFI_ERROR(Status)) {
                    if (name)
                        bs->FreePool(name);

                    do_print_error("find_file_in_dir", Status);
                    return Status;
                }

                if (backslash == u16string_view::npos)
                    break;

                fn = u16string_view(fn.data() + backslash + 1, fn.size() - backslash - 1);
            }
        }
    }

    Status = bs->AllocatePool(EfiBootServicesData, sizeof(inode), (void**)&ino);
    if (EFI_ERROR(Status)) {
        if (name)
            bs->FreePool(name);

        do_print_error("AllocatePool", Status);
        return Status;
    }

    memset(ino, 0, sizeof(inode));

    new (ino) inode(file->vol);

    populate_file_handle(&ino->proto);

    ino->ino = inode_num;
    ino->name = name;
    ino->name_len = name_len;

    *NewHandle = &ino->proto;

    return EFI_SUCCESS;
}

inode::~inode() {
    if (name)
        bs->FreePool(name);

    if (data)
        bs->FreePool(data);

    if (!inode_loaded)
        return;

    if (index_root)
        bs->FreePool(index_root);

    while (!IsListEmpty(&index_mappings)) {
        mapping* m = _CR(index_mappings.Flink, mapping, list_entry);
        RemoveEntryList(&m->list_entry);
        bs->FreePool(m);
    }

    while (!IsListEmpty(&levels)) {
        auto l = _CR(levels.Flink, btree_level, list_entry);
        RemoveEntryList(&l->list_entry);
        bs->FreePool(l);
    }

    while (!IsListEmpty(&data_mappings)) {
        mapping* m = _CR(data_mappings.Flink, mapping, list_entry);
        RemoveEntryList(&m->list_entry);
        bs->FreePool(m);
    }
}

static EFI_STATUS EFIAPI file_close(struct _EFI_FILE_HANDLE* File) {
    inode* ino = _CR(File, inode, proto);

    ino->inode::~inode();
    bs->FreePool(ino);

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI file_delete(struct _EFI_FILE_HANDLE* File) {
    UNUSED(File);

    return EFI_UNSUPPORTED;
}

static EFI_STATUS read_from_mappings(const volume& vol, const LIST_ENTRY* mappings, uint64_t offset, uint8_t* buf,
                                     uint64_t size) {
    EFI_STATUS Status;
    uint32_t cluster_size = vol.boot_sector->BytesPerSector * vol.boot_sector->SectorsPerCluster;
    uint64_t vcn = offset / cluster_size;
    uint64_t last_vcn = sector_align(offset + size, cluster_size) / cluster_size;
    LIST_ENTRY* le;

    le = mappings->Flink;
    while (le != mappings) {
        mapping* m = _CR(le, mapping, list_entry);

        if (m->vcn < last_vcn && m->vcn + m->length >= vcn) {
            uint64_t to_read, mapping_offset;

            mapping_offset = offset - (m->vcn * cluster_size);
            to_read = ((m->vcn + m->length) * cluster_size) - mapping_offset;

            if (to_read > size)
                to_read = size;

            if (m->lcn == 0) // sparse
                memset(buf, 0, to_read);
            else {
                Status = vol.block->ReadBlocks(vol.block, vol.block->Media->MediaId,
                                                ((m->lcn * cluster_size) + mapping_offset) / vol.block->Media->BlockSize,
                                                to_read, buf);
                if (EFI_ERROR(Status)) {
                    do_print_error("ReadBlocks", Status);
                    return Status;
                }
            }

            if (to_read == size)
                break;

            offset += to_read;
            buf += to_read;
            size -= to_read;
            vcn = offset / cluster_size;
        }

        le = le->Flink;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS process_fixups(MULTI_SECTOR_HEADER* header, uint64_t length, unsigned int sector_size) {
    uint64_t sectors;
    uint16_t* seq;
    uint8_t* ptr;

    sectors = length / sector_size;

    if (header->UpdateSequenceArraySize < sectors + 1)
        return EFI_INVALID_PARAMETER;

    seq = (uint16_t*)((uint8_t*)header + header->UpdateSequenceArrayOffset);

    ptr = (uint8_t*)header + sector_size - sizeof(uint16_t);

    for (unsigned int i = 0; i < sectors; i++) {
        if (*(uint16_t*)ptr != seq[0])
            return EFI_INVALID_PARAMETER;

        *(uint16_t*)ptr = seq[i + 1];

        ptr += sector_size;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS next_index_item(inode* ino, const invocable<string_view> auto& func) {
    EFI_STATUS Status;
    const index_root& ir = *ino->index_root;

    if (IsListEmpty(&ino->levels))
        return EFI_NOT_FOUND;

    auto l = _CR(ino->levels.Blink, btree_level, list_entry);

    do {
        if (l->ent->flags & INDEX_ENTRY_SUBNODE) {
            btree_level* l2;
            uint64_t vcn = ((MFT_SEGMENT_REFERENCE*)((uint8_t*)l->ent + l->ent->entry_length - sizeof(uint64_t)))->SegmentNumber;

            if (ir.bytes_per_index_record < ino->vol.boot_sector->BytesPerSector * ino->vol.boot_sector->SectorsPerCluster)
                vcn *= ino->vol.boot_sector->BytesPerSector;
            else
                vcn *= (uint64_t)ino->vol.boot_sector->BytesPerSector * (uint64_t)ino->vol.boot_sector->SectorsPerCluster;

            Status = bs->AllocatePool(EfiBootServicesData, offsetof(btree_level, data) + ir.bytes_per_index_record,
                                      (void**)&l2);
            if (EFI_ERROR(Status)) {
                do_print_error("AllocatePool", Status);
                return Status;
            }

            Status = read_from_mappings(ino->vol, &ino->index_mappings, vcn, l2->data, ir.bytes_per_index_record);
            if (EFI_ERROR(Status)) {
                bs->FreePool(l2);
                do_print_error("read_from_mappings", Status);
                return Status;
            }

            auto rec = reinterpret_cast<index_record*>(l2->data);

            if (rec->MultiSectorHeader.Signature != INDEX_RECORD_MAGIC) {
                bs->FreePool(l2);
                // FIXME - print error
                return EFI_INVALID_PARAMETER;
            }

            Status = process_fixups(&rec->MultiSectorHeader, ir.bytes_per_index_record,
                                    ino->vol.boot_sector->BytesPerSector);
            if (EFI_ERROR(Status)) {
                bs->FreePool(l2);
                do_print_error("process_fixups", Status);
                return EFI_INVALID_PARAMETER;
            }

            InsertTailList(&ino->levels, &l2->list_entry);
            l = l2;
            l->ent = reinterpret_cast<const index_entry*>((uint8_t*)&rec->header + rec->header.first_entry);

            continue;
        }

        while (l->ent->flags & INDEX_ENTRY_LAST) {
            RemoveEntryList(&l->list_entry);
            bs->FreePool(l);

            if (IsListEmpty(&ino->levels))
                break;

            l = _CR(ino->levels.Blink, btree_level, list_entry);
        }

        if (IsListEmpty(&ino->levels))
            break;

        if (!(l->ent->flags & INDEX_ENTRY_LAST)) {
            if (func(string_view((const char*)l->ent + sizeof(index_entry), l->ent->stream_length)))
                l->ent = reinterpret_cast<const index_entry*>((uint8_t*)l->ent + l->ent->entry_length);

            return EFI_SUCCESS;
        }
    } while (!IsListEmpty(&ino->levels));

    return EFI_SUCCESS;
}

static void win_time_to_efi(int64_t win, EFI_TIME* efi) {
    int64_t secs, time, days;

    secs = win / 10000000;
    time = secs % 86400;
    days = secs / 86400;

    unsigned int jd = 2305814 + days; // Julian date

    unsigned int f = jd + 1401 + (((((4 * jd) + 274277) / 146097) * 3) / 4) - 38;
    unsigned int e = (4 * f) + 3;
    unsigned int g = (e % 1461) / 4;
    unsigned int h = (5 * g) + 2;

    efi->Month = (((h / 153) + 2) % 12) + 1;
    efi->Year = (e / 1461) - 4716 + ((14 - efi->Month) / 12);
    efi->Day = ((h % 153) / 5) + 1;
    efi->Hour = time / 3600;
    efi->Minute = (time % 3600) / 60;
    efi->Second = time % 60;
    efi->Pad1 = 0;
    efi->Nanosecond = (win % 10000000) * 100;
    efi->TimeZone = 0;
    efi->Daylight = 0;
    efi->Pad2 = 0;
}

static uint64_t win_attributes_to_efi(uint32_t attr, bool is_dir) {
    uint64_t ret = 0;

    if (is_dir)
        ret |= EFI_FILE_DIRECTORY;

    if (attr & FILE_ATTRIBUTE_READONLY)
        ret |= EFI_FILE_READ_ONLY;

    if (attr & FILE_ATTRIBUTE_HIDDEN)
        ret |= EFI_FILE_HIDDEN;

    if (attr & FILE_ATTRIBUTE_SYSTEM)
        ret |= EFI_FILE_SYSTEM;

    if (attr & EFI_FILE_ARCHIVE)
        ret |= EFI_FILE_ARCHIVE;

    return ret;
}

static EFI_STATUS read_dir(inode* ino, UINTN* BufferSize, VOID* Buffer) {
    EFI_STATUS Status;
    bool overflow = false, again;

    UNUSED(BufferSize);
    UNUSED(Buffer);

    if (!ino->inode_loaded) {
        Status = load_inode(ino);
        if (EFI_ERROR(Status)) {
            do_print_error("load_inode", Status);
            return Status;
        }
    }

    if (ino->position == 0 && IsListEmpty(&ino->levels)) {
        btree_level* l;

        Status = bs->AllocatePool(EfiBootServicesData, offsetof(btree_level, data), (void**)&l);
        if (EFI_ERROR(Status)) {
            do_print_error("AllocatePool", Status);
            return Status;
        }

        l->ent = reinterpret_cast<const index_entry*>((uint8_t*)&ino->index_root->node_header + ino->index_root->node_header.first_entry);
        InsertTailList(&ino->levels, &l->list_entry);
    }

    // FIXME - ignore special files in root

    do {
        again = false;

        Status = next_index_item(ino, [&](string_view data) -> bool {
            size_t size;

            const auto& fn = *reinterpret_cast<const FILE_NAME*>(data.data());

            if (fn.Namespace == file_name_type::DOS) { // ignore DOS filenames
                again = true;
                return true;
            }

            size = offsetof(EFI_FILE_INFO, FileName[0]) + ((fn.FileNameLength + 1) * sizeof(char16_t));

            if (*BufferSize < size) {
                *BufferSize = size;
                overflow = true;
                return false;
            }

            auto& info = *(EFI_FILE_INFO*)Buffer;

            info.Size = size;
            info.FileSize = fn.EndOfFile;
            info.PhysicalSize = fn.AllocationSize;
            win_time_to_efi(fn.CreationTime, &info.CreateTime);
            win_time_to_efi(fn.LastAccessTime, &info.LastAccessTime);
            win_time_to_efi(fn.LastWriteTime, &info.ModificationTime);
            info.Attribute = win_attributes_to_efi(fn.FileAttributes, fn.FileAttributes & FILE_ATTRIBUTE_DIRECTORY_MFT);

            memcpy(info.FileName, fn.FileName, fn.FileNameLength * sizeof(char16_t));
            info.FileName[fn.FileNameLength] = 0;

            *BufferSize = size;

            ino->position++;

            return true;
        });
    } while (again);

    if (overflow)
        return EFI_BUFFER_TOO_SMALL;

    if (Status == EFI_NOT_FOUND) { // last one
        *BufferSize = 0;
        return EFI_SUCCESS;
    }

    if (EFI_ERROR(Status))
        return Status;

    return EFI_SUCCESS;
}

static EFI_STATUS read_file(inode* ino, UINTN* BufferSize, VOID* Buffer) {
    EFI_STATUS Status, Status2;
    uint64_t start, end;

    if (ino->position >= ino->size || *BufferSize == 0) {
        *BufferSize = 0;
        return EFI_SUCCESS;
    }

    if (ino->position >= ino->vdl) {
        UINTN to_read = *BufferSize;

        if (to_read > ino->size - ino->position)
            to_read = ino->size - ino->position;

        memset(Buffer, 0, to_read);

        *BufferSize = to_read;

        return EFI_SUCCESS;
    }

    if (!ino->data_loaded) {
        FILE_RECORD_SEGMENT_HEADER* file;

        Status = bs->AllocatePool(EfiBootServicesData, ino->vol.file_record_size, (void**)&file);
        if (EFI_ERROR(Status)) {
            do_print_error("AllocatePool", Status);
            return Status;
        }

        Status = read_from_mappings(ino->vol, &ino->vol.mft_mappings, ino->ino * ino->vol.file_record_size,
                                    (uint8_t*)file, ino->vol.file_record_size);
        if (EFI_ERROR(Status)) {
            do_print_error("read_from_mappings", Status);
            bs->FreePool(file);
            return Status;
        }

        if (file->MultiSectorHeader.Signature != NTFS_FILE_SIGNATURE) {
            bs->FreePool(file);
            return EFI_INVALID_PARAMETER;
        }

        Status = EFI_SUCCESS;

        Status2 = loop_through_atts(ino->vol, ino->ino, file, [&](const ATTRIBUTE_RECORD_HEADER& att, string_view data, u16string_view att_name) -> bool {
            if (att.TypeCode == ntfs_attribute::DATA && att_name.empty()) {
                switch (att.FormCode) {
                    case NTFS_ATTRIBUTE_FORM::NONRESIDENT_FORM:
                        Status = read_mappings(ino->vol, att, &ino->data_mappings);
                        if (EFI_ERROR(Status))
                            do_print_error("read_mappings", Status);
                        break;

                    case NTFS_ATTRIBUTE_FORM::RESIDENT_FORM:
                        Status = bs->AllocatePool(EfiBootServicesData, data.size(), (void**)&ino->data);
                        if (EFI_ERROR(Status)) {
                            do_print_error("AllocatePool", Status);
                            break;
                        }

                        memcpy(ino->data, data.data(), data.size());
                        break;
                }

                return false;
            }

            return true;
        });

        bs->FreePool(file);

        if (EFI_ERROR(Status2))
            return Status2;

        if (EFI_ERROR(Status))
            return Status;

        ino->data_loaded = true;
    }

    start = ino->position;
    end = ino->position + *BufferSize;

    if (end > ino->size)
        end = ino->size;

    if (ino->data)
        memcpy(Buffer, ino->data + start, end - start);
    else {
        uint64_t start_aligned, valid_end, end_aligned;
        uint8_t* tmp = nullptr;

        valid_end = end;

        if (valid_end > ino->vdl)
            valid_end = ino->vdl;

        start_aligned = start & ~(ino->vol.boot_sector->BytesPerSector - 1);
        end_aligned = sector_align(valid_end, ino->vol.boot_sector->BytesPerSector);

        if (start_aligned != start || end_aligned != valid_end) {
            Status = bs->AllocatePool(EfiBootServicesData, end_aligned - start_aligned, (void**)&tmp);
            if (EFI_ERROR(Status)) {
                do_print_error("AllocatePool", Status);
                return Status;
            }
        }

        // FIXME - compressed data (LZNT1 and WOF)

        Status = read_from_mappings(ino->vol, &ino->data_mappings, start_aligned,
                                    tmp ? tmp : (uint8_t*)Buffer, end_aligned - start_aligned);
        if (EFI_ERROR(Status)) {
            do_print_error("read_from_mappings", Status);

            if (tmp)
                bs->FreePool(tmp);

            return Status;
        }

        if (tmp) {
            memcpy(Buffer, tmp + start - start_aligned, valid_end - start);
            bs->FreePool(tmp);
        }

        if (valid_end < end)
            memset((uint8_t*)Buffer + valid_end - start, 0, end - valid_end - start);
    }

    ino->position = end;
    *BufferSize = end - start;

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI file_read(struct _EFI_FILE_HANDLE* File, UINTN* BufferSize, VOID* Buffer) {
    EFI_STATUS Status;
    inode* ino = _CR(File, inode, proto);

    if (!ino->inode_loaded) {
        Status = load_inode(ino);
        if (EFI_ERROR(Status)) {
            do_print_error("load_inode", Status);
            return Status;
        }
    }

    if (ino->is_dir)
        return read_dir(ino, BufferSize, Buffer);
    else
        return read_file(ino, BufferSize, Buffer);
}

static EFI_STATUS EFIAPI file_write(struct _EFI_FILE_HANDLE* File, UINTN* BufferSize, VOID* Buffer) {
    UNUSED(File);
    UNUSED(BufferSize);
    UNUSED(Buffer);

    return EFI_UNSUPPORTED;
}

static EFI_STATUS EFIAPI file_set_position(struct _EFI_FILE_HANDLE* File, UINT64 Position) {
    EFI_STATUS Status;
    inode* ino = _CR(File, inode, proto);

    if (!ino->inode_loaded) {
        Status = load_inode(ino);
        if (EFI_ERROR(Status)) {
            do_print_error("load_inode", Status);
            return Status;
        }
    }

    if (ino->is_dir) {
        if (Position != 0)
            return EFI_UNSUPPORTED;

        ino->position = 0;

        while (!IsListEmpty(&ino->levels)) {
            auto l = _CR(ino->levels.Flink, btree_level, list_entry);
            RemoveEntryList(&l->list_entry);
            bs->FreePool(l);
        }
    } else {
        if (Position == 0xffffffffffffffff)
            ino->position = ino->size;
        else
            ino->position = Position;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI file_get_position(struct _EFI_FILE_HANDLE* File, UINT64* Position) {
    UNUSED(File);
    UNUSED(Position);

    systable->ConOut->OutputString(systable->ConOut, (CHAR16*)L"file_get_position\r\n");

    // FIXME

    return EFI_UNSUPPORTED;
}

static EFI_STATUS loop_through_atts(const volume& vol, uint64_t inode, const FILE_RECORD_SEGMENT_HEADER* file_record,
                                    invocable<const ATTRIBUTE_RECORD_HEADER&, string_view, u16string_view> auto func) {
    EFI_STATUS Status;
    auto att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)file_record + file_record->FirstAttributeOffset);
    size_t offset = file_record->FirstAttributeOffset;
    uint8_t* attlist = nullptr;
    size_t attlist_size;

    while (true) {
        if (att->TypeCode == (enum ntfs_attribute)0xffffffff || att->RecordLength == 0)
            break;

        if (att->TypeCode == ntfs_attribute::ATTRIBUTE_LIST) {
            switch (att->FormCode) {
                case NTFS_ATTRIBUTE_FORM::NONRESIDENT_FORM: {
                    uint32_t cluster_size = vol.boot_sector->BytesPerSector * vol.boot_sector->SectorsPerCluster;
                    LIST_ENTRY mappings;

                    if (att->Form.Nonresident.FileSize == 0)
                        break;

                    attlist_size = att->Form.Nonresident.FileSize;

                    Status = bs->AllocatePool(EfiBootServicesData, sector_align(attlist_size, cluster_size), (void**)&attlist);
                    if (EFI_ERROR(Status)) {
                        do_print_error("AllocatePool", Status);
                        return Status;
                    }

                    InitializeListHead(&mappings);

                    Status = read_mappings(vol, *att, &mappings);
                    if (EFI_ERROR(Status)) {
                        bs->FreePool(attlist);
                        do_print_error("read_mappings", Status);
                        return Status;
                    }

                    Status = read_from_mappings(vol, &mappings, 0, attlist, sector_align(attlist_size, cluster_size));
                    if (EFI_ERROR(Status)) {
                        while (!IsListEmpty(&mappings)) {
                            mapping* m = _CR(mappings.Flink, mapping, list_entry);
                            RemoveEntryList(&m->list_entry);
                            bs->FreePool(m);
                        }

                        bs->FreePool(attlist);
                        do_print_error("read_from_mappings", Status);
                        return Status;
                    }

                    while (!IsListEmpty(&mappings)) {
                        mapping* m = _CR(mappings.Flink, mapping, list_entry);
                        RemoveEntryList(&m->list_entry);
                        bs->FreePool(m);
                    }

                    break;
                }

                case NTFS_ATTRIBUTE_FORM::RESIDENT_FORM:
                    if (att->Form.Resident.ValueLength == 0)
                        break;

                    attlist_size = att->Form.Resident.ValueLength;

                    Status = bs->AllocatePool(EfiBootServicesData, attlist_size, (void**)&attlist);
                    if (EFI_ERROR(Status)) {
                        do_print_error("AllocatePool", Status);
                        return Status;
                    }

                    memcpy(attlist, (uint8_t*)att + att->Form.Resident.ValueOffset, attlist_size);
                break;
            }

            break;
        }

        offset += att->RecordLength;
        att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)att + att->RecordLength);
    }

    if (attlist) {
        {
            auto ent = (const attribute_list_entry*)attlist;
            size_t left = attlist_size;

            while (true) {
                uint64_t file_reference = ent->file_reference.SegmentNumber;

                if (file_reference == inode) { // contained elsewhere in this inode
                    att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)file_record + file_record->FirstAttributeOffset);
                    offset = file_record->FirstAttributeOffset;

                    while (true) {
                        if (att->TypeCode == (enum ntfs_attribute)0xffffffff || att->RecordLength == 0)
                            break;

                        if (att->TypeCode == ent->type && att->NameLength == ent->name_length && att->Instance == ent->instance) {
                            if (att->NameLength == 0 || !memcmp((uint8_t*)file_record + offset + att->NameOffset, (uint8_t*)ent + ent->name_offset, att->NameLength * sizeof(char16_t))) {
                                string_view data;
                                u16string_view name;

                                if (att->FormCode == NTFS_ATTRIBUTE_FORM::RESIDENT_FORM)
                                    data = string_view((const char*)file_record + offset + att->Form.Resident.ValueOffset, att->Form.Resident.ValueLength);

                                if (att->NameLength != 0)
                                    name = u16string_view((char16_t*)((uint8_t*)file_record + offset + att->NameOffset), att->NameLength);

                                if (!func(*att, data, name)) {
                                    bs->FreePool(attlist);
                                    return EFI_SUCCESS;
                                }

                                break;
                            }
                        }

                        offset += att->RecordLength;
                        att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)att + att->RecordLength);
                    }
                }

                if (left <= ent->record_length)
                    break;

                left -= ent->record_length;
                ent = (const attribute_list_entry*)((uint8_t*)ent + ent->record_length);
            }
        }

        while (true) {
            auto ent = (attribute_list_entry*)attlist;
            size_t left = attlist_size;
            optional<uint64_t> ref;
            FILE_RECORD_SEGMENT_HEADER* file2 = nullptr;

            while (true) {
                uint64_t file_reference = ent->file_reference.SegmentNumber;

                // skip entries already handled
                if (file_reference == inode) {
                    if (left <= ent->record_length)
                        break;

                    left -= ent->record_length;
                    ent = (attribute_list_entry*)((uint8_t*)ent + ent->record_length);
                    continue;
                }

                if (ref.has_value() && *ref != file_reference) {
                    if (left <= ent->record_length)
                        break;

                    left -= ent->record_length;
                    ent = (attribute_list_entry*)((uint8_t*)ent + ent->record_length);
                    continue;
                }

                if (!ref.has_value()) {
                    ref = file_reference;

                    Status = bs->AllocatePool(EfiBootServicesData, vol.file_record_size, (void**)&file2);
                    if (EFI_ERROR(Status)) {
                        do_print_error("AllocatePool", Status);
                        bs->FreePool(attlist);
                        return Status;
                    }

                    Status = read_from_mappings(vol, &vol.mft_mappings, file_reference * vol.file_record_size,
                                                (uint8_t*)file2, vol.file_record_size);
                    if (EFI_ERROR(Status)) {
                        do_print_error("read_from_mappings", Status);
                        bs->FreePool(file2);
                        bs->FreePool(attlist);
                        return Status;
                    }

                    if (file2->MultiSectorHeader.Signature != NTFS_FILE_SIGNATURE) {
                        bs->FreePool(file2);
                        bs->FreePool(attlist);
                        return EFI_INVALID_PARAMETER;
                    }

                    Status = process_fixups(&file2->MultiSectorHeader, vol.file_record_size,
                                            vol.boot_sector->BytesPerSector);

                    if (EFI_ERROR(Status)) {
                        bs->FreePool(file2);
                        bs->FreePool(attlist);
                        return Status;
                    }
                }

                att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)file2 + file2->FirstAttributeOffset);
                offset = file2->FirstAttributeOffset;

                while (true) {
                    if (att->TypeCode == (enum ntfs_attribute)0xffffffff || att->RecordLength == 0)
                        break;

                    if (att->TypeCode == ent->type && att->NameLength == ent->name_length && att->Instance == ent->instance) {
                        if (att->NameLength == 0 || !memcmp((uint8_t*)file2 + offset + att->NameOffset, (uint8_t*)ent + ent->name_offset, att->NameLength * sizeof(char16_t))) {
                            string_view data;
                            u16string_view name;

                            if (att->FormCode == NTFS_ATTRIBUTE_FORM::RESIDENT_FORM)
                                data = string_view((const char*)file2 + offset + att->Form.Resident.ValueOffset, att->Form.Resident.ValueLength);

                            if (att->NameLength != 0)
                                name = u16string_view((char16_t*)((uint8_t*)file2 + offset + att->NameOffset), att->NameLength);

                            if (!func(*att, data, name)) {
                                bs->FreePool(file2);
                                bs->FreePool(attlist);
                                return EFI_SUCCESS;
                            }

                            break;
                        }
                    }

                    offset += att->RecordLength;
                    att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)att + att->RecordLength);
                }

                // don't process this again
                ent->file_reference.SegmentNumber = inode;

                if (left <= ent->record_length)
                    break;

                left -= ent->record_length;
                ent = (attribute_list_entry*)((uint8_t*)ent + ent->record_length);
            }

            if (file2)
                bs->FreePool(file2);

            if (!ref.has_value()) {
                bs->FreePool(attlist);
                return EFI_SUCCESS;
            }
        }

        bs->FreePool(attlist);

        return EFI_SUCCESS;
    }

    att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)file_record + file_record->FirstAttributeOffset);
    offset = file_record->FirstAttributeOffset;

    while (true) {
        if (att->TypeCode == (enum ntfs_attribute)0xffffffff || att->RecordLength == 0)
            break;

        string_view data;
        u16string_view name;

        if (att->FormCode == NTFS_ATTRIBUTE_FORM::RESIDENT_FORM)
            data = string_view((const char*)file_record + offset + att->Form.Resident.ValueOffset, att->Form.Resident.ValueLength);

        if (att->NameLength != 0)
            name = u16string_view((char16_t*)((uint8_t*)file_record + offset + att->NameOffset), att->NameLength);

        if (!func(*att, data, name))
            return EFI_SUCCESS;

        offset += att->RecordLength;
        att = reinterpret_cast<const ATTRIBUTE_RECORD_HEADER*>((uint8_t*)att + att->RecordLength);
    }

    return EFI_SUCCESS;
}

static EFI_STATUS read_mappings(const volume& vol, const ATTRIBUTE_RECORD_HEADER& att, LIST_ENTRY* mappings) {
    EFI_STATUS Status;
    uint64_t next_vcn, current_lcn = 0, current_vcn;
    uint32_t cluster_size = vol.boot_sector->BytesPerSector * vol.boot_sector->SectorsPerCluster;
    uint8_t* stream;
    uint64_t max_cluster;

    if (att.FormCode != NTFS_ATTRIBUTE_FORM::NONRESIDENT_FORM)
        return EFI_INVALID_PARAMETER;

    if (att.Flags & ATTRIBUTE_FLAG_ENCRYPTED)
        return EFI_INVALID_PARAMETER;

    if (att.Flags & ATTRIBUTE_FLAG_COMPRESSION_MASK)
        return EFI_INVALID_PARAMETER;

    next_vcn = att.Form.Nonresident.LowestVcn;
    stream = (uint8_t*)&att + att.Form.Nonresident.MappingPairsOffset;

    max_cluster = att.Form.Nonresident.ValidDataLength / cluster_size;

    if (att.Form.Nonresident.ValidDataLength & (cluster_size - 1))
        max_cluster++;

    if (max_cluster == 0)
        return EFI_SUCCESS;

    while (true) {
        uint64_t v, l;
        int64_t v_val, l_val;
        mapping* m;

        current_vcn = next_vcn;

        if (*stream == 0)
            break;

        v = *stream & 0xf;
        l = *stream >> 4;

        stream++;

        if (v > 8)
            return EFI_INVALID_PARAMETER;

        if (l > 8)
            return EFI_INVALID_PARAMETER;

        // FIXME - do we need to make sure that int64_t pointers don't go past end of buffer?

        v_val = *(int64_t*)stream;
        v_val &= (1ull << (v * 8)) - 1;

        if ((uint64_t)v_val & (1ull << ((v * 8) - 1))) // sign-extend if negative
            v_val |= 0xffffffffffffffff & ~((1ull << (v * 8)) - 1);

        stream += v;

        next_vcn += v_val;

        Status = bs->AllocatePool(EfiBootServicesData, sizeof(mapping), (void**)&m);
        if (EFI_ERROR(Status))
            return Status;

        if (l != 0) {
            l_val = *(int64_t*)stream;
            l_val &= (1ull << (l * 8)) - 1;

            if ((uint64_t)l_val & (1ull << ((l * 8) - 1))) // sign-extend if negative
                l_val |= 0xffffffffffffffff & ~((1ull << (l * 8)) - 1);

            stream += l;

            current_lcn += l_val;

            if (next_vcn > max_cluster)
                next_vcn = max_cluster;

            m->lcn = current_lcn;
        } else
            m->lcn = 0;

        m->vcn = current_vcn;
        m->length = next_vcn - current_vcn;

        InsertTailList(mappings, &m->list_entry);

        if (next_vcn == max_cluster)
            break;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS load_inode(inode* ino) {
    EFI_STATUS Status, Status2;
    FILE_RECORD_SEGMENT_HEADER* file;

    InitializeListHead(&ino->index_mappings);
    InitializeListHead(&ino->levels);
    InitializeListHead(&ino->data_mappings);

    Status = bs->AllocatePool(EfiBootServicesData, ino->vol.file_record_size, (void**)&file);
    if (EFI_ERROR(Status))
        return Status;

    Status = read_from_mappings(ino->vol, &ino->vol.mft_mappings, ino->ino * ino->vol.file_record_size,
                                (uint8_t*)file, ino->vol.file_record_size);
    if (EFI_ERROR(Status)) {
        bs->FreePool(file);
        return Status;
    }

    if (file->MultiSectorHeader.Signature != NTFS_FILE_SIGNATURE) {
        bs->FreePool(file);
        return EFI_INVALID_PARAMETER;
    }

    Status = process_fixups(&file->MultiSectorHeader, ino->vol.file_record_size,
                            ino->vol.boot_sector->BytesPerSector);

    if (EFI_ERROR(Status)) {
        bs->FreePool(file);
        return Status;
    }

    memset(&ino->standard_info, 0, sizeof(STANDARD_INFORMATION));

    Status = EFI_SUCCESS;

    Status2 = loop_through_atts(ino->vol, ino->ino, file, [&](const ATTRIBUTE_RECORD_HEADER& att, string_view res_data, u16string_view att_name) -> bool {
        switch (att.TypeCode) {
            case ntfs_attribute::STANDARD_INFORMATION:
                if (att.FormCode == NTFS_ATTRIBUTE_FORM::RESIDENT_FORM) {
                    size_t to_copy = res_data.size();

                    if (to_copy > sizeof(STANDARD_INFORMATION))
                        to_copy = sizeof(STANDARD_INFORMATION);

                    memcpy(&ino->standard_info, res_data.data(), to_copy);
                }
            break;

            case ntfs_attribute::FILE_NAME:
                if (att.FormCode == NTFS_ATTRIBUTE_FORM::RESIDENT_FORM) {
                    const auto& fn = *(FILE_NAME*)res_data.data();

                    if (res_data.size() >= offsetof(FILE_NAME, EaSize))
                        ino->is_dir = fn.FileAttributes & FILE_ATTRIBUTE_DIRECTORY_MFT;
                }

            break;

            case ntfs_attribute::INDEX_ALLOCATION:
                if (att_name == u"$I30" && att.FormCode == NTFS_ATTRIBUTE_FORM::NONRESIDENT_FORM) {
                    ino->size = att.Form.Nonresident.FileSize;
                    ino->phys_size = att.Form.Nonresident.AllocatedLength;

                    Status = read_mappings(ino->vol, att, &ino->index_mappings);
                    if (EFI_ERROR(Status))
                        return false;
                }
            break;

            case ntfs_attribute::INDEX_ROOT:
                if (att_name == u"$I30" && att.FormCode == NTFS_ATTRIBUTE_FORM::RESIDENT_FORM && !res_data.empty() && !ino->index_root) {
                    Status = bs->AllocatePool(EfiBootServicesData, res_data.size(), (void**)&ino->index_root);
                    if (EFI_ERROR(Status))
                        return false;

                    memcpy(ino->index_root, res_data.data(), res_data.size());
                }
            break;

            case ntfs_attribute::DATA:
                if (att_name.empty()) {
                    switch (att.FormCode) {
                        case NTFS_ATTRIBUTE_FORM::NONRESIDENT_FORM:
                            ino->size = att.Form.Nonresident.FileSize;
                            ino->phys_size = att.Form.Nonresident.AllocatedLength;
                            ino->vdl = att.Form.Nonresident.ValidDataLength;
                        break;

                        case NTFS_ATTRIBUTE_FORM::RESIDENT_FORM:
                            ino->size = ino->phys_size = ino->vdl = att.Form.Resident.ValueLength;
                        break;
                    }
                }
            break;

            default:
                break;
        }

        return true;
    });

    if (EFI_ERROR(Status2))
        Status = Status2;

    if (EFI_ERROR(Status)) {
        if (ino->index_root) {
            bs->FreePool(ino->index_root);
            ino->index_root = nullptr;
        }

        bs->FreePool(file);
        return Status;
    }

    ino->inode_loaded = true;

    bs->FreePool(file);

    return EFI_SUCCESS;
}

static EFI_STATUS get_inode_file_info(inode* ino, UINTN* BufferSize, VOID* Buffer) {
    EFI_STATUS Status;
    unsigned int size = offsetof(EFI_FILE_INFO, FileName[0]) + sizeof(CHAR16);
    EFI_FILE_INFO* info = (EFI_FILE_INFO*)Buffer;
    u16string_view name;

    if (ino->name) {
        name = u16string_view(ino->name, ino->name_len);

        if (auto bs = name.rfind(u'\\'); bs != u16string_view::npos)
            name = u16string_view(name.data() + bs + 1, name.size() - bs - 1);

        size += name.size() * sizeof(char16_t);
    }

    if (*BufferSize < size) {
        *BufferSize = size;
        return EFI_BUFFER_TOO_SMALL;
    }

    if (!ino->inode_loaded) {
        Status = load_inode(ino);
        if (EFI_ERROR(Status)) {
            do_print_error("load_inode", Status);
            return Status;
        }
    }

    info->Size = size;
    info->FileSize = ino->size;
    info->PhysicalSize = ino->phys_size;
    win_time_to_efi(ino->standard_info.CreationTime, &info->CreateTime);
    win_time_to_efi(ino->standard_info.LastAccessTime, &info->LastAccessTime);
    win_time_to_efi(ino->standard_info.LastWriteTime, &info->ModificationTime);
    info->Attribute = win_attributes_to_efi(ino->standard_info.FileAttributes, ino->is_dir);

    if (!name.empty()) {
        memcpy(info->FileName, name.data(), name.size() * sizeof(char16_t));
        info->FileName[name.size()] = 0;
    } else
        info->FileName[0] = 0;

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI file_get_info(struct _EFI_FILE_HANDLE* File, EFI_GUID* InformationType, UINTN* BufferSize, VOID* Buffer) {
    inode* ino = _CR(File, inode, proto);
    EFI_GUID guid = EFI_FILE_INFO_ID;

    // FIXME - EFI_FILE_SYSTEM_INFO

    if (memcmp(InformationType, &guid, sizeof(EFI_GUID)))
        return EFI_UNSUPPORTED;

    return get_inode_file_info(ino, BufferSize, Buffer);
}

static EFI_STATUS EFIAPI file_set_info(struct _EFI_FILE_HANDLE* File, EFI_GUID* InformationType, UINTN BufferSize, VOID* Buffer) {
    UNUSED(File);
    UNUSED(InformationType);
    UNUSED(BufferSize);
    UNUSED(Buffer);

    return EFI_UNSUPPORTED;
}

static EFI_STATUS file_flush(struct _EFI_FILE_HANDLE* File) {
    UNUSED(File);

    // nop

    return EFI_SUCCESS;
}

static void populate_file_handle(EFI_FILE_PROTOCOL* h) {
    h->Revision = EFI_FILE_PROTOCOL_REVISION;
    h->Open = file_open;
    h->Close = file_close;
    h->Delete = file_delete;
    h->Read = file_read;
    h->Write = file_write;
    h->GetPosition = file_get_position;
    h->SetPosition = file_set_position;
    h->GetInfo = file_get_info;
    h->SetInfo = file_set_info;
    h->Flush = file_flush;
}

static EFI_STATUS EFIAPI open_volume(EFI_SIMPLE_FILE_SYSTEM_PROTOCOL* This, EFI_FILE_PROTOCOL** Root) {
    EFI_STATUS Status;
    volume* vol = _CR(This, volume, proto);
    inode* ino;

    Status = bs->AllocatePool(EfiBootServicesData, sizeof(inode), (void**)&ino);
    if (EFI_ERROR(Status)) {
        do_print_error("AllocatePool", Status);
        return Status;
    }

    memset(ino, 0, sizeof(inode));

    new (ino) inode(*vol);

    populate_file_handle(&ino->proto);

    ino->ino = NTFS_ROOT_DIR_INODE;

    *Root = &ino->proto;

    return EFI_SUCCESS;
}

static EFI_STATUS read_mft(volume& vol) {
    EFI_STATUS Status, Status2;
    FILE_RECORD_SEGMENT_HEADER* mft;

    Status = bs->AllocatePool(EfiBootServicesData, vol.file_record_size, (void**)&mft);
    if (EFI_ERROR(Status))
        return Status;

    Status = vol.block->ReadBlocks(vol.block, vol.block->Media->MediaId,
                                    (vol.boot_sector->MFT * vol.boot_sector->BytesPerSector * vol.boot_sector->SectorsPerCluster) / vol.block->Media->BlockSize,
                                    vol.file_record_size, mft);
    if (EFI_ERROR(Status)) {
        bs->FreePool(mft);
        return Status;
    }

    if (mft->MultiSectorHeader.Signature != NTFS_FILE_SIGNATURE) {
        bs->FreePool(mft);
        return EFI_INVALID_PARAMETER;
    }

    Status = process_fixups(&mft->MultiSectorHeader, vol.file_record_size,
                            vol.boot_sector->BytesPerSector);
    if (EFI_ERROR(Status)) {
        bs->FreePool(mft);
        return Status;
    }

    // read DATA mappings

    Status = EFI_INVALID_PARAMETER;

    Status2 = loop_through_atts(vol, NTFS_MFT_INODE, mft, [&](const ATTRIBUTE_RECORD_HEADER& att, string_view, u16string_view att_name) -> bool {
        if (att.TypeCode == ntfs_attribute::DATA && att_name.empty()) {
            Status = read_mappings(vol, att, &vol.mft_mappings);
            return false;
        }

        return true;
    });

    bs->FreePool(mft);

    if (EFI_ERROR(Status2))
        return Status2;

    return Status;
}

volume::~volume() {
    while (!IsListEmpty(&mft_mappings)) {
        mapping* m = _CR(mft_mappings.Flink, mapping, list_entry);
        RemoveEntryList(&m->list_entry);
        bs->FreePool(m);
    }

    bs->FreePool(boot_sector);
}

static EFI_STATUS read_upcase(volume& vol) {
    EFI_STATUS Status, Status2;
    FILE_RECORD_SEGMENT_HEADER* file;
    LIST_ENTRY mappings;
    uint64_t size;

    InitializeListHead(&mappings);

    Status = bs->AllocatePool(EfiBootServicesData, vol.file_record_size, (void**)&file);
    if (EFI_ERROR(Status))
        return Status;

    Status = read_from_mappings(vol, &vol.mft_mappings, NTFS_UPCASE_INODE * vol.file_record_size,
                                (uint8_t*)file, vol.file_record_size);
    if (EFI_ERROR(Status)) {
        bs->FreePool(file);
        return Status;
    }

    if (file->MultiSectorHeader.Signature != NTFS_FILE_SIGNATURE) {
        bs->FreePool(file);
        return EFI_INVALID_PARAMETER;
    }

    Status = process_fixups(&file->MultiSectorHeader, vol.file_record_size,
                            vol.boot_sector->BytesPerSector);

    if (EFI_ERROR(Status)) {
        bs->FreePool(file);
        return Status;
    }

    Status2 = loop_through_atts(vol, NTFS_UPCASE_INODE, file, [&](const ATTRIBUTE_RECORD_HEADER& att, string_view, u16string_view att_name) -> bool {
        switch (att.TypeCode) {
            case ntfs_attribute::DATA:
                // assuming that $UpCase DATA can never be resident
                if (att_name.empty() && att.FormCode == NTFS_ATTRIBUTE_FORM::NONRESIDENT_FORM) {
                    size = att.Form.Nonresident.AllocatedLength;
                    Status = read_mappings(vol, att, &mappings);
                    return false;
                }
            break;

            default:
                break;
        }

        return true;
    });

    if (EFI_ERROR(Status2))
        return Status2;

    if (EFI_ERROR(Status))
        return Status;

    Status = read_from_mappings(vol, &mappings, 0, (uint8_t*)vol.upcase, min(size, sizeof(vol.upcase)));

    while (!IsListEmpty(&mappings)) {
        mapping* m = _CR(mappings.Flink, mapping, list_entry);
        RemoveEntryList(&m->list_entry);
        bs->FreePool(m);
    }

    return Status;
}

static EFI_STATUS EFIAPI drv_start(EFI_DRIVER_BINDING_PROTOCOL* This, EFI_HANDLE ControllerHandle,
                                   EFI_DEVICE_PATH_PROTOCOL* RemainingDevicePath) {
    EFI_STATUS Status;
    EFI_GUID disk_guid = EFI_DISK_IO_PROTOCOL_GUID;
    EFI_GUID block_guid = EFI_BLOCK_IO_PROTOCOL_GUID;
    EFI_GUID fs_guid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;
    EFI_BLOCK_IO_PROTOCOL* block;
    uint32_t sblen;
    NTFS_BOOT_SECTOR* sb;
    EFI_DISK_IO_PROTOCOL* disk_io;
    volume* vol;

    UNUSED(RemainingDevicePath);

    Status = bs->OpenProtocol(ControllerHandle, &block_guid, (void**)&block, This->DriverBindingHandle,
                              ControllerHandle, EFI_OPEN_PROTOCOL_GET_PROTOCOL);
    if (EFI_ERROR(Status))
        return Status;

    if (block->Media->BlockSize == 0) {
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        return EFI_UNSUPPORTED;
    }

    Status = bs->OpenProtocol(ControllerHandle, &disk_guid, (void**)&disk_io, This->DriverBindingHandle,
                              ControllerHandle, EFI_OPEN_PROTOCOL_BY_DRIVER);
    if (EFI_ERROR(Status)) {
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    // FIXME - FAT driver also claims DISK_IO 2 protocol - do we need to?

    sblen = sector_align(sizeof(NTFS_BOOT_SECTOR), block->Media->BlockSize);

    Status = bs->AllocatePool(EfiBootServicesData, sblen, (void**)&sb);
    if (EFI_ERROR(Status)) {
        do_print_error("AllocatePool", Status);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    // read superblock

    Status = block->ReadBlocks(block, block->Media->MediaId, 0, sblen, sb);
    if (EFI_ERROR(Status)) {
        bs->FreePool(sb);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    if (memcmp(sb->FsName, NTFS_FS_NAME, sizeof(NTFS_FS_NAME) - 1)) { // not NTFS
        bs->FreePool(sb);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return EFI_UNSUPPORTED;
    }

    Status = bs->AllocatePool(EfiBootServicesData, sizeof(volume), (void**)&vol);
    if (EFI_ERROR(Status)) {
        do_print_error("AllocatePool", Status);
        bs->FreePool(sb);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    memset(vol, 0, sizeof(volume));

    vol->proto.Revision = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_REVISION;
    vol->proto.OpenVolume = open_volume;
    vol->boot_sector = sb;
    vol->controller = ControllerHandle;
    vol->block = block;
    vol->disk_io = disk_io;

    if (sb->ClustersPerMFTRecord < 0)
        vol->file_record_size = 1ull << -sb->ClustersPerMFTRecord;
    else
        vol->file_record_size = (uint64_t)sb->BytesPerSector * (uint64_t)sb->SectorsPerCluster * (uint64_t)sb->ClustersPerMFTRecord;

    InitializeListHead(&vol->mft_mappings);

    Status = read_mft(*vol);
    if (EFI_ERROR(Status)) {
        do_print_error("read_mft", Status);
        vol->volume::~volume();
        bs->FreePool(vol);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    Status = read_upcase(*vol);
    if (EFI_ERROR(Status)) {
        do_print_error("read_upcase", Status);
        vol->volume::~volume();
        bs->FreePool(vol);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    // FIXME - quibble protocol

    Status = bs->InstallProtocolInterface(&ControllerHandle, &fs_guid, EFI_NATIVE_INTERFACE, &vol->proto);
    if (EFI_ERROR(Status)) {
        do_print_error("InstallProtocolInterface", Status);
        vol->volume::~volume();
        bs->FreePool(vol);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI drv_stop(EFI_DRIVER_BINDING_PROTOCOL* This, EFI_HANDLE ControllerHandle,
                                  UINTN NumberOfChildren, EFI_HANDLE* ChildHandleBuffer) {
    UNUSED(This);
    UNUSED(ControllerHandle);
    UNUSED(NumberOfChildren);
    UNUSED(ChildHandleBuffer);

    systable->ConOut->OutputString(systable->ConOut, (CHAR16*)L"drv_stop\r\n");
    // FIXME

    return EFI_INVALID_PARAMETER;
}

extern "C"
EFI_STATUS EFIAPI efi_main(EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE* SystemTable) {
    EFI_STATUS Status;
    EFI_GUID guid = EFI_DRIVER_BINDING_PROTOCOL_GUID;

    systable = SystemTable;
    bs = SystemTable->BootServices;

    drvbind.Supported = drv_supported;
    drvbind.Start = drv_start;
    drvbind.Stop = drv_stop;
    drvbind.Version = 0x10;
    drvbind.ImageHandle = ImageHandle;
    drvbind.DriverBindingHandle = ImageHandle;

    Status = bs->InstallProtocolInterface(&drvbind.DriverBindingHandle, &guid,
                                          EFI_NATIVE_INTERFACE, &drvbind);
    if (EFI_ERROR(Status)) {
        do_print_error("InstallProtocolInterface", Status);
        return Status;
    }

    return EFI_SUCCESS;
}
