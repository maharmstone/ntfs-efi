#include <efi.h>
#include <string.h>
#include <stdbool.h>
#include <uchar.h>
#include "ntfs.h"

#define UNUSED(x) (void)(x)
#define sector_align(n, a) ((n)&((a)-1)?(((n)+(a))&~((a)-1)):(n))

typedef struct {
    LIST_ENTRY list_entry;
    uint64_t lcn;
    uint64_t vcn;
    uint64_t length;
} mapping;

typedef struct {
    EFI_SIMPLE_FILE_SYSTEM_PROTOCOL proto;
    NTFS_BOOT_SECTOR* boot_sector;
    EFI_HANDLE controller;
    EFI_BLOCK_IO_PROTOCOL* block;
    EFI_DISK_IO_PROTOCOL* disk_io;
    uint64_t file_record_size;
    LIST_ENTRY mft_mappings;
} volume;

typedef struct {
    EFI_FILE_PROTOCOL proto;
    uint64_t inode;
    volume* vol;
} inode;

static EFI_SYSTEM_TABLE* systable;
static EFI_BOOT_SERVICES* bs;
static EFI_DRIVER_BINDING_PROTOCOL drvbind;

static void do_print_error(const char* func, EFI_STATUS Status) {
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

static EFI_STATUS EFIAPI file_open(struct _EFI_FILE_HANDLE* File, struct _EFI_FILE_HANDLE** NewHandle, CHAR16* FileName,
                                   UINT64 OpenMode, UINT64 Attributes) {
    systable->ConOut->OutputString(systable->ConOut, L"file_open(");
    systable->ConOut->OutputString(systable->ConOut, FileName);
    systable->ConOut->OutputString(systable->ConOut, L")\r\n");

    // FIXME

    return EFI_UNSUPPORTED;
}

static EFI_STATUS EFIAPI file_close(struct _EFI_FILE_HANDLE* File) {
    inode* ino = _CR(File, inode, proto);

    bs->FreePool(ino);

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI file_delete(struct _EFI_FILE_HANDLE* File) {
    UNUSED(File);

    return EFI_UNSUPPORTED;
}

static EFI_STATUS EFIAPI file_read(struct _EFI_FILE_HANDLE* File, UINTN* BufferSize, VOID* Buffer) {
    systable->ConOut->OutputString(systable->ConOut, L"file_read\r\n");

    // FIXME

    return EFI_UNSUPPORTED;
}

static EFI_STATUS EFIAPI file_write(struct _EFI_FILE_HANDLE* File, UINTN* BufferSize, VOID* Buffer) {
    UNUSED(File);
    UNUSED(BufferSize);
    UNUSED(Buffer);

    return EFI_UNSUPPORTED;
}

static EFI_STATUS EFIAPI file_set_position(struct _EFI_FILE_HANDLE* File, UINT64 Position) {
    systable->ConOut->OutputString(systable->ConOut, L"file_set_position\r\n");

    // FIXME

    return EFI_UNSUPPORTED;
}

static EFI_STATUS EFIAPI file_get_position(struct _EFI_FILE_HANDLE* File, UINT64* Position) {
    systable->ConOut->OutputString(systable->ConOut, L"file_get_position\r\n");

    // FIXME

    return EFI_UNSUPPORTED;
}

static EFI_STATUS EFIAPI file_get_info(struct _EFI_FILE_HANDLE* File, EFI_GUID* InformationType, UINTN* BufferSize, VOID* Buffer) {
    systable->ConOut->OutputString(systable->ConOut, L"file_get_info\r\n");

    // FIXME

    return EFI_UNSUPPORTED;
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

    populate_file_handle(&ino->proto);

    ino->inode = NTFS_ROOT_DIR_INODE;
    ino->vol = vol;

    *Root = &ino->proto;

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

static EFI_STATUS read_mft_data(volume* vol, ATTRIBUTE_RECORD_HEADER* att) {
    EFI_STATUS Status;
    uint64_t next_vcn, current_lcn = 0, current_vcn;
    uint32_t cluster_size = vol->boot_sector->BytesPerSector * vol->boot_sector->SectorsPerCluster;
    uint8_t* stream;
    uint64_t max_cluster;

    if (att->FormCode != NONRESIDENT_FORM)
        return EFI_INVALID_PARAMETER;

    if (att->Flags & ATTRIBUTE_FLAG_ENCRYPTED)
        return EFI_INVALID_PARAMETER;

    if (att->Flags & ATTRIBUTE_FLAG_COMPRESSION_MASK)
        return EFI_INVALID_PARAMETER;

    next_vcn = att->Form.Nonresident.LowestVcn;
    stream = (uint8_t*)att + att->Form.Nonresident.MappingPairsOffset;

    max_cluster = att->Form.Nonresident.ValidDataLength / cluster_size;

    if (att->Form.Nonresident.ValidDataLength & (cluster_size - 1))
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

        InsertTailList(&vol->mft_mappings, &m->list_entry);

        if (next_vcn == max_cluster)
            break;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS read_mft(volume* vol) {
    EFI_STATUS Status;
    FILE_RECORD_SEGMENT_HEADER* mft;
    ATTRIBUTE_RECORD_HEADER* att;

    Status = bs->AllocatePool(EfiBootServicesData, vol->file_record_size, (void**)&mft);
    if (EFI_ERROR(Status))
        return Status;

    Status = vol->block->ReadBlocks(vol->block, vol->block->Media->MediaId,
                                    (vol->boot_sector->MFT * vol->boot_sector->BytesPerSector * vol->boot_sector->SectorsPerCluster) / vol->block->Media->BlockSize,
                                    vol->file_record_size, mft);
    if (EFI_ERROR(Status)) {
        bs->FreePool(mft);
        return Status;
    }

    if (mft->MultiSectorHeader.Signature != NTFS_FILE_SIGNATURE) {
        bs->FreePool(mft);
        return EFI_INVALID_PARAMETER;
    }

    Status = process_fixups(&mft->MultiSectorHeader, vol->file_record_size,
                            vol->boot_sector->BytesPerSector);
    if (EFI_ERROR(Status)) {
        bs->FreePool(mft);
        return Status;
    }

    // read DATA mappings

    att = (ATTRIBUTE_RECORD_HEADER*)((uint8_t*)mft + mft->FirstAttributeOffset);

    while (att->TypeCode != 0xffffffff) {
        if (att->TypeCode == DATA && att->NameLength == 0) {
            Status = read_mft_data(vol, att);
            if (EFI_ERROR(Status)) {
                bs->FreePool(mft);
                return Status;
            }

            break;
        }

        att = (ATTRIBUTE_RECORD_HEADER*)((uint8_t*)att + att->RecordLength);
    }

    bs->FreePool(mft);

    return EFI_SUCCESS;
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

    Status = read_mft(vol);
    if (EFI_ERROR(Status)) {
        do_print_error("read_mft", Status);

        while (!IsListEmpty(&vol->mft_mappings)) {
            mapping* m = _CR(vol->mft_mappings.Flink, mapping, list_entry);
            RemoveEntryList(&m->list_entry);
            bs->FreePool(m);
        }

        bs->FreePool(sb);
        bs->FreePool(vol);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    // FIXME - quibble protocol

    Status = bs->InstallProtocolInterface(&ControllerHandle, &fs_guid, EFI_NATIVE_INTERFACE, &vol->proto);
    if (EFI_ERROR(Status)) {
        do_print_error("InstallProtocolInterface", Status);

        while (!IsListEmpty(&vol->mft_mappings)) {
            mapping* m = _CR(vol->mft_mappings.Flink, mapping, list_entry);
            RemoveEntryList(&m->list_entry);
            bs->FreePool(m);
        }

        bs->FreePool(sb);
        bs->FreePool(vol);
        bs->CloseProtocol(ControllerHandle, &block_guid, This->DriverBindingHandle, ControllerHandle);
        bs->CloseProtocol(ControllerHandle, &disk_guid, This->DriverBindingHandle, ControllerHandle);
        return Status;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS EFIAPI drv_stop(EFI_DRIVER_BINDING_PROTOCOL* This, EFI_HANDLE ControllerHandle,
                                  UINTN NumberOfChildren, EFI_HANDLE* ChildHandleBuffer) {
    systable->ConOut->OutputString(systable->ConOut, L"drv_stop\r\n");
    // FIXME

    return EFI_INVALID_PARAMETER;
}

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
