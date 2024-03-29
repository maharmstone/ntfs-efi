/* Copyright (c) Mark Harmstone 2023
 *
 * This file is part of ntfs-efi.
 *
 * ntfs-efi is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licence as published by
 * the Free Software Foundation, either version 2 of the Licence, or
 * (at your option) any later version.
 *
 * ntfs-efi is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public Licence for more details.
 *
 * You should have received a copy of the GNU General Public Licence
 * along with ntfs-efi.  If not, see <http://www.gnu.org/licenses/>. */

#pragma once

enum class ntfs_attribute : uint32_t {
    STANDARD_INFORMATION = 0x10,
    ATTRIBUTE_LIST = 0x20,
    FILE_NAME = 0x30,
    VOLUME_VERSION = 0x40,
    SECURITY_DESCRIPTOR = 0x50,
    VOLUME_NAME = 0x60,
    VOLUME_INFORMATION = 0x70,
    DATA = 0x80,
    INDEX_ROOT = 0x90,
    INDEX_ALLOCATION = 0xA0,
    BITMAP = 0xB0,
    REPARSE_POINT = 0xC0,
    EA_INFORMATION = 0xD0,
    EA = 0xE0,
    PROPERTY_SET = 0xF0,
    LOGGED_UTILITY_STREAM = 0x100,
};

enum class NTFS_ATTRIBUTE_FORM : uint8_t {
    RESIDENT_FORM = 0,
    NONRESIDENT_FORM = 1
};

#pragma pack(push,1)

struct NTFS_BOOT_SECTOR {
    uint8_t Jmp[3];
    uint8_t FsName[8];
    uint16_t BytesPerSector;
    uint8_t SectorsPerCluster;
    uint16_t ReservedSectors;
    uint8_t Unused1[5];
    uint8_t Media;
    uint8_t Unused2[2];
    uint16_t SectorsPerTrack;
    uint16_t Heads;
    uint32_t HiddenSectors;
    uint32_t Unused3;
    uint32_t Unknown;
    uint64_t TotalSectors;
    uint64_t MFT;
    uint64_t MFTMirr;
    int8_t ClustersPerMFTRecord;
    uint8_t Padding1[3];
    int8_t ClustersPerIndexRecord;
    uint8_t Padding2[3];
    uint64_t SerialNumber;
    uint32_t Checksum;
};

// https://docs.microsoft.com/en-us/windows/win32/devnotes/multi-sector-header
struct MULTI_SECTOR_HEADER {
    uint32_t Signature;
    uint16_t UpdateSequenceArrayOffset;
    uint16_t UpdateSequenceArraySize;
};

// https://docs.microsoft.com/en-us/windows/win32/devnotes/mft-segment-reference
struct MFT_SEGMENT_REFERENCE {
    uint64_t SegmentNumber : 48;
    uint64_t SequenceNumber : 16;
};

// based on https://docs.microsoft.com/en-us/windows/win32/devnotes/file-record-segment-header and
// http://www.cse.scu.edu/~tschwarz/coen252_07Fall/Lectures/NTFS.html
struct FILE_RECORD_SEGMENT_HEADER {
    MULTI_SECTOR_HEADER MultiSectorHeader;
    uint64_t LogFileSequenceNumber;
    uint16_t SequenceNumber;
    uint16_t HardLinkCount;
    uint16_t FirstAttributeOffset;
    uint16_t Flags;
    uint32_t EntryUsedSize;
    uint32_t EntryAllocatedSize;
    MFT_SEGMENT_REFERENCE BaseFileRecordSegment;
    uint16_t NextAttributeID;
};

struct ATTRIBUTE_RECORD_HEADER {
    enum ntfs_attribute TypeCode;
    uint16_t RecordLength;
    uint16_t Unknown;
    enum NTFS_ATTRIBUTE_FORM FormCode;
    uint8_t NameLength;
    uint16_t NameOffset;
    uint16_t Flags;
    uint16_t Instance;
    union {
        struct {
            uint32_t ValueLength;
            uint16_t ValueOffset;
            uint8_t Reserved[2];
        } Resident;
        struct {
            uint64_t LowestVcn;
            uint64_t HighestVcn;
            uint16_t MappingPairsOffset;
            uint16_t CompressionUnit;
            uint32_t Padding;
            uint64_t AllocatedLength;
            uint64_t FileSize;
            uint64_t ValidDataLength;
            uint64_t TotalAllocated;
        } Nonresident;
    } Form;
};

// https://flatcap.org/linux-ntfs/ntfs/attributes/standard_information.html

struct STANDARD_INFORMATION {
    int64_t CreationTime;
    int64_t LastAccessTime;
    int64_t LastWriteTime;
    int64_t ChangeTime;
    uint32_t FileAttributes;
    uint32_t MaximumVersions;
    uint32_t VersionNumber;
    uint32_t ClassId;
    uint32_t OwnerId;
    uint32_t SecurityId;
    uint64_t QuotaCharged;
    uint64_t USN;
};

// https://flatcap.org/linux-ntfs/ntfs/concepts/node_header.html

struct index_node_header {
    uint32_t first_entry;
    uint32_t total_size;
    uint32_t allocated_size;
    uint32_t flags;
};

// https://flatcap.org/linux-ntfs/ntfs/concepts/index_entry.html

#define INDEX_ENTRY_SUBNODE     1
#define INDEX_ENTRY_LAST        2

struct index_entry {
    MFT_SEGMENT_REFERENCE file_reference;
    uint16_t entry_length;
    uint16_t stream_length;
    uint32_t flags;
};

// https://flatcap.org/linux-ntfs/ntfs/attributes/index_root.html

struct index_root {
    enum ntfs_attribute attribute_type;
    uint32_t collation_rule;
    uint32_t bytes_per_index_record;
    uint8_t clusters_per_index_record;
    uint8_t padding[3];
    index_node_header node_header;
    index_entry entries[1];
};

// https://flatcap.org/linux-ntfs/ntfs/concepts/index_record.html

struct index_record {
    MULTI_SECTOR_HEADER MultiSectorHeader;
    uint64_t sequence_number;
    uint64_t vcn;
    index_node_header header;
    uint16_t update_sequence;
};

#define INDEX_RECORD_MAGIC 0x58444e49 // "INDX"

// https://flatcap.org/linux-ntfs/ntfs/attributes/file_name.html

enum class file_name_type : uint8_t {
    POSIX = 0,
    WINDOWS = 1,
    DOS = 2,
    WINDOWS_AND_DOS = 3
};

struct FILE_NAME {
    MFT_SEGMENT_REFERENCE Parent;
    int64_t CreationTime;
    int64_t LastAccessTime;
    int64_t LastWriteTime;
    int64_t ChangeTime;
    uint64_t AllocationSize;
    uint64_t EndOfFile;
    uint32_t FileAttributes;
    uint32_t EaSize;
    uint8_t FileNameLength;
    file_name_type Namespace;
    char16_t FileName[1];
};

// https://flatcap.org/linux-ntfs/ntfs/attributes/attribute_list.html

struct attribute_list_entry {
    enum ntfs_attribute type;
    uint16_t record_length;
    uint8_t name_length;
    uint8_t name_offset;
    uint64_t starting_vcn;
    MFT_SEGMENT_REFERENCE file_reference;
    uint16_t instance;
};

struct reparse_point_header {  // edited form of REPARSE_DATA_BUFFER
    uint32_t ReparseTag;
    uint16_t ReparseDataLength;
    uint16_t Reserved;
    uint8_t DataBuffer[1];
};

static const uint32_t WOF_CURRENT_VERSION = 1;

static const uint32_t WOF_PROVIDER_WIM = 1;
static const uint32_t WOF_PROVIDER_FILE = 2;

struct wof_external_info { // WOF_EXTERNAL_INFO in winioctl.h
    uint32_t Version;
    uint32_t Provider;
};

static const uint32_t FILE_PROVIDER_CURRENT_VERSION = 1;

static const uint32_t FILE_PROVIDER_COMPRESSION_XPRESS4K = 0;
static const uint32_t FILE_PROVIDER_COMPRESSION_LZX = 1;
static const uint32_t FILE_PROVIDER_COMPRESSION_XPRESS8K = 2;
static const uint32_t FILE_PROVIDER_COMPRESSION_XPRESS16K = 3;

struct file_provider_external_info_v0 { // FILE_PROVIDER_EXTERNAL_INFO_V0 in winioctl.h
    uint32_t Version;
    uint32_t Algorithm;
};

#pragma pack(pop)

#define NTFS_FS_NAME "NTFS    "

#define NTFS_MFT_INODE          0
#define NTFS_ROOT_DIR_INODE     5
#define NTFS_UPCASE_INODE       10

#define NTFS_FILE_SIGNATURE     0x454c4946 // "FILE"

// https://docs.microsoft.com/en-us/windows/win32/devnotes/attribute-record-header
#define ATTRIBUTE_FLAG_COMPRESSION_MASK 0x00ff
#define ATTRIBUTE_FLAG_ENCRYPTED 0x4000

#define FILE_ATTRIBUTE_READONLY             0x00000001
#define FILE_ATTRIBUTE_HIDDEN               0x00000002
#define FILE_ATTRIBUTE_SYSTEM               0x00000004
#define FILE_ATTRIBUTE_DIRECTORY            0x00000010
#define FILE_ATTRIBUTE_ARCHIVE              0x00000020
#define FILE_ATTRIBUTE_DIRECTORY_MFT        0x10000000

static const uint32_t IO_REPARSE_TAG_WOF = 0x80000017;
