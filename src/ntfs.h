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

#pragma pack(pop)

#define NTFS_FS_NAME "NTFS    "

#define NTFS_ROOT_DIR_INODE     5

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
