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

#include "misc.h"
#include <stddef.h>
#include <stdint.h>

extern "C"
void* memset(void* s, int c, size_t n) {
    void* orig_s = s;

    // FIXME - faster if we make sure we're aligned (also in memcpy)?

#if __INTPTR_WIDTH__ == 64
    uint64_t v;

    v = 0;

    for (unsigned int i = 0; i < sizeof(uint64_t); i++) {
        v <<= 8;
        v |= c & 0xff;
    }

    while (n >= sizeof(uint64_t)) {
        *(uint64_t*)s = v;

        s = (uint8_t*)s + sizeof(uint64_t);
        n -= sizeof(uint64_t);
    }
#else
    uint32_t v;

    v = 0;

    for (unsigned int i = 0; i < sizeof(uint32_t); i++) {
        v <<= 8;
        v |= c & 0xff;
    }

    while (n >= sizeof(uint32_t)) {
        *(uint32_t*)s = v;

        s = (uint8_t*)s + sizeof(uint32_t);
        n -= sizeof(uint32_t);
    }
#endif

    while (n > 0) {
        *(uint8_t*)s = c;

        s = (uint8_t*)s + 1;
        n--;
    }

    return orig_s;
}

extern "C"
int memcmp(const void* s1, const void* s2, size_t n) {
#if __INTPTR_WIDTH__ == 64
    while (n > sizeof(uint64_t)) {
        uint64_t c1 = *(uint64_t*)s1;
        uint64_t c2 = *(uint64_t*)s2;

        if (c1 != c2)
            return c1 > c2 ? 1 : -1;

        s1 = (uint64_t*)s1 + 1;
        s2 = (uint64_t*)s2 + 1;
        n -= sizeof(uint64_t);
    }
#endif

    while (n > sizeof(uint32_t)) {
        uint32_t c1 = *(uint32_t*)s1;
        uint32_t c2 = *(uint32_t*)s2;

        if (c1 != c2)
            return c1 > c2 ? 1 : -1;

        s1 = (uint32_t*)s1 + 1;
        s2 = (uint32_t*)s2 + 1;
        n -= sizeof(uint32_t);
    }

    while (n > 0) {
        uint8_t c1 = *(uint8_t*)s1;
        uint8_t c2 = *(uint8_t*)s2;

        if (c1 != c2)
            return c1 > c2 ? 1 : -1;

        s1 = (uint8_t*)s1 + 1;
        s2 = (uint8_t*)s2 + 1;
        n--;
    }

    return 0;
}

extern "C"
void* memcpy(void* dest, const void* src, size_t n) {
    void* orig_dest = dest;

#if __INTPTR_WIDTH__ == 64
    while (n >= sizeof(uint64_t)) {
        *(uint64_t*)dest = *(uint64_t*)src;

        dest = (uint8_t*)dest + sizeof(uint64_t);
        src = (uint8_t*)src + sizeof(uint64_t);

        n -= sizeof(uint64_t);
    }
#endif

    while (n >= sizeof(uint32_t)) {
        *(uint32_t*)dest = *(uint32_t*)src;

        dest = (uint8_t*)dest + sizeof(uint32_t);
        src = (uint8_t*)src + sizeof(uint32_t);

        n -= sizeof(uint32_t);
    }

    while (n >= sizeof(uint16_t)) {
        *(uint16_t*)dest = *(uint16_t*)src;

        dest = (uint8_t*)dest + sizeof(uint16_t);
        src = (uint8_t*)src + sizeof(uint16_t);

        n -= sizeof(uint16_t);
    }

    while (n >= sizeof(uint8_t)) {
        *(uint8_t*)dest = *(uint8_t*)src;

        dest = (uint8_t*)dest + sizeof(uint8_t);
        src = (uint8_t*)src + sizeof(uint8_t);

        n -= sizeof(uint8_t);
    }

    return orig_dest;
}

const char* error_string(EFI_STATUS Status) {
    switch (Status) {
        case EFI_SUCCESS:
            return "EFI_SUCCESS";

        case EFI_LOAD_ERROR:
            return "EFI_LOAD_ERROR";

        case EFI_INVALID_PARAMETER:
            return "EFI_INVALID_PARAMETER";

        case EFI_UNSUPPORTED:
            return "EFI_UNSUPPORTED";

        case EFI_BAD_BUFFER_SIZE:
            return "EFI_BAD_BUFFER_SIZE";

        case EFI_BUFFER_TOO_SMALL:
            return "EFI_BUFFER_TOO_SMALL";

        case EFI_NOT_READY:
            return "EFI_NOT_READY";

        case EFI_DEVICE_ERROR:
            return "EFI_DEVICE_ERROR";

        case EFI_WRITE_PROTECTED:
            return "EFI_WRITE_PROTECTED";

        case EFI_OUT_OF_RESOURCES:
            return "EFI_OUT_OF_RESOURCES";

        case EFI_VOLUME_CORRUPTED:
            return "EFI_VOLUME_CORRUPTED";

        case EFI_VOLUME_FULL:
            return "EFI_VOLUME_FULL";

        case EFI_NO_MEDIA:
            return "EFI_NO_MEDIA";

        case EFI_MEDIA_CHANGED:
            return "EFI_MEDIA_CHANGED";

        case EFI_NOT_FOUND:
            return "EFI_NOT_FOUND";

        case EFI_ACCESS_DENIED:
            return "EFI_ACCESS_DENIED";

        case EFI_NO_RESPONSE:
            return "EFI_NO_RESPONSE";

        case EFI_NO_MAPPING:
            return "EFI_NO_MAPPING";

        case EFI_TIMEOUT:
            return "EFI_TIMEOUT";

        case EFI_NOT_STARTED:
            return "EFI_NOT_STARTED";

        case EFI_ALREADY_STARTED:
            return "EFI_ALREADY_STARTED";

        case EFI_ABORTED:
            return "EFI_ABORTED";

        case EFI_ICMP_ERROR:
            return "EFI_ICMP_ERROR";

        case EFI_TFTP_ERROR:
            return "EFI_TFTP_ERROR";

        case EFI_PROTOCOL_ERROR:
            return "EFI_PROTOCOL_ERROR";

        case EFI_INCOMPATIBLE_VERSION:
            return "EFI_INCOMPATIBLE_VERSION";

        case EFI_SECURITY_VIOLATION:
            return "EFI_SECURITY_VIOLATION";

        case EFI_CRC_ERROR:
            return "EFI_CRC_ERROR";

        case EFI_END_OF_MEDIA:
            return "EFI_END_OF_MEDIA";

        case EFI_END_OF_FILE:
            return "EFI_END_OF_FILE";

        case EFI_INVALID_LANGUAGE:
            return "EFI_INVALID_LANGUAGE";

        case EFI_COMPROMISED_DATA:
            return "EFI_COMPROMISED_DATA";

        default:
            return "(unknown error)";
    }
}

char* stpcpy(char* dest, const char* src) {
    while (*src != 0) {
        *dest = *src;
        dest++;
        src++;
    }

    *dest = 0;

    return dest;
}
