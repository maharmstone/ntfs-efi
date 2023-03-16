#pragma once

#include <efi.h>

const char* error_string(EFI_STATUS Status);
char* stpcpy(char* dest, const char* src);
