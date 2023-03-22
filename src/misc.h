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

#include <efi.h>

const char* error_string(EFI_STATUS Status);
char* stpcpy(char* dest, const char* src);
