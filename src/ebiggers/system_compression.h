/*
 * system_compression.h - declarations for accessing System Compressed files
 *
 * Copyright (C) 2015 Eric Biggers
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdbool.h>
#include <sys/types.h>

/* System compressed file access  */

struct ntfs_system_decompression_ctx;

extern void
ntfs_close_system_decompression_ctx(struct ntfs_system_decompression_ctx *ctx);

/* XPRESS decompression  */

#define XPRESS_NUM_CHARS	256
#define XPRESS_NUM_SYMBOLS	512
#define XPRESS_MAX_CODEWORD_LEN	15

#define XPRESS_MIN_MATCH_LEN	3

#define DECODE_TABLE_ALIGNMENT 16

struct xpress_decompressor {
	union {
		uint16_t decode_table[2566] __attribute__((aligned(DECODE_TABLE_ALIGNMENT)));
		uint8_t lens[XPRESS_NUM_SYMBOLS];
	};
	uint16_t working_space[2 * (XPRESS_MAX_CODEWORD_LEN + 1) + XPRESS_NUM_SYMBOLS];
} __attribute__((aligned(DECODE_TABLE_ALIGNMENT)));

extern struct xpress_decompressor *xpress_allocate_decompressor(void);

extern int xpress_decompress(struct xpress_decompressor *decompressor,
		      const void *compressed_data, size_t compressed_size,
		      void *uncompressed_data, size_t uncompressed_size);

extern void xpress_free_decompressor(struct xpress_decompressor *decompressor);

/* LZX decompression  */

struct lzx_decompressor;

extern bool
lzx_init_decompressor(size_t max_block_size, struct lzx_decompressor *d);

extern int lzx_decompress(struct lzx_decompressor *decompressor,
			  const void *compressed_data, size_t compressed_size,
			  void *uncompressed_data, size_t uncompressed_size);

#ifdef __cplusplus
}
#endif
