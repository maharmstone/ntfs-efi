/*
 * lzx_common.h
 *
 * Declarations shared between LZX compression and decompression.
 */

#ifndef _LZX_COMMON_H
#define _LZX_COMMON_H

#include "lzx_constants.h"
#include "common_defs.h"

extern const int32_t lzx_offset_slot_base[LZX_MAX_OFFSET_SLOTS + 1];

extern const uint8_t lzx_extra_offset_bits[LZX_MAX_OFFSET_SLOTS];

extern unsigned
lzx_get_window_order(size_t max_bufsize);

extern unsigned
lzx_get_num_main_syms(unsigned window_order);

extern void
lzx_preprocess(uint8_t *data, uint32_t size);

extern void
lzx_postprocess(uint8_t *data, uint32_t size);

#endif /* _LZX_COMMON_H */
