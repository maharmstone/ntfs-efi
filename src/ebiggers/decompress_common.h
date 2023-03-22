/*
 * decompress_common.h
 *
 * Header for decompression code shared by multiple compression formats.
 *
 * The following copying information applies to this specific source code file:
 *
 * Written in 2012-2016 by Eric Biggers <ebiggers3@gmail.com>
 *
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide via the Creative Commons Zero 1.0 Universal Public Domain
 * Dedication (the "CC0").
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the CC0 for more details.
 *
 * You should have received a copy of the CC0 along with this software; if not
 * see <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

#ifndef _DECOMPRESS_COMMON_H
#define _DECOMPRESS_COMMON_H

#include <string.h>

#include "common_defs.h"

/******************************************************************************/
/*                   Input bitstream for XPRESS and LZX                       */
/*----------------------------------------------------------------------------*/

/* Structure that encapsulates a block of in-memory data being interpreted as a
 * stream of bits, optionally with interwoven literal bytes.  Bits are assumed
 * to be stored in little endian 16-bit coding units, with the bits ordered high
 * to low.  */
struct input_bitstream {

	/* Bits that have been read from the input buffer.  The bits are
	 * left-justified; the next bit is always bit 31.  */
	uint32_t bitbuf;

	/* Number of bits currently held in @bitbuf.  */
	uint32_t bitsleft;

	/* Pointer to the next byte to be retrieved from the input buffer.  */
	const uint8_t *next;

	/* Pointer past the end of the input buffer.  */
	const uint8_t *end;
};

/* Initialize a bitstream to read from the specified input buffer.  */
static forceinline void
init_input_bitstream(struct input_bitstream *is, const void *buffer, uint32_t size)
{
	is->bitbuf = 0;
	is->bitsleft = 0;
	is->next = buffer;
	is->end = is->next + size;
}

/* Note: for performance reasons, the following methods don't return error codes
 * to the caller if the input buffer is overrun.  Instead, they just assume that
 * all overrun data is zeroes.  This has no effect on well-formed compressed
 * data.  The only disadvantage is that bad compressed data may go undetected,
 * but even this is irrelevant if higher level code checksums the uncompressed
 * data anyway.  */

/* Ensure the bit buffer variable for the bitstream contains at least @num_bits
 * bits.  Following this, bitstream_peek_bits() and/or bitstream_remove_bits()
 * may be called on the bitstream to peek or remove up to @num_bits bits.  */
static forceinline void
bitstream_ensure_bits(struct input_bitstream *is, const unsigned num_bits)
{
	/* This currently works for at most 17 bits.  */

	if (is->bitsleft >= num_bits)
		return;

	if (unlikely(is->end - is->next < 2))
		goto overflow;

	is->bitbuf |= (uint32_t)*((uint16_t*)is->next) << (16 - is->bitsleft);
	is->next += 2;
	is->bitsleft += 16;

	if (unlikely(num_bits == 17 && is->bitsleft == 16)) {
		if (unlikely(is->end - is->next < 2))
			goto overflow;

		is->bitbuf |= (uint32_t)*((uint16_t*)(is->next));
		is->next += 2;
		is->bitsleft = 32;
	}

	return;

overflow:
	is->bitsleft = 32;
}

/* Return the next @num_bits bits from the bitstream, without removing them.
 * There must be at least @num_bits remaining in the buffer variable, from a
 * previous call to bitstream_ensure_bits().  */
static forceinline uint32_t
bitstream_peek_bits(const struct input_bitstream *is, const unsigned num_bits)
{
	return (is->bitbuf >> 1) >> (sizeof(is->bitbuf) * 8 - num_bits - 1);
}

/* Remove @num_bits from the bitstream.  There must be at least @num_bits
 * remaining in the buffer variable, from a previous call to
 * bitstream_ensure_bits().  */
static forceinline void
bitstream_remove_bits(struct input_bitstream *is, unsigned num_bits)
{
	is->bitbuf <<= num_bits;
	is->bitsleft -= num_bits;
}

/* Remove and return @num_bits bits from the bitstream.  There must be at least
 * @num_bits remaining in the buffer variable, from a previous call to
 * bitstream_ensure_bits().  */
static forceinline uint32_t
bitstream_pop_bits(struct input_bitstream *is, unsigned num_bits)
{
	uint32_t bits = bitstream_peek_bits(is, num_bits);
	bitstream_remove_bits(is, num_bits);
	return bits;
}

/* Read and return the next @num_bits bits from the bitstream.  */
static forceinline uint32_t
bitstream_read_bits(struct input_bitstream *is, unsigned num_bits)
{
	bitstream_ensure_bits(is, num_bits);
	return bitstream_pop_bits(is, num_bits);
}

/* Read and return the next literal byte embedded in the bitstream.  */
static forceinline uint8_t
bitstream_read_byte(struct input_bitstream *is)
{
	if (unlikely(is->end == is->next))
		return 0;
	return *is->next++;
}

/* Read and return the next 16-bit integer embedded in the bitstream.  */
static forceinline uint16_t
bitstream_read_u16(struct input_bitstream *is)
{
	uint16_t v;

	if (unlikely(is->end - is->next < 2))
		return 0;
	v = *(uint16_t*)is->next;
	is->next += 2;
	return v;
}

/* Read and return the next 32-bit integer embedded in the bitstream.  */
static forceinline uint32_t
bitstream_read_u32(struct input_bitstream *is)
{
	uint32_t v;

	if (unlikely(is->end - is->next < 4))
		return 0;
	v = *(uint32_t*)is->next;
	is->next += 4;
	return v;
}

/* Read into @dst_buffer an array of literal bytes embedded in the bitstream.
 * Return 0 if there were enough bytes remaining in the input, otherwise -1. */
static forceinline int
bitstream_read_bytes(struct input_bitstream *is, void *dst_buffer, size_t count)
{
	if (unlikely(is->end - is->next < count))
		return -1;
	memcpy(dst_buffer, is->next, count);
	is->next += count;
	return 0;
}

/* Align the input bitstream on a coding-unit boundary.  */
static forceinline void
bitstream_align(struct input_bitstream *is)
{
	is->bitsleft = 0;
	is->bitbuf = 0;
}

/******************************************************************************/
/*                             Huffman decoding                               */
/*----------------------------------------------------------------------------*/

/*
 * Required alignment for the Huffman decode tables.  We require this alignment
 * so that we can fill the entries with vector or word instructions and not have
 * to deal with misaligned buffers.
 */
#define DECODE_TABLE_ALIGNMENT 16

/*
 * Each decode table entry is 16 bits divided into two fields: 'symbol' (high 12
 * bits) and 'length' (low 4 bits).  The precise meaning of these fields depends
 * on the type of entry:
 *
 * Root table entries which are *not* subtable pointers:
 *	symbol: symbol to decode
 *	length: codeword length in bits
 *
 * Root table entries which are subtable pointers:
 *	symbol: index of start of subtable
 *	length: number of bits with which the subtable is indexed
 *
 * Subtable entries:
 *	symbol: symbol to decode
 *	length: codeword length in bits, minus the number of bits with which the
 *		root table is indexed
 */
#define DECODE_TABLE_SYMBOL_SHIFT  4
#define DECODE_TABLE_MAX_SYMBOL	   ((1 << (16 - DECODE_TABLE_SYMBOL_SHIFT)) - 1)
#define DECODE_TABLE_MAX_LENGTH    ((1 << DECODE_TABLE_SYMBOL_SHIFT) - 1)
#define DECODE_TABLE_LENGTH_MASK   DECODE_TABLE_MAX_LENGTH
#define MAKE_DECODE_TABLE_ENTRY(symbol, length) \
	(((symbol) << DECODE_TABLE_SYMBOL_SHIFT) | (length))

/*
 * Read and return the next Huffman-encoded symbol from the given bitstream
 * using the given decode table.
 *
 * If the input data is exhausted, then the Huffman symbol will be decoded as if
 * the missing bits were all zeroes.
 *
 * XXX: This is mostly duplicated in lzms_decode_huffman_symbol() in
 * lzms_decompress.c; keep them in sync!
 */
static forceinline unsigned
read_huffsym(struct input_bitstream *is, const uint16_t decode_table[],
	     unsigned table_bits, unsigned max_codeword_len)
{
	unsigned entry;
	unsigned symbol;
	unsigned length;

	/* Preload the bitbuffer with 'max_codeword_len' bits so that we're
	 * guaranteed to be able to fully decode a codeword. */
	bitstream_ensure_bits(is, max_codeword_len);

	/* Index the root table by the next 'table_bits' bits of input. */
	entry = decode_table[bitstream_peek_bits(is, table_bits)];

	/* Extract the "symbol" and "length" from the entry. */
	symbol = entry >> DECODE_TABLE_SYMBOL_SHIFT;
	length = entry & DECODE_TABLE_LENGTH_MASK;

	/* If the root table is indexed by the full 'max_codeword_len' bits,
	 * then there cannot be any subtables, and this will be known at compile
	 * time.  Otherwise, we must check whether the decoded symbol is really
	 * a subtable pointer.  If so, we must discard the bits with which the
	 * root table was indexed, then index the subtable by the next 'length'
	 * bits of input to get the real entry. */
	if (max_codeword_len > table_bits &&
	    entry >= (1U << (table_bits + DECODE_TABLE_SYMBOL_SHIFT)))
	{
		/* Subtable required */
		bitstream_remove_bits(is, table_bits);
		entry = decode_table[symbol + bitstream_peek_bits(is, length)];
		symbol = entry >> DECODE_TABLE_SYMBOL_SHIFT;
		length = entry & DECODE_TABLE_LENGTH_MASK;
	}

	/* Discard the bits (or the remaining bits, if a subtable was required)
	 * of the codeword. */
	bitstream_remove_bits(is, length);

	/* Return the decoded symbol. */
	return symbol;
}

/*
 * The DECODE_TABLE_ENOUGH() macro evaluates to the maximum number of decode
 * table entries, including all subtable entries, that may be required for
 * decoding a given Huffman code.  This depends on three parameters:
 *
 *	num_syms: the maximum number of symbols in the code
 *	table_bits: the number of bits with which the root table will be indexed
 *	max_codeword_len: the maximum allowed codeword length in the code
 *
 * Given these parameters, the utility program 'enough' from zlib, when passed
 * the three arguments 'num_syms', 'table_bits', and 'max_codeword_len', will
 * compute the maximum number of entries required.  This has already been done
 * for the combinations we need and incorporated into the macro below so that
 * the mapping can be done at compilation time.  If an unknown combination is
 * used, then a compilation error will result.  To fix this, use 'enough' to
 * find the missing value and add it below.  If that still doesn't fix the
 * compilation error, then most likely a constraint would be violated by the
 * requested parameters, so they cannot be used, at least without other changes
 * to the decode table --- see DECODE_TABLE_SIZE().
 */
#define DECODE_TABLE_ENOUGH(num_syms, table_bits, max_codeword_len) ( \
	((num_syms) == 8 && (table_bits) == 7 && (max_codeword_len) == 15) ? 128 : \
	((num_syms) == 8 && (table_bits) == 5 && (max_codeword_len) == 7) ? 36 : \
	((num_syms) == 8 && (table_bits) == 6 && (max_codeword_len) == 7) ? 66 : \
	((num_syms) == 8 && (table_bits) == 7 && (max_codeword_len) == 7) ? 128 : \
	((num_syms) == 20 && (table_bits) == 5 && (max_codeword_len) == 15) ? 1062 : \
	((num_syms) == 20 && (table_bits) == 6 && (max_codeword_len) == 15) ? 582 : \
	((num_syms) == 20 && (table_bits) == 7 && (max_codeword_len) == 15) ? 390 : \
	((num_syms) == 54 && (table_bits) == 9 && (max_codeword_len) == 15) ? 618 : \
	((num_syms) == 54 && (table_bits) == 10 && (max_codeword_len) == 15) ? 1098 : \
	((num_syms) == 249 && (table_bits) == 9 && (max_codeword_len) == 16) ? 878 : \
	((num_syms) == 249 && (table_bits) == 10 && (max_codeword_len) == 16) ? 1326 : \
	((num_syms) == 249 && (table_bits) == 11 && (max_codeword_len) == 16) ? 2318 : \
	((num_syms) == 256 && (table_bits) == 9 && (max_codeword_len) == 15) ? 822 : \
	((num_syms) == 256 && (table_bits) == 10 && (max_codeword_len) == 15) ? 1302 : \
	((num_syms) == 256 && (table_bits) == 11 && (max_codeword_len) == 15) ? 2310 : \
	((num_syms) == 512 && (table_bits) == 10 && (max_codeword_len) == 15) ? 1558 : \
	((num_syms) == 512 && (table_bits) == 11 && (max_codeword_len) == 15) ? 2566 : \
	((num_syms) == 512 && (table_bits) == 12 && (max_codeword_len) == 15) ? 4606 : \
	((num_syms) == 656 && (table_bits) == 10 && (max_codeword_len) == 16) ? 1734 : \
	((num_syms) == 656 && (table_bits) == 11 && (max_codeword_len) == 16) ? 2726 : \
	((num_syms) == 656 && (table_bits) == 12 && (max_codeword_len) == 16) ? 4758 : \
	((num_syms) == 799 && (table_bits) == 9 && (max_codeword_len) == 15) ? 1366 : \
	((num_syms) == 799 && (table_bits) == 10 && (max_codeword_len) == 15) ? 1846 : \
	((num_syms) == 799 && (table_bits) == 11 && (max_codeword_len) == 15) ? 2854 : \
	-1)

extern int
make_huffman_decode_table(uint16_t decode_table[], unsigned num_syms,
			  unsigned table_bits, const uint8_t lens[],
			  unsigned max_codeword_len, uint16_t working_space[]);

/******************************************************************************/
/*                             LZ match copying                               */
/*----------------------------------------------------------------------------*/

static forceinline void
copy_word_unaligned(const void *src, void *dst)
{
	*(machine_word_t*)dst = *(machine_word_t*)src;
}

static forceinline machine_word_t
repeat_u16(uint16_t b)
{
	machine_word_t v = b;

	STATIC_ASSERT(WORDBITS == 32 || WORDBITS == 64);
	v |= v << 16;
	v |= v << ((WORDBITS == 64) ? 32 : 0);
	return v;
}

static forceinline machine_word_t
repeat_byte(uint8_t b)
{
	return repeat_u16(((uint16_t)b << 8) | b);
}

/*
 * Copy an LZ77 match of 'length' bytes from the match source at 'out_next -
 * offset' to the match destination at 'out_next'.  The source and destination
 * may overlap.
 *
 * This handles validating the length and offset.  It is validated that the
 * beginning of the match source is '>= out_begin' and that end of the match
 * destination is '<= out_end'.  The return value is 0 if the match was valid
 * (and was copied), otherwise -1.
 *
 * 'min_length' is a hint which specifies the minimum possible match length.
 * This should be a compile-time constant.
 */
static forceinline int
lz_copy(uint32_t length, uint32_t offset, uint8_t *out_begin, uint8_t *out_next, uint8_t *out_end,
	uint32_t min_length)
{
	const uint8_t *src;
	uint8_t *end;

	/* Validate the offset. */
	if (unlikely(offset > out_next - out_begin))
		return -1;

	/*
	 * Fast path: copy a match which is no longer than a few words, is not
	 * overlapped such that copying a word at a time would produce incorrect
	 * results, and is not too close to the end of the buffer.  Note that
	 * this might copy more than the length of the match, but that's okay in
	 * this scenario.
	 */
	src = out_next - offset;
	if (UNALIGNED_ACCESS_IS_FAST && length <= 3 * WORDBYTES &&
	    offset >= WORDBYTES && out_end - out_next >= 3 * WORDBYTES)
	{
		copy_word_unaligned(src + WORDBYTES*0, out_next + WORDBYTES*0);
		copy_word_unaligned(src + WORDBYTES*1, out_next + WORDBYTES*1);
		copy_word_unaligned(src + WORDBYTES*2, out_next + WORDBYTES*2);
		return 0;
	}

	/* Validate the length.  This isn't needed in the fast path above, due
	 * to the additional conditions tested, but we do need it here. */
	if (unlikely(length > out_end - out_next))
		return -1;
	end = out_next + length;

	/*
	 * Try to copy one word at a time.  On i386 and x86_64 this is faster
	 * than copying one byte at a time, unless the data is near-random and
	 * all the matches have very short lengths.  Note that since this
	 * requires unaligned memory accesses, it won't necessarily be faster on
	 * every architecture.
	 *
	 * Also note that we might copy more than the length of the match.  For
	 * example, if a word is 8 bytes and the match is of length 5, then
	 * we'll simply copy 8 bytes.  This is okay as long as we don't write
	 * beyond the end of the output buffer, hence the check for (out_end -
	 * end >= WORDBYTES - 1).
	 */
	if (UNALIGNED_ACCESS_IS_FAST && likely(out_end - end >= WORDBYTES - 1))
	{
		if (offset >= WORDBYTES) {
			/* The source and destination words don't overlap. */
			do {
				copy_word_unaligned(src, out_next);
				src += WORDBYTES;
				out_next += WORDBYTES;
			} while (out_next < end);
			return 0;
		} else if (offset == 1) {
			/* Offset 1 matches are equivalent to run-length
			 * encoding of the previous byte.  This case is common
			 * if the data contains many repeated bytes. */
			machine_word_t v = repeat_byte(*(out_next - 1));
			do {
				*(machine_word_t*)out_next = v;
				src += WORDBYTES;
				out_next += WORDBYTES;
			} while (out_next < end);
			return 0;
		}
		/*
		 * We don't bother with special cases for other 'offset <
		 * WORDBYTES', which are usually rarer than 'offset == 1'.
		 * Extra checks will just slow things down.  Actually, it's
		 * possible to handle all the 'offset < WORDBYTES' cases using
		 * the same code, but it still becomes more complicated doesn't
		 * seem any faster overall; it definitely slows down the more
		 * common 'offset == 1' case.
		 */
	}

	/* Fall back to a bytewise copy.  */
	if (min_length >= 2)
		*out_next++ = *src++;
	if (min_length >= 3)
		*out_next++ = *src++;
	if (min_length >= 4)
		*out_next++ = *src++;
	do {
		*out_next++ = *src++;
	} while (out_next != end);
	return 0;
}

#endif /* _DECOMPRESS_COMMON_H */
