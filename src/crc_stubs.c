#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unix_utils.h"

/* This file is based on code with the following header: */
/* Copyright abandoned; this code is in the public domain. */
/* Provided to GNUnet by peter@horizon.com */

/**
 * @file util/crc32.c
 * @brief implementation of CRC32
 **/

/* #include "gnunet_util.h" */

#define POLYNOMIAL ((unsigned int) 0xedb88320)
static unsigned int crc_table[256];

/*
 * This routine writes each crc_table entry exactly once, with the
 * correct final value.  Thus, it is safe to call even on a table that
 * someone else is using concurrently.
 */
static void make_crc_table()
{
  unsigned int i, j;
  unsigned int h = 1;
  crc_table[0] = 0;
  for (i = 128; i; i >>= 1) {
    h = (h >> 1) ^ ((h & 1) ? POLYNOMIAL : 0);
    /* h is now crc_table[i] */
    for (j = 0; j < 256; j += 2*i)
      crc_table[i+j] = crc_table[j] ^ h;
  }
}

/*
 * This computes the standard preset and inverted CRC, as used by most
 * networking standards.  Start by passing in an initial chaining
 * value of 0, and then pass in the return value from the previous
 * crc32() call.  The final return value is the CRC.  Note that this
 * is a little-endian CRC, which is best used with data transmitted
 * lsbit-first, and it should, itself, be appended to data in
 * little-endian byte and bit order to preserve the property of
 * detecting all burst errors of length 32 bits or less.
 */
static unsigned int crc32(unsigned int crc, char const *buf, size_t len)
{
  if (crc_table[255] == 0)
    make_crc_table();
  crc ^= 0xffffffff;
  while (len--)
    crc = (crc >> 8) ^ crc_table[(crc ^ *buf++) & 0xff];
  return crc ^ 0xffffffff;
}

value core_crc_string_crc32 (value v_str)
{
  char *octets = String_val (v_str);
  size_t len = caml_string_length (v_str);
  unsigned int crc = crc32 (0, octets, len);
  return caml_alloc_int63 (crc);
}

value core_crc_bigstring_crc32 (value v_bstr, value v_pos, value v_len)
{
  char *octets = get_bstr (v_bstr, v_pos);
  size_t len = Long_val (v_len);
  unsigned int crc = crc32 (0, octets, len);
  return caml_alloc_int63 (crc);
}
