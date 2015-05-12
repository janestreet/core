#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* The code for crc_octets and the constants involded is taken from
   RFC2440 http://sunsite.icm.edu.pl/gnupg/rfc2440-6.html
 */
#define CRC24_INIT 0xb704ceL
#define CRC24_POLY 0x1864cfbL

typedef long crc24;
crc24 crc_octets(unsigned char *octets, size_t len) {
  crc24 crc = CRC24_INIT;
  int i;

  while (len--) {
    crc ^= (*octets++) << 16;
    for (i = 0; i < 8; i++) {
      crc <<= 1;
      if (crc & 0x1000000)
        crc ^= CRC24_POLY;
    }
  }
  return crc & 0xffffffL;
}

value caml_crc_octets(value v_str)
{
  unsigned char *octets = (unsigned char *) String_val(v_str);
  size_t len = caml_string_length(v_str);
  long crc = crc_octets(octets, len);
  return Val_int(crc);
}

/* Copyright abandoned; this code is in the public domain. */
/* Provided to GNUnet by peter@horizon.com */

/**
 * @file util/crc32.c
 * @brief implementation of CRC32
 **/

/* #include "gnunet_util.h" */

#define POLYNOMIAL (uLong)0xedb88320
typedef unsigned int uLong;
static uLong crc_table[256];

/*
 * This routine writes each crc_table entry exactly once,
 * with the ccorrect final value.  Thus, it is safe to call
 * even on a table that someone else is using concurrently.
 */
static void make_crc_table() {
  unsigned int i, j;
  uLong h = 1;
  crc_table[0] = 0;
  for (i = 128; i; i >>= 1) {
    h = (h >> 1) ^ ((h & 1) ? POLYNOMIAL : 0);
    /* h is now crc_table[i] */
    for (j = 0; j < 256; j += 2*i)
      crc_table[i+j] = crc_table[j] ^ h;
  }
}

/*
 * This computes the standard preset and inverted CRC, as used
 * by most networking standards.  Start by passing in an initial
 * chaining value of 0, and then pass in the return value from the
 * previous crc32() call.  The final return value is the CRC.
 * Note that this is a little-endian CRC, which is best used with
 * data transmitted lsbit-first, and it should, itself, be appended
 * to data in little-endian byte and bit order to preserve the
 * property of detecting all burst errors of length 32 bits or less.
 */
static uLong crc32(uLong crc, char const *buf, size_t len) {
  if (crc_table[255] == 0)
    make_crc_table();
  crc ^= 0xffffffff;
  while (len--)
    crc = (crc >> 8) ^ crc_table[(crc ^ *buf++) & 0xff];
  return crc ^ 0xffffffff;
}

value caml_crc32(value v_str) {
  char *octets = String_val(v_str);
  size_t len = caml_string_length(v_str);
  uLong crc = crc32(0, octets, len);
  return caml_copy_int64(crc);
}
