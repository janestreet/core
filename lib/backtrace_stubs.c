/* Dumping of OCaml stack backtraces for x86-64.

   Much of this module is taken from the OCaml source.
*/

#include "config.h"
#if (defined JSC_LINUX_EXT && defined JSC_ARCH_x86_64)

#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef __x86_64__
  #error "backtrace_stubs.c is for x86-64 only"
#endif

typedef struct {
  char* buf;
  size_t size;
  size_t pos;  /* 0-based */
} extensible_buffer;

typedef struct {
  uintnat retaddr;
  unsigned short frame_size;
  unsigned short num_live;
  unsigned short live_ofs[1];
} frame_descr;

typedef struct link {
  void *data;
  struct link *next;
} link;

struct caml_context {
  char * bottom_of_stack;       /* beginning of Caml stack chunk */
  uintnat last_retaddr;         /* last return address in Caml code */
  value * gc_regs;              /* pointer to register block */
};

#define Hash_retaddr(addr) \
  (((uintnat)(addr) >> 3) & caml_frame_descriptors_mask)

extern frame_descr** caml_frame_descriptors __attribute__((weak));
extern int caml_frame_descriptors_mask __attribute__((weak));
extern void caml_init_frame_descriptors(void) __attribute__((weak));
extern char * caml_top_of_stack __attribute__((weak));
extern char * caml_bottom_of_stack __attribute__((weak));
extern uintnat caml_last_return_address __attribute__((weak));
extern value * caml_gc_regs;

/* These next two lines are the x86-64 specific part. */
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))

struct loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

static extensible_buffer*
extensible_buffer_create(void)
{
  extensible_buffer* buf;
  buf = malloc(sizeof(extensible_buffer));
  if (!buf) abort();
  buf->size = 1024;
  buf->buf = malloc(buf->size);
  buf->pos = 0;
  if (!buf->buf) abort();
  return buf;
}

static void
extensible_buffer_make_room(extensible_buffer* buf, size_t how_much)
{
  size_t space_available = buf->size - buf->pos;
  if (space_available < how_much) {
    buf->size = buf->size * 2;
    buf->buf = realloc(buf->buf, buf->size);
  }
}

static void
extensible_buffer_free(extensible_buffer* buf)
{
  free(buf->buf);
  free(buf);
}

static void
extensible_buffer_sprintf(extensible_buffer* buf, char* fmt, ...)
{
  va_list args;
  size_t size = strlen(fmt) * 2;
  int finished = 0;

  while (!finished) {
    int retval;
    extensible_buffer_make_room(buf, size);
    va_start(args, fmt);
    retval = vsnprintf(&buf->buf[buf->pos], size, fmt, args);
    va_end(args);
    if (retval < 0) abort();  /* highly unlikely */
    if ((size_t) retval >= size) {
      size *= 2;
    }
    else {
      finished = 1;
      buf->pos += retval;
    }
  }
}

static void extract_location_info(frame_descr * d,
                                  /*out*/ struct loc_info * li)
{
  uintnat infoptr;
  uint32 info1, info2;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if ((d->frame_size & 1) == 0) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    return;
  }
  /* Recover debugging info */
  infoptr = ((uintnat) d +
             sizeof(char *) + sizeof(short) + sizeof(short) +
             sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
            & -sizeof(frame_descr *);
  info1 = ((uint32 *)infoptr)[0];
  info2 = ((uint32 *)infoptr)[1];
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn kk
                          44       36         26                       2  0
                       (32+12)    (32+4)
     k ( 2 bits): 0 if it's a call, 1 if it's a raise
     n (24 bits): offset (in 4-byte words) of file name relative to infoptr
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 3) != 0;
  li->loc_filename = (char *) infoptr + (info1 & 0x3FFFFFC);
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

static void
backtrace_dump(extensible_buffer* buf)
{
  char * sp;
  uintnat retaddr;
  frame_descr * d;
  uintnat h;

  if (&caml_last_return_address == (void*) 0) {
    extensible_buffer_sprintf(buf, "no backtrace support for bytecode, sorry\n");
    return;
  }

  /* The stack and local roots */
  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;
  if (sp != NULL) {
    while (1) {
      struct loc_info li;
      int stop = 0;
      li.loc_filename = NULL;
      li.loc_lnum = 0;
      li.loc_startchr = 0;
      li.loc_endchr = 0;
      /* Find the descriptor corresponding to the return address */
      h = Hash_retaddr(retaddr);
      while(!stop) {
        d = caml_frame_descriptors[h];
        if (!d) {
          extensible_buffer_sprintf(buf,
            "sp %p            : cannot find frame descriptor, searching...\n", sp);
          stop = 1;
        }
        else {
          if (d->retaddr == retaddr) break;
          h = (h+1) & caml_frame_descriptors_mask;
        }
      }
      if (d) {
        extract_location_info(d, &li);
        extensible_buffer_sprintf(buf, "sp %p ip %p: ", (void*) sp, (void*) retaddr);
        if (li.loc_valid && li.loc_filename && li.loc_lnum) {
          extensible_buffer_sprintf(buf, "%s:%d, chars %d--%d\n",
                  li.loc_filename, li.loc_lnum,
                  li.loc_startchr, li.loc_endchr);
        }
        else {
          extensible_buffer_sprintf(buf, "<no debug info>\n");
        }
        if (d->frame_size != 0xFFFF) {
          /* Move to next frame */
  #ifndef Stack_grows_upwards
          sp += (d->frame_size & 0xFFFC);
  #else
          sp -= (d->frame_size & 0xFFFC);
  #endif
          retaddr = Saved_return_address(sp);
        } else {
          /* This marks the top of a stack chunk for an ML callback.
             Skip C portion of stack and continue with next ML stack chunk. */
          void* old_sp;
          struct caml_context * next_context = Callback_link(sp);
          old_sp = sp;
          sp = next_context->bottom_of_stack;
          if (sp) {
            extensible_buffer_sprintf(buf,
              "sp %p            : C code starts\n", old_sp);
            extensible_buffer_sprintf(buf,
              "sp %p            : C code ends\n", sp);
          }
          else {
            extensible_buffer_sprintf(buf,
              "sp %p            : C code until top of stack\n", old_sp);
          }
          retaddr = next_context->last_retaddr;
          /* A null sp means no more ML stack chunks; stop here. */
          if (sp == NULL) break;
        }
      }
      else {
        sp += 8;
        if (sp >= caml_top_of_stack - 8) {
          return;
        }
        retaddr = Saved_return_address(sp);
      }
    }
  }
}

CAMLprim value
backtrace_get(value unused)
{
  /* For use from backtrace.ml. */

  value retval;
  extensible_buffer* buf;

  unused = unused;

  buf = extensible_buffer_create();
  backtrace_dump(buf);
  retval = caml_copy_string(buf->buf);
  extensible_buffer_free(buf);

  return retval;
}

void
backtrace_dump_stderr(void)
{
  /* For use from gdb. */

  extensible_buffer* buf;
  buf = extensible_buffer_create();
  backtrace_dump(buf);
  fputs(buf->buf, stderr);
  fflush(stderr);
  extensible_buffer_free(buf);
}

#endif /* JSC_LINUX_EXT && JSC_ARCH_x86_64 */
