#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "../src/nanosecond_stat.h"

int main() {
  struct stat buf;
  double a, m, c;
  a = (double)buf.NSEC(a);
  m = (double)buf.NSEC(m);
  c = (double)buf.NSEC(c);
  return 0;
}
