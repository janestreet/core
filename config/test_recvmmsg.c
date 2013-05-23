/* This file is only compiled to test the existence of the recvmmsg
   system call */

#define _GNU_SOURCE
#include <sys/socket.h>
int main () { return recvmmsg(0, 0, 0, 0, 0); }
