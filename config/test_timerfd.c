#include <sys/timerfd.h>

int main()
{
  int fd;
  struct itimerspec it;
  fd = timerfd_create(CLOCK_REALTIME, 0);
  timerfd_settime(fd, 0, &it, NULL);
  return 0;
}
