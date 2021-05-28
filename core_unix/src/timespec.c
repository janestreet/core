#include <math.h>
#include <time.h>

struct timespec timespec_of_double(double seconds)
{
  struct timespec ts;

  ts.tv_sec = (time_t) floor(seconds);
  ts.tv_nsec = (long) (1e9 * (seconds - ts.tv_sec));

  return ts;
}

double timespec_to_double(struct timespec ts)
{
  return (double) ts.tv_sec + ((double) ts.tv_nsec / 1e9);
}
