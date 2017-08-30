#include <sys/eventfd.h>

int main()
{
  eventfd(0, 0);
  return 0;
}
