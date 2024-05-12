#include "Rts.h"

#if darwin_HOST_OS

#include <mach/mach_time.h>

StgWord64 criterion_rdtsc(void)
{
  return mach_absolute_time();
}

#elif aarch64_HOST_ARCH

StgWord64 criterion_rdtsc(void)
{
  StgWord64 ret;
  __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r"(ret));
  return ret;
}

#elif x86_64_HOST_ARCH || i386_HOST_ARCH

StgWord64 criterion_rdtsc(void)
{
  StgWord32 hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((StgWord64) lo) | (((StgWord64) hi)<<32);
}

#elif linux_HOST_OS

/*
 * This should work on all Linux.
 *
 * Technique by Austin Seipp found here:
 *
 * http://neocontra.blogspot.com/2013/05/user-mode-performance-counters-for.html
 */

#include <unistd.h>
#include <asm-generic/unistd.h>
#include <linux/perf_event.h>

static int fddev = -1;
__attribute__((constructor))
static void
init(void)
{
  static struct perf_event_attr attr;
  attr.type = PERF_TYPE_HARDWARE;
  attr.config = PERF_COUNT_HW_CPU_CYCLES;
  fddev = syscall (__NR_perf_event_open, &attr, 0, -1, -1, 0);
}

__attribute__((destructor))
static void
fini(void)
{
  close(fddev);
}

StgWord64
criterion_rdtsc (void)
{
  StgWord64 result = 0;
  if (read (fddev, &result, sizeof(result)) < sizeof(result))
    return 0;
  return result;
}

#else

#error Unsupported OS/architecture/compiler!

#endif
