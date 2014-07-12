#include "Rts.h"

StgWord64 criterion_rdtsc(void)
{
  StgWord32 hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((StgWord64) lo) | (((StgWord64) hi)<<32);
}
