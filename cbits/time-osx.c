#include <mach/mach.h>
#include <mach/mach_time.h>

static mach_timebase_info_data_t timebase_info;
static double timebase_recip;

void criterion_inittime(void)
{
    mach_timebase_info(&timebase_info);
    timebase_recip = (timebase_info.denom / timebase_info.numer) / 1e9;
}

double criterion_gettime(void)
{
    return mach_absolute_time() * timebase_recip;
}
