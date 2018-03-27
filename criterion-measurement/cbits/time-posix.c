#include <time.h>

void criterion_inittime(void)
{
}

double criterion_gettime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_MONOTONIC, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}


double criterion_getcputime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}
