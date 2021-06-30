#include <mach/mach.h>
#include <time.h>

void criterion_inittime(void) {}

double criterion_gettime(void)
{
    return clock_gettime_nsec_np(CLOCK_UPTIME_RAW) / 1e9;
}

static double to_double(time_value_t time)
{
    return time.seconds + time.microseconds / 1e6;
}

double criterion_getcputime(void)
{
    struct task_thread_times_info thread_info_data;
    mach_msg_type_number_t thread_info_count = TASK_THREAD_TIMES_INFO_COUNT;
    kern_return_t kr = task_info(mach_task_self(),
				 TASK_THREAD_TIMES_INFO,
				 (task_info_t) &thread_info_data,
				 &thread_info_count);
    return (to_double(thread_info_data.user_time) +
	    to_double(thread_info_data.system_time));
}
