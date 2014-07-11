/*
 * Try to set up the RTS to enable GC statistics collection, using a
 * weak symbol that can be overridden.

 * (Collecting stats has no apparent effect on performance; I'm a bit
 * mystified why it's not enabled by default.)
 */

#include "Rts.h"

#if defined(__GNUC__) || defined(__clang__)

char *ghc_rts_opts __attribute__((weak));

char *ghc_rts_opts = "-T";

#elif defined(_MSC_VER)

extern const char *criterion_ghc_rts_opts = "-T";
extern const char *ghc_rts_opts;

#pragma comment(linker, "/alternatename:_ghc_rts_opts=_criterion_ghc_rts_opts")

#endif

char *criterion_use_ghc_rts_opts(void)
{
    return ghc_rts_opts;
}
