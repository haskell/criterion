/*
 * Try to set up the RTS to enable GC statistics collection.
 */

#include "Rts.h"

void
criterion_initGCStatistics(void)
{
  /* Workaround for GHC #8754: if the GC stats aren't enabled because
   the compiler couldn't use -Bsymbolic to link the default hooks,
   then initialize them sensibly. See Note [-Bsymbolic and hooks] in
   Main.hs. */
  if (RtsFlags.GcFlags.giveStats == NO_GC_STATS) {
    RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
  }
}
