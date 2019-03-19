#include "HsFFI.h"
#include "Rts.h"

int main(int argc, char* argv[]) {
  RtsConfig config = defaultRtsConfig;

  config.keep_cafs = 1;

  config.rts_opts_enabled = RtsOptsAll;

  extern StgClosure ZCMain_main_closure;
  hs_main(argc, argv, &ZCMain_main_closure, config);
}
