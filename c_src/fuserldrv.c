#include "driver2pipe.h"
#include "fuserl.h"
#include "fuserl.c"
#include "fuserl_portability.c"

int 
main (int       argc,
      char**    argv)
{
  (void) argc;

  return driver_to_pipe_main (argv[0], driver_init ());
}
