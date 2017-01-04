/*---------------------------------------------------------------------------

   int systemfunction_(char *comando)

   input: comando
   output: systemfunction_ (function)

   Executes a command specified in string by calling /bin/sh -c string

   char *comando -> command to be executed (do not forget to add the
                    suffix \0)
  ---------------------------------------------------------------------------*/
#include <stdlib.h>

int systemfunction_(const char *comando)
{
  int retorno;

  retorno=system(comando);
  return(retorno);
}
