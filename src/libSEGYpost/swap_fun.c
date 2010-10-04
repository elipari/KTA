/*
! $Id: swap_fun.c,v 1.1 2002/07/31 10:37:17 agimicv5 Exp $
!
! CHANGE LOG:
! who  when	vers.   what
!
! GCA  16/05/02   1.1   New file containing swap function routines
!
! ------------------------------------------------------------------------
*/


#include "sgy_def.h"


/******************* SWAP LONG ***************************************/

float swap_long (fdato)
float *fdato;
{
  unsigned char first, second, third, fourth;
  unsigned char *pol;

  pol = (unsigned char *)fdato;
  
  first   = *pol;
  second  = *(pol+1);
  third   = *(pol+2);
  fourth  = *(pol+3);

  *pol     = fourth;
  *(pol+1) = third;
  *(pol+2) = second;
  *(pol+3) = first;

  return (*fdato);
}

/******************* SWAP_ARRAY **************************************/

void swap_array (farray,dim)
     float *farray;
     int   dim;
{
  int i;

  for(i=0;i<dim;i++)
    farray[i] = swap_long(&farray[i]);
}
