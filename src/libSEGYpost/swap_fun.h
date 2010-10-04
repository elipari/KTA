/*
! $Id: swap_fun.h,v 1.1 2002/07/31 10:37:17 agimicv5 Exp $
!
! CHANGE LOG:
! who   when	 vers.  what
!
! GCA  16/05/02   1.1   Swap function routines in a separate file
!
*/

#ifndef _SWAP_FUN
#define _SWAP_FUN

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/* ANSI C & C++ Prototypes */

  extern float swap_long (float *fdato);
  extern void swap_array (float *farray, int dim);

#if defined(__cplusplus) || defined(c_plusplus)
}
/* C++ and not ANSI C */
#endif

#endif    /* _SWAP_FUN */
