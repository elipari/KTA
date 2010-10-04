/*
! $Id: ieee_ibm.h,v 1.1 2002/07/31 10:37:18 agimicv5 Exp $
!
! ------------------------------------------------------------------------
!
! CHANGE LOG:
! who  when	vers.   what
!
! GCA  16/05/02  1.1    First version
!
! ------------------------------------------------------------------------
*/

/* prototypes of functions */

#ifndef _IEEE_IBM
#define _IEEE_IBM

#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined(__cplusplus) || defined(c_plusplus)
#include <iostream.h>
extern "C" {
#endif
 
/* ANSI C & C++ Prototypes */

extern void   ieee_ibm ( int in[], int out[], int nsamp );
extern void   ibm_ieee ( int in[], int out[], int nsamp );


#if defined(__cplusplus) || defined(c_plusplus)
}
/* C++ and not ANSI C */
#endif

#endif    /* _IEEE_IBM */

