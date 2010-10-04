/*
! $Id: sgy_def.h,v 1.1 2002/07/31 10:37:17 agimicv5 Exp $
!
! CHANGE LOG:
! who  when	vers.   what
!
! GCA  16/05/02   1.1   all common definitions out of the routines
!
*/


#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include "ieee_ibm.h"

#define MAXPATHLEN     119      /* max path length - please dimension
                                   the fortran variable to MAXPATHLEN + 1 */

/* swap macros for big-little endian conversion */
#define SWAP4(N)  ( (( (N) & 0x00FF)       << 24) | \
                    (( (N) & 0xFF00)       <<  8) | \
                    (( (N) & 0xFF0000)     >>  8) | \
                    (( (N) & 0xFF000000)   >> 24)  )

#define SWAP2(N)  ( (( (N) & 0x00FF)       <<  8) | \
                    (( (N) & 0xFF00)       >>  8) )

