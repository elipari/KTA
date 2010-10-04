#include        "second.h"
#include        <unistd.h>
#include        <sys/time.h>
#include        <sys/times.h>
#include        <sys/resource.h>

/* The same code is used for both C and Fortran entry points.
 */
#define WC_GUTS                                                         \
                                                                        \
  {                                                                     \
    struct timeval s_val;                                               \
                                                                        \
    gettimeofday(&s_val,0);                                             \
    time = ((double) s_val.tv_sec + 0.000001*s_val.tv_usec);            \
  }
  
#define US_GUTS                                                         \
                                                                        \
  {                                                                     \
    struct      rusage  ru;                                             \
    double      tu, ts;                                                 \
                                                                        \
    getrusage(RUSAGE_SELF,&ru);                                         \
                                                                        \
    tu = ru.ru_utime.tv_sec + 1.0e-6*ru.ru_utime.tv_usec;               \
    ts = ru.ru_stime.tv_sec + 1.0e-6*ru.ru_stime.tv_usec;               \
                                                                        \
    time =  (tu + ts);                                                  \
  }

#define U_GUTS                                                          \
                                                                        \
  {                                                                     \
    struct      rusage  ru;                                             \
    double      tu;                                                     \
                                                                        \
    getrusage(RUSAGE_SELF,&ru);                                         \
                                                                        \
    time = ru.ru_utime.tv_sec + 1.0e-6*ru.ru_utime.tv_usec;             \
  }

#define S_GUTS                                                          \
                                                                        \
  {                                                                     \
    struct      rusage  ru;                                             \
    double      ts;                                                     \
                                                                        \
    getrusage(RUSAGE_SELF,&ru);                                         \
                                                                        \
    time = ru.ru_stime.tv_sec + 1.0e-6*ru.ru_stime.tv_usec;             \
  }

#include <sys/types.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/syssgi.h>
#include <sys/immu.h>
#include <stdio.h>

#define TICKUNITS               (1.0e-12)
#define TRUE                    (1)
#define FALSE                   (0)
#define IO4_TIMER_IS_64BIT      (TRUE)

#if IO4_TIMER_IS_64BIT
typedef unsigned long   long iotimer_t;
#else
typedef unsigned int    iotimer_t;
#endif

#define RT_GUTS                                                         \
                                                                        \
  {                                                                     \
    static int                  first = TRUE;                           \
    static volatile iotimer_t   *iotimer_addr, counter_value0;          \
    static double               tick;                                   \
    unsigned int                cycleval;                               \
    volatile iotimer_t          counter_value;                          \
                                                                        \
    if (first) {                                                        \
                                                                        \
      __psunsigned_t    phys_addr, raddr, poffmask;                     \
      int               fd;                                             \
                                                                        \
      poffmask       = getpagesize() - 1;                               \
      phys_addr      = syssgi(SGI_QUERY_CYCLECNTR, &cycleval);          \
      raddr          = phys_addr & ~poffmask;                           \
      fd             = open("/dev/mmem", O_RDONLY);                     \
      iotimer_addr   = (volatile iotimer_t *) mmap(NULL, poffmask,      \
                                                   PROT_READ,           \
                                                   MAP_PRIVATE, fd,     \
                                                   (__psint_t) raddr);  \
      iotimer_addr   = (iotimer_t *) ((__psunsigned_t) iotimer_addr +   \
                                      (phys_addr & poffmask));          \
      tick           = cycleval * TICKUNITS;                            \
      counter_value0 = *iotimer_addr;                                   \
      first          = FALSE;                                           \
      return (0.0);                                                     \
    }                                                                   \
                                                                        \
    counter_value = *iotimer_addr;                                      \
                                                                        \
    time = ((counter_value - counter_value0) * tick);                   \
  }

#define SECOND_GUTS(TMODE)                                              \
                                                                        \
  double        time;                                                   \
                                                                        \
  switch (TMODE) {                                                      \
  case SECOND_RT:                                                       \
    RT_GUTS;                                                            \
    break;                                                              \
  case SECOND_US:                                                       \
    US_GUTS;                                                            \
    break;                                                              \
  case SECOND_WC:                                                       \
    WC_GUTS;                                                            \
    break;                                                              \
  case SECOND_U:                                                        \
    U_GUTS;                                                             \
    break;                                                              \
  case SECOND_S:                                                        \
    S_GUTS;                                                             \
    break;                                                              \
  default:                                                              \
    RT_GUTS;                                                            \
    break;                                                              \
  }                                                                     \
                                                                        \
  return (time);

/* Returns the current value of the real-time timer, wall clock timer, or
 * user+system timer depending on the value of tmode: 0 means the real-time
 * timer, less than zero the wall-clock timer, and greater than zero
 * user+system time.  C entry point.
 */
double
mysecond(

int     tmode)

{
  SECOND_GUTS(tmode);
}

/* Returns the current value of the real-time timer, wall clock timer, or
 * user+system timer depending on the value of tmode: 0 means the real-time
 * timer, less than zero the wall-clock timer, and greater than zero
 * user+system time.  C entry point.
 */
double
second_(

int     *ptmode)

{
  SECOND_GUTS(*ptmode);
}

