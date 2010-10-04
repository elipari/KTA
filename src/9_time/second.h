/* Timing modes for use in calls to second.
 */
#define SECOND_RT       (0)     /* uses real-time counter */
#define SECOND_US       (1)     /* user+system time from getrusage */
#define SECOND_U        (2)     /* user time from getrusage */
#define SECOND_S        (3)     /* system time from getrusage */
#define SECOND_WC       (-1)    /* wall clock time from gettimeofday */

/* C function prototypes for utility routines.
 */
extern  double  second(int tmode);

/* Fortran function prototypes for utility routines.
 */
extern  double  second_(int *tmode);

