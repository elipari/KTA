/*
****************************************************************************
****************************** SEGYREAD.C *********************************
****************************************************************************
by C.Nardone (CRS4, Cagliari) from NBortolotti SEGYWRITE.C
Rel.:      Apr  4, 1996
Rev.:      Apr 23, 1996 (modified IBM-RISC-compatible) cmn
Rev.:      Mar 20, 1997 (include Agip standard) LC
****************************************************************************

*/



#include <stdio.h>
#include <fcntl.h>
#include <string.h>

#define MAXPATHLEN     119      /* max path length - please dimension
                                   the fortran variable to MAXPATHLEN + 1 */

/*
****************************************************************************
******************************** segyopenread ******************************
****************************************************************************
*/

#ifdef CRAY
void SEGYOPENREAD(fdesc,filename)
#elif IBM
void segyopenread(fdesc,filename)
#else
void segyopenread_(fdesc,filename)
#endif
int *fdesc;
char *filename;
{
   int i;
   /* rtrim filename */
   for(i=0;i<MAXPATHLEN;i++)
   {
      if(filename[i] == 32) filename[i] = 0;
   }
   filename[MAXPATHLEN] = 0;
   /* opens the file */
   *fdesc = open(filename, 0, 0);
}



/*
****************************************************************************
******************************** buff400get ********************************
****************************************************************************
*/

void buff400get(reelheader, buff_400)
int reelheader[];
#ifdef CRAY
char *buff_400;
#elif SGI
union {
       short  i2[200];
       int    i4[100];
       float  r4[100];
      }  *buff_400;
#else
union {
       short  i2[200];
       long   i4[100];
       float  r4[100];
      }  *buff_400;
#endif

{
       int indarray;

#ifdef CRAY
           fortran int CRAY2IBM();
           int ind, k, ierr, type, num, ioff, tracetmp[1];
           char buff[4];
            tracetmp[0] = 0;
           ioff = 0;
           num = 1;
#endif

       for(indarray=0;indarray<3;indarray++)
        {
#ifndef CRAY
           reelheader[indarray] = buff_400->i4[indarray];
#endif
        }

       for(indarray=3;indarray<27;indarray++)
        {
#ifndef CRAY
           reelheader[indarray] = buff_400->i2[indarray+3];
#endif
        }

       return;
}

/*
****************************************************************************
******************************** buff240get ********************************
****************************************************************************
*/

void buff240get(traceheader, buff_240)
int traceheader[];
#ifdef CRAY
char *buff_240;
#elif SGI
union {
       short  i2[120];
       int    i4[60];
       float  r4[60];
      }  *buff_240;
#else
union {
       short  i2[120];
       long   i4[60];
       float  r4[60];
      }  *buff_240;
#endif

{
       int indarray;

#ifdef CRAY
           fortran int CRAY2IBM();
           int ind, k, ierr, type, num, ioff, tracetmp[1];
           char buff[4];
            tracetmp[0] = 0;
           ioff = 0;
           num = 1;
#endif

       for(indarray=0;indarray<7;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i4[indarray];
#endif
        }

       for(indarray=7;indarray<11;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i2[indarray+7];
#endif
        }

       for(indarray=11;indarray<19;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i4[indarray-2];
#endif
        }

       for(indarray=19;indarray<21;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i2[indarray+15];
#endif
        }

       for(indarray=21;indarray<25;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i4[indarray-3];
#endif
        }

       for(indarray=25;indarray<71;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i2[indarray+19];
#endif
        }

/*      start Agip standard locations      */

       for(indarray=71;indarray<81;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i2[indarray+19];
#endif
        }

       for(indarray=81;indarray<91;indarray++)
        {
#ifndef CRAY
           traceheader[indarray] = buff_240->i4[indarray-31];
#endif
        }

       return;
}


/*
****************************************************************************
******************************** segygetheader *****************************
****************************************************************************
*/

#ifdef CRAY
void SEGYGETHEADER(fdesc, reelheader)
#elif IBM
void segygetheader(fdesc, reelheader)
#else
void segygetheader_(fdesc, reelheader)
#endif
int reelheader[];
int *fdesc;
/*
   reelheader (from fortran: integer * 27)
   fdesc      (to fortran: must be used to append traces)
   filename   (from fortran: MUST be always the last argument of the call)
*/

{
   int indarray;
   char buff_3200[3201];
    short buff_400[200];
#ifdef CRAY
    int n;
#else
    long n;
#endif

   /* pointer to BOF */
/* lseek(*fdesc,0L,0);  */

   /* ... reading */
   n = read (*fdesc, buff_3200, 3200);
   if (n != 3200)
        {
        close(*fdesc);
        *fdesc = -1;
        return;
        }
   n = read (*fdesc, buff_400, 400);
   if (n != 400)
        {
        close(*fdesc);
        *fdesc = -1;
        return;
        }

   /* get reel header */
   buff400get(reelheader, buff_400);

   /* return to fortran */
   return;
}



/*
****************************************************************************
******************************** segygetttrace *****************************
****************************************************************************
*/

#ifdef CRAY
void SEGYGETTRACE(fdesc,traceheader,tracedata)
#elif IBM
void segygettrace(fdesc,traceheader,tracedata)
#else
void segygettrace_(fdesc,traceheader,tracedata)
#endif
int  fdesc[1];
int  traceheader[];
float tracedata[];

{
#ifdef CRAY
    int nsamp_w, i, nbytes;
#else
    long i, nbytes;
    int nsamp_w;
    short int tempo;
#endif
    short buff_240[120];
   /* header reading... */
   i = read(fdesc[0],buff_240,240);
    if (i != 240)
        {
        close(fdesc[0]);
        fdesc[0] = -1;
        return;
        }

   /* trace header conversion */
   buff240get(traceheader,buff_240);
   /* number of samples from trace header */
   tempo=(short int)traceheader[38];
#ifdef INTEL
   byteswap(&tempo,sizeof(short int),1);
#endif
  nsamp_w=tempo;
   /* trace reading... */
   nbytes = nsamp_w * 4;
   i = read(fdesc[0],tracedata,nbytes);
   if (i != nbytes)
        {
        close(fdesc[0]);
        fdesc[0] = -1;
        return;
        }
#ifdef INTEL
   byteswap(&tracedata[0],4,nsamp_w);
#endif
   ibm_ieee(&tracedata[0], &tracedata[0], nsamp_w);
   /* return to fortran */
   return;
}


























