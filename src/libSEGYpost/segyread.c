/*
****************************************************************************
****************************** SEGYREAD.C *********************************
****************************************************************************
by C.Nardone (CRS4, Cagliari) from NBortolotti SEGYWRITE.C
Rel.:      Apr  4, 1996
Rev.:      Apr 23, 1996 (modified IBM-RISC-compatible) cmn
Rev.:	   Mar 20, 1997 (include Agip standard) LC
****************************************************************************

*/
/*
! $Id: segyread.c,v 1.1 2002/07/31 10:37:17 agimicv5 Exp $
!
! CHANGE LOG:
! who  when	vers.   what
!
! GCA  16/05/02   1.1   Erased pre-compilation directive CRAY
! GCA  16/05/02   1.1   Definitions and inclusions moved to sgy_def.h
! GCA  16/05/02   1.1   Erased DEBUG1 pre-compilation directive
! GCA  16/05/02   1.1   fflush call directed only to stdout
! GCA  26/03/02         Corrected bad dimensioning of buff_240 for AlphaSC
! GCA  11/02/02         Add the pre-compilation directive LINUX
! GCA  11/02/02         Change QLC to AlphaSC directive
! FVA  08/08/01         Add the SWAP4 and SWAP2 macros
! FVA  08/08/01         Add the swap_long and swap_array functions
! FVA  08/08/01         Add the pre-compilation directive QLC
!
! ------------------------------------------------------------------------
*/


#include "sgy_def.h"
#include "swap_fun.h"


/*
****************************************************************************
******************************** segyopenread ******************************
****************************************************************************
*/

#ifdef IBM
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
#if defined SGI || AlphaSC || LINUX
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

       for(indarray=0;indarray<3;indarray++)
        {
#if defined AlphaSC || LINUX
	  reelheader[indarray] = SWAP4(buff_400->i4[indarray]);
#else
	  reelheader[indarray] = buff_400->i4[indarray];
#endif
#ifdef  DEBUG
	  printf("buff400get: reelheader[indarray] = %d\n", reelheader[indarray]);
	  fflush(stdout);
#endif
        }

       for(indarray=3;indarray<27;indarray++)
        {
#if defined AlphaSC || LINUX
	  reelheader[indarray] = SWAP2(buff_400->i2[indarray+3]);
#else
	  reelheader[indarray] = buff_400->i2[indarray+3];
#endif
#ifdef  DEBUG
	  printf("buff400get: reelheader[indarray] = %d\n", reelheader[indarray]);
	  fflush(stdout);
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
#if defined SGI || AlphaSC || LINUX
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

       for(indarray=0;indarray<7;indarray++)
        {
#if defined AlphaSC || LINUX
	  traceheader[indarray] = SWAP4(buff_240->i4[indarray]);
#else
	  traceheader[indarray] = buff_240->i4[indarray];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }

       for(indarray=7;indarray<11;indarray++)
        {
#if defined AlphaSC || LINUX
           traceheader[indarray] = SWAP2(buff_240->i2[indarray+7]);
#else
           traceheader[indarray] = buff_240->i2[indarray+7];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }

       for(indarray=11;indarray<19;indarray++)
        {
#if defined AlphaSC || LINUX
           traceheader[indarray] = SWAP4(buff_240->i4[indarray-2]);
#else
           traceheader[indarray] = buff_240->i4[indarray-2];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }

       for(indarray=19;indarray<21;indarray++)
        {
#if defined AlphaSC || LINUX
           traceheader[indarray] = SWAP2(buff_240->i2[indarray+15]);
#else
           traceheader[indarray] = buff_240->i2[indarray+15];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }

       for(indarray=21;indarray<25;indarray++)
        {
#if defined AlphaSC || LINUX
           traceheader[indarray] = SWAP4(buff_240->i4[indarray-3]);
#else
           traceheader[indarray] = buff_240->i4[indarray-3];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }

       for(indarray=25;indarray<71;indarray++)
        {
#if defined AlphaSC || LINUX
           traceheader[indarray] = SWAP2(buff_240->i2[indarray+19]);
#else
           traceheader[indarray] = buff_240->i2[indarray+19];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }

/*      start Agip standard locations      */

       for(indarray=71;indarray<81;indarray++)
        {
#if defined AlphaSC || LINUX
           traceheader[indarray] = SWAP2(buff_240->i2[indarray+19]);
#else
           traceheader[indarray] = buff_240->i2[indarray+19];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }      
 
       for(indarray=81;indarray<91;indarray++)
        {
#if defined AlphaSC || LINUX
           traceheader[indarray] = SWAP4(buff_240->i4[indarray-31]);
#else
           traceheader[indarray] = buff_240->i4[indarray-31];
#endif
#ifdef  DEBUG
	  printf("buff240get: traceheader[indarray] = %d\n", traceheader[indarray]);
	  fflush(stdout);
	  getchar();
#endif
        }

       return;
}


/*
****************************************************************************
******************************** segygetheader *****************************
****************************************************************************
*/

#ifdef IBM
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
   long n;


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
   
#ifdef IBM
void segygettrace(fdesc,traceheader,tracedata)
#else
void segygettrace_(fdesc,traceheader,tracedata)
#endif
int  fdesc[1];
int  traceheader[];
float tracedata[];
{
    long i, nbytes;
    int nsamp_w;
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
    nsamp_w = traceheader[38];
    
    /* trace reading... */
    nbytes = nsamp_w * 4;
    
    /* read trace */
    i = read(fdesc[0],tracedata,nbytes);
    if (i != nbytes)
      {
	close(fdesc[0]);
	fdesc[0] = -1;
	return;
      }
    
#if defined AlphaSC || LINUX
    swap_array(tracedata, nsamp_w);
#endif

    ibm_ieee((int *)&tracedata[0], (int *)&tracedata[0], nsamp_w);
 
   
    /* return to fortran */
    return;
}
    

