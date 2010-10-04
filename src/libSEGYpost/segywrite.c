/*
****************************************************************************
****************************** SEGYWRITE.C *********************************
****************************************************************************
by NBortolotti (sgeo59) RICE/RISG int. 35008 (except for APPL's routines)
Rel.:      Feb 11, 1994
Rev.:      Mar  1, 1994 (tested on SUN and SILICON)
Rev.:      Mar 31, 1994 (modified for CRAY)
Rev.:      Apr  6, 1994 (first CRAY-compatible (!) release)
Rev.:      Apr  4, 1996 (modified SGI-POWER-compatible) cmn - Cazzola
Rev.:      Apr 23, 1996 (modified IBM-RISC-compatible) cmn
Rev.:      Mar 20, 1997 (include Agip standard) LC
****************************************************************************

*/
/*
! $Id: segywrite.c,v 1.2 2002/08/20 14:26:41 agimicv5 Exp $
!
! CHANGE LOG:
! who  when	vers.   what
!
! GCA  16/05/02   1.1   Erased pre-compilation directive CRAY
! GCA  16/05/02   1.1   Definitions and inclusions moved to sgy_def.h
! GCA  16/05/02   1.1.  Added the pre-compilation directives AlphaSC and LINUX
!
! --------------------------------------------------------------------------
*/
/*
DESCRIPTION
Le routines invocabili da FORTRAN sono quattro:
segyopen(fdesc,dataset)
             per aprire il dataset (se preesistente) o crearlo
segyclose(fdesc)
             per chiudere il dataset
segyputheader(fdesc,reelheader)
             per scrivere o modificare la Ebcdic e binary
             header. In caso di creazione di un file
             va invocata SUBITO DOPO segyopen.
segyputtrace(fdesc,traceheader,tracedata)
             per appendere una traccia al dataset. tracedata viene
             restituito IN FLOATING POINT IBM.

Si dovra' avere cura di effettuare le seguenti dichiarazioni fortran:

        integer reelheader(27)
        integer traceheader(91)
        real    tracedata(<dimensione traccia>)
        character*<MAXPATHLEN+1> dataset
        integer fdesc
        external segyopen     
        external segyclose     
        external segyputheader 
        external segyputtrace 

Sequenza tipica:
        dataset = '...'
        call segyopen(fdesc, dataset)
        reelheader(...) = ...
        call segyputheader(fdesc,reelheader)
        do traccia = ..., ...
           traceheader(...) = ...
           tracedata(...)   = ...
           call segyputtrace(fdesc,traceheader,tracedata)
        enddo
        call segyclose(fdesc)
  
Gli arrays reelheader e traceheader contengono, nello stesso 
ordine delle specifiche SEG, i parametri rispettivamente della
REEL BINARY HEADER e della TRACE BINARY HEADER.
Ai parametri RACCOMANDATI dalle specifiche sono stati assegnati,
nel file segyheader.h (che va #incluso nel sorgente FORTRAN),
degli opportuni mnemonici.
Le Agip standard locations concernenti i campi R*4 non sono attivati, 
tuttavia il codice e' gia' predisposto.

****************************************************************************

*/


#include "sgy_def.h"
#include "swap_fun.h"


/*
****************************************************************************
******************************** buff240set ********************************
****************************************************************************
*/

void buff240set(traceheader, buff_240)
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
           buff_240->i4[indarray] = SWAP4(traceheader[indarray]);
#elif SGI
           buff_240->i4[indarray] = traceheader[indarray];
#else
           buff_240->i4[indarray] = (long) traceheader[indarray];
#endif
	}

       for(indarray=7;indarray<11;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_240->i2[indarray+7] = SWAP2((short) traceheader[indarray]);
#else
           buff_240->i2[indarray+7] = (short) traceheader[indarray];
#endif 
	}

       for(indarray=11;indarray<19;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_240->i4[indarray-2] = SWAP4(traceheader[indarray]);
#elif SGI
           buff_240->i4[indarray-2] = traceheader[indarray];
#else
           buff_240->i4[indarray-2] = (long) traceheader[indarray];
#endif
	}

       for(indarray=19;indarray<21;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_240->i2[indarray+15] = SWAP2((short) traceheader[indarray]);
#else
           buff_240->i2[indarray+15] = (short) traceheader[indarray];
#endif
	}

       for(indarray=21;indarray<25;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_240->i4[indarray-3] = SWAP4(traceheader[indarray]);
#elif SGI
           buff_240->i4[indarray-3] = traceheader[indarray];
#else
           buff_240->i4[indarray-3] = (long) traceheader[indarray];
#endif
	}

       for(indarray=25;indarray<71;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_240->i2[indarray+19] = SWAP2((short) traceheader[indarray]);
#else
           buff_240->i2[indarray+19] = (short) traceheader[indarray];
#endif
	}

       /*  start Agip standard locations    */

       for(indarray=71;indarray<81;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_240->i2[indarray+19] = SWAP2((short) traceheader[indarray]);
#else
           buff_240->i2[indarray+19] = (short) traceheader[indarray];
#endif
	}

       for(indarray=81;indarray<91;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_240->i4[indarray-31] = SWAP4(traceheader[indarray]);
#elif SGI
           buff_240->i4[indarray-31] = traceheader[indarray];
#else
           buff_240->i4[indarray-31] = (long) traceheader[indarray];
#endif
	}

       return;
}


/*
****************************************************************************
******************************** buff3200set *******************************
****************************************************************************
*/

void buff3200set(buff_3200)
char buff_3200[];
{
    strncpy ( &buff_3200[   0], "C 1 CLIENT                        COMPAN", 40);
    strncpy ( &buff_3200[  40], "Y                       CREW NO         ", 40);
    strncpy ( &buff_3200[  80], "C 2 LINE            AREA                ", 40);
    strncpy ( &buff_3200[ 120], "        MAP ID                          ", 40);
    strncpy ( &buff_3200[ 160], "C 3 REEL NO           DAY-START OF REEL ", 40);
    strncpy ( &buff_3200[ 200], "    YEAR      OBSERVER                  ", 40);
    strncpy ( &buff_3200[ 240], "C 4 INSTRUMENT: MFG            MODEL    ", 40);
    strncpy ( &buff_3200[ 280], "        SERIAL NO                       ", 40);
    strncpy ( &buff_3200[ 320], "C 5 DATA TRACES/RECORD        AUXILIARY ", 40);
    strncpy ( &buff_3200[ 360], "TRACES/RECORD         CDP FOLD          ", 40);
    strncpy ( &buff_3200[ 400], "C 6 SAMPLE INTERNAL         SAMPLES/TRAC", 40);
    strncpy ( &buff_3200[ 440], "E       BITS/IN      BYTES/SAMPLE       ", 40);
    strncpy ( &buff_3200[ 480], "C 7 RECORDING FORMAT        FORMAT THIS ", 40);
    strncpy ( &buff_3200[ 520], "REEL        MEASUREMENT SYSTEM          ", 40);
    strncpy ( &buff_3200[ 560], "C 8 SAMPLE CODE: FLOATING PT     FIXED P", 40);
    strncpy ( &buff_3200[ 600], "T     FIXED PT-GAIN     CORRELATED      ", 40);
    strncpy ( &buff_3200[ 640], "C 9 GAIN  TYPE: FIXED     BINARY     FLO", 40);
    strncpy ( &buff_3200[ 680], "ATING POINT     OTHER                   ", 40);
    strncpy ( &buff_3200[ 720], "C10 FILTERS: ALIAS     HZ  NOTCH     HZ ", 40);
    strncpy ( &buff_3200[ 760], " BAND     -     HZ  SLOPE    -    DB/OCT", 40);
    strncpy ( &buff_3200[ 800], "C11 SOURCE: TYPE            NUMBER/POINT", 40);
    strncpy ( &buff_3200[ 840], "        POINT INTERVAL                  ", 40);
    strncpy ( &buff_3200[ 880], "C12     PATTERN:                        ", 40);
    strncpy ( &buff_3200[ 920], "   LENGTH        WIDTH                  ", 40);
    strncpy ( &buff_3200[ 960], "C13 SWEEP: START     HZ  END     HZ  LEN", 40);
    strncpy ( &buff_3200[1000], "GTH      MS  CHANNEL NO     TYPE        ", 40);
    strncpy ( &buff_3200[1040], "C14 TAPER: START LENGTH       MS  END LE", 40);
    strncpy ( &buff_3200[1080], "NGTH       MS  TYPE                     ", 40);
    strncpy ( &buff_3200[1120], "C15 SPREAD: PFFSET        MAX DISTANCE  ", 40);
    strncpy ( &buff_3200[1160], "      GROUP INTERVAL                    ", 40);
    strncpy ( &buff_3200[1200], "C16 GEOPHONES: PER GROUP     SPACING    ", 40);
    strncpy ( &buff_3200[1240], " FREQUENCY     MFG          MODEL       ", 40);
    strncpy ( &buff_3200[1280], "C17     PATTERN:                        ", 40);
    strncpy ( &buff_3200[1320], "   LENGTH        WIDTH                  ", 40);
    strncpy ( &buff_3200[1360], "C18 TRACES SORTED BY: RECORD     CDP    ", 40);
    strncpy ( &buff_3200[1400], " OTHER                                  ", 40);
    strncpy ( &buff_3200[1440], "C19 AMPLITUDE RECOVERY: NONE      SPHERI", 40);
    strncpy ( &buff_3200[1480], "CAL DIV       AGC    OTHER              ", 40);
    strncpy ( &buff_3200[1520], "C20 MAP PROJECTION                      ", 40);
    strncpy ( &buff_3200[1560], "ZONE ID       COORDINATE UNITS          ", 40);
    strncpy ( &buff_3200[1600], "C21                                     ", 40);
    strncpy ( &buff_3200[1640], "                                        ", 40);
    strncpy ( &buff_3200[1680], "C22                                     ", 40);
    strncpy ( &buff_3200[1720], "                                        ", 40);
    strncpy ( &buff_3200[1760], "C23                                     ", 40);
    strncpy ( &buff_3200[1800], "                                        ", 40);
    strncpy ( &buff_3200[1840], "C24                                     ", 40);
    strncpy ( &buff_3200[1880], "                                        ", 40);
    strncpy ( &buff_3200[1920], "C25                                     ", 40);
    strncpy ( &buff_3200[1960], "                                        ", 40);
    strncpy ( &buff_3200[2000], "C26                                     ", 40);
    strncpy ( &buff_3200[2040], "                                        ", 40);
    strncpy ( &buff_3200[2080], "C27                                     ", 40);
    strncpy ( &buff_3200[2120], "                                        ", 40);
    strncpy ( &buff_3200[2160], "C28                                     ", 40);
    strncpy ( &buff_3200[2200], "                                        ", 40);
    strncpy ( &buff_3200[2240], "C29                                     ", 40);
    strncpy ( &buff_3200[2280], "                                        ", 40);
    strncpy ( &buff_3200[2320], "C30                                     ", 40);
    strncpy ( &buff_3200[2360], "                                        ", 40);
    strncpy ( &buff_3200[2400], "C31                                     ", 40);
    strncpy ( &buff_3200[2440], "                                        ", 40);
    strncpy ( &buff_3200[2480], "C32                                     ", 40);
    strncpy ( &buff_3200[2520], "                                        ", 40);
    strncpy ( &buff_3200[2560], "C33                                     ", 40);
    strncpy ( &buff_3200[2600], "                                        ", 40);
    strncpy ( &buff_3200[2640], "C34                                     ", 40);
    strncpy ( &buff_3200[2680], "                                        ", 40);
    strncpy ( &buff_3200[2720], "C35                                     ", 40);
    strncpy ( &buff_3200[2760], "                                        ", 40);
    strncpy ( &buff_3200[2800], "C36                                     ", 40);
    strncpy ( &buff_3200[2840], "                                        ", 40);
    strncpy ( &buff_3200[2880], "C37                                     ", 40);
    strncpy ( &buff_3200[2920], "                                        ", 40);
    strncpy ( &buff_3200[2960], "C38                                     ", 40);
    strncpy ( &buff_3200[3000], "                                        ", 40);
    strncpy ( &buff_3200[3040], "C39                                     ", 40);
    strncpy ( &buff_3200[3080], "                                        ", 40);
    strncpy ( &buff_3200[3120], "C40 END EBCDIC                          ", 40);
    strncpy ( &buff_3200[3160], "                                        ", 40);
return;
}




/*
****************************************************************************
******************************** buff400set ********************************
****************************************************************************
*/

void buff400set(reelheader, buff_400)
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
           buff_400->i4[indarray] = SWAP4(reelheader[indarray]);
#elif SGI
           buff_400->i4[indarray] = reelheader[indarray];
#else
           buff_400->i4[indarray] = (long) reelheader[indarray];
#endif 
	}

       for(indarray=3;indarray<27;indarray++)
	{
#if defined AlphaSC || LINUX
           buff_400->i2[indarray+3] = SWAP2((short) reelheader[indarray]);
#else
           buff_400->i2[indarray+3] = (short) reelheader[indarray];
#endif 
	}

       /* zeroes optional information */
       for(indarray=30;indarray<200;indarray++)
	{
           buff_400->i2[indarray] = 0;
	}

       return;
}




/*
****************************************************************************
******************************** ebcdic_ascii ******************************
****************************************************************************
*/


/*
  * * * * * *                                           * * * * * * * *
  *    C    *                                           *EBCDIC_ASCII02*
  * * * * * *                                           * * * * * * * *
                         EBCDIC_ASCII02

     Conversione da formato EBCDIC a formato ASCII e viceversa.

---------------------- Discussione Tecnica ---------------------------


----------------------------------------------------------------------

AUTORE .............. Marco Bellani
DOCUMENTATA ......... Marco Bellani
DATA CREAZIONE ...... 20/NOV/1992

----------------------------------------------------------------------

REVISIONE :

 EBCDIC_ASCII02   20/NOV/1992   M.Bellani
                                Routine completamente riscritta.  


----------------------------------------------------------------------

 ESEMPIO DI CHIAMATA :
 
 void ebcdic_ascii( str_in, str_out, icode )
 
 char str_in[]    max. 10000
 char str_out[]
 long icode
 

    Parametri di input
    ------------------
    
    str_in         Stinga di caratteri da convertire 
    
    icode          Tipo di conversione
    
                   0  EBCDIC ----> ASCII
                   1  ASCII  ----> EBCDIC

    Parametri di output
    -------------------

    str_out        Stinga di caratteri convertita 


======================================================================
*/



void ebcdic_ascii( str_in, str_out, icode )

char str_in[];
char str_out[];

long icode;

{

  FILE *fp;
  
  char cmd[10000];
  
  if ( icode == 0 )
    sprintf( cmd, "echo '%s' | dd conv=ascii 2> /dev/null", str_in );
  else
    sprintf( cmd, "echo '%s' | dd conv=ebcdic 2> /dev/null", str_in );
      
  fp = popen( cmd, "r" );
  fgets( str_out, (strlen(str_in)+1), fp );
  
  pclose( fp );
  
}


/*
****************************************************************************
******************************** segyopen **********************************
****************************************************************************
*/

#ifdef IBM
void segyopen(fdesc,filename)
#else
void segyopen_(fdesc,filename)
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
   *fdesc = open(filename, 1, 0);
   /* if open fails, creates the file */
   if (*fdesc == -1)
   {
      *fdesc = creat(filename, 0666);
   }
}


/*
****************************************************************************
******************************** segyclose  ********************************
****************************************************************************
*/

#ifdef IBM
void segyclose(fdesc)
#else
void segyclose_(fdesc)
#endif
int *fdesc;
{
   close(*fdesc);
}


/*
****************************************************************************
******************************** segyputheader *****************************
****************************************************************************
*/

#ifdef IBM
void segyputheader(fdesc, reelheader)
#else
void segyputheader_(fdesc, reelheader)
#endif
int reelheader[];
int *fdesc;
/*
   reelheader (from fortran: integer * 27)
   fdesc      (to fortran: must be used to append traces)
   filename   (from fortran: MUST be always the last argument of the call)
*/

{
   char buff_3200[3201];
   short buff_400[200];
   long n;

   /* ebcdic header initialization */
   buff3200set(buff_3200);
   /* reel header initialization */
   buff400set(reelheader, buff_400);
   /* pointer to BOF */
   lseek(*fdesc,0L,0);
   /* buff3200 conversion (ascii->ebcdic) */
   ebcdic_ascii(buff_3200, buff_3200, (long) 1);   
   /* ... writing */
   n = write (*fdesc, buff_3200, 3200);
   if (n != 3200)
	{
	close(*fdesc);
	*fdesc = -1;
	return;
	}
   n = write (*fdesc, buff_400, 400);
   if (n != 400)
	{
	close(*fdesc);
	*fdesc = -1;
	return;
	}
   /* return to fortran */
   return;
}



/*
****************************************************************************
******************************** segyputttrace *****************************
****************************************************************************
*/
   
#ifdef IBM
void segyputtrace(fdesc,traceheader,tracedata)
#else
void segyputtrace_(fdesc,traceheader,tracedata)
#endif
int  fdesc[1];
int  traceheader[];
float tracedata[];
{
    long nsamp_w, i, nbytes;
    short buff_240[120];
   /* searching for EOF... */
   lseek(fdesc[0],0L,2);
   /* number of samples from trace header */
   nsamp_w = traceheader[38];
   /* trace header conversion */
   buff240set(traceheader,buff_240);
   /* header writing... */
   i = write(fdesc[0],buff_240,240);
   if (i != 240)
	{
	close(fdesc[0]);
	fdesc[0] = -1;
	return;
	}
   /* trace writing... */
   lseek(fdesc[0],0L,2);
   nbytes = nsamp_w * 4;

   ieee_ibm((int *)&tracedata[0], (int *)&tracedata[0], nsamp_w);

/*====================================================================================
 NICOLA: lo swap stava prima dell'ieee_ibm, ma era nel posto sbagliato. Adesso l'ho
         rimesso al posto giusto.
======================================================================================*/
#if defined AlphaSC || LINUX
    swap_array(tracedata, nsamp_w);
#endif

   i = write(fdesc[0],tracedata,nbytes);
   if (i != nbytes)
	{
	close(fdesc[0]);
	fdesc[0] = -1;
	return;
	}
   /* return to fortran */
   return;
}
  
