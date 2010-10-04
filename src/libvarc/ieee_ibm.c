/*
! $Id: ieee_ibm.c,v 1.1 2002/07/31 10:37:18 agimicv5 Exp $
!
! CHANGE LOG:
! who  when	vers.   what
!
! GCA  16/05/02   1.1   First version in libvarc
!
! --------------------------------------------------------------------------
*/

/* SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS */

/*
  * * * * * * *                                          * * * * * * * *
  *     C     *                                          *  IEEE_IBM01 *
  * * * * * * *                                          * * * * * * * *
                               IEEE_IBM01
 
                   IBM TO SUN - SUN TO IBM CONVERSION
 
 ------------------------DISCUSSIONE TECNICA----------------------------
 
 
    Convert IBM format floating point numbers to native and viceversa.
 
 
 -----------------------------------------------------------------------
 
   DECK TYPE ......................... C
   AUTHOR(s) ......................... A. Milani N. Lisi
   DUCUMENTED......................... A. Milani
   DATE      ......................... 10/09/1992
 
 -----------------------------------------------------------------------
 
 
     Revised :
 
 ------------------------------------------------------------------------
 
 
 
   ESEMPIO DI CHIAMATA :
 
 
      void ieee_ibm(in, out, nb)
 
 
      Parametri di input
      ------------------
 
     in               buffer float in formato IEEE (4 bytes)
     nb               numero di float da convertire
 
 
      Parametri di output
      -------------------
 
     out              buffer float in formato ibm (4 bytes)
 
 
 
 
 
      void ibm_ieee(in, out, nb)
 
 
      Parametri di input
      ------------------
 
     in               buffer float in formato ibm (4 bytes)
     nb               numero di float da convertire
 
 
      Parametri di output
      -------------------
 
     out              buffer float in formato IEEE (4 bytes)
 
 
 
 -------------------SOFTWARE - HARDWARE NECESSARI ---------------------
 
   SUBROUTINES USATE :
 
   AREE COMMON :
 
   HARDWARE RICHIESTO :
 
 
 
 ///////////////////////////////////////////////////////////////////////
 
   NOTE:
 
 
 
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 
*/
 
#include "ieee_ibm.h"

void ieee_ibm ( int in[], int out[], int nb )
{
        int i;
        static int mt[] = {2, 1, 0, 3};
        static int it[] = {0x21200000, 0x21400000, 0x21800000, 0x22100000};
 
        for (i = 0; i < nb ; i++ ){
                if ( in[i] == 0 )
                        out[i] = 0;
                else {
                        register int ix = (in[i]>>23) & 0x3;
                        register int mant = (in[i] & 0x7fffff)>>mt[ix];
                        register int iexp=((in[i]&0x7e000000)>>1) + it[ix];
                        out[i] = (mant+iexp) | (in[i]&0x80000000);
                }
        }
}
 
 
#define IEMAXIBM 0x611fffff
#define IEMINIBM 0x21200000
#define IEEEMAX  0x7fffffff
 
void ibm_ieee ( int in[], int out[], int nb )
{
        int i;
        static int mt[] = { 8, 4, 2, 2, 1, 1, 1, 1 };
        static int it[] = { 0x21800000, 0x21400000, 0x21000000, 0x21000000,
                            0x20c00000, 0x20c00000, 0x20c00000, 0x20c00000};
 
        for ( i = 0 ; i < nb ; i++) {
                register int inabs = in[i]&0x7fffffff;
                if( inabs > IEMAXIBM )
                        out[i] = IEEEMAX | (in[i]&0x80000000);
                else if( inabs < IEMINIBM )
                        out[i] = 0;
                else {
                        register int mant = in[i]&0xffffff;
                        register int ix = mant>>21;
                        register int iexp = (in[i]&0x7f000000) - it[ix];
                        mant = mant*mt[ix]+iexp*2;
                        out[i] = mant | (in[i]&0x80000000);
                }
        }
}

