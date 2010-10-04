
************************************************************************
*     FILE ibm2ieee.f
*     Converte il formato ibm in ieee
*
*     INPUT: la traccia letta dal file;
*     ogni campione della traccia va letto da file come quattro character*1
*
*     OUTPUT: la traccia convertita;
*
*     Fabrizio & Mirco
*
*     Ottobre 1999
*
************************************************************************
      SUBROUTINE IBM2IEEE(TRACCIA_TEMPC,TRACCIA_TEMP,NCAMP)

      IMPLICIT NONE

      INTEGER*4   NCAMP,I
      REAL*8      EXPO, MANTISSA, SIGN
      REAL*4      TRACCIA_TEMP(NCAMP)
      CHARACTER*1 TRACCIA_TEMPC(NCAMP*4)
      INTEGER*4      DATA(4)

      DO 10 I=1,NCAMP

         DATA(1)=ICHAR(TRACCIA_TEMPC((I-1)*4+1))
         DATA(2)=ICHAR(TRACCIA_TEMPC((I-1)*4+2))
         DATA(3)=ICHAR(TRACCIA_TEMPC((I-1)*4+3))
         DATA(4)=ICHAR(TRACCIA_TEMPC((I-1)*4+4))

         MANTISSA = DATA(2)/256.0+DATA(3)/65536.0+DATA(4)/16777216.0
         EXPO = MOD(DATA(1),128)-64.0

         IF (DATA(1).LE.127.0)  THEN
            SIGN=1
         ELSE
            SIGN=-1
         ENDIF

         TRACCIA_TEMP(I)=SIGN*MANTISSA*(16.0**EXPO)

 10   CONTINUE

      RETURN
      END

