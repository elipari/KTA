***********************************************************************
*     FILE trovabin.f
*     Contiene la subroutine trovabin che data la posizione della traccia
*     restituisce il bin relativo.
*
*     INPUT: le coordinate della traccia e le dimensioni dei bin
*
*     OUTPUT: le coordinate del bin
*
*     Fabrizio & Mirco - Luglio 1999
*     Ludovico         - Agosto 2000
*
***********************************************************************
      SUBROUTINE TROVABIN(OFF64, X1, X2, X3,
     $    TYPE,
     $    O1, O2, O3,
     $    CA1,CA2,CA3,
     $    CB1,CB2,CB3,
     $    CC1,CC2,CC3,
     $    DA, DB, NA, NB,
     $    DDA,DDB,NNA,NNB,
     $    DC,  NC,  DDC,
     $    NPAN )

      IMPLICIT NONE


* Varaibili output
      INTEGER*8 OFF64


* Variabili input
      REAL*4    X1, X2, X3

      INTEGER*4 TYPE
      INTEGER*4 NPAN 
      REAL*4    O1(NPAN), O2(NPAN), O3(NPAN)
      REAL*4    CA1(NPAN),CA2(NPAN),CA3(NPAN)
      REAL*4    CB1(NPAN),CB2(NPAN),CB3(NPAN)
      REAL*4    CC1(NPAN),CC2(NPAN),CC3(NPAN)
      REAL*4    DA(NPAN), DB(NPAN)
      INTEGER*4 NA(NPAN), NB(NPAN)
      REAL*4    DDA(NPAN),DDB(NPAN)
      INTEGER*4 NNA(NPAN),NNB(NPAN)
      REAL*4    DC, DDC
      INTEGER*4 NC


* Variabili locali
      INTEGER*4 I, J
      INTEGER*4 NTOT
      REAL*4    XA, XB, XC
      INTEGER*4 IA, IB, IC
      INTEGER*8 IA_8, IB_8, IC_8, NTOT_8, NNA_I_8, NNB_I_8

      REAL*4 DMIN

      DMIN = 3

      OFF64 = -1


      IF (TYPE .EQ. 1) THEN

        DO 10 I=1,NPAN


           IF ( ( ABS(X1-O1(I)) .LE. DMIN ) .AND.
     $          ( ABS(X2-O2(I)) .LE. DMIN ) .AND.
     $          ( ABS(X3-O3(I)) .LE. DMIN ) ) THEN

              OFF64 = I

              GOTO 1000

           ENDIF

10      CONTINUE

      ENDIF


      IF (TYPE .EQ. 2) THEN

        NTOT = 0
        NTOT_8 = 0
        DO 20 I=1,NPAN

           XA = (X1-O1(I))*CA1(I)+(X2-O2(I))*CA2(I)+
     $          (X3-O3(I))*CA3(I)
           XB = (X1-O1(I))*CB1(I)+(X2-O2(I))*CB2(I)+
     $          (X3-O3(I))*CB3(I)
           XC = (X1-O1(I))*CC1(I)+(X2-O2(I))*CC2(I)+
     $          (X3-O3(I))*CC3(I)
              
              NNA_I_8 = NNA(I)
              NNB_I_8 = NNB(I)

          IF ( (XA.GE.0) .AND. (XA.LE.((NA(I)-1)*DA(I))).AND.
     $         (XB.GE.0) .AND. (XB.LE.((NB(I)-1)*DB(I))).AND.
     $         (ABS(XC).LE.DMIN) ) THEN

              IA = NINT(XA/DDA(I))+1
              IB = NINT(XB/DDB(I))+1

!              IA = INT(XA/DDA(I))+1
!              IB = INT(XB/DDB(I))+1

!              NTOT = 0
!              DO 21 J=1,I-1
!                 NTOT = NTOT + NNA(J)*NNB(J)
!21            CONTINUE

              IA_8 = IA
              IB_8 = IB

              OFF64 = IA_8 + (IB_8-1)* NNA_I_8 + NTOT_8

              GOTO 1000

           ENDIF
           NTOT_8 = NTOT_8 + NNA_I_8*NNB_I_8

20      CONTINUE

      ENDIF


      IF (TYPE .EQ. 3) THEN

           I=1

           XA = (X1-O1(I))*CA1(I)+(X2-O2(I))*CA2(I)+
     $          (X3-O3(I))*CA3(I)
           XB = (X1-O1(I))*CB1(I)+(X2-O2(I))*CB2(I)+
     $          (X3-O3(I))*CB3(I)
           XC = (X1-O1(I))*CC1(I)+(X2-O2(I))*CC2(I)+
     $          (X3-O3(I))*CC3(I)
             
           NNA_I_8 = NNA(I)
           NNB_I_8 = NNB(I)
           
           IF ((XA.GE.0) .AND. (XA.LE.((NA(I)-1)*DA(I))).AND.
     $         (XB.GE.0) .AND. (XB.LE.((NB(I)-1)*DB(I))).AND.
     $         (XC.GE.0) .AND. (XC.LE.((NC-1)*DC   ))) THEN

              IA = NINT(XA/DDA(I))+1
              IB = NINT(XB/DDB(I))+1
              IC = NINT(XC/DDC   )+1
              IA_8 = IA
              IB_8 = IB
              IC_8 = IC
              
              OFF64 = ( IA_8 + (IB_8-1)* NNA_I_8) +
     $                   (IC_8-1) * NNA_I_8 * NNB_I_8 

              GOTO 1000

           ENDIF

      ENDIF


1000  CONTINUE

      RETURN
      END






































