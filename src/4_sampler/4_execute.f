      SUBROUTINE EXECUTE(TG_MTX_X1,TG_MTX_X2,TG_MTX_X3,
     $                   TG_NT,TG_NA,TG_NB,
     $                   TG_O1,TG_O2,TG_O3,
     $                   TG_DA,TG_DB,
     $                   TG_CA1,TG_CA2,TG_CA3,
     $                   TG_CB1,TG_CB2,TG_CB3)

      IMPLICIT NONE

      INTEGER*4 TG_NT,TG_NA,TG_NB
      REAL*4    TG_O1,TG_O2,TG_O3
      REAL*4    TG_DA,TG_DB
      REAL*4    TG_CA1,TG_CA2,TG_CA3
      REAL*4    TG_CB1,TG_CB2,TG_CB3
      REAL*4    TG_MTX_X1(TG_NT),TG_MTX_X2(TG_NT),TG_MTX_X3(TG_NT)

      INTEGER*4 IA,IB,IS

      IS=1
      DO 10 IB=1,TG_NB

         DO 20 IA=1,TG_NA
!Clara
            TG_MTX_X1(IS)=(ANINT((TG_O1+(IA-1)*TG_DA*TG_CA1+(IB-1)*TG_DB*TG_CB1)*10))/10
            TG_MTX_X2(IS)=(ANINT((TG_O2+(IA-1)*TG_DA*TG_CA2+(IB-1)*TG_DB*TG_CB2)*10))/10
            TG_MTX_X3(IS)=(ANINT((TG_O3+(IA-1)*TG_DA*TG_CA3+(IB-1)*TG_DB*TG_CB3)*10))/10
            IS = IS +1

 20      CONTINUE

 10   CONTINUE

      RETURN
      END
