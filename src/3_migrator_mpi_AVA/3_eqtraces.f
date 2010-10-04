      SUBROUTINE EQTRACES (FPESI,TRACCIA_LETTA,TR_WEIGHTS,NCAMP)
      
      IMPLICIT NONE
      
      INTEGER*4 FPESI, NCAMP
      REAL*4    TRACCIA_LETTA(NCAMP), TR_WEIGHTS
      
      INTEGER*4 I
                      
        IF (( FPESI .EQ. 1) .AND. (TR_WEIGHTS .GT. 0)) THEN
             DO 665 I=1,NCAMP
                TRACCIA_LETTA(I)=TRACCIA_LETTA(I)*TR_WEIGHTS
 665         CONTINUE
        ENDIF
        
      END   