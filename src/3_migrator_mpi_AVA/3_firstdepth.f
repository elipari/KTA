      SUBROUTINE FIRSTDEPTH (IASTART,MUTEFLAG,PAN1,VETT_OFF,OFFCUR,OFFOUT)
      IMPLICIT NONE
      
      INTEGER*4 IASTART, MUTEFLAG, PAN1, KK, OFFOUT
      REAL*4    VETT_OFF(PAN1), OFFCUR
           
           OFFOUT=0
           IASTART=1
           IF (MUTEFLAG .EQ. 1) THEN
              DO 450 KK=1,PAN1
                 IF ( VETT_OFF(KK) .GE. OFFCUR ) THEN
                 	IASTART = KK
                 	GOTO 451
                 ENDIF
                 
 450          CONTINUE
 451          CONTINUE
              IF ( VETT_OFF(PAN1) .LT. OFFCUR ) THEN
                 	OFFOUT=1
              ENDIF
            ENDIF     
      END
     