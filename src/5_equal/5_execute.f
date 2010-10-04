      SUBROUTINE EXECUTE(FIDSGY, NF, N_SHOT_TOT, N_TR_SHOT,
     $                   MAX_NTRACCESHOT, NCAMP,
     $                   A11, A12, A21, A22, B1, B2,
     $                   DX, DY, XR, YR, WEIGHT, FIDPESI)

      IMPLICIT NONE
      INTEGER*4 FIDSGY,NF
      INTEGER*4 FIDPESI,NCAMP
      INTEGER*4 N_TR_SHOT,MAX_NTRACCESHOT
      INTEGER*4 I,IND,J,S,TR,N_SHOT_TOT
      INTEGER*4 OLDSHOT,NEWSHOT,REST
      REAL*4 XR(MAX_NTRACCESHOT),YR(MAX_NTRACCESHOT)
      REAL*4 WEIGHT(MAX_NTRACCESHOT),XA,YA
      REAL*4 DX,DY
      REAL*4 A11,A12,A21,A22,B1,B2
      INTEGER*4 XSI,YSI,XRIC,YRIC
      INTEGER*4 FILE_ERROR
      INTEGER*4 FILE_STATUS

*     *********************************
*      versione 17/1/2000
*      autore Ratti Davide
*      Crea la matrice dei pesi dei ricevitori
*      di tutto l'esperimento
*      Uses:pesoric.f,tooldav.f
*     *********************************

*     *********************************
*     PARAMETRI:
*     NUMERO DI SORGENTI:
*      N_SHOT_TOT
*     LUNGHEZZA MASSIMA DEI VETTORI:
*      N_TR_SHOT
*     NUMERO DI CAMPIONI PER TRACCIA:
*      NCAMP
*     DIMENSIONE X DELLA CELLA:
*      DX
*     DIMENSIONE Y DELLA CELLA:
*      DY
*     NUMERO PROGRESSIVO DI TRACCE LETTE:
*      I
*     NUMERO DI TRACCE CARICATE DI UNO SHOT:
*      IND
*     **********************************



      IND=1
      I=0
      REST=240+NCAMP*4

*'''''''''''''''''''CICLO PRINCIPALE SUGLI SHOT```````````````````````

      DO 100 S=1,N_SHOT_TOT

      I = I+1
      CALL GLOB_FSEEK(FIDSGY,1+3600+8+((I-1)*REST),0,FILE_ERROR)
      CALL GLOB_FREAD(FIDSGY,OLDSHOT,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 300
          ENDIF
      ENDIF
      CALL GLOB_FSEEK(FIDSGY,1+3600+72+((I-1)*REST),0,FILE_ERROR)
      CALL GLOB_FREAD(FIDSGY,XSI,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 300
          ENDIF
      ENDIF
      CALL GLOB_FREAD(FIDSGY,YSI,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 300
          ENDIF
      ENDIF
      CALL GLOB_FREAD(FIDSGY,XRIC,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 300
          ENDIF
      ENDIF
      CALL GLOB_FREAD(FIDSGY,YRIC,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 300
          ENDIF
      ENDIF

      XA=REAL(XRIC)
      YA=REAL(YRIC)
      CALL RUOTA(XR(1),YR(1),XA,YA,A11,A12,A21,A22,B1,B2)
      IND=1


*'''''''''''''''''''CARICAMENTO DATI DAL SEGY```````````````````````

      DO 110 TR=2,N_TR_SHOT

      I = I+1
      CALL GLOB_FSEEK(FIDSGY,1+3600+8+((I-1)*REST),0,FILE_ERROR)

      CALL GLOB_FREAD(FIDSGY,NEWSHOT,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 200
          ENDIF
      ENDIF

      CALL GLOB_FSEEK(FIDSGY,1+3600+72+((I-1)*REST),0,FILE_ERROR)
      CALL GLOB_FREAD(FIDSGY,XSI,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 200
          ENDIF
      ENDIF
      CALL GLOB_FREAD(FIDSGY,YSI,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 200
          ENDIF
      ENDIF
      CALL GLOB_FREAD(FIDSGY,XRIC,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 200
          ENDIF
      ENDIF
      CALL GLOB_FREAD(FIDSGY,YRIC,4,1,FILE_ERROR)
      IF (FILE_ERROR .NE. 0) THEN
          CALL GLOB_FEOF(FIDSGY, FILE_STATUS)
          IF (FILE_STATUS .NE. 0) THEN
             GOTO 200
          ENDIF
      ENDIF

      IF (NEWSHOT.EQ.OLDSHOT) THEN
           IND=IND+1
           XA=REAL(XRIC)
           YA=REAL(YRIC)
           CALL RUOTA(XR(IND),YR(IND),XA,YA,A11,A12,A21,A22,B1,B2)
      ELSE
           GOTO 200
      ENDIF

 110  CONTINUE


*''''''''''''''''''''''''EQUALIZZAZIONE`````````````````````````````

 200  CONTINUE

      WRITE(*,*) 'Sorgente N. : ', OLDSHOT, ' - xs ys : ', XSI, YSI
      WRITE(*,*) 'Numero ricevitori : ', IND
      WRITE(*,*)

      CALL PESORIC(XR,YR,WEIGHT,IND,DX,DY,N_TR_SHOT)


*'''''''''''''''''''SCRITTURA SU FILE DEI PESI``````````````````````

      DO 15 J=1,N_TR_SHOT

          IF (WEIGHT(J).NE.0.0) THEN
          CALL GLOB_FWRITE(FIDPESI,WEIGHT(J),4,1,FILE_ERROR)
          ENDIF

 15   CONTINUE


 100  CONTINUE

 300  CONTINUE

      RETURN
      END
