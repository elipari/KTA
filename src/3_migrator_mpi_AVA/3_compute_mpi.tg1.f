      SUBROUTINE COMPUTE_TG1(NF, DIMGRUPPOTR, DIMBLOCCOTR,
     $    MAXDIMBLOCCOTR, MAXNRBLOCCHITR, PRIMATR, DBRESTART, NCAMP, NCAMPFILT,
     $    DT, DX, TSHIFT, MAX_NCAMP,MAX_NCAMPFILT, NTRACCEFILE, VEL_SUP, FPESI, BORDF,
     $    SMUSSAMENTO, SEMIAPERTURA, SOGLIA_AMPIEZZE, IMAGING, VERBOSE, MY_TRACES, ANTIALIAS,
     $    TRACCE, TRACCIA1, TRACCIA2, TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET,
     $    TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS, PAN_PARZ, CONT_RPARZ, PAN1, PAN2, PAN3,
     $    PAN4, PAN5, AVA, DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN,
     $    AVO, OFFMIN, OFFMAX,
     $    TG_TYPE, TG_O1, TG_O2, TG_O3, TG_CA1, TG_CA2, TG_CA3, TG_CB1, TG_CB2, TG_CB3,
     $    TG_CC1, TG_CC2, TG_CC3, TG_DA, TG_DB, TG_NA, TG_NB, TG_NT, TG_DDA, TG_DDB,
     $    TG_NNA, TG_NNB, TG_NNT, TG_DC, TG_NC, TG_DDC, TG_NNC, TG_NPAN, TG_NT_TOT,
     $    ATMIN, ATMAX, ATD, ATN, ATTABLE, NTR_FILE,
     $    RUZ, FIDDBA, FIDDBT, FIDDBUZ, NTR_MIG_CICLO,
     $    SS, S, VETT_APE, VETT_DISTX, VETT_DISTY, MUTEFLAG, VETT_OFF,
     $    FIDDBVX, FIDDBVY, FIDDBVZ, SVX, SVY, SVZ, RVX, RVY, RVZ, TG_NT_AVA,
     $    QUANTTAC, QUANTV, SORTFILETAC, SORTFILEV, ACQ_NT_TOT,
     $    myrank
#ifdef TIMING
     $    ,readtimeplus, readtimeless
#endif
     $    )
     
      IMPLICIT NONE
#ifdef ALPHA
      include 'mpif.h'
#endif
      INTEGER*4     NF
      INTEGER*4     FIDDBA, FIDDBT, FIDDBUZ,FIDDBVX, FIDDBVY, FIDDBVZ
      INTEGER*4     DIMGRUPPOTR,MAXGRUPPI,NRGRUPPITR,MAXNRBLOCCHITR
      INTEGER*4     DIMBLOCCOTR,MAXDIMBLOCCOTR
      INTEGER*4     PRIMATR
      INTEGER*4     DBRESTART(MAXNRBLOCCHITR)
      INTEGER*4     NCAMP,  NCAMPFILT
      REAL*4        DT, DX, TSHIFT
      INTEGER*4     MAX_NCAMP,  MAX_NCAMPFILT
      INTEGER*4     NTRACCEFILE
      REAL*4        VEL_SUP
      INTEGER*4     ATN
      REAL*4        ATTABLE(ATN)
      REAL*4        SMUSSAMENTO, SOGLIA_AMPIEZZE
      INTEGER*4     IMAGING, VERBOSE, ANTIALIAS, BORDF, FPESI
      REAL*4        TRACCE(MAX_NCAMPFILT)
      REAL*4        TRACCIA_TEMP(MAX_NCAMP)
      REAL*4        TRACCIA_LETTA(MAX_NCAMP)
      REAL*4        TRACCIA1(MAX_NCAMP+BORDF),TRACCIA2(MAX_NCAMP+BORDF)
      REAL*4        TR_XR(MAXDIMBLOCCOTR)
      REAL*4        TR_YR(MAXDIMBLOCCOTR)
      REAL*4        TR_ZR(MAXDIMBLOCCOTR)
      INTEGER*8     BINR_OFFSET(MAXDIMBLOCCOTR)
CCC 03/05/2002 MPI-
      REAL*4        TR_WEIGHTS(MAXDIMBLOCCOTR)
CCC      REAL*4        TR_WEIGHTS(MAX_NTRACCE)
      REAL*4        TR_XS(MAXDIMBLOCCOTR)
      REAL*4        TR_YS(MAXDIMBLOCCOTR)
      REAL*4        TR_ZS(MAXDIMBLOCCOTR)
CCC 06/05/2002 MPI- Upgrade TR_NS da integer*4 --> integer*8 per fare un'unico MPI_BCAST di integer
CCC                 in execute
      INTEGER*8     TR_NS(MAXDIMBLOCCOTR)
CCC      INTEGER*4     TR_NS(MAXDIMBLOCCOTR)
      INTEGER*8     BINS_OFFSET(MAXDIMBLOCCOTR)
      INTEGER*4     PAN1,PAN2,PAN3,PAN4,PAN5
      REAL*4        PAN_PARZ(PAN1,PAN2,PAN3,PAN4,PAN5)
      INTEGER*4     CONT_RPARZ(PAN1,PAN2,PAN3,PAN4,PAN5)
! Clara2 apertura variabile con la profondita'      
      REAL*4        VETT_APE(PAN1)
!Clara4 distanza massima Y (XLINE) e X (ILINE) VARIABILE
      REAL*4        VETT_DISTX(PAN1), VETT_DISTY(PAN1)
      INTEGER*4     MAXDISTX, MAXDISTY
!Clara5
      INTEGER*4     IASTART, MUTEFLAG
      REAL*4        VETT_OFF(PAN1)     
      
      INTEGER*4     TG_TYPE
      INTEGER*4     TG_NPAN, TG_NT_TOT
      REAL*4        TG_O1(TG_NPAN), TG_O2(TG_NPAN), TG_O3(TG_NPAN)
      REAL*4        TG_CA1(TG_NPAN),TG_CA2(TG_NPAN),TG_CA3(TG_NPAN)
      REAL*4        TG_CB1(TG_NPAN),TG_CB2(TG_NPAN),TG_CB3(TG_NPAN)
      REAL*4        TG_CC1(TG_NPAN),TG_CC2(TG_NPAN),TG_CC3(TG_NPAN)
      REAL*4        TG_DA(TG_NPAN), TG_DB(TG_NPAN)
      INTEGER*4     TG_NA(TG_NPAN), TG_NB(TG_NPAN), TG_NT(TG_NPAN)
      REAL*4        TG_DDA(TG_NPAN),TG_DDB(TG_NPAN)
      INTEGER*4     TG_NNA(TG_NPAN),TG_NNB(TG_NPAN),TG_NNT(TG_NPAN)
      REAL*4        TG_DC, TG_DDC
      INTEGER*4     TG_NC, TG_NNC
      REAL*4        SA(TG_NT_TOT),ST(TG_NT_TOT)
      REAL*4        RA(TG_NT_TOT), RT(TG_NT_TOT)
      REAL*4        RUZ(TG_NT_TOT)
!      REAL*4        RUX(TG_NT_TOT),RUY(TG_NT_TOT)
! AVA
      INTEGER*4     AVA
      REAL*4        DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN
      REAL*4        DIP_ANGLE, AZ_ANGLE, STEP_DIP, STEP_AZ
      INTEGER*4     TG_NT_AVA
      REAL*4        SVX(TG_NT_AVA), SVY(TG_NT_AVA), SVZ(TG_NT_AVA)
      REAL*4        RVX(TG_NT_AVA), RVY(TG_NT_AVA), RVZ(TG_NT_AVA)
      
      INTEGER*4     QUANTTAC, QUANTV
      
      INTEGER*4     ACQ_NT_TOT, SORTFILETAC(ACQ_NT_TOT),SORTFILEV(ACQ_NT_TOT)
      INTEGER*8     SORT_INDEX
      
      INTEGER*4     AVO,STEP_OFF, OFFMIN, OFFMAX

**********Variabili locali**********************************************
      REAL*4        C0XP, C0YP, C0ZP, C1XP, C1YP, C1ZP
      REAL*4        C2XP, C2YP, C2ZP, C3XP, C3YP, C3ZP
      REAL*4        B0XP, B0YP, B0ZP
      REAL*4        PIGRECO
      PARAMETER(PIGRECO = 3.14159265358979)
      REAL*4        XB,YB,ZB,XP,YP,ZP
      INTEGER*4     NTR_CICLO
      INTEGER*4     NTR_FILE, NTR_MIG, NTR_MIG_CICLO
      INTEGER*8     OFF64,OFF64S
      INTEGER*4     FILE_ERROR
      INTEGER*4     I, S, T, TD, IA, IB, IC, KK, SS, TT, ID, IE
      INTEGER*4     NNA, NNB
      INTEGER*4     BINSHIFT
      REAL*4        AT, ATD, ATMIN, ATMAX
      REAL*4        DELTAPAN
      REAL*4        CZR
      REAL*4        TTOT, ASORG, AR
      REAL*4        VAL_FILTRATO
      INTEGER*4     K, POSCAMP
      REAL*4        DT_AA, SENTETA, Z1
      REAL*4        T0,T1,T2
      REAL*4        THETA,SEMIAPERTURA,SAGOMATURA
      INTEGER*4     NTR_CARICATE
CCC 06/05/2002 MPI- Upgrade TR_NS da integer*4 --> integer*8 per fare un'unico MPI_BCAST di integer
CCC                 in execute
      INTEGER*8     NUMSORGPREC
CCC      INTEGER*4     NUMSORGPREC
      INTEGER*4     LEGGISHOT
      INTEGER*4     PIVOT
      REAL*4        OFFCUR
#ifdef AVACODE
      real *4       SVX_LASCO1(2,2),SVY_LASCO1(2,2),SVZ_LASCO1(2,2),RVX_LASCO1(2,2),RVY_LASCO1(2,2), RVZ_LASCO1(2,2)
      REAL*4        SVX_LASCO2(2),SVY_LASCO2(2),SVZ_LASCO2(2),RVX_LASCO2(2),RVY_LASCO2(2), RVZ_LASCO2(2)
      REAL*4        SVX_FITTO,SVY_FITTO,SVZ_FITTO,RVX_FITTO,RVY_FITTO, RVZ_FITTO
      REAL*4        UX, UY      
#endif      
CCC 29/04/2002 Variabili per controllo temporale
#ifdef TIMING
      real*8 second
      real*8 readtimeplus(DIMGRUPPOTR), readtimeless(DIMGRUPPOTR)
#endif
CCC 03/05/2002 MPI- MPI variables
      integer*4 myrank
      real*4    MY_TRACES(MAX_NCAMP, DIMGRUPPOTR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Inizializzazioni
CCC 27/06/2002 Inizializzazione PROVVISORIA effettiva per TG_TYPE = 1
        NNB=PAN2
        NNA=PAN1
        ID = 1
        IE = 1
       STEP_OFF = 0       
       IF (AVO .EQ. 1) THEN
           STEP_OFF = ((OFFMAX-OFFMIN)/PAN4)
       ENDIF
CCC 08/05/2002 MPI- Messo qui per funzionare anche se nell'ultimo SS qualche PE non deve fare nulla
        DO 14 IE=1,PAN5
         DO 15 ID=1,PAN4
          DO 16 IC=1,PAN3
           DO 17 IB=1,PAN2
            DO 18 IA=1,PAN1
              PAN_PARZ(IA,IB,IC,ID,IE)   = 0.0
              CONT_RPARZ(IA,IB,IC,ID,IE) = 0
 18         CONTINUE
 17        CONTINUE
 16       CONTINUE
 15      CONTINUE
 14     CONTINUE
        IF (DBRESTART(SS).EQ.1) THEN
CCC 27/06/2002 MPI- Commentato altrimenti confusione sullo schermo
CCC          IF (VERBOSE .GT. 1) THEN
CCC            WRITE(*,*) 'comp PE: myrank = ",myrank," Restart: skipping file SEG-Y N.: ',NF,
CCC    $                 ' - Blocco N.: ',SS,' Gruppo N.:',S
CCC          ENDIF
          GOTO 200
        ENDIF
        DO 51 KK=1,TG_NT_TOT
          RA(KK) =-2
          RT(KK) =-2
          RUZ(KK)=-2
 51     CONTINUE
        LEGGISHOT=0
        NUMSORGPREC=INT8(-2)
        NTR_MIG_CICLO =0
        NTR_CICLO=0
        T=(SS-1)*DIMBLOCCOTR+(S-1)*DIMGRUPPOTR +1
C        IF (T .LE. NTR_FILE)  THEN
CCC         IF (VERBOSE .GT. 1) THEN
CCC 27/05/2002 MPI- Commentato altrimenti confusione sullo schermo
CCC           WRITE(*,*) 'File SEG-Y N.: ',NF,' - Blocco N.: ', SS, ' - Gruppo N.: ',S
CCC           WRITE(*,*) 'myrank = ",myrank," Inizio caricamento da traccia (compreso offset): ',
CCC     $           (SS-1)*DIMBLOCCOTR +(S-1)*DIMGRUPPOTR+ PRIMATR
CCC         ENDIF
C        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    *
*         CICLO SULLE TRACCE DI UN GRUPPO - PREFILTRAGGIO & MIGRAZIONE         *
*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    *
        DO 250 TD=1,DIMGRUPPOTR
         TT = (S-1)*DIMGRUPPOTR + TD                  ! indice tracce nel blocco
         T = (SS-1)*DIMBLOCCOTR + TT            ! indice tracce totale nel file
         NTR_CARICATE = T-1                     ! nr. tracce precedenti
CCC 10/06/2002 Altrimenti l'ultima traccia del blocco (dell'ultimo gruppo) non viene
CCC    mai migrata e il risultato finale dipende dalla dimenzione del blocco
         IF ( (T .GT. NTR_FILE) .OR.
     $        (T .GT. NTRACCEFILE))  THEN
              GOTO 251
         ENDIF
   
!      Clara5 mute
           OFFCUR = SQRT((TR_XS(TT)-TR_XR(TT))**2+(TR_YS(TT)-TR_YR(TT))**2+(TR_ZS(TT)-TR_ZR(TT))**2)
!------------------AVO------------------------------------------
         IF (AVO.EQ.1) THEN
            ID = (OFFCUR - OFFMIN)/STEP_OFF + 1
            IF (ID .GT. PAN4) ID=PAN4
         ENDIF
!------------------AVO------------------------------------------           
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
                 	BINR_OFFSET(TT)=INT8(-2)           ! Flag esclusione traccia
                        BINS_OFFSET(TT)=INT8(-2)
              ENDIF
            ENDIF
        
*** - IF - esclusione di una traccia se il ricevitore non appartiene al database-----*
         IF ( (BINR_OFFSET(TT) .GT. INT8(0)) .AND.
     $        (TR_WEIGHTS(TT) .GT. 0) .AND.
     $        (BINS_OFFSET(TT) .GT. INT8(0)) ) THEN
           OFF64 = (BINR_OFFSET(TT)- INT8(1))*INT8(TG_NT_TOT)*INT8(4)+INT8(1)
           IF (TR_NS(TT) .NE. NUMSORGPREC) THEN
             NUMSORGPREC=TR_NS(TT)
             LEGGISHOT=1
             OFF64S= (BINS_OFFSET(TT) -INT8(1))*INT8(TG_NT_TOT)*INT8(4)+INT8(1)
           ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Lettura dati da disco
* ---- caricamento dei dati relativi a tempi ampiezze e angoli sul bin della traccia
#ifdef TIMING
           readtimeless(TD)=second(0)
#endif
           IF ( LEGGISHOT .EQ. 1) THEN
             CALL GLOB_FSEEK64(FIDDBA,OFF64S,0,FILE_ERROR)
             CALL GLOB_FREAD(FIDDBA,SA,4,TG_NT_TOT,FILE_ERROR)
             CALL GLOB_FSEEK64(FIDDBT,OFF64S,0,FILE_ERROR)
             CALL GLOB_FREAD(FIDDBT,ST,4,TG_NT_TOT,FILE_ERROR)
             LEGGISHOT=0
           ENDIF
           CALL GLOB_FSEEK64(FIDDBA,OFF64,0,FILE_ERROR)
           CALL GLOB_FREAD(FIDDBA,RA,4,TG_NT_TOT,FILE_ERROR)
           CALL GLOB_FSEEK64(FIDDBT,OFF64,0,FILE_ERROR)
           CALL GLOB_FREAD(FIDDBT,RT,4,TG_NT_TOT,FILE_ERROR)
           CALL GLOB_FSEEK64(FIDDBUZ,OFF64,0,FILE_ERROR)
           CALL GLOB_FREAD(FIDDBUZ,RUZ,4,TG_NT_TOT,FILE_ERROR)
#ifdef TIMING
           readtimeplus(TD) = second(0)
CCC           write(140,*) TD, readtimeplus(TD), readtimeless(TD)
#endif
           IF (FILE_ERROR .NE. 0) THEN
             WRITE(*,*) 'comp PE: myrank = ',myrank,'...fine file'
             GOTO 251
           ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
           DO 3000 I=1,NCAMP
CCC 03/05/2002 MPI- Traspongo matrice
CCC             TRACCIA_LETTA(I)=TRACES(TT,I)
CCC 08/05/2002 MPI-
CCC             TRACCIA_LETTA(I)=TRACES(I,TT)
             TRACCIA_LETTA(I)=MY_TRACES(I,TD) 
 3000      CONTINUE
* --------------- equalizzazione tracce -----------------------------------------------
           IF (( FPESI .EQ. 1) .AND. (TR_WEIGHTS(TT) .GT. 0)) THEN
             DO 665 I=1,NCAMP
                TRACCIA_LETTA(I)=TRACCIA_LETTA(I)*TR_WEIGHTS(TT)
 665         CONTINUE
           ENDIF
* --------------- derivatore ----------------------------------------------------------
           TRACCIA_TEMP(1)=(TRACCIA_LETTA(2)-TRACCIA_LETTA(1))/DT
           DO 666 I=2,NCAMP-1
               TRACCIA_TEMP(I)=(TRACCIA_LETTA(I+1)-TRACCIA_LETTA(I-1))/(2.0*DT)
 666       CONTINUE
           TRACCIA_TEMP(NCAMP)=(TRACCIA_LETTA(NCAMP)-TRACCIA_LETTA(NCAMP-1))/DT
           IF ( ANTIALIAS .EQ. 1) THEN
* --------------- filtraggio della traccia con scalino causale e anticausale ----------
             CALL PREFILTRO(TRACCIA_TEMP,NCAMP,NCAMPFILT,BORDF,TRACCE,TRACCIA1,
     $                    TRACCIA2)
        ENDIF

           XB = (TR_XS(TT)+TR_XR(TT))/2.0
           YB = (TR_YS(TT)+TR_YR(TT))/2.0
           ZB = (TR_ZS(TT)+TR_ZR(TT))/2.0
           BINSHIFT = 0
           



           IF (TG_TYPE.EQ.1) THEN
CCC TG_TYPE = 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
****     INIZIO CICLO SUI PANNELLI O SU ASSE C
           DT_AA=(2.0*DX)/(DT*VEL_SUP) 
           DO 500 IC=1,PAN3
              XP=TG_O1(IC)
              YP=TG_O2(IC)
              ZP=TG_O3(IC)
!Clara4 distanza massima Y (XLINE) a cui si migrano i dati (FISSA) 
              MAXDISTX = VETT_DISTX( PAN1)
              MAXDISTY = VETT_DISTY( PAN1)
	
!Clara3 distanza massima Y (XLINE) a cui si migrano i dati  
!* --- IF(3) ---- controllo se i dati distano dal punto meno di maxdisty e maxdistx ------*
                IF ( ( ((ABS(YP-TR_YS(TT)) .LE. MAXDISTY ).AND.
     $               (ABS(YP-TR_YR(TT)) .LE. MAXDISTY)) .OR. 
     $               (MAXDISTY.EQ.-1) ) .AND. ( ((ABS(XP-TR_XS(TT)) .LE. MAXDISTX ).AND.
     $               (ABS(XP-TR_XR(TT)) .LE. MAXDISTX)) .OR. 
     $               (MAXDISTX.EQ.-1) ) )  THEN
* ------ calcolo dell'angolo di vista ----------------------------------------------
                   THETA = PIGRECO / 2
                   IF(ABS(ZB-ZP) .GE. 0.01) THEN
                      AT = SQRT( (XB-XP)**2+(YB-YP)**2 ) / ABS(ZB-ZP)
                      PIVOT = INT( (AT-ATMIN)/ATD ) + 1
                      IF (PIVOT.GT.ATN)   PIVOT=ATN
                      THETA = ATTABLE(PIVOT)
                   ENDIF 
* --- IF(4) ---- controllo se il punto e' interno all'apertura dell'operatore ------*
                   IF (THETA .LE. SEMIAPERTURA) THEN
* ------ calcolo dello smussamento da applicare all'apertura dell'operatore ---------
                     SAGOMATURA=1
                     IF (THETA .GT. SEMIAPERTURA*(1-SMUSSAMENTO)) THEN
                       SAGOMATURA=.5+.5*COS(PIGRECO*(THETA-SEMIAPERTURA*
     $                      (1-SMUSSAMENTO))/(SEMIAPERTURA*SMUSSAMENTO))
                     ENDIF
* ------ per ogni pto da migrare si determinano i vicini per interpolare ------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                     IF ( (ST(IC).LT.0).OR.
     $                    (RT(IC).LT.0) ) THEN
                          ASORG = -20
                          TTOT = -20
                     ELSE
                          AR    = RA(IC)
                          ASORG = SA(IC)
                          TTOT  = ST(IC)+RT(IC)-TSHIFT                     
                          CZR   = RUZ(IC)
                     ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
* --- IF(5) --------- esclusione se dati eikonal non corretti ------------------------*
                     IF (ASORG.GE.(0.0)) THEN
                       SENTETA=SQRT(1-CZR**2)   !fattore di obliquita'
                       POSCAMP=NINT(TTOT/DT) +1      !campione da filtrare
                       VAL_FILTRATO = 0
                       IF ( ANTIALIAS .EQ. 1 ) THEN
************ ANTIALIAS ****************************************************************
                         K=INT(MAX(SENTETA*DT_AA-1,0.0))+1 !semi-lunghezza del filtro
                                                          !antialias a tre campioni
                         IF ( (POSCAMP+BORDF+K.LE.NCAMPFILT)
     $                        .AND.(POSCAMP+BORDF-K.GE.1)) THEN
                              Z1=1/((REAL(K))**2)            !coefficienti del filtro
                              T0=TRACCE(POSCAMP+BORDF)
                              T1=TRACCE(POSCAMP+BORDF+K)
                              T2=TRACCE(POSCAMP+BORDF-K)                      
                              VAL_FILTRATO=Z1*(T1+T2-2*T0)
                         ENDIF
************ FINE ANTIALIAS ***********************************************************
                       ELSE
                         IF ( (POSCAMP.LE.NCAMP)
     $                        .AND.(POSCAMP.GE.1)) THEN
                           VAL_FILTRATO = -TRACCIA_TEMP(POSCAMP)
                         ENDIF
                       ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Calcolo contributo alla matrice finale
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC IMAGING = 1
                       IF (IMAGING .EQ.1) THEN      !IMAGING CONDITION 1:
                                                    !Prodotto ampiezze
                            DELTAPAN=
     $                         ASORG * AR * VAL_FILTRATO *
     $                         ABS(CZR) * SAGOMATURA
                            PAN_PARZ(IA,IB,IC,ID,IE)=PAN_PARZ(IA,IB,IC,ID,IE)+DELTAPAN
                            CONT_RPARZ(IA,IB,IC,ID,IE) = CONT_RPARZ(IA,IB,IC,ID,IE)+1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC IMAGING = 2
                       ELSEIF (IMAGING .EQ.2) THEN  !IMAGING CONDITION 2:
                                                    !Divisione ampiezze  modo 1
                            IF (ASORG.GE.SOGLIA_AMPIEZZE) THEN
                               DELTAPAN=
     $                             AR / ASORG * VAL_FILTRATO *
     $                             ABS(CZR) * SAGOMATURA
                               PAN_PARZ(IA,IB,IC,ID,IE)=PAN_PARZ(IA,IB,IC,ID,IE)+DELTAPAN
                               CONT_RPARZ(IA,IB,IC,ID,IE) = CONT_RPARZ(IA,IB,IC,ID,IE)+1
                            ELSE
                               DELTAPAN=0
                            ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC IMAGING = 3
                       ELSEIF (IMAGING .EQ.3) THEN  !IMAGING CONDITION 3:
                                                    !Divisione ampiezze  modo 2
                            DELTAPAN=
     $                          AR / (ASORG + SOGLIA_AMPIEZZE) *
     $                          VAL_FILTRATO *
     $                          ABS(CZR) * SAGOMATURA
                            PAN_PARZ(IA,IB,IC,ID,IE)=PAN_PARZ(IA,IB,IC,ID,IE)+DELTAPAN
                            CONT_RPARZ(IA,IB,IC,ID,IE) = CONT_RPARZ(IA,IB,IC,ID,IE)+1
!                           CONT_SPARZ(IA,IB,IC) = 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC IMAGING = 4
                       ELSEIF (IMAGING .EQ.4) THEN  !IMAGING CONDITION 4:
                                                    !No ampiezze
                            DELTAPAN=
     $                          VAL_FILTRATO *
     $                          ABS(CZR) * SAGOMATURA
                            PAN_PARZ(IA,IB,IC,ID,IE)=PAN_PARZ(IA,IB,IC,ID,IE)+DELTAPAN
                            CONT_RPARZ(IA,IB,IC,ID,IE) = CONT_RPARZ(IA,IB,IC,ID,IE)+1
!                           CONT_SPARZ(IA,IB,IC) = 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                       ENDIF
                     ENDIF
* --- ENDIF(5) --------- endif di esclusione sorgente -------------------*
                    ENDIF
* --- ENDIF(4) -------- endif di esclusione apertura THETA --------------*
              ENDIF
* --- ENDIF(3) -------- endif di esclusione per maxdisty --------------*
 500        CONTINUE
****     FINE CICLO SUI PANNELLI O SU ASSE C

            ENDIF
            NTR_MIG_CICLO=NTR_MIG_CICLO+1
       ENDIF
* --- ENDIF ---------- endif di esclusione traccia ------------------*
       NTR_CICLO=NTR_CICLO+1   ! incremento nr. tracce lette nel ciclo
 250   CONTINUE
***********************************************************************************
*    FINE MIGRAZIONE delle tracce di un gruppo                                    *
***********************************************************************************
 251     CONTINUE
       IF ( (T .LE. NTR_FILE) .OR.
     $      (T .LE. NTRACCEFILE))  THEN
         IF (VERBOSE .GT. 1) THEN
           WRITE(*,*) 'comp PE: myrank = ',myrank,' Tracce analizzate:        ', NTR_CICLO
           WRITE(*,*) 'comp PE: myrank = ',myrank,' Tracce migrate effettive: ', NTR_MIG_CICLO
         ENDIF
       ENDIF
CCC 28/05/2002 Per confrontare risultati parziali
CCC       write(100*myrank+10*SS+S,*) PAN_PARZ, CONT_RPARZ
 200  CONTINUE
      END
