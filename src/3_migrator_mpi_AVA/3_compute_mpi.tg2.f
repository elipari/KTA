      SUBROUTINE COMPUTE_TG2(NF, DIMGRUPPOTR, DIMBLOCCOTR,
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
     $    W_A, W_B, ATMIN, ATMAX, ATD, ATN, ATTABLE, NTR_FILE,
     $    RUZ, FIDDBA, FIDDBT, FIDDBUZ, NTR_MIG_CICLO,
     $    SS, S, VETT_APE, VETT_DISTX, VETT_DISTY, MUTEFLAG, VETT_OFF,     
     $    TG_WEIGHTS_LIN_A, TG_WEIGHTS_LIN_B, TG_WEIGHTS_CUB_B,
     $    RANGET, RANGEA, RANGECZ, TEMPFDG,DAMP_AMPIEZZE,
     $    FIDDBVX, FIDDBVY, FIDDBVZ, SVX, SVY, SVZ, RVX, RVY, RVZ, TG_NT_AVA,
     $    QUANTTAC, QUANTV, SORTFILETAC, SORTFILEV, ACQ_NT_TOT,
     $    myrank,GRID_EL
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
      REAL*4        TRACCIA1(MAX_NCAMP+BORDF),TRACCIA2(MAX_NCAMP+BORDF)
      REAL*4        TR_XR(MAXDIMBLOCCOTR)
      REAL*4        TR_YR(MAXDIMBLOCCOTR)
      REAL*4        TR_ZR(MAXDIMBLOCCOTR)
      INTEGER*8     BINR_OFFSET(MAXDIMBLOCCOTR)
CCC 03/05/2002 MPI-
      REAL*4        TR_WEIGHTS(MAXDIMBLOCCOTR)
      REAL*4        TR_XS(MAXDIMBLOCCOTR)
      REAL*4        TR_YS(MAXDIMBLOCCOTR)
      REAL*4        TR_ZS(MAXDIMBLOCCOTR)

      INTEGER*8     TR_NS(MAXDIMBLOCCOTR)
      INTEGER*8     BINS_OFFSET(MAXDIMBLOCCOTR)
      INTEGER*4     PAN1,PAN2,PAN3,PAN4,PAN5
      REAL*4        PAN_PARZ(PAN1,PAN2,PAN3,PAN4,PAN5)
      INTEGER*4     CONT_RPARZ(PAN1,PAN2,PAN3,PAN4,PAN5)
      REAL*4        VETT_APE(PAN1)
      REAL*4        VETT_DISTX(PAN1), VETT_DISTY(PAN1)
      REAL*4        MAXDISTX, MAXDISTY
      INTEGER*4     IASTART, MUTEFLAG
      REAL*4        VETT_OFF(PAN1)     
        REAL*4		GRID_EL(PAN4+1)
      
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
      INTEGER*4     W_A, W_B                                  
      REAL*4        TG_WEIGHTS_LIN_B(W_B,TG_NPAN), TG_WEIGHTS_LIN_A(W_A,TG_NPAN), TG_WEIGHTS_CUB_B(4,W_B,4,TG_NPAN)
      REAL*4        SA(TG_NT_TOT),ST(TG_NT_TOT)
      REAL*4        RA(TG_NT_TOT), RT(TG_NT_TOT)
      REAL*4        RUZ(TG_NT_TOT)
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
      REAL*4        PIGRECO
      PARAMETER(PIGRECO = 3.14159265358979)
      REAL*4        PIMEZZI
      PARAMETER(PIMEZZI = 1.57079632679490)
      REAL*4        PISUSMUS
      INTEGER*4     NTR_CICLO
      INTEGER*4     NTR_FILE, NTR_MIG, NTR_MIG_CICLO
      INTEGER*8     OFF64, OFF64S, OFF64_AS, OFF64_AR
      INTEGER*8     DIMELEM(0:1)
      INTEGER*8     OFF64OLD, OFF64SOLD, OFF64_ASOLD, OFF64_AROLD 
      INTEGER*4     FILE_ERROR
      INTEGER*4     I, S, T, TD, IA, IB, IC, KK, SS, TT, ID, IE
      INTEGER*4     NNA, NNB, NA, NB
      INTEGER*4     BINSHIFT
      REAL*4        AT, ATD, ATMIN, ATMAX
      REAL*4        DELTAPAN
      REAL*4        CZR
      REAL*4        TTOT, ASORG, AR
      REAL*4        VAL_FILTRATO
      INTEGER*4     K, POSCAMP
      REAL*4        DT_AA, SENTETA, Z1
      REAL*4        T0,T1,T2
      REAL*4        SEMIAPERTURA,SAGOMATURA
      INTEGER*4     NTR_CARICATE

      INTEGER*8     NUMSORGPREC
      INTEGER*4     LEGGISHOT
      REAL*4        OFFCUR
      INTEGER*4     OVER_A,OVER_B
      
      REAL*4        TR_XS_REL,TR_YS_REL,TR_ZS_REL,TR_XR_REL,TR_YR_REL,TR_ZR_REL
      REAL*4        IAA_S,IBB_S,ICC_S,IAA_R,IBB_R,ICC_R
      REAL*4        IAA_MID,IBB_MID,ICC_MID
      INTEGER*4     PRIMOB,PRIMOBLIN,IBB_LASCO,NBB_CELLA,IASTARTLASCO,IAA_LASCO,NAA_CELLA,FLAG,BASE,INDICE,J
      REAL*4        ST_LASCO(4,2),RT_LASCO(4,2),RA_LASCO(2,2),SA_LASCO(2,2),RUZ_LASCO(2,2)
      INTEGER*4     IBB,IBB_P,IAA,IASTARTFITTO,IAA_P,IASTARTFITTOFIRST,BASEB,BASEB_LIN,BASE_LIN
      REAL*4        PLIN_B(2),PCUB_B(4),PLIN_A(2),AR_LASCO(2),ASORG_LASCO(2),CZR_LASCO(2),TTOT_LASCO(2)
#ifdef AVACODE
      real *4       SVX_LASCO1(2,2),SVY_LASCO1(2,2),SVZ_LASCO1(2,2),RVX_LASCO1(2,2),RVY_LASCO1(2,2), RVZ_LASCO1(2,2)
      REAL*4        SVX_LASCO2(2),SVY_LASCO2(2),SVZ_LASCO2(2),RVX_LASCO2(2),RVY_LASCO2(2), RVZ_LASCO2(2)
      REAL*4        SVX_FITTO,SVY_FITTO,SVZ_FITTO,RVX_FITTO,RVY_FITTO, RVZ_FITTO
      REAL*4        UX, UY, UZ
      INTEGER*4     FLAGAVA
      
#endif   
      real*4       COSALIAS   
#ifdef TIMING
      real*8 second
      real*8 readtimeplus(DIMGRUPPOTR), readtimeless(DIMGRUPPOTR)
#endif
CCC 03/05/2002 MPI- MPI variables
      integer*4 myrank
      real*4    MY_TRACES(MAX_NCAMP, DIMGRUPPOTR)
      
      REAL*4       RANGET(TG_NT_TOT,2), RANGEA(TG_NT_TOT,2), RANGECZ(TG_NT_TOT,2)
      INTEGER*2    TEMPFDG(TG_NT_TOT)
     
      REAL*4     RAGGIOINIZ,RAGGIOC,RAGGIOINIZC,RAGGIOB,RAGGIOINIZB
      REAL*4     DXB,DXC,SAGOMATURAB,SAGOMATURAC
      INTEGER*4  OFFOUT
      
      REAL*4    DAMP_AMPIEZZE
      
      INTEGER*8 ZERO_8,UNO_8,DUE_8,TG_NT_TOT_8

      REAL*4    NBB_REAL,APEMAX_B
      INTEGER*4 IBSTART_MAXAPE,IBEND_MAXAPE
      
      integer*4 migrata
      REAL*4    TEMP,TSHIFTSUDT

      INTEGER*4 INDEX_TAC, INDEX_V, IEL

      PISUSMUS = PIGRECO/SMUSSAMENTO
      TSHIFTSUDT = TSHIFT/DT
        
     
    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCC INIZIALIZZAZIONI
CCC 27/06/2002 Inizializzazione PROVVISORIA effettiva per TG_TYPE = 1
        ZERO_8=0
        UNO_8 =1
        DUE_8 =2
        TG_NT_TOT_8=TG_NT_TOT
        
        INDEX_TAC =0
        IF ((QUANTTAC .EQ. 1) .OR.(QUANTTAC .EQ. 2)) INDEX_TAC = 1          
        INDEX_V =0
        IF ((QUANTV .EQ. 1) .OR.(QUANTV .EQ. 2)) INDEX_V = 1          
        DIMELEM(0) = 4
        DIMELEM(1) = 2
        
        NNB=PAN2
        NNA=PAN1
        
        OFF64OLD    = -1
        OFF64SOLD   = -1 
        OFF64_ASOLD = -1
        OFF64_AROLD = -1
!CCC 08/05/2002 MPI- Messo qui per funzionare anche se nell'ultimo SS qualche PE non deve fare nulla
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
        
        IF (DBRESTART(SS).EQ.1) RETURN

        DO 50 KK=1,TG_NT_TOT
          RA(KK) =-2
          RT(KK) =-2
          RUZ(KK)=-2
 50     CONTINUE
          
        DO 51 KK=1,TG_NT_AVA
          RVX(KK)=-2
          RVY(KK)=-2
          RVZ(KK)=-2
          SVX(KK)=-2
          SVY(KK)=-2
          SVZ(KK)=-2
 51     CONTINUE
 
       STEP_DIP = 0
       STEP_AZ  = 0
       IF (DIP_MAX .NE. -1.0) THEN
           STEP_DIP = ((DIP_MAX-DIP_MIN)/PAN4)
       ENDIF
       IF (AZIMUTH_MAX .NE. -1.0) THEN
           STEP_AZ = ((AZIMUTH_MAX-AZIMUTH_MIN)/PAN5)
       ENDIF
       STEP_OFF = 0       
       IF (AVO .EQ. 1) THEN
           STEP_OFF = ((OFFMAX-OFFMIN)/PAN4)
       ENDIF
  
       ID=1
       IE=1
 
        LEGGISHOT=0
        NUMSORGPREC=-2
        NTR_MIG_CICLO =0
        NTR_CICLO=0
        T=(SS-1)*DIMBLOCCOTR+(S-1)*DIMGRUPPOTR +1

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    *
!*         CICLO SULLE TRACCE DI UN GRUPPO - PREFILTRAGGIO & MIGRAZIONE         *
!*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    *
        DO 250 TD=1,DIMGRUPPOTR
         TT = (S-1)*DIMGRUPPOTR + TD                  ! indice tracce nel blocco
         T = (SS-1)*DIMBLOCCOTR + TT            ! indice tracce totale nel file
         NTR_CARICATE = T-1                     ! nr. tracce precedenti
!CCC 10/06/2002 Altrimenti l'ultima traccia del blocco (dell'ultimo gruppo) non viene
!CCC    mai migrata e il risultato finale dipende dalla dimenzione del blocco
         IF ( (T .GT. NTR_FILE) .OR.
     $        (T .GT. NTRACCEFILE))  RETURN
   
! CALCOLO OFFSET DELLA TRACCIA CORRENTE
         OFFCUR = SQRT((TR_XS(TT)-TR_XR(TT))**2+(TR_YS(TT)-TR_YR(TT))**2+(TR_ZS(TT)-TR_ZR(TT))**2)

!------------------AVO------------------------------------------
         IF (AVO.EQ.1) THEN
            ID = (OFFCUR - OFFMIN)/STEP_OFF + 1
            IF (ID .GT. PAN4) ID=PAN4
         ENDIF
        
!*** - IF - esclusione di una traccia se il ricevitore non appartiene al database-----*
         IF ( (BINR_OFFSET(TT) .GT. ZERO_8) .AND.
     $        (TR_WEIGHTS(TT) .GT. 0) .AND.
     $        (BINS_OFFSET(TT) .GT. ZERO_8)) THEN
           SORT_INDEX = SORTFILETAC(BINR_OFFSET(TT))
           OFF64 = (SORT_INDEX- UNO_8)*TG_NT_TOT_8
#ifdef AVACODE
           SORT_INDEX = SORTFILEV(BINR_OFFSET(TT))
           OFF64_AR =( SORT_INDEX - UNO_8)*TG_NT_TOT_8
#endif           
           IF (TR_NS(TT) .NE. NUMSORGPREC) THEN
             NUMSORGPREC = TR_NS(TT)
             LEGGISHOT   = 1
             SORT_INDEX = SORTFILETAC(BINS_OFFSET(TT))
             OFF64S   = (SORT_INDEX - UNO_8)*TG_NT_TOT_8
#ifdef AVACODE
             SORT_INDEX = SORTFILEV(BINS_OFFSET(TT))
             OFF64_AS = ( SORT_INDEX - UNO_8)*TG_NT_TOT_8
#endif
           ENDIF


#ifdef TIMING
           readtimeless(TD)=second(0)
#endif

           IF ( LEGGISHOT .EQ. 1) THEN
              if ( off64S .NE. OFF64SOLD) THEN
              	 CALL READDBFILE ( FIDDBA, OFF64S, TG_NT_TOT, RANGEA, SA,
     $                             TEMPFDG, FILE_ERROR, QUANTTAC, DIMELEM(INDEX_TAC))     
                 CALL READDBFILE ( FIDDBT, OFF64S, TG_NT_TOT, RANGET, ST,
     $                             TEMPFDG, FILE_ERROR, QUANTTAC, DIMELEM(INDEX_TAC))

                 OFF64SOLD=OFF64S
              ENDIF
        
#ifdef AVACODE
              if ( off64_aS .NE. OFF64_ASOLD) THEN
              	 CALL READDBFILE ( FIDDBVX, OFF64_AS, TG_NT_AVA, RANGEA, SVX,
     $                             TEMPFDG, FILE_ERROR, QUANTV, DIMELEM(INDEX_V))     
                 CALL READDBFILE ( FIDDBVY, OFF64_AS, TG_NT_AVA, RANGEA, SVY,
     $                             TEMPFDG, FILE_ERROR, QUANTV, DIMELEM(INDEX_V))    
                 CALL READDBFILE ( FIDDBVZ, OFF64_AS, TG_NT_AVA, RANGEA, SVZ,
     $                             TEMPFDG, FILE_ERROR, QUANTV, DIMELEM(INDEX_V))

                 OFF64_ASOLD = OFF64_AS
              ENDIF
#endif
              LEGGISHOT=0  
           ENDIF
           
           if ( off64 .NE. OFF64OLD) THEN
              CALL READDBFILE ( FIDDBA, OFF64, TG_NT_TOT, RANGEA, RA,
     $                          TEMPFDG, FILE_ERROR, QUANTTAC, DIMELEM(INDEX_TAC))
              CALL READDBFILE ( FIDDBT, OFF64, TG_NT_TOT, RANGET, RT,
     $                          TEMPFDG, FILE_ERROR, QUANTTAC, DIMELEM(INDEX_TAC))
              CALL READDBFILE ( FIDDBUZ, OFF64, TG_NT_TOT, RANGECZ, RUZ,
     $                          TEMPFDG, FILE_ERROR, QUANTTAC, DIMELEM(INDEX_TAC))

              OFF64OLD = OFF64
           ENDIF
           
#ifdef AVACODE
          if ( off64_AR .NE. OFF64_arOLD) THEN
              CALL READDBFILE ( FIDDBVX, OFF64_AR, TG_NT_AVA, RANGEA, RVX,
     $                          TEMPFDG, FILE_ERROR, QUANTV, DIMELEM(INDEX_V))
              CALL READDBFILE ( FIDDBVY, OFF64_AR, TG_NT_AVA, RANGEA, RVY,
     $                          TEMPFDG, FILE_ERROR, QUANTV, DIMELEM(INDEX_V))
              CALL READDBFILE ( FIDDBVZ, OFF64_AR, TG_NT_AVA, RANGEA, RVZ,
     $                          TEMPFDG, FILE_ERROR, QUANTV, DIMELEM(INDEX_V))
              OFF64_AROLD = OFF64_AR
          ENDIF
#endif
     
#ifdef TIMING
           readtimeplus(TD) = second(0)
#endif
           IF (FILE_ERROR .NE. 0) THEN
             WRITE(*,*) 'comp PE: myrank = ',myrank,'...fine file'
             GOTO 251
           ENDIF

! --------------- equalizzazione tracce -----------------------------------------------
           CALL EQTRACES (FPESI,MY_TRACES(1,TD),TR_WEIGHTS(TT),NCAMP)
! --------------- derivatore ----------------------------------------------------------
           CALL DERIV (MY_TRACES(1,TD),TRACCIA_TEMP,NCAMP,DT)
!--------------- ANTIALIAS ----------------------------------------------------------
           IF ( ANTIALIAS .EQ. 1) THEN
! --------------- filtraggio della traccia con scalino causale e anticausale ----------
              CALL PREFILTRO(TRACCIA_TEMP,NCAMP,NCAMPFILT,BORDF,TRACCE,TRACCIA1,
     $                    TRACCIA2)
            ENDIF

           BINSHIFT = 0
           migrata=0

!****     INIZIO CICLO SUI PANNELLI 
           DT_AA=(2.0*DX)/(DT*VEL_SUP)          
           
!        return
           DO 700 IC=1,PAN3
              NNB=TG_NNB(IC)
              NNA=TG_NNA(IC)
              NB =TG_NB(IC)
              NA =TG_NA(IC)
              OVER_A=(TG_NNA(IC)-1)/(TG_NA(IC)-1)
              OVER_B=(TG_NNB(IC)-1)/(TG_NB(IC)-1)
              
! CALCOLO DEL MUTE ESTERNO - in base all'offset calcolo la profondità da cui partire a migrare la traccia           
              CALL FIRSTDEPTH (IASTART,MUTEFLAG,PAN1,VETT_OFF,OFFCUR,OFFOUT)
              IASTARTLASCO=(IASTART-1)/OVER_A + 1
              IASTARTFITTOFIRST=IASTART-(IASTARTLASCO-1)*OVER_A 
              
         
              ! Rotazione delle coordinate di sorgente e ricevitore dal sistema di riferimento del modello 
              !al sistema di riferimento del pannello
              TR_XS_REL = TR_XS(TT)- TG_O1(IC)
              TR_YS_REL = TR_YS(TT)- TG_O2(IC)
              TR_ZS_REL = TR_ZS(TT)- TG_O3(IC)
              TR_XR_REL = TR_XR(TT)- TG_O1(IC)
              TR_YR_REL = TR_YR(TT)- TG_O2(IC)
              TR_ZR_REL = TR_ZR(TT)- TG_O3(IC)
              TG_DDC=1
              IAA_S=(TR_XS_REL*TG_CA1(IC)+TR_YS_REL*TG_CA2(IC)+TR_ZS_REL*TG_CA3(IC))/TG_DDA(IC)+1
              IBB_S=(TR_XS_REL*TG_CB1(IC)+TR_YS_REL*TG_CB2(IC)+TR_ZS_REL*TG_CB3(IC))/TG_DDB(IC)+1  
              ICC_S=(TR_XS_REL*TG_CC1(IC)+TR_YS_REL*TG_CC2(IC)+TR_ZS_REL*TG_CC3(IC))/TG_DDC+1  
              IAA_R=(TR_XR_REL*TG_CA1(IC)+TR_YR_REL*TG_CA2(IC)+TR_ZR_REL*TG_CA3(IC))/TG_DDA(IC)+1 
              IBB_R=(TR_XR_REL*TG_CB1(IC)+TR_YR_REL*TG_CB2(IC)+TR_ZR_REL*TG_CB3(IC))/TG_DDB(IC)+1  
              ICC_R=(TR_XR_REL*TG_CC1(IC)+TR_YR_REL*TG_CC2(IC)+TR_ZR_REL*TG_CC3(IC))/TG_DDC+1 

!   ! OFFSET CORRENTE SULLA GRIGLIA DEL MODELLO FITTO           
!              OFFCUR_NORM = SQRT((IAA_S-IAA_R)**2+(IBB_S-IBB_R)**2+(ICC_S-ICC_R)**2)
!              RAGGIOINIZ=OFFCUR_NORM/2          
   
              RAGGIOINIZ=OFFCUR/2
              RAGGIOINIZB=RAGGIOINIZ/TG_DDB(IC)
              RAGGIOINIZC=RAGGIOINIZ/TG_DDC

   ! COORDINATE DEL MID-POINT
              IAA_MID = (IAA_S+IAA_R)/2.0
              IBB_MID = (IBB_S+IBB_R)/2.0
              ICC_MID = (ICC_S+ICC_R)/2.0
                          
              APEMAX_B = RAGGIOINIZB+VETT_APE(NNA)*(NNA-1)/TG_DDB(IC)
              IBSTART_MAXAPE=(MAX(IBB_MID-APEMAX_B-1,1.0))/OVER_B + 1
              NBB_REAL=NNB-1
              IBEND_MAXAPE=(MIN(IBB_MID+APEMAX_B-1,NBB_REAL))/OVER_B + 1
             
!***     INIZIO CICLO lasco SU ASSE B
              
              DO 710 IB=IBSTART_MAXAPE,IBEND_MAXAPE   
                 PRIMOB=1
                 PRIMOBLIN=0
!GESTIONE BORDI
                 IF (IB.EQ.NB) THEN 
                 	PRIMOB=3
                 	PRIMOBLIN=1
                 ELSEIF (IB.EQ.NB-1) THEN
                 	PRIMOB=2
                 ELSEIF (IB.EQ.1) THEN
                 	PRIMOB=0
                 ENDIF
                 
                 IBB_LASCO=(IB-1)*OVER_B
                 NBB_CELLA=OVER_B
                 IF (IB.EQ.NB) NBB_CELLA=1  
                 BASEB= (IB-1-PRIMOB)* NA  
                 BASEB_LIN= (IB-1-PRIMOBLIN)* NA                            	     
!***     INIZIO CICLO lasco SU ASSE A
                 DO 720 IA=IASTARTLASCO,NA-1

                       IASTARTFITTO=1
                       IF (IA.EQ. IASTARTLASCO) IASTARTFITTO=IASTARTFITTOFIRST
                       IAA_LASCO=(IA-1)*OVER_A
                       NAA_CELLA=OVER_A
                       IF (IA.EQ.NA-1) NAA_CELLA=OVER_A+1
! ESTRAZIONE VALORI RETICOLO LASCO PER INTERPOLAZIONE BICUBICA                    
                       FLAG = 0
                       BASE = IA + BASEB + BINSHIFT 
                       BASE_LIN = IA + BASEB_LIN + BINSHIFT
                     
                       INDICE = BASE   
                                                              
                       DO 760 J = 1,4
                          DO 761 I = 1,2 
                             INDICE = INDICE +I -1
                             ST_LASCO(J,I)= ST(INDICE)
                             RT_LASCO(J,I)= RT(INDICE) 
! CONTROLLO VALIDITA' TEMPI SUL RETICOLO LASCO
                             IF ((ST_LASCO(J,I).LT.0).OR.(RT_LASCO(J,I).LT.0)) THEN
                             	FLAG=1
                             	GOTO 770
                             ELSE
                             	ST_LASCO(J,I) = ST_LASCO(J,I)/DT
                             	RT_LASCO(J,I) = RT_LASCO(J,I)/DT
                             ENDIF
 761                      CONTINUE
                          INDICE = INDICE + NA -1
 760                   CONTINUE 

                      
! ESTRAZIONE VALORI RETICOLO LASCO PER INTERPOLAZIONE BILINEARE                                          
                       INDICE = BASE_LIN
                                        
#ifdef AVACODE                             
                       FLAGAVA=0
#endif     
                       DO 763 J = 1,2
                          DO 764 I = 1,2
                             INDICE = INDICE +I -1
                             RA_LASCO(J,I) = RA(INDICE)
                             SA_LASCO(J,I) = SA(INDICE)
                             RUZ_LASCO(J,I)= RUZ(INDICE)
#ifdef AVACODE                             
                             RVX_LASCO1(J,I)= RVX(INDICE)
                             RVY_LASCO1(J,I)= RVY(INDICE)
                             RVZ_LASCO1(J,I)= RVZ(INDICE)
                             SVX_LASCO1(J,I)= SVX(INDICE)
                             SVY_LASCO1(J,I)= SVY(INDICE)
                             SVZ_LASCO1(J,I)= SVZ(INDICE)
                             IF ((abs(RVX_LASCO1(J,I)).GT.1.0).OR.(abs(SVX_LASCO1(J,I)).GT.1.0)) THEN
!                             	FLAG=1
                                FLAGAVA=1
                             	GOTO 770
                             ENDIF
#endif                             

 764                      CONTINUE
                          INDICE = INDICE + NA -1 
 763                   CONTINUE 
 770                 CONTINUE

#ifdef AVACODE 
                     IF ( FLAGAVA.EQ.1 ) THEN
                     	DO 765 J = 1,2
                          DO 766 I = 1,2
                             RVX_LASCO1(J,I)= 0.0
                             RVY_LASCO1(J,I)= 0.0
                             RVZ_LASCO1(J,I)= 1.0
                             SVX_LASCO1(J,I)= 0.0
                             SVY_LASCO1(J,I)= 0.0
                             SVZ_LASCO1(J,I)= 1.0
 766                      CONTINUE
 765                    CONTINUE 
                     ENDIF	
#endif

                     IF ( FLAG.EQ.0 ) THEN

****     INIZIO CICLO FITTO SU ASSE B                   
                        DO 730 IBB=1,NBB_CELLA

                            IBB_P = IBB_LASCO+IBB
! PESI PER L'INTERPOLAZIONE BILINEARE                              
                            PLIN_B(1)= TG_WEIGHTS_LIN_B(IBB,IC)
!                            PLIN_B(2)= 1.0 - PLIN_B(1)
                            IF ( IB .EQ. NB) THEN
                            	PLIN_B(2)= TG_WEIGHTS_LIN_B(IBB,IC)
                                PLIN_B(1)= 1.0 - PLIN_B(2)
                            ENDIF

! PESI PER L'INTERPOLAZIONE CUBICA+LINEARE                              
                            PCUB_B(1) = TG_WEIGHTS_CUB_B(1,IBB,PRIMOB+1,IC)
                            PCUB_B(2) = TG_WEIGHTS_CUB_B(2,IBB,PRIMOB+1,IC)
                            PCUB_B(3) = TG_WEIGHTS_CUB_B(3,IBB,PRIMOB+1,IC)
                            PCUB_B(4) = TG_WEIGHTS_CUB_B(4,IBB,PRIMOB+1,IC)
                            
! CALCOLO AMPIEZZE E COSENO DIRETTORE Z CON INTERPOLAZIONE LIN+LIN E TEMPI CON CUBICA+LIN
                            DO 780 I=1,2                            

                                TTOT_LASCO(I)  = -TSHIFTSUDT
                                ! INTERPOLAZIONE LINEARE
!                                DO 781 J=1,2
                                    AR_LASCO(I)    = PLIN_B(1)*(RA_LASCO(1,I)   - RA_LASCO(2,I)  ) + RA_LASCO(2,I) 
                                    ASORG_LASCO(I) = PLIN_B(1)*(SA_LASCO(1,I)   - SA_LASCO(2,I)  ) + SA_LASCO(2,I) 
                                    CZR_LASCO(I)   = PLIN_B(1)*(RUZ_LASCO(1,I)  - RUZ_LASCO(2,I) ) + RUZ_LASCO(2,I) 
#ifdef AVACODE    
                                    RVX_LASCO2(I)  = PLIN_B(1)*(RVX_LASCO1(1,I) - RVX_LASCO1(2,I)) + RVX_LASCO1(2,I)
                                    RVY_LASCO2(I)  = PLIN_B(1)*(RVY_LASCO1(1,I) - RVY_LASCO1(2,I)) + RVY_LASCO1(2,I)
                                    RVZ_LASCO2(I)  = PLIN_B(1)*(RVZ_LASCO1(1,I) - RVZ_LASCO1(2,I)) + RVZ_LASCO1(2,I)
                                    SVX_LASCO2(I)  = PLIN_B(1)*(SVX_LASCO1(1,I) - SVX_LASCO1(2,I)) + SVX_LASCO1(2,I)
                                    SVY_LASCO2(I)  = PLIN_B(1)*(SVY_LASCO1(1,I) - SVY_LASCO1(2,I)) + SVY_LASCO1(2,I)
                                    SVZ_LASCO2(I)  = PLIN_B(1)*(SVZ_LASCO1(1,I) - SVZ_LASCO1(2,I)) + SVZ_LASCO1(2,I)
#endif                                                          
! 781                            CONTINUE
                                ! INTERPOLAZIONE CUBICA
                                DO 782 J=1,4
                                    TTOT_LASCO(I)  = TTOT_LASCO(I)  + PCUB_B(J)*(ST_LASCO(J,I)+RT_LASCO(J,I))
 782                            CONTINUE
 780                        CONTINUE
                              	
****     INIZIO CICLO FITTO SU ASSE A                     
                            DO 740 IAA=IASTARTFITTO,NAA_CELLA
                               IAA_P = IAA_LASCO + IAA
                               SEMIAPERTURA=VETT_APE(IAA_P)
#ifdef TEST
                               MAXDISTX = VETT_DISTX(IAA_P)
                               MAXDISTY = VETT_DISTY(IAA_P)
! --- IF(3) ---- controllo se i dati distano dal punto meno di maxdisty e maxdistx ------*
                               IF ( ( ((ABS(IBB_P-IBB_S) .LE. MAXDISTX ).AND.
     $                                 (ABS(IBB_P-IBB_R) .LE. MAXDISTX)) .OR. 
     $                                 (MAXDISTX.EQ.-1) ) .AND. 
     $                              ( ((ABS(1.0-ICC_S) .LE. MAXDISTY ).AND.
     $                                 (ABS(1.0-ICC_R) .LE. MAXDISTY)) .OR. 
     $                                 (MAXDISTY.EQ.-1) ) )  THEN
! ------ calcolo dell'angolo di vista ----------------------------------------------
#endif
                                        TEMP = (IAA_P-1)*SEMIAPERTURA
                                        RAGGIOB=RAGGIOINIZB+TEMP/TG_DDB(IC)
                                        RAGGIOC=RAGGIOINIZC+TEMP/TG_DDC                                        
                                        DXB=ABS(IBB_P-IBB_MID)
                                        DXC=ABS(1.0-ICC_MID)
! --- IF(4) ---- controllo se il punto e' interno all'apertura dell'operatore ------*

                                        IF ((DXB.LE. RAGGIOB).AND.
     $	                                    (DXC.LE. RAGGIOC)) THEN
! ------ calcolo dello smussamento da applicare all'apertura dell'operatore ---------
                                             SAGOMATURAB=1
                                             SAGOMATURAC=1

                                             IF (DXB .GT. RAGGIOB*(1-SMUSSAMENTO)) THEN
!                                                  SAGOMATURAB=(-DXB/RAGGIOB +1.0)/SMUSSAMENTO
                                                 SAGOMATURAB=.5+.5*COS(PISUSMUS*(DXB-RAGGIOB*
     $                                                    (1-SMUSSAMENTO))/RAGGIOB)
                                             ENDIF
                                             IF (DXC .GT. RAGGIOC*(1-SMUSSAMENTO)) THEN
!                                             	  SAGOMATURAc=(-DXC/RAGGIOC +1.0)/SMUSSAMENTO
                                                 SAGOMATURAC=.5+.5*COS(PISUSMUS*(DXC-RAGGIOC*
     $                                                    (1-SMUSSAMENTO))/RAGGIOC)
                                             ENDIF
                                             SAGOMATURA=SAGOMATURAB*SAGOMATURAC
! PESI PER L'INTERPOLAZIONE BILINEARE        
                                                
                                             PLIN_A(1)= TG_WEIGHTS_LIN_A(IAA,IC)
!                                             PLIN_A(2)= 1.0 - PLIN_A(1) 
                                                                         
! CALCOLO AMPIEZZE E COSENO DIRETTORE Z CON INTERPOLAZIONE LIN+LIN E TEMPI CON CUBICA+LIN
                                             AR    = PLIN_A(1)*(AR_LASCO(1)    - AR_LASCO(2)   ) + AR_LASCO(2)
                                             ASORG = PLIN_A(1)*(ASORG_LASCO(1) - ASORG_LASCO(2)) + ASORG_LASCO(2)
                                             CZR   = PLIN_A(1)*(CZR_LASCO(1)   - CZR_LASCO(2)  ) + CZR_LASCO(2)
                                             TTOT  = PLIN_A(1)*(TTOT _LASCO(1) - TTOT _LASCO(2)) + TTOT _LASCO(2)  
#ifdef AVACODE
                                             RVX_FITTO   = PLIN_A(1)*(RVX_LASCO2(1) - RVX_LASCO2(2)) + RVX_LASCO2(2)
                                             RVY_FITTO   = PLIN_A(1)*(RVY_LASCO2(1) - RVY_LASCO2(2)) + RVY_LASCO2(2)
                                             RVZ_FITTO   = PLIN_A(1)*(RVZ_LASCO2(1) - RVZ_LASCO2(2)) + RVZ_LASCO2(2)
                                             SVX_FITTO   = PLIN_A(1)*(SVX_LASCO2(1) - SVX_LASCO2(2)) + SVX_LASCO2(2)
                                             SVY_FITTO   = PLIN_A(1)*(SVY_LASCO2(1) - SVY_LASCO2(2)) + SVY_LASCO2(2)
                                             SVZ_FITTO   = PLIN_A(1)*(SVZ_LASCO2(1) - SVZ_LASCO2(2)) + SVZ_LASCO2(2)
#endif
! --- IF(5) --------- esclusione se dati eikonal non corretti ------------------------*
                                             IF (ASORG.GE.SOGLIA_AMPIEZZE) THEN
                                               SENTETA=SQRT(1-CZR*CZR)   !fattore di obliquita'
!                                               COSALIAS=(RVZ_FITTO+SVZ_FITTO)/2   !fattore di obliquita'
!                                               SENTETA=SQRT(1-COSALIAS*COSALIAS)   !fattore di obliquita'

!                                               POSCAMP=NINT(TTOT/DT) +1      !campione da filtrare
                                               POSCAMP = NINT(TTOT) + 1
                                               VAL_FILTRATO = 0
                                               IF ( ANTIALIAS .EQ. 1 ) THEN
!************ ANTIALIAS ****************************************************************
                                                 K=INT(MAX(SENTETA*DT_AA-1,0.0))+1 !semi-lunghezza del filtro
                                                                                  !antialias a tre campioni
                                                 IF ( (POSCAMP+BORDF+K.LE.NCAMPFILT)
     $                                                .AND.(POSCAMP+BORDF-K.GE.1)) THEN
                                                      Z1=1/(REAL(K)*REAL(K))       !coefficienti del filtro
                                                      T0=TRACCE(POSCAMP+BORDF)
                                                      T1=TRACCE(POSCAMP+BORDF+K)
                                                      T2=TRACCE(POSCAMP+BORDF-K)                 
                                                      VAL_FILTRATO=Z1*(T1+T2-2*T0)
                                                 ENDIF
!************ FINE ANTIALIAS ***********************************************************
                                               ELSE
                                                 IF ( (POSCAMP.LE.NCAMP)
     $                                              .AND.(POSCAMP.GE.1)) THEN
                                                    VAL_FILTRATO = -TRACCIA_TEMP(POSCAMP)
                                                 ENDIF
                                               ENDIF  ! IF ( ANTIALIAS .EQ. 1)                                               
!-----------------------------------ava-----------------------------------------------------------
#ifdef AVACODE



!                                               IF (DIP_MAX .NE. -1) THEN
!                                               	   UX = SVX_FITTO+RVX_FITTO 
!                                               	   UY = SVY_FITTO+RVY_FITTO
!                                                   UZ = SVZ_FITTO+RVZ_FITTO
!                                                   DIP_ANGLE=sqrt(UX**2+UY**2)/sqrt(UX**2+UY**2+UZ**2)
!                                                   IF ( abs(DIP_ANGLE) .gt. 1) THEN
!!                                                      write(6,*) "coseno maggiore di 1", DIP_ANGLE
!                                                      DIP_ANGLE = 1
!                                                   ENDIF
!                                                   
!						   DIP_ANGLE = abs(asin(DIP_ANGLE))
!                                                   
!!                                                   DIP_ANGLE = abs(ACOS(SVX_FITTO*RVX_FITTO + 
!!     $                                                                    SVY_FITTO*RVY_FITTO + SVZ_FITTO*RVZ_FITTO))
 !                                                  ID = (DIP_ANGLE - DIP_MIN)/STEP_DIP + 1
!                                                   IF (ID .GT. PAN4) GOTO 740
!                                               ENDIF
!                                             
!                                               IF (AZIMUTH_MAX .NE. -1) THEN
!                                               	   UX = SVX_FITTO+RVX_FITTO 
!                                               	   UY = SVY_FITTO+RVY_FITTO
!                                                   AZ_ANGLE  = ATAN2( UX, -UY)+PIGRECO
!                                                   
!                                                   IE = (AZ_ANGLE - AZIMUTH_MIN)/STEP_AZ + 1
!                                                   IF (IE .GT. PAN5) GOTO 740
!                                               ENDIF
!//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                  DIP_ANGLE = abs(ACOS(SVX_FITTO*RVX_FITTO +  SVY_FITTO*RVY_FITTO + SVZ_FITTO*RVZ_FITTO))
                  IF ((DIP_ANGLE.GE.0/180.*PIGRECO).AND.(DIP_ANGLE.LT.5/180.*PIGRECO)) THEN


				   IF (DIP_MAX .NE. -1) THEN
                        	   	UX = SVX_FITTO+RVX_FITTO 
					UY = SVY_FITTO+RVY_FITTO
                                        UZ = SVZ_FITTO+RVZ_FITTO
					DIP_ANGLE=sqrt(UX**2+UY**2)/sqrt(UX**2+UY**2+UZ**2)
				   	IF ( abs(DIP_ANGLE) .gt. 1) THEN
                                        DIP_ANGLE = 1
                                        ENDIF

					DIP_ANGLE = abs(acos(DIP_ANGLE))
					ID=PAN4
					DO 1010 IEL=1,PAN4
						IF ((DIP_ANGLE .GE. GRID_EL(IEL)) .AND. (DIP_ANGLE .LE. GRID_EL(IEL+1)) ) THEN
						ID=IEL
						ENDIF
 1010					CONTINUE							    
		
					IF (ID .GT. PAN4) GOTO 740
                                   ENDIF
				
				
                                               IF (AZIMUTH_MAX .NE. -1) THEN
                                               	   UX = SVX_FITTO+RVX_FITTO 
                                               	   UY = SVY_FITTO+RVY_FITTO
                                                   AZ_ANGLE  = ATAN2( UX, -UY)+PIGRECO
                                                   
                                                   IE = (AZ_ANGLE - AZIMUTH_MIN)/STEP_AZ + 1
                                                   IF (IE .GT. PAN5) GOTO 740
                                               ENDIF
				  
!//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////






#endif
!-----------------------------------ava-----------------------------------------------------------

! Calcolo contributo alla matrice finale
!********************************************* IMAGING CONDITION 4
                                               DELTAPAN = VAL_FILTRATO * ABS(CZR) * SAGOMATURA
                                  
!********************************************* IMAGING CONDITION 1:Prodotto ampiezze
                                               IF (IMAGING .EQ. 1) THEN 
                                                  DELTAPAN=DELTAPAN / ASORG / AR 
!********************************************* IMAGING CONDITION 2: Rapporto ampiezze
                                               ELSEIF (IMAGING .NE. 4) THEN  
                                                 DELTAPAN=DELTAPAN * AR / (ASORG + DAMP_AMPIEZZE) 
                                               ENDIF
                                             
                                               PAN_PARZ(IAA_P,IBB_P,IC,ID,IE)  =PAN_PARZ(IAA_P,IBB_P,IC,ID,IE)+DELTAPAN
                                               CONT_RPARZ(IAA_P,IBB_P,IC,ID,IE)=CONT_RPARZ(IAA_P,IBB_P,IC,ID,IE) + 1
                                               migrata=1
                                               
                                           ENDIF
                                           ENDIF

! --- ENDIF(5) --------- endif di esclusione sorgente -------------------*
                                       ENDIF
! --- ENDIF(4) -------- endif di esclusione apertura THETA --------------*
#ifdef TEST
                               ENDIF
! --- ENDIF(3) -------- endif di esclusione per maxdisty --------------*
#endif
 740                        CONTINUE
****     FINE CICLO SU ASSE FITTO A
 730                    CONTINUE
****     FINE CICLO SU ASSE FITTO B 
                     ENDIF
 720             CONTINUE
****     FINE CICLO SU ASSE LASCO A
 710          CONTINUE
****     FINE CICLO SU ASSE LASCO B
              BINSHIFT = BINSHIFT + NA*NB
 700       CONTINUE
****     FINE CICLO SUI PANNELLI O SU ASSE C
            if (migrata.eq.1) then
                NTR_MIG_CICLO=NTR_MIG_CICLO+1
            endif
       ENDIF
! --- ENDIF ---------- endif di esclusione traccia ------------------*
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
       
 200  CONTINUE

      END
