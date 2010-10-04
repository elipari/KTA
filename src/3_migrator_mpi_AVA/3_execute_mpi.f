      SUBROUTINE EXECUTE(FIDSGY,NF,PRIMOBITNS,NSHOTNBYTES,DIMGRUPPOTR,MAXGRUPPI,
     $    NRGRUPPITR,NRBLOCCHITR,DIMBLOCCOTR,MAXDIMBLOCCOTR, MAXNRBLOCCHITR,
     $    PRIMOBITXS, XSNBYTES, PRIMOBITYS, YSNBYTES, PRIMOBITZS, ZSNBYTES,
     $    PRIMOBITXR, XRNBYTES, PRIMOBITYR, YRNBYTES, PRIMOBITZR, ZRNBYTES,
     $    PRIMATR,OFFMIN,OFFMAX,DBRESTART,ISRESTART,
     $    NCAMP, NCAMPFILT, FORMATO, DT, DX, TSHIFT,
     $    A11, A12, A21, A22, B1, B2,
     $    MAX_NCAMP, MAX_NCAMPFILT,
     $    MAX_NTRACCE,NTRACCEFILE,
     $    VEL_SUP,FPESI,
     $    BORDF,SEMIAP,SMUSSAMENTO,SOGLIA_AMPIEZZE,
     $    IMAGING,VERBOSE,AVA,TRACES,ANTIALIAS,
     $    TRACCE,TRACCIA_TEMP,TRACCIA_LETTA,TRACCIA_LETTAC,TRACCIA_LETTASH,
     $    TRACCIA1,TRACCIA2,TR_XR,TR_YR,TR_ZR,BINR_OFFSET,BINS_OFFSET,
     $    TR_WEIGHTS,TR_XS,TR_YS,TR_ZS,TR_NS,
     $    PAN_PARZ,PAN_TOT,PAN_SAVE,
     $    CONT_RPARZ,CONT_R,CONT_SAVE,
     $    PAN1, PAN2, PAN3, PAN4, PAN5,
     $    ACQ_TYPE,
     $    ACQ_O1, ACQ_O2, ACQ_O3,
     $    ACQ_CA1,ACQ_CA2,ACQ_CA3,
     $    ACQ_CB1,ACQ_CB2,ACQ_CB3,
     $    ACQ_CC1,ACQ_CC2,ACQ_CC3,
     $    ACQ_DA, ACQ_DB, ACQ_NA, ACQ_NB, ACQ_NT,
     $    ACQ_DDA,ACQ_DDB,ACQ_NNA,ACQ_NNB,ACQ_NNT,
     $    ACQ_DC, ACQ_NC, ACQ_DDC, ACQ_NNC,
     $    ACQ_NPAN, ACQ_NT_TOT,
     $    TG_TYPE,
     $    TG_O1, TG_O2, TG_O3,
     $    TG_CA1,TG_CA2,TG_CA3,
     $    TG_CB1,TG_CB2,TG_CB3,
     $    TG_CC1,TG_CC2,TG_CC3,
     $    TG_DA, TG_DB, TG_NA, TG_NB, TG_NT,
     $    TG_DDA,TG_DDB,TG_NNA,TG_NNB,TG_NNT,
     $    TG_DC, TG_NC, TG_DDC, TG_NNC,
     $    TG_NPAN, TG_NT_TOT,
     $    W_A, W_B, W_C,
     $    SA,ST,
     $    RA,RT,RUZ,
     $    ATN,ATTABLE,
     $    DBDIR, LENDBDIR,OUTDIR, LENOUTDIR,FPDIR,LENFPDIR,
     $    myrank, size, iope, numtracceperpe,
     $    DATADECIMATION, VETT_APE, APEINIZ, ZETAMAXAPE, MAXDISTY, MAXDISTX,
     $    VETT_DISTX, DISTXINIZ, ZMAXDISTX, VETT_DISTY, DISTYINIZ, ZMAXDISTY,
     $    MUTEFLAG, VETT_OFF, ZDISTYINIZ,
     $    TG_WEIGHTS_CUB_B, TG_WEIGHTS_CUB_C, TG_WEIGHTS_LIN_B, TG_WEIGHTS_LIN_C, TG_WEIGHTS_LIN_A,
     $    RANGET, RANGEA, RANGECZ, TEMPFDG,
     $    DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN,
     $    SVX, SVY, SVZ, RVX, RVY, RVZ, TG_NT_AVA, AVO,
     $    QUANTTAC, QUANTV, SORTFILETAC, SORTFILEV,GRID_EL)
      IMPLICIT NONE

      include 'mpif.h'
! NUOVA APERTURA
      INTEGER*4     FIDSGY, NF
      INTEGER*4     PRIMOBITNS, NSHOTNBYTES
      INTEGER*4     DIMGRUPPOTR,MAXGRUPPI,NRGRUPPITR,MAXNRBLOCCHITR
      INTEGER*4     NRBLOCCHITR,DIMBLOCCOTR,MAXDIMBLOCCOTR
      INTEGER*4     PRIMOBITXS,XSNBYTES,PRIMOBITYS,YSNBYTES,PRIMOBITZS,ZSNBYTES
      INTEGER*4     PRIMOBITXR,XRNBYTES,PRIMOBITYR,YRNBYTES,PRIMOBITZR,ZRNBYTES
      INTEGER*4     PRIMATR

      INTEGER*4     OFFMIN,OFFMAX

      INTEGER*4     DBRESTART(MAXNRBLOCCHITR), DBNR
      INTEGER*4     ISRESTART

      INTEGER*4     NCAMP,  NCAMPFILT
      REAL*4        DT, DX, TSHIFT
      REAL*4        A11,  A12,  A21,  A22,  B1,  B2

      INTEGER*4     MAX_NCAMP,  MAX_NCAMPFILT
      INTEGER*4     MAX_NTRACCE,NTRACCEFILE
      REAL*4        VEL_SUP

      INTEGER*4     ATN
      REAL*4        ATTABLE(ATN)

      REAL*4        SEMIAP, SMUSSAMENTO, SOGLIA_AMPIEZZE
      INTEGER*4     IMAGING, VERBOSE, AVA, ANTIALIAS,BORDF,FPESI,FORMATO,AVO
! Clara fattore di decimazione delle tracce   
      INTEGER*4     DATADECIMATION
! Clara2 apertura variabile con la profondita'      
      REAL*4        APEINIZ, ZETAMAXAPE
      REAL*4        APEINIZRAD, DELTAAPE
      INTEGER*4     NZETAMAXAPE
! Clara3 distanza massima Y (XLINE) e X (ILINE) a cui si migrano i dati
      INTEGER*4     MAXDISTY, MAXDISTX
! Clara4 dist x e y variabile con la profondita'     
      INTEGER*4     ZMAXDISTY, ZMAXDISTX
      INTEGER*4     DISTYINIZ, DISTXINIZ 
! Clara4_bis  
      INTEGER*4     ZDISTYINIZ     
! AVA
      REAL*4        DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN, RANGEVX(2), RANGEVY(2), RANGEVZ(2)
      
      REAL*4        TRACCE(MAX_NCAMPFILT)
CCC 03/05/2002 MPI- Traspongo matrice
CCC      REAL*4        TRACES(MAXDIMBLOCCOTR,MAX_NCAMP)
      REAL*4        TRACES(MAX_NCAMP,MAXDIMBLOCCOTR)
      REAL*4        TRACCIA_TEMP(MAX_NCAMP)
      REAL*4        TRACCIA_LETTA(MAX_NCAMP)
      CHARACTER*1   TRACCIA_LETTAC(MAX_NCAMP*4)
CCC    Clara
      INTEGER*2     TRACCIA_LETTASH(MAX_NCAMP)
      
      REAL*4        TRACCIA1(MAX_NCAMP+BORDF),TRACCIA2(MAX_NCAMP+BORDF)

      REAL*4        TR_XR(MAXDIMBLOCCOTR)
      REAL*4        TR_YR(MAXDIMBLOCCOTR)
      REAL*4        TR_ZR(MAXDIMBLOCCOTR)
      INTEGER*8     BINR_OFFSET(MAXDIMBLOCCOTR)
      REAL*4        TR_WEIGHTS(MAXDIMBLOCCOTR)

      REAL*4        TR_XS(MAXDIMBLOCCOTR)
      REAL*4        TR_YS(MAXDIMBLOCCOTR)
      REAL*4        TR_ZS(MAXDIMBLOCCOTR)
CCC 06/05/2002 MPI- Upgrade TR_NS da integer*4 --> integer*8 per fare un'unico MPI_BCAST di integer
CCC                 in execute
      INTEGER*8     TR_NS(MAXDIMBLOCCOTR)
CCC      INTEGER*4     TR_NS(MAXDIMBLOCCOTR)
      INTEGER*8     BINS_OFFSET(MAXDIMBLOCCOTR)

      INTEGER*4     PAN1,PAN2,PAN3, PAN4, PAN5

      REAL*4        PAN_SAVE(PAN1*PAN2*PAN4*PAN5)
      REAL*4        PAN_PARZ(PAN1,PAN2,PAN3,PAN4,PAN5)
      REAL*4        PAN_TOT(PAN1,PAN2,PAN3,PAN4,PAN5)

      INTEGER*4     CONT_SAVE(PAN1*PAN2*PAN4*PAN5)
      INTEGER*4     CONT_RPARZ(PAN1,PAN2,PAN3,PAN4,PAN5)
      INTEGER*4     CONT_R(PAN1,PAN2,PAN3,PAN4,PAN5)

! Clara2 apertura variabile con la profondita'      
      REAL*4        VETT_APE(PAN1)
! Clara4 distx e y variabile con la profondita'      
      REAL*4        VETT_DISTX(PAN1)
      REAL*4        VETT_DISTY(PAN1)
  ! Clara5 mute
      INTEGER*4     MUTEFLAG
      REAL*4        VETT_OFF(PAN1)
        
      INTEGER*4     ACQ_TYPE
      INTEGER*4     ACQ_NPAN, ACQ_NT_TOT
      REAL*4        ACQ_O1(ACQ_NPAN), ACQ_O2(ACQ_NPAN), ACQ_O3(ACQ_NPAN)
      REAL*4        ACQ_CA1(ACQ_NPAN),ACQ_CA2(ACQ_NPAN),ACQ_CA3(ACQ_NPAN)
      REAL*4        ACQ_CB1(ACQ_NPAN),ACQ_CB2(ACQ_NPAN),ACQ_CB3(ACQ_NPAN)
      REAL*4        ACQ_CC1(ACQ_NPAN),ACQ_CC2(ACQ_NPAN),ACQ_CC3(ACQ_NPAN)
      REAL*4        ACQ_DA(ACQ_NPAN), ACQ_DB(ACQ_NPAN)
      INTEGER*4     ACQ_NA(ACQ_NPAN), ACQ_NB(ACQ_NPAN), ACQ_NT(ACQ_NPAN)
      REAL*4        ACQ_DDA(ACQ_NPAN),ACQ_DDB(ACQ_NPAN)
      INTEGER*4     ACQ_NNA(ACQ_NPAN),ACQ_NNB(ACQ_NPAN),ACQ_NNT(ACQ_NPAN)
      REAL*4        ACQ_DC, ACQ_DDC
      INTEGER*4     ACQ_NC, ACQ_NNC

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

      INTEGER*4     W_A, W_B, W_C
      REAL*4        TG_WEIGHTS_LIN_A(W_A,TG_NPAN), TG_WEIGHTS_LIN_B(W_B,TG_NPAN), TG_WEIGHTS_LIN_C(W_C,TG_NPAN) 
      REAL*4        TG_WEIGHTS_CUB_B(4,W_B,4,TG_NPAN),TG_WEIGHTS_CUB_C(4,W_C,4,TG_NPAN)

      REAL*4        SA(TG_NT_TOT),ST(TG_NT_TOT)
      REAL*4        RA(TG_NT_TOT), RT(TG_NT_TOT)
      REAL*4        RUZ(TG_NT_TOT)
	  REAL*4		GRID_EL(PAN4+1)
! AVA 
      INTEGER*4     TG_NT_AVA
      REAL*4        SVX(TG_NT_AVA), SVY(TG_NT_AVA), SVZ(TG_NT_AVA)
      REAL*4        RVX(TG_NT_AVA), RVY(TG_NT_AVA), RVZ(TG_NT_AVA)

      CHARACTER*255 DBDIR, OUTDIR,FPDIR
      INTEGER*4     LENDBDIR, LENOUTDIR,LENFPDIR
      
      REAL*4       RANGET(TG_NT_TOT,2), RANGEA(TG_NT_TOT,2), RANGECZ(TG_NT_TOT,2)
      INTEGER*2    TEMPFDG(TG_NT_TOT)
      INTEGER*4    QUANTTAC, QUANTV
      INTEGER*4    SORTFILETAC(ACQ_NT_TOT),  SORTFILEV(ACQ_NT_TOT), SORT_DIM

**********Variabili locali**********************************************

      REAL*4        PIGRECO
      PARAMETER(PIGRECO = 3.14159265358979)

      INTEGER*4     FIDIO,FIDPESI
      INTEGER*4     FIDDBA,  FIDDBT, FIDDBUZ
      INTEGER*4     FIDDBVX, FIDDBVY, FIDDBVZ

      INTEGER*4     NTR_FILE, NTR_MIG, NTR_MIG_CICLO

      INTEGER*4     TRACE_LEN
      INTEGER*8     SGYH64
      INTEGER*4     FILE_ERROR
      CHARACTER*255 AUXSTR1, AUXSTR2

      INTEGER*4     I, S, T, TD, KK, SS, TT
      INTEGER*4     IA, IB, IC, ID, IE

      REAL*4        AT, ATD, ATMIN, ATMAX

      REAL*4        SEMIAPERTURA
      INTEGER*4     NTR_CARICATE

      INTEGER*4     SAVE_DIM
      CHARACTER*8   TGT_STR

      INTEGER*4     NF_OLD
      INTEGER*4     INDCICLO
 ! Clara4 disty variabile con la profondita'      
      INTEGER*4     NZDISTY, NZDISTX  
      REAL*4        DELTADISTY, DELTADISTX
! Clara4 bis
      INTEGER*4     NZDISTYCOST   
     
      REAL*4       DAMP_AMPIEZZE 
      INTEGER*4    INDICE
      integer      qq
      
      INTEGER*8 UNO_8,TRACE_LEN_8,HEADERLEN_8,PRIMATR_8
      
#ifdef TIMING
CCC 29/04/2002 Variabili per controllo temporale CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 30/05/2002  
CCC second = Dichiarazione funzione esterna utilizzata per avere l'Elapsed time
      real*8 second

CCC itostr = Converte integer --> stringa
CCC strlen = Ritorna lunghezza di una stringa
      external itostr, strlen

CCC timech    = Unita' logica file timing parziale
CCC tottimech = Unita' logica file timing totale
      integer*4 timech, tottimech

CCC cc, itostr, strlen = Variabili utilizzate per costruire nome files timing dei comp PE
CCC timefile*30    = Nome file timing parziale
CCC tottimefile*30 = Nome file timing totale
      integer*4 strlen
      character cc*9, itostr*9, timefile*30, tottimefile*30

CCC mainlooptime   = Tempo impiegato dal loop di calcolo (tutte le iterazioni)
      real*8 mainlooptime

CCC inputtime = Tempo lettura input (subroutine INPUT) di un gruppo diI/O PE
CCC outputtime = Tempo scrittura output (subroutine OUTPUT) di un gruppo di I/O PE
      real*8 inputtime, outputtime

CCC iopetottime   = Tempo impiegato per I/O (OUTPUT + INPUT) di un blocco di I/O PE
      real*8 iopetottime

CCC iopebranchtime = Tempo impiegato per I/O branch di un blocco da I/O PE in computazione utile
CCC iopewaittime = Tempo impiegato da I/O PE aspettando, senza eseguire nulla, i vari comp PE
CCC    per raccogliere i risultati della migrazione dopo aver eseguito l'I/O di un blocco
      real*8 iopebranchtime, iopewaittime
CCC bilanc = fattore di bilanciamento = % tempo utilizzato per fare I/O / tempo totale = (tempo I/O
CCC    + tempo impiegato per aspettare i comp PE) da I/O PE
      real*4 bilanc

CCC elap = Elapsed time esecuzione della subroutine COMPUTE su un gruppo da parte dei comp PE
      real*8 elap 

CCC readtimeplus(TD) - readtimeless(TD) = Tempo lettura parametri_traccia (dall'output di eikonal)
CCC    della traccia numero TD di un gruppo dei comp PE
CCC sumplus, sumless = Somma di tutti gli elementi di readtimeplus, readtimeless
CCC readtimecum = sumplus - sumless = Tempo lettura totale parametri traccia di un gruppo dei comp PE
      real*8 readtimeplus(DIMGRUPPOTR), readtimeless(DIMGRUPPOTR), sumplus, sumless, readtimecum

CCC comppetottime = Tempo impiegato in computazione utile nel computational branch per l'elaborazione
CCC    di un gruppo da parte dei comp PE (tempo per copiare i dati di input dal buffer dove sono stati i
CCC    ricevuti + esecuzione della subroutine COMPUTE su un gruppo) 
      real*8 comppetottime

CCC MPIinputtime  = Tempo MPI_BCAST e MPI_SCATTERV (distribuzione MPI input) di un blocco di tracce
CCC MPIoutputtime = Tempo MPI_REDUCE (raccolta MPI output) di un blocco di tracce
CCC MPItottime    = MPIinputtime + MPIoutputtime (tutto MPI) dell'intero loop di calcolo
      real*8 MPIinputtime, MPIoutputtime, MPItottime
#endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCC 03/05/2002 MPI- MPI variables CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 07/06/2002 N.B. Le variabili SENZA kind (es. senza *4) sono state dichiarate cosi' per garantire la
CCC            portabilita' degli argomenti delle subroutine MPI su macchine con precisione INTEGER, REAL
CCC            diverse (standard --> un argomento sia INTEGER, non INTEGER*4)

CCC 31/05/2002
CCC myrank = Rank PE
CCC size   = Numero totale PE utilizzati
      integer*4 myrank, size

CCC numtracceperpe = Numero tracce per comp PE da elaborare = dimensione gruppo
      integer*4 numtracceperpe

CCC ierr = Codice errore chiamate MPI
      integer ierr

CCC iope = size-1 = Rank di I/O PE
      integer iope

CCC SSEXAM = Iterazione in esame, utilizzata nelle stampe dei report
      integer*4 SSEXAM

CCC realsendcount(i) = Numero di elementi REAL*4 di input che I/O PE invia al comp PE i-esimo
CCC realdispls(i)    = Displacement per MPI_SCATTERV sul buffer realsend per inviare al comp_PE 
CCC    i-esimo i dati appropriati
CCC realrecvcount    = Numero elementi REAL*4 che ogni comp PE si aspetta di ricevere
      integer realsendcount(0:size-1), realdispls(0:size-1), realrecvcount

CCC intsendcount(i)  = Numero di elementi INTEGER*4 di input che I/O PE invia al comp PE i-esimo
CCC intdispls(i)     = Displacement per MPI_SCATTERV sul buffer intsend per inviare al comp_PE 
CCC    i-esimo i dati appropriati
CCC intrecvcount     = Numero elementi INTEGER*4 che ogni comp PE si aspetta di ricevere
      integer intsendcount(0:size-1) , intdispls(0:size-1) , intrecvcount

CCC tracessendcount(i) = Numero di elementi REAL*4 delle tracce che I/O PE invia al comp PE i-esimo
CCC tracesdispls(i)    = Displacement per MPI_SCATTERV sul buffer TRACES per inviare al comp_PE 
CCC    i-esimo i dati appropriati
CCC tracesrecvcount    = Numero elementi REAL*4 delle tracce che ogni comp PE si aspetta di ricevere
      integer tracessendcount(0:size-1), tracesdispls(0:size-1) , tracesrecvcount

CCC realsend = Buffer utilizzato da I/O PE dove sono impacchettati i dati REAL*4 di input letti da
CCC    disco prima di essere inviati ai comp PE
CCC realrecv = Buffer utilizzato dai comp PE dove sono impacchettati i dati di input REAL*4 spediti
CCC    da I/O PE
      real*4    realsend(7,DIMGRUPPOTR,size-1), realrecv(7,DIMGRUPPOTR)

CCC intsend  = Buffer utilizzato da I/O PE dove sono impacchettati i dati INTEGER*8 di input letti da
CCC    disco prima di essere inviati ai comp PE
CCC intrecv  = Buffer utilizzato dai comp PE dove sono impacchettati i dati di input INTEGER*8 spediti
CCC    da I/O PE
      integer*8 intsend(3,DIMGRUPPOTR,size-1) , intrecv(3,DIMGRUPPOTR)

CCC MY_TRACES = Buffer utilizzato per ricevere dai comp PE per ricevere le tracce del proprio gruppo
      real*4    MY_TRACES(MAX_NCAMP, DIMGRUPPOTR)

CCC NTR_MIG_CICLO_RECV = NTR_MIG_CICLO ricevuto dai comp PE e sommato da MPI_REDUCE per I/O PE
      integer*4     NTR_MIG_CICLO_RECV

CCC outputcount = Numero elementi di PAN_PARZ e CONT_RPARZ scambiati nelle MPI_REDUCE
      integer outputcount

CCC PAN_PARZ_RECV   = PAN_PARZ ricevuti dai comp PE e sommati da MPI_REDUCE per I/O PE
CCC CONT_RPARZ_RECV = CONT_RPARZ ricevuti dai comp PE e sommati da MPI_REDUCE per I/O PE

      real*4        PAN_PARZ_RECV(PAN1,PAN2,PAN3,PAN4,PAN5)
      integer*4     CONT_RPARZ_RECV(PAN1,PAN2,PAN3,PAN4,PAN5)

CCC ii, jj, kk = Indici usati nei loop di inizializzazione
      integer*4 ii, jj
      real*4 a1,a2,a3

       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC START INIZIALIZZAZIONI *********************************************************************************
       UNO_8 = 1
       HEADERLEN_8 = 3600
       PRIMATR_8 = PRIMATR

     
* -------- inizializzazioni  --------------------------------------
CCC 03/05/2002 MPI- Inizializzazioni MPI
      

#ifdef TIMING
      timech = 120 + myrank
      if(myrank .NE. iope) then
         cc=itostr(myrank)
         timefile = 'timing-PE-comp-'//cc(1:strlen(cc))//'.mig'
      else
         timefile = 'timing-PE-IO.mig'
      endif
      open (timech, FILE = timefile, STATUS = 'UNKNOWN')
      tottimech = 4120 + myrank
      if(myrank .NE. iope) then
         cc=itostr(myrank)
         tottimefile = 'total-time-PE-comp-'//cc(1:strlen(cc))//'.mig'
      else
         tottimefile = 'total-time-PE-IO.mig'
      endif
      open (tottimech, FILE = tottimefile, STATUS = 'UNKNOWN')
      MPItottime   = 0.0
#endif

CCC 03/06/2002 Inizializzazioni per MPI_SCATTERV
      if(myrank .EQ. iope) then
         do 466 ii=0, iope-1
            realsendcount(ii)   = 7 * DIMGRUPPOTR
            intsendcount(ii)    = 3 * DIMGRUPPOTR
            tracessendcount(ii) = MAX_NCAMP * DIMGRUPPOTR
            realdispls(ii)      = ii * realsendcount(ii)
            intdispls(ii)       = ii * intsendcount(ii)
            tracesdispls(ii)    = ii * tracessendcount(ii)
 466     continue
         realsendcount(iope)   = 0
         intsendcount(iope)    = 0
         tracessendcount(iope) = 0
         realdispls(iope)      = 0
         intdispls(iope)       = 0
         tracesdispls(iope)    = 0
         realrecvcount         = 0
         intrecvcount          = 0
         tracesrecvcount       = 0
      else
         realrecvcount       = 7 * DIMGRUPPOTR
         intrecvcount        = 3 * DIMGRUPPOTR
         tracesrecvcount     = MAX_NCAMP * DIMGRUPPOTR
      endif
      outputcount    = PAN1 * PAN2 * PAN3 * PAN4 * PAN5
      
CCC 07/05/2002 MPI- Serve per MPI_REDUCE per l'output (altrimenti iope somma una matrice diversa da zero)
      DO 814 IE=1,PAN5
        DO 815 ID=1,PAN4
           do 816 IC=1, PAN3
              do 817 IB=1, PAN2
                 do 818 IA=1, PAN1
                    PAN_PARZ(IA,IB,IC,ID,IE)   = 0.0
                    CONT_RPARZ(IA,IB,IC,ID,IE) = 0.0
 818             continue
 817          continue
 816       continue
 815   CONTINUE
 814  CONTINUE

     
CCC 03/06/2002 MPI- Inizializzazione buffer x send e recv
      do 440 kk=1, size-1
         do 441 jj=1, DIMGRUPPOTR
             do 442 ii=1, 3
                realsend(ii,jj,kk) = 0.0
                intsend (ii,jj,kk) = 0
 442         continue
             do 443 ii=4, 7
                realsend(ii,jj,kk) = 0.0
 443         continue
 441     continue
 440  continue
      do 444 jj=1, DIMGRUPPOTR
         do 445 ii=1, 3
            realrecv(ii,jj) = 0.0
            intrecv (ii,jj) = 0
 445     continue
         do 446 ii=4, 7
            realrecv(ii,jj) = 0.0
 446     continue
 444  continue

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Inizializzazione strutture dati

      do 930 jj=1, MAXDIMBLOCCOTR
         do 931 ii=1, MAX_NCAMP
            TRACES(ii,jj) = 0.0
 931     continue
 930  continue
      do 940 jj=1, DIMGRUPPOTR
         do 941 ii=1, MAX_NCAMP
            MY_TRACES(ii,jj) = 0.0
 941     continue
 940  continue
 
        DAMP_AMPIEZZE=0.0
        IF (IMAGING .EQ. 3) DAMP_AMPIEZZE=SOGLIA_AMPIEZZE
        IF (IMAGING .NE. 2) SOGLIA_AMPIEZZE=0.0
        
         
! Clara2 apertura variabile con la profondita'      
        IF ( APEINIZ .EQ. -1 ) THEN
           SEMIAPERTURA = SEMIAP/180*PIGRECO
           
           DO 950 ii = 1, PAN1     
              VETT_APE(ii) =  SIN(SEMIAPERTURA)*TG_DDA(1)        
 950       CONTINUE
        ELSE	
           APEINIZRAD   = APEINIZ/180*PIGRECO
           SEMIAPERTURA = SEMIAP/180*PIGRECO
           NZETAMAXAPE  = NINT((ZETAMAXAPE-TG_O3(1))/TG_DDA(1))+1
           IF ( NZETAMAXAPE .NE. 0 ) THEN
              DELTAAPE = (SEMIAPERTURA-APEINIZRAD)/NZETAMAXAPE 
           ENDIF
           DO 951 ii = 1, PAN1
               IF ( ii .LE. NZETAMAXAPE) THEN
                  VETT_APE(ii) = SIN(APEINIZRAD + DELTAAPE * (ii-1))*TG_DDA(1)
               ELSE
               	  VETT_APE(ii) = SIN(SEMIAPERTURA)*TG_DDA(1)
               ENDIF
 951       CONTINUE
        ENDIF

        
! Clara4_bis disty variabile con la profondita'      
#ifdef TEST
           IF (ZDISTYINIZ .EQ. -1) THEN
           	ZDISTYINIZ = TG_O3(1)
           ENDIF
           NZDISTYCOST=NINT((ZDISTYINIZ-TG_O3(1))/TG_DDA(1))+1
           NZDISTY  = NINT((ZMAXDISTY-ZDISTYINIZ)/TG_DDA(1))
           IF (NZDISTY .GT. PAN1) THEN
           	NZDISTY = PAN1
           ENDIF
           IF ( ZMAXDISTY .NE. -1 ) THEN
              DELTADISTY = REAL(MAXDISTY-DISTYINIZ)/REAL(NZDISTY) 
           ELSE
              DELTADISTY = 0
           ENDIF
           DO 952 ii = 1, PAN1
               IF ( ii .LE. NZDISTYCOST) THEN
                  VETT_DISTY(ii) = DISTYINIZ 
               ELSEIF ( ii .LE. (NZDISTY+NZDISTYCOST-1)) THEN
                  VETT_DISTY(ii) = VETT_DISTY(ii-1) + DELTADISTY 
               ELSE
               	  VETT_DISTY(ii) = MAXDISTY
               ENDIF
 
 952       CONTINUE
     
           NZDISTX  = NINT((ZMAXDISTX-TG_O3(1))/TG_DDA(1))+1
           IF (NZDISTX .GT. PAN1) THEN
           	NZDISTX = PAN1
           ENDIF
           IF ( ZMAXDISTX .NE. -1 ) THEN
              DELTADISTX = REAL(MAXDISTX-DISTXINIZ)/REAL(NZDISTX)
           ELSE
              DELTADISTX = 0
           ENDIF
           DO 953 ii = 1, PAN1
               IF ( ii .LE. NZDISTX) THEN
                  VETT_DISTX(ii) = DISTXINIZ + DELTADISTX * (ii-1)
               ELSE
               	  VETT_DISTX(ii) = MAXDISTX
               ENDIF
 953       CONTINUE
! Normalizzazione rispetto ai passi di campionamento
           DO 954 ii = 1, PAN1
               IF ( VETT_DISTX(ii).NE.-1) VETT_DISTX(ii) = VETT_DISTX(ii)/TG_DDB(1)
               IF ( VETT_DISTY(ii).NE.-1) VETT_DISTY(ii) = VETT_DISTY(ii)/TG_DDC
 954       CONTINUE               
             
               	

#endif             
            
CCC 03/05/2002 MPI- Definizione INDCICLO
CCC      INDCICLO=DIMBLOCCOTR/DIMGRUPPOTR
      INDCICLO = size-1

      NTR_FILE=0      ! nr. totale di tracce nel file SGY
      NTR_MIG =0      ! nr. totale tracce migrate
CCC 08/05/2002 MPI-
      NTR_MIG_CICLO = 0
      NTR_CARICATE=0  ! nr. tracce gia caricate

      ATMIN=TAN(0.0)
      ATMAX=TAN(70.0*PIGRECO/180.0)
      ATD = (ATMAX-ATMIN) / REAL(ATN-1)
      DO 307 KK=1,Atn
        ATTABLE(KK)=ATAN(REAL(KK-1)*ATD)   
 307  CONTINUE

      DO 308 KK=1,MAXNRBLOCCHITR
        DBRESTART(KK)=0
 308  CONTINUE

      DO 309 KK=1,MAXDIMBLOCCOTR
        TR_WEIGHTS(KK)=0
 309  CONTINUE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
* -------- apertura files
      
      FIDIO=0
      FIDDBA=3
      FIDDBT=4
      FIDDBUZ=7
      FIDDBVX=8
      FIDDBVY=9
      FIDDBVZ=10
     

      FIDPESI=11
           
CCC Clara
      IF (FORMATO .NE. 3) THEN
         TRACE_LEN=240+NCAMP*4
         TRACE_LEN_8 = TRACE_LEN
         SGYH64=HEADERLEN_8+(PRIMATR_8-UNO_8)*(TRACE_LEN_8)
      ELSE
         TRACE_LEN=240+NCAMP*2
         TRACE_LEN_8 = TRACE_LEN
         SGYH64=HEADERLEN_8+(PRIMATR_8-UNO_8)*(TRACE_LEN_8)
      ENDIF
      
      AUXSTR1 = DBDIR(1:LENDBDIR)//'DBa.H@'
      CALL GLOB_FOPEN64(FIDDBA,AUXSTR1,'rb',FILE_ERROR)

      AUXSTR1 = DBDIR(1:LENDBDIR)//'DBt.H@'
      CALL GLOB_FOPEN64(FIDDBT,AUXSTR1,'rb',FILE_ERROR)

      AUXSTR1 = DBDIR(1:LENDBDIR)//'DBuz.H@'
      CALL GLOB_FOPEN64(FIDDBUZ,AUXSTR1, 'rb',FILE_ERROR)
      
      IF (AVA .EQ. 1) THEN
      	 AUXSTR1 = DBDIR(1:LENDBDIR)//'DBvx.H@'
         CALL GLOB_FOPEN64(FIDDBVX,AUXSTR1,'rb',FILE_ERROR)
   
         AUXSTR1 = DBDIR(1:LENDBDIR)//'DBvy.H@'
         CALL GLOB_FOPEN64(FIDDBVY,AUXSTR1,'rb',FILE_ERROR)
   
         AUXSTR1 = DBDIR(1:LENDBDIR)//'DBvz.H@'
         CALL GLOB_FOPEN64(FIDDBVZ,AUXSTR1, 'rb',FILE_ERROR)
      ENDIF
      
      IF (FPESI .EQ. 1) THEN
        AUXSTR1 = FPDIR(1:LENFPDIR)
        CALL GLOB_FOPEN64(FIDPESI,AUXSTR1,'rb',FILE_ERROR)
      ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*------- Inizio procedura di ripristino ----

      IF (ISRESTART .EQ. 1) THEN

         AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.RestartSGY'
         CALL GLOB_FOPEN(FIDIO,AUXSTR1, 'rb',FILE_ERROR)
         CALL GLOB_FREAD(FIDIO,NF_OLD,4,1,FILE_ERROR)
         CALL GLOB_FCLOSE(FIDIO,FILE_ERROR)

         IF (NF_OLD .GT. NF) THEN
           if(myrank .EQ. iope) then
              WRITE(*,*) 'Restart: file SGY nr. ', NF,' gia processato'
           endif
           RETURN
         ENDIF

         if(myrank .EQ. iope) then
            WRITE(*,*) 'Restart: ripartenza da file SGY nr. ', NF
            WRITE(*,*) 'Restart: caricamento dati...'
         endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*------- caricamento dati di ripristino ( vettore cicli gia' eseguiti ) ----

         AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.RestartDBnr'
         CALL GLOB_FOPEN(FIDIO,AUXSTR1, 'rb',FILE_ERROR)
         CALL GLOB_FREAD(FIDIO,DBNR,4,1,FILE_ERROR)
         CALL GLOB_FCLOSE(FIDIO,FILE_ERROR)

         IF (DBNR .EQ. 1) AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.RestartDB1'
         IF (DBNR .EQ. 2) AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.RestartDB2'
         CALL GLOB_FOPEN(FIDIO,AUXSTR1, 'rb',FILE_ERROR)
         CALL GLOB_FREAD(FIDIO,DBRESTART,4,NRBLOCCHITR,FILE_ERROR)
         CALL GLOB_FCLOSE(FIDIO,FILE_ERROR)

*------- caricamento dati di ripristino ( pannello o cubo di migrazione )---

         IF (TG_TYPE .EQ. 1) THEN
           SAVE_DIM=PAN3*PAN4*PAN5

           IF (DBNR .EQ. 1) THEN
               AUXSTR1 = OUTDIR(1:LENOUTDIR)//'LISTr.H@'
               AUXSTR2 = OUTDIR(1:LENOUTDIR)//'LISTfr.H@'
           ELSE IF (DBNR .EQ. 2) THEN
               AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.LISTr.H@'
               AUXSTR2 = OUTDIR(1:LENOUTDIR)//'.LISTfr.H@'
           ENDIF

           CALL GLOB_FOPEN64(FIDIO,AUXSTR1,'rb',FILE_ERROR)
           CALL GLOB_FREAD(FIDIO,PAN_TOT, 4, SAVE_DIM,FILE_ERROR)
           CALL GLOB_FCLOSE64(FIDIO,FILE_ERROR)
           CALL GLOB_FOPEN64(FIDIO,AUXSTR2,'rb',FILE_ERROR)
           CALL GLOB_FREAD(FIDIO,CONT_R, 4, SAVE_DIM,FILE_ERROR)
           CALL GLOB_FCLOSE64(FIDIO,FILE_ERROR)

         ELSEIF (TG_TYPE .EQ. 2) THEN

           DO 1215 IC=1,TG_NPAN
             SAVE_DIM=TG_NNB(IC)*TG_NNA(IC)*PAN4*PAN5
             WRITE(TGT_STR,"(I8)") IC+10000000

             IF (DBNR .EQ. 1) THEN
                AUXSTR1 = OUTDIR(1:LENOUTDIR)//'PAN'//TGT_STR//'r.H@'
                AUXSTR2  = OUTDIR(1:LENOUTDIR)//'PAN'//TGT_STR//'fr.H@'
             ELSE IF (DBNR .EQ. 2) THEN
                AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.PAN'//TGT_STR//'r.H@'
                AUXSTR2 = OUTDIR(1:LENOUTDIR)//'.PAN'//TGT_STR//'fr.H@'
             ENDIF

             CALL GLOB_FOPEN64(FIDIO,AUXSTR1,'rb',FILE_ERROR)
             CALL GLOB_FREAD(FIDIO,PAN_SAVE, 4, SAVE_DIM,FILE_ERROR)
             CALL GLOB_FCLOSE64(FIDIO,FILE_ERROR)
             CALL GLOB_FOPEN64(FIDIO,AUXSTR2,'rb',FILE_ERROR)
             CALL GLOB_FREAD(FIDIO,CONT_SAVE, 4, SAVE_DIM,FILE_ERROR)
             CALL GLOB_FCLOSE64(FIDIO,FILE_ERROR)

             DO 1211 IE=1,PAN5
               DO 1212 ID=1,PAN4
                 DO 1213 IB=1,TG_NNB(IC)
                   DO 1214 IA=1,TG_NNA(IC)
                     INDICE=( ( (IE-1)*PAN4+(ID-1) )*TG_NNB(IC)+ (IB-1) )*TG_NNA(IC) +IA
                     PAN_TOT(IA,IB,IC,ID,IE)= PAN_SAVE( INDICE )
                     CONT_R(IA,IB,IC,ID,IE) = CONT_SAVE( INDICE )
 1214              CONTINUE
 1213            CONTINUE
 1212          CONTINUE
 1211        CONTINUE
 
 1215      CONTINUE

         ELSEIF (TG_TYPE .EQ. 3) THEN
           SAVE_DIM=PAN1*PAN2*PAN3*PAN4*PAN5

           IF (DBNR .EQ. 1) THEN
              AUXSTR1 = OUTDIR(1:LENOUTDIR)//'VOLUMEr.H@'
              AUXSTR2 = OUTDIR(1:LENOUTDIR)//'VOLUMEfr.H@'
           ELSE IF (DBNR .EQ. 2) THEN
              AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.VOLUMEr.H@'
              AUXSTR2 = OUTDIR(1:LENOUTDIR)//'.VOLUMEfr.H@'
           ENDIF

           CALL GLOB_FOPEN64(FIDIO,AUXSTR1,'rb',FILE_ERROR)
           CALL GLOB_FREAD(FIDIO,PAN_TOT, 4, SAVE_DIM,FILE_ERROR)
           CALL GLOB_FCLOSE64(FIDIO,FILE_ERROR)
           CALL GLOB_FOPEN64(FIDIO,AUXSTR2,'rb',FILE_ERROR)
           CALL GLOB_FREAD(FIDIO,CONT_R, 4, SAVE_DIM,FILE_ERROR)
           CALL GLOB_FCLOSE64(FIDIO,FILE_ERROR)

         ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ELSE

         DBNR=1

CCC 07052002 MPI-
         if(myrank .EQ. iope) then
            AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.RestartSGY'
            CALL GLOB_FOPEN(FIDIO,AUXSTR1, 'w+b',FILE_ERROR)
            CALL GLOB_FWRITE(FIDIO,NF,4,1,FILE_ERROR)
            CALL GLOB_FCLOSE(FIDIO,FILE_ERROR)

*------- salvataggio dati di ripristino ( azzeramento ) ----

            AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.RestartDB1'
            CALL GLOB_FOPEN(FIDIO,AUXSTR1, 'w+b',FILE_ERROR)
            CALL GLOB_FWRITE(FIDIO,DBRESTART,4,NRBLOCCHITR,FILE_ERROR)
            CALL GLOB_FCLOSE(FIDIO,FILE_ERROR)

            AUXSTR1 = OUTDIR(1:LENOUTDIR)//'.RestartDBnr'
            CALL GLOB_FOPEN(FIDIO,AUXSTR1, 'w+b',FILE_ERROR)
            CALL GLOB_FWRITE(FIDIO,DBNR,4,1,FILE_ERROR)
            CALL GLOB_FCLOSE(FIDIO,FILE_ERROR)
CCC 07052002 MPI-
         endif

      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ISRESTART=0

CCC END INIZIALIZZAZIONI ***********************************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*------- caricamento geometrie ------------------------------
      IF (VERBOSE.GT.1) THEN
      if(myrank .EQ. iope) then
        WRITE(*,*) 'Processing file SEGY nr: ', NF
        WRITE(*,*) 'PARAMETRI: '
        WRITE(*,*) '   numero massimo di tracce:          ',MAX_NTRACCE
        WRITE(*,*) '   dimensione del blocco di tracce:   ',DIMBLOCCOTR
        WRITE(*,*) '   numero di blocchi:                 ',NRBLOCCHITR
        WRITE(*,*) '   dimensione del gruppo di tracce:   ',DIMGRUPPOTR
        WRITE(*,*) '   numero di gruppi in un blocco:     ',INDCICLO
        WRITE(*,*) '   numero totale di gruppi di tracce: ',NRGRUPPITR
      endif
      ENDIF

      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      write(*,*) "I am ",myrank," and I am starting ..."

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC START LOOP CALCOLO ************************************************************************************

#ifdef TIMING
      mainlooptime = second(0)
#endif

      DO 7777 SS=1,NRBLOCCHITR
      

#ifdef TIMING
         if(myrank .EQ. iope) then
            write(timech,*) "SS = ",SS," ######################################################"
         endif
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 23/05/2002 MPI- I/O PE riceve output dell'iterazione SS-1 (se non siamo alla prima iterazione)
         if(SS .NE. 1) then

#ifdef TIMING
            MPIoutputtime = second(0)
#endif


            if(myrank .EQ. iope) then
               NTR_MIG_CICLO_RECV = 0
               
               do 914 IE=1, PAN5
                 do 915 ID=1, PAN4
                   do 916 IC=1, PAN3
                      do 917 IB=1, PAN2
                         do 918 IA=1, PAN1           
                            PAN_PARZ_RECV(IA,IB,IC,ID,IE)   = 0
                            CONT_RPARZ_RECV(IA,IB,IC,ID,IE) = 0
 918                     continue
 917                  continue
 916               continue
 915             continue
 914           continue
            endif

#ifdef TIMING
            if(myrank. EQ. iope) then
               if(SS .NE. 2) then
                  iopebranchtime = second(0) - iopebranchtime
               endif
            endif
            iopewaittime = second(0)
#endif
            
            call MPI_REDUCE(NTR_MIG_CICLO, NTR_MIG_CICLO_RECV, INT(1)     , MPI_INTEGER4,
     $                      MPI_SUM, iope, MPI_COMM_WORLD, ierr)
#ifdef TIMING
            iopewaittime = second(0) - iopewaittime 
#endif


            call MPI_REDUCE(PAN_PARZ     , PAN_PARZ_RECV     , outputcount, MPI_REAL4,
     $                      MPI_SUM, iope, MPI_COMM_WORLD, ierr)
            call MPI_REDUCE(CONT_RPARZ   , CONT_RPARZ_RECV   , outputcount, MPI_INTEGER4,
     $                      MPI_SUM, iope, MPI_COMM_WORLD, ierr)


#ifdef TIMING
            MPIoutputtime = second(0) - MPIoutputtime - iopewaittime
            MPItottime    = MPItottime + MPIoutputtime
            if(myrank .EQ. iope) then
               SSEXAM = SS - 1
               write(timech,*) "I/O PE: MPI Output Time per blocco n. ",SSEXAM," = ",MPIoutputtime
               write(tottimech,*) "I/O PE: Wait Time nel blocco n. ",SSEXAM," = ",iopewaittime
               if(SS .NE. 1) then
                  bilanc = REAL((iopebranchtime / (iopebranchtime + iopewaittime)) * 100.0)
                  write(tottimech,*) "I/O PE: Bilanciamento  I/O vs comput = ",bilanc," %"
               endif
#ifdef SGI_ORIGIN
               call flush(timech)
               call flush(tottimech)
#endif
            endif
#endif
         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 23/05/2002 MPI- I/O PE fa la prima input se siamo alla prima iterazione

         if((SS .EQ. 1) .AND. (myrank .EQ. iope)) then

#ifdef TIMING
            inputtime = second(0)
#endif

            SSEXAM = SS
            CALL INPUT(FIDSGY, PRIMOBITNS, NSHOTNBYTES, DIMGRUPPOTR, DIMBLOCCOTR, MAXDIMBLOCCOTR,
     $         MAXNRBLOCCHITR, NCAMP, FORMATO, MAX_NCAMP, NTRACCEFILE, FPESI, 
     $         PRIMOBITXS, XSNBYTES, PRIMOBITYS, YSNBYTES, PRIMOBITZS, ZSNBYTES,
     $         PRIMOBITXR, XRNBYTES, PRIMOBITYR, YRNBYTES, PRIMOBITZR, ZRNBYTES,
     $         PRIMATR, OFFMIN, OFFMAX, DBRESTART, A11, A12, A21, A22, B1, B2,
     $         TRACCIA_LETTA, TRACCIA_LETTAC, TRACCIA_LETTASH, TRACES, SGYH64, TRACE_LEN, INDCICLO,
     $         TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET, TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS,
     $         ACQ_TYPE, ACQ_O1, ACQ_O2, ACQ_O3, ACQ_CA1,ACQ_CA2,ACQ_CA3, ACQ_CB1,ACQ_CB2,ACQ_CB3,
     $         ACQ_CC1,ACQ_CC2,ACQ_CC3, ACQ_DC, ACQ_NC, ACQ_DDC, ACQ_NNC, ACQ_NPAN, ACQ_NT_TOT, ACQ_DA,
     $         ACQ_DB, ACQ_NA, ACQ_NB, ACQ_NT, ACQ_DDA, ACQ_DDB, ACQ_NNA, ACQ_NNB, ACQ_NNT, NTR_FILE,
     $         SSEXAM,
     $         size, realsend, intsend,
     $         DATADECIMATION)


#ifdef TIMING
            inputtime   = second(0) - inputtime
            SSEXAM =  SS
            write(timech,*) "I/O PE: Input Time      per blocco n. ",SSEXAM," = ",inputtime
#endif

         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 23/05/2002 MPI- I/O PE distribuisce gli input letti nell'iterazione precedente (mentre i 
CCC                 computing PEs stavano elaborando i dati)

#ifdef TIMING
         MPIinputtime = second(0)
#endif
         
         call MPI_SCATTERV(realsend, realsendcount, realdispls, MPI_REAL4,
     $                     realrecv, realrecvcount, MPI_REAL4,
     $                     iope, MPI_COMM_WORLD, ierr)
         call MPI_SCATTERV(intsend, intsendcount, intdispls, MPI_INTEGER8,
     $                     intrecv, intrecvcount, MPI_INTEGER8,
     $                     iope, MPI_COMM_WORLD, ierr)
         call MPI_SCATTERV(TRACES   , tracessendcount, tracesdispls, MPI_REAL4, 
     $                     MY_TRACES, tracesrecvcount, MPI_REAL4, 
     $                     iope, MPI_COMM_WORLD, ierr)
         call MPI_BCAST(NTR_FILE, INT(1), MPI_INTEGER4, iope, MPI_COMM_WORLD, ierr)

#ifdef TIMING
         MPIinputtime = second(0) - MPIinputtime
         MPItottime   = MPItottime + MPIinputtime
         if(myrank .EQ. iope) then
            SSEXAM = SS
            write(timech,*) "I/O PE: MPI Input Time  per blocco n. ",SSEXAM," = ",MPIinputtime
         endif
#endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        if(myrank .EQ. iope) then 
           
           IF (DBRESTART(SS) .EQ. 1) THEN
              do 887 S=1, size-1
                 T=(SS-1)*DIMBLOCCOTR+(S-1)*DIMGRUPPOTR +1
                 IF (VERBOSE .GT. 1) THEN
CCC 27/06/2002 MPI- Questa stampa la esegue solo I/O PE altrimenti confusione sullo schermo
                    WRITE(*,*) 'comp PE: myrank = ",myrank," Restart: skipping file SEG-Y N.: ',NF,
     $                         ' - Blocco N.: ',SS,' Gruppo N.:',S
                 ENDIF
 887          continue
           ENDIF

           IF (T .LE. NTR_FILE)  THEN
              do 888 S=1, size-1
                 T=(SS-1)*DIMBLOCCOTR+(S-1)*DIMGRUPPOTR +1
                 IF (VERBOSE .GT. 1) THEN
CCC 27/05/2002 MPI- Questa stampa la esegue solo I/O PE altrimenti confusione sullo schermo
                    WRITE(*,*) 'File SEG-Y N.: ',NF,' - Blocco N.: ', SS, ' - Gruppo N.: ',S
                    WRITE(*,*) '       comp PE: myrank = ',S-1
                    WRITE(*,*) '       Inizio caricamento da traccia (compreso offset): ',
     $                         (SS-1)*DIMBLOCCOTR +(S-1)*DIMGRUPPOTR+ PRIMATR
                 ENDIF
 888          continue
           ENDIF
        endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 23/05/2002 MPI- Mentre i computing PEs calcolano I/O PE fa l'output dell'iterazione SS-1 e 
CCC                 legge l'input della prossima iterazione SS+1

         if(myrank .EQ. iope) then

#ifdef TIMING
            iopebranchtime = second(0)
#endif

#ifdef TIMING
            iopetottime = second(0) 
#endif

            if(SS .NE. 1) then

#ifdef TIMING
               outputtime = second(0)
#endif

               SSEXAM = SS - 1

               CALL OUTPUT(NRBLOCCHITR, MAXNRBLOCCHITR, DBRESTART, DBNR, NF,
     $            TG_TYPE, TG_NPAN, TG_NNA, TG_NNB, TGT_STR, NTR_MIG, OUTDIR, LENOUTDIR,
     $            PAN_SAVE, PAN_TOT, CONT_SAVE, CONT_R, PAN1, PAN2, PAN3, PAN4, PAN5, 
     $            NTR_MIG_CICLO_RECV, PAN_PARZ_RECV, CONT_RPARZ_RECV, 
     $            SSEXAM)
          


#ifdef TIMING    
               outputtime = second(0) - outputtime
               write(timech,*) "I/O PE: Output Time     per blocco n. ",SSEXAM," = ",outputtime
#endif

            endif

            if(SS .NE. NRBLOCCHITR) then

#ifdef TIMING
               inputtime = second(0)
#endif
         do 932 jj=1, MAXDIMBLOCCOTR
         do 933 ii=1, MAX_NCAMP
            TRACES(ii,jj) = 0.0
 933     continue
 932     continue
         do 942 jj=1, DIMGRUPPOTR
         do 943 ii=1, MAX_NCAMP
            MY_TRACES(ii,jj) = 0.0
 943     continue
 942     continue
               SSEXAM = SS + 1
               CALL INPUT(FIDSGY, PRIMOBITNS, NSHOTNBYTES, DIMGRUPPOTR, DIMBLOCCOTR, MAXDIMBLOCCOTR,
     $            MAXNRBLOCCHITR, NCAMP, FORMATO, MAX_NCAMP, NTRACCEFILE, FPESI, 
     $            PRIMOBITXS, XSNBYTES, PRIMOBITYS, YSNBYTES, PRIMOBITZS, ZSNBYTES,
     $            PRIMOBITXR, XRNBYTES, PRIMOBITYR, YRNBYTES, PRIMOBITZR, ZRNBYTES,
     $            PRIMATR, OFFMIN, OFFMAX, DBRESTART, A11, A12, A21, A22, B1, B2,
     $            TRACCIA_LETTA, TRACCIA_LETTAC, TRACCIA_LETTASH, TRACES, SGYH64, TRACE_LEN, INDCICLO,
     $            TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET, TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS,
     $            ACQ_TYPE, ACQ_O1, ACQ_O2, ACQ_O3, ACQ_CA1,ACQ_CA2,ACQ_CA3, ACQ_CB1,ACQ_CB2,ACQ_CB3,
     $            ACQ_CC1,ACQ_CC2,ACQ_CC3, ACQ_DC, ACQ_NC, ACQ_DDC, ACQ_NNC, ACQ_NPAN, ACQ_NT_TOT, ACQ_DA,
     $            ACQ_DB, ACQ_NA, ACQ_NB, ACQ_NT, ACQ_DDA, ACQ_DDB, ACQ_NNA, ACQ_NNB, ACQ_NNT, NTR_FILE,
     $            SSEXAM,
     $            size, realsend, intsend,
     $            DATADECIMATION)
     
#ifdef TIMING
               inputtime   = second(0) - inputtime
               SSEXAM = SS + 1
               write(timech,*) "I/O PE: Input Time      per blocco n. ",SSEXAM," = ",inputtime
#endif





            endif

#ifdef TIMING
            iopetottime = second(0) - iopetottime
            SSEXAM = SS
            write(tottimech,*) "------------------------------------------------------------------------"
            write(tottimech,*) "I/O PE: Iterazione SS = ",SSEXAM," Total I/O time = ",iopetottime
#endif

         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 23/05/2002 MPI- Mentre l'I/O PE fa input/output i computing PEs elaborano l'attuale iterazione SS
         if(myrank .NE. iope) then

#ifdef TIMING
            comppetottime = second(0)
#endif

CCC 23/05/2002 MPI- myrank deve computare il gruppo myrank+1
            S = myrank+1     

#ifdef TIMING
            do 1234 ii = 1, DIMGRUPPOTR
               readtimeplus(ii) = 0.0
               readtimeless(ii) = 0.0
 1234       continue
#endif
              
CCC 03/06/2002 MPI- I comp PEs copiano i dati dal buffer di recv nei vettori appropriati 
            do 3200 TD=1, DIMGRUPPOTR
               TT = (S-1)*DIMGRUPPOTR+TD
               TR_XR(TT)       = realrecv(1,TD)
               TR_YR(TT)       = realrecv(2,TD)
               TR_ZR(TT)       = realrecv(3,TD)
               TR_XS(TT)       = realrecv(4,TD)
               TR_YS(TT)       = realrecv(5,TD)
               TR_ZS(TT)       = realrecv(6,TD)
               TR_WEIGHTS(TT)  = realrecv(7,TD)
               BINR_OFFSET(TT) = intrecv(1,TD)
               BINS_OFFSET(TT) = intrecv(2,TD)
               TR_NS(TT)       = intrecv(3,TD)
 3200       continue

#ifdef TIMING
            elap = second(0)
#endif

            IF (TG_TYPE .EQ. 1) THEN 
            CALL COMPUTE_TG1(NF, DIMGRUPPOTR, DIMBLOCCOTR,
     $         MAXDIMBLOCCOTR, MAXNRBLOCCHITR, PRIMATR, DBRESTART, NCAMP, NCAMPFILT,
     $         DT, DX, TSHIFT, MAX_NCAMP,MAX_NCAMPFILT, NTRACCEFILE, VEL_SUP, FPESI, BORDF,
     $         SMUSSAMENTO, SEMIAPERTURA, SOGLIA_AMPIEZZE, IMAGING, VERBOSE, MY_TRACES, ANTIALIAS,
     $         TRACCE, TRACCIA1, TRACCIA2, TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET,
     $         TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS, PAN_PARZ, CONT_RPARZ, PAN1, PAN2, PAN3,
     $         PAN4, PAN5, AVA, DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN,
     $         AVO, OFFMIN, OFFMAX,
     $         TG_TYPE, TG_O1, TG_O2, TG_O3, TG_CA1, TG_CA2, TG_CA3, TG_CB1, TG_CB2, TG_CB3,
     $         TG_CC1, TG_CC2, TG_CC3, TG_DA, TG_DB, TG_NA, TG_NB, TG_NT, TG_DDA, TG_DDB,
     $         TG_NNA, TG_NNB, TG_NNT, TG_DC, TG_NC, TG_DDC, TG_NNC, TG_NPAN, TG_NT_TOT,
     $         ATMIN, ATMAX, ATD, ATN, ATTABLE, NTR_FILE,
     $         RUZ, FIDDBA, FIDDBT, FIDDBUZ, NTR_MIG_CICLO,
     $         SS, S, VETT_APE, VETT_DISTX, VETT_DISTY, MUTEFLAG, VETT_OFF,
     $         FIDDBVX, FIDDBVY, FIDDBVZ, SVX, SVY, SVZ, RVX, RVY, RVZ, TG_NT_AVA,
     $         QUANTTAC, QUANTV, SORTFILETAC, SORTFILEV, ACQ_NT_TOT,
     $         myrank
#ifdef TIMING
     $         ,readtimeplus, readtimeless
#endif
     $         )
            ELSEIF (TG_TYPE .EQ. 2) THEN 
            CALL COMPUTE_TG2(NF, DIMGRUPPOTR, DIMBLOCCOTR,
     $         MAXDIMBLOCCOTR, MAXNRBLOCCHITR, PRIMATR, DBRESTART, NCAMP, NCAMPFILT,
     $         DT, DX, TSHIFT, MAX_NCAMP,MAX_NCAMPFILT, NTRACCEFILE, VEL_SUP, FPESI, BORDF,
     $         SMUSSAMENTO, SEMIAPERTURA, SOGLIA_AMPIEZZE, IMAGING, VERBOSE, MY_TRACES, ANTIALIAS,
     $         TRACCE, TRACCIA1, TRACCIA2, TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET,
     $         TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS, PAN_PARZ, CONT_RPARZ, PAN1, PAN2, PAN3,
     $         PAN4, PAN5, AVA, DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN,
     $         AVO, OFFMIN, OFFMAX,
     $         TG_TYPE, TG_O1, TG_O2, TG_O3, TG_CA1, TG_CA2, TG_CA3, TG_CB1, TG_CB2, TG_CB3,
     $         TG_CC1, TG_CC2, TG_CC3, TG_DA, TG_DB, TG_NA, TG_NB, TG_NT, TG_DDA, TG_DDB,
     $         TG_NNA, TG_NNB, TG_NNT, TG_DC, TG_NC, TG_DDC, TG_NNC, TG_NPAN, TG_NT_TOT,
     $         W_A, W_B, ATMIN, ATMAX, ATD, Atn, ATTABLE, NTR_FILE,
     $         RUZ, FIDDBA, FIDDBT, FIDDBUZ, NTR_MIG_CICLO,
     $         SS, S, VETT_APE, VETT_DISTX, VETT_DISTY, MUTEFLAG, VETT_OFF,
     $         TG_WEIGHTS_LIN_A, TG_WEIGHTS_LIN_B, TG_WEIGHTS_CUB_B,
     $         RANGET, RANGEA, RANGECZ, TEMPFDG,DAMP_AMPIEZZE,
     $         FIDDBVX, FIDDBVY, FIDDBVZ, SVX, SVY, SVZ, RVX, RVY, RVZ, TG_NT_AVA,
     $         QUANTTAC, QUANTV, SORTFILETAC, SORTFILEV, ACQ_NT_TOT,
     $         myrank,GRID_EL
#ifdef TIMING
     $         ,readtimeplus, readtimeless
#endif
     $         )
            ELSEIF ((TG_TYPE .EQ. 3).AND.(TG_NNC .EQ. TG_NC)) THEN 
            CALL COMPUTE_TG4(NF, DIMGRUPPOTR, DIMBLOCCOTR,
     $         MAXDIMBLOCCOTR, MAXNRBLOCCHITR, PRIMATR, DBRESTART, NCAMP, NCAMPFILT,
     $         DT, DX, TSHIFT, MAX_NCAMP,MAX_NCAMPFILT, NTRACCEFILE, VEL_SUP, FPESI, BORDF,
     $         SMUSSAMENTO, SEMIAPERTURA, SOGLIA_AMPIEZZE, IMAGING, VERBOSE, MY_TRACES, ANTIALIAS,
     $         TRACCE, TRACCIA1, TRACCIA2, TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET,
     $         TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS, PAN_PARZ, CONT_RPARZ, PAN1, PAN2, PAN3,
     $         PAN4, PAN5, AVA, DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN,
     $         AVO, OFFMIN, OFFMAX,
     $         TG_TYPE, TG_O1, TG_O2, TG_O3, TG_CA1, TG_CA2, TG_CA3, TG_CB1, TG_CB2, TG_CB3,
     $         TG_CC1, TG_CC2, TG_CC3, TG_DA, TG_DB, TG_NA, TG_NB, TG_NT, TG_DDA, TG_DDB,
     $         TG_NNA, TG_NNB, TG_NNT, TG_DC, TG_NC, TG_DDC, TG_NNC, TG_NPAN, TG_NT_TOT,
     $         W_A, W_B, ATMIN, ATMAX, ATD, ATN, ATTABLE, NTR_FILE,
     $         RUZ, FIDDBA, FIDDBT, FIDDBUZ, NTR_MIG_CICLO,
     $         SS, S, VETT_APE, VETT_DISTX, VETT_DISTY, MUTEFLAG, VETT_OFF,
     $         TG_WEIGHTS_LIN_A, TG_WEIGHTS_LIN_B, TG_WEIGHTS_CUB_B,
     $         RANGET, RANGEA, RANGECZ, TEMPFDG,DAMP_AMPIEZZE,
     $         FIDDBVX, FIDDBVY, FIDDBVZ, SVX, SVY, SVZ, RVX, RVY, RVZ, TG_NT_AVA,
     $         QUANTTAC, QUANTV, SORTFILETAC, SORTFILEV, ACQ_NT_TOT,
     $         myrank
#ifdef TIMING
     $         ,readtimeplus, readtimeless
#endif
     $         )
            ELSEIF (TG_TYPE .EQ. 3) THEN 
            CALL COMPUTE_TG3(NF, DIMGRUPPOTR, DIMBLOCCOTR,
     $         MAXDIMBLOCCOTR, MAXNRBLOCCHITR, PRIMATR, DBRESTART, NCAMP, NCAMPFILT,
     $         DT, DX, TSHIFT, MAX_NCAMP,MAX_NCAMPFILT, NTRACCEFILE, VEL_SUP, FPESI, BORDF,
     $         SMUSSAMENTO, SEMIAPERTURA, SOGLIA_AMPIEZZE, IMAGING, VERBOSE, MY_TRACES, ANTIALIAS,
     $         TRACCE, TRACCIA1, TRACCIA2, TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET,
     $         TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS, PAN_PARZ, CONT_RPARZ, PAN1, PAN2, PAN3,
     $         PAN4, PAN5, AVA, DIP_MAX, DIP_MIN, AZIMUTH_MAX, AZIMUTH_MIN,
     $         AVO, OFFMIN, OFFMAX,
     $         TG_TYPE, TG_O1, TG_O2, TG_O3, TG_CA1, TG_CA2, TG_CA3, TG_CB1, TG_CB2, TG_CB3,
     $         TG_CC1, TG_CC2, TG_CC3, TG_DA, TG_DB, TG_NA, TG_NB, TG_NT, TG_DDA, TG_DDB,
     $         TG_NNA, TG_NNB, TG_NNT, TG_DC, TG_NC, TG_DDC, TG_NNC, TG_NPAN, TG_NT_TOT,
     $         W_A, W_B, W_C, ATMIN, ATMAX, ATD, ATN, ATTABLE, NTR_FILE,
     $         RUZ, FIDDBA, FIDDBT, FIDDBUZ, NTR_MIG_CICLO,
     $         SS, S, VETT_APE, VETT_DISTX, VETT_DISTY, MUTEFLAG, VETT_OFF,
     $         TG_WEIGHTS_LIN_A, TG_WEIGHTS_LIN_B, TG_WEIGHTS_LIN_C, 
     $         TG_WEIGHTS_CUB_B, TG_WEIGHTS_CUB_C,
     $         RANGET, RANGEA, RANGECZ, TEMPFDG,DAMP_AMPIEZZE,
     $         FIDDBVX, FIDDBVY, FIDDBVZ, SVX, SVY, SVZ, RVX, RVY, RVZ, TG_NT_AVA,
     $         QUANTTAC, QUANTV, SORTFILETAC, SORTFILEV, ACQ_NT_TOT,

     $         myrank
#ifdef TIMING
     $         ,readtimeplus, readtimeless
#endif
     $         )
            ENDIF 

#ifdef TIMING
            elap = second(0) - elap
            comppetottime = second(0) - comppetottime
            sumplus = 0.0
            sumless = 0.0
            do 1235 ii = 1, DIMGRUPPOTR
               sumplus = sumplus + readtimeplus(ii)
               sumless = sumless + readtimeless(ii)
 1235       continue 
            readtimecum = sumplus - sumless
            write(timech,*) "########################################################################"
            write(timech,*) "comp PE: myrank = ",myrank," Iterazione SS, S = ",SS," , ",S,
     $                      "Elapsed Time = ",elap," Read Time = ",readtimecum

            SSEXAM = SS
            if(myrank .NE. iope) then
               write(tottimech,*)  "------------------------------------------------------------------------"
            endif
            write(tottimech,*) "comp PE: myrank = ",myrank," Iterazione SS = ",SSEXAM,
     $                         "Total computing time = ",comppetottime
#ifdef SGI_ORIGIN
           call flush(timech)
           call flush(tottimech)
#endif

#endif

         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 7777 CONTINUE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 23/05/2002 MPI- Per l'ultimo output (non ancora scaricato su disco) I/O PE raccoglie i dati e poi output

#ifdef TIMING
       MPIoutputtime = second(0)
#endif

      if(myrank .EQ. iope) then
         NTR_MIG_CICLO_RECV = 0

         DO 714 IE=1, PAN5
           DO 715 ID=1, PAN4
              do 716 IC=1, PAN3
                 do 717 IB=1, PAN2
                    do 718 IA=1, PAN1
                       PAN_PARZ_RECV(IA,IB,IC,ID,IE)   = 0
                       CONT_RPARZ_RECV(IA,IB,IC,ID,IE) = 0
 718                continue
 717             continue
 716          continue
 715       CONTINUE
 714     CONTINUE
      endif

#ifdef TIMING
      if(myrank .EQ. iope) then
         if(SS .NE. 2) then
            iopebranchtime = second(0) - iopebranchtime
         endif
      endif
      iopewaittime = second(0)
#endif

      call MPI_REDUCE(NTR_MIG_CICLO, NTR_MIG_CICLO_RECV, INT(1)     , MPI_INTEGER4,
     $                MPI_SUM, iope, MPI_COMM_WORLD, ierr)
#ifdef TIMING
      iopewaittime= second(0) - iopewaittime 
#endif


      call MPI_REDUCE(PAN_PARZ     , PAN_PARZ_RECV     , outputcount, MPI_REAL4,
     $                MPI_SUM, iope, MPI_COMM_WORLD, ierr)
      call MPI_REDUCE(CONT_RPARZ   , CONT_RPARZ_RECV   , outputcount, MPI_INTEGER4,
     $                MPI_SUM, iope, MPI_COMM_WORLD, ierr)

#ifdef TIMING
      MPIoutputtime = second(0) - MPIoutputtime - iopewaittime
      MPItottime    = MPItottime + MPIoutputtime
      if(myrank .EQ. iope) then
         if(SS .NE. 1) then
            write(timech,*) "SS = OUT OF LOOP  ######################################################"
            write(timech,*) "I/O PE: MPI Output Time per blocco n. ","OUT OF LOOP"," = ",MPIoutputtime
            write(tottimech,*) "I/O PE: Wait Time nel blocco n. ","OUT OF LOOP"," = ",iopewaittime
            bilanc = REAL((iopebranchtime / (iopebranchtime + iopewaittime)) * 100.0)
            write(tottimech,*) "I/O PE: Bilanciamento  I/O vs comput = ",bilanc," %"
         endif
      endif
#endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if(myrank .EQ. iope) then

#ifdef TIMING
         iopetottime = second(0)
#endif

#ifdef TIMING
         outputtime = second(0)
#endif

         SSEXAM = NRBLOCCHITR
         CALL OUTPUT(NRBLOCCHITR, MAXNRBLOCCHITR, DBRESTART, DBNR, NF,
     $      TG_TYPE, TG_NPAN, TG_NNA, TG_NNB, TGT_STR, NTR_MIG, OUTDIR, LENOUTDIR,
     $      PAN_SAVE, PAN_TOT, CONT_SAVE, CONT_R, PAN1, PAN2, PAN3, PAN4, PAN5,
     $      NTR_MIG_CICLO_RECV, PAN_PARZ_RECV, CONT_RPARZ_RECV,
     $      SSEXAM)
     
         write(*,*) 'Tracce migrate totali: ', NTR_MIG

#ifdef TIMING
         outputtime = second(0) - outputtime
         write(timech,*) "I/O PE: Output Time     per blocco n. ","OUT OF LOOP"," = ",outputtime
#endif

#ifdef TIMING
            iopetottime = second(0) - iopetottime
            write(tottimech,*) "------------------------------------------------------------------------"
            write(tottimech,*) "I/O PE: Iterazione SS = ","OUT OF LOOP"," Total I/O time = ",iopetottime
#endif

         endif

CCC END LOOP CALCOLO **************************************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

#ifdef TIMING
      mainlooptime = second(0) - mainlooptime 
      if(myrank. eq. iope) then
         write(*,*) "************************************************************************"
         write(*,*) "I/O PE: Total MPI Elapsed Time = ",MPItottime," sec"
         write(*,*) "************************************************************************"
         write(*,*) "I/O PE: Total Main Loop Elapsed Time = ",mainlooptime," sec"
         write(*,*) "************************************************************************"
         write(tottimech,*) "------------------------------------------------------------------------"
         write(tottimech,*) ""
         write(tottimech,*) "************************************************************************"
         write(tottimech,*) "I/O PE: Total MPI Elapsed Time = ",MPItottime," sec"
         write(tottimech,*) "************************************************************************"
         write(tottimech,*) "I/O PE: Total Main Loop Elapsed Time = ",mainlooptime," sec"
         write(tottimech,*) "************************************************************************"
      endif
#endif

      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      write(*,*) "I am ",myrank," and I am finishing ..."

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
* -------- fine processing file SEG-Y corrente ------------------------------------
* -------- Chiusura file ----------------------------------------------------------

      CALL GLOB_FCLOSE64(FIDDBA,FILE_ERROR)
      CALL GLOB_FCLOSE64(FIDDBT,FILE_ERROR)
      CALL GLOB_FCLOSE64(FIDDBUZ,FILE_ERROR)
      
      IF (AVA .EQ. 1) THEN
      	  CALL GLOB_FCLOSE64(FIDDBVX,FILE_ERROR)
          CALL GLOB_FCLOSE64(FIDDBVY,FILE_ERROR)
          CALL GLOB_FCLOSE64(FIDDBVZ,FILE_ERROR)
      ENDIF
      
      IF (FPESI .EQ. 1) THEN
        CALL GLOB_FCLOSE64(FIDPESI,FILE_ERROR)
      ENDIF

#ifdef TIMING
      close(timech)
      close(tottimech)
#endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
      if(myrank .EQ. iope) then
         IF (VERBOSE .GT. 1) THEN
            WRITE(*,*) 'FINE processing file SEGY nr: ', NF
            WRITE(*,*) 'Tracce totali effettive     : ', NTR_FILE
            WRITE(*,*) 'Nr totale di tracce migrate : ', NTR_MIG
         ENDIF
      endif

      RETURN
      END

