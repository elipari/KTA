      SUBROUTINE INPUT(FIDSGY, PRIMOBITNS, NSHOTNBYTES, DIMGRUPPOTR, DIMBLOCCOTR, MAXDIMBLOCCOTR,
     $      MAXNRBLOCCHITR, NCAMP, FORMATO, MAX_NCAMP, NTRACCEFILE, FPESI, 
     $      PRIMOBITXS, XSNBYTES, PRIMOBITYS, YSNBYTES, PRIMOBITZS, ZSNBYTES,
     $      PRIMOBITXR, XRNBYTES, PRIMOBITYR, YRNBYTES, PRIMOBITZR, ZRNBYTES,
     $      PRIMATR, OFFMIN, OFFMAX, DBRESTART, A11, A12, A21, A22, B1, B2,
     $      TRACCIA_LETTA, TRACCIA_LETTAC, TRACCIA_LETTASH, TRACES, SGYH64, TRACE_LEN, INDCICLO,
     $      TR_XR, TR_YR, TR_ZR, BINR_OFFSET, BINS_OFFSET, TR_WEIGHTS, TR_XS, TR_YS, TR_ZS, TR_NS,
     $      ACQ_TYPE, ACQ_O1, ACQ_O2, ACQ_O3, ACQ_CA1,ACQ_CA2,ACQ_CA3, ACQ_CB1,ACQ_CB2,ACQ_CB3,
     $      ACQ_CC1,ACQ_CC2,ACQ_CC3, ACQ_DC, ACQ_NC, ACQ_DDC, ACQ_NNC, ACQ_NPAN, ACQ_NT_TOT, ACQ_DA,
     $      ACQ_DB, ACQ_NA, ACQ_NB, ACQ_NT, ACQ_DDA, ACQ_DDB, ACQ_NNA, ACQ_NNB, ACQ_NNT, NTR_FILE,
     $      SS,
     $      size, realsend, intsend,
     $      DATADECIMATION)

      IMPLICIT NONE

      INTEGER*8     INT8 

      INTEGER*4     FIDSGY
      INTEGER*4     PRIMOBITNS, NSHOTNBYTES
      INTEGER*4     DIMGRUPPOTR,MAXNRBLOCCHITR
      INTEGER*4     DIMBLOCCOTR,MAXDIMBLOCCOTR
      INTEGER*4     PRIMOBITXS,XSNBYTES,PRIMOBITYS,YSNBYTES,PRIMOBITZS,ZSNBYTES
      INTEGER*4     PRIMOBITXR,XRNBYTES,PRIMOBITYR,YRNBYTES,PRIMOBITZR,ZRNBYTES
      INTEGER*4     PRIMATR

      INTEGER*4     OFFMIN,OFFMAX

      INTEGER*4     DBRESTART(MAXNRBLOCCHITR)

      INTEGER*4     NCAMP
      REAL*4        A11,  A12,  A21,  A22,  B1,  B2

      INTEGER*4     MAX_NCAMP
      INTEGER*4     NTRACCEFILE

      INTEGER*4     FPESI,FORMATO
! Clara fattore di decimazione delle tracce   
      INTEGER*4     DATADECIMATION
      
CCC 03/05/2002 MPI- Traspongo matrice
CCC      REAL*4        TRACES(MAXDIMBLOCCOTR,MAX_NCAMP)
      REAL*4        TRACES(MAX_NCAMP,MAXDIMBLOCCOTR)
      REAL*4        TRACCIA_LETTA(MAX_NCAMP)
      CHARACTER*1   TRACCIA_LETTAC(MAX_NCAMP*4)
CCC Clara
      INTEGER*2   TRACCIA_LETTASH(MAX_NCAMP)
      
      REAL*4        TR_XR(MAXDIMBLOCCOTR)
      REAL*4        TR_YR(MAXDIMBLOCCOTR)
      REAL*4        TR_ZR(MAXDIMBLOCCOTR)
      INTEGER*8     BINR_OFFSET(MAXDIMBLOCCOTR)
      REAL*4        TR_WEIGHTS(MAXDIMBLOCCOTR)

      REAL*4        TR_XS(MAXDIMBLOCCOTR)
      REAL*4        TR_YS(MAXDIMBLOCCOTR)
      REAL*4        TR_ZS(MAXDIMBLOCCOTR)
CCC 06/05/2002 MPI- Upgrade tr_ns da integer*4 --> integer*8 per fare un'unico MPI_BCAST di integer
CCC                 in execute
      INTEGER*8     TR_NS(MAXDIMBLOCCOTR)
CCC      INTEGER*4     TR_NS(MAXDIMBLOCCOTR)
      INTEGER*8     BINS_OFFSET(MAXDIMBLOCCOTR)

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

      INTEGER*4     NTR_FILE

      INTEGER*4     TRACE_LEN

      INTEGER*8     SGYH64

      INTEGER*4     INDCICLO

**********Variabili locali**********************************************

      INTEGER*4     FIDPESI

      INTEGER*4     NSHOT_SEGY
      INTEGER*4     XSI,YSI,ZSI,XRI,YRI,ZRI
      REAL*4        XS,YS,ZS,XR,YR,ZR

      INTEGER*8     OFF64, OFF64OFFSET
      INTEGER*4     FILE_ERROR

      INTEGER*4     I, S, T, TD, SS, TT

CCC clara      
      INTEGER*4     ISH
      
      INTEGER*4     OFFCUR

CCC 03/05/2002 MPI- MPI variables
      integer*4 size
      real*4    realsend(7,DIMGRUPPOTR,size-1)
      integer*8 intsend(3,DIMGRUPPOTR,size-1) 

      INTEGER*8 UNO_8,T_8,TRACE_LEN_8,DATADECIMATION_8,NSHOT_SEGY_8,HEADERLEN_8,PRIMATR_8,SIZE_REAL_8,DUE_8
      INTEGER*8 PRIMOBITNS_8,PRIMOBITXS_8,PRIMOBITYS_8,PRIMOBITZS_8,PRIMOBITXR_8,PRIMOBITYR_8,PRIMOBITZR_8

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Inizializzazioni
       
       UNO_8=1
       DUE_8=2
       TRACE_LEN_8=TRACE_LEN
       DATADECIMATION_8=DATADECIMATION
       PRIMOBITNS_8=PRIMOBITNS
       PRIMOBITXS_8=PRIMOBITXS
       PRIMOBITYS_8=PRIMOBITYS
       PRIMOBITZS_8=PRIMOBITZS
       PRIMOBITXR_8=PRIMOBITXR
       PRIMOBITYR_8=PRIMOBITYR
       PRIMOBITZR_8=PRIMOBITZR
       HEADERLEN_8=240
       PRIMATR_8=PRIMATR
       SIZE_REAL_8=4
       


       
       DO 295 T=1,DIMBLOCCOTR
         TR_XS(T) = -1
         TR_YS(T) = -1
         TR_ZS(T) = -1
         TR_NS(T) = -UNO_8
         TR_XR(T) = -1
         TR_YR(T) = -1
         TR_ZR(T) = -1
         BINR_OFFSET(T) = -UNO_8
         BINS_OFFSET(T) = -UNO_8
         
 295   CONTINUE
       WRITE(*,*) 'Caricamento geometrie sorgenti e ricevitori...'

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*------------------loading data.....-----------------------------------------

       DO 294 TD=1,DIMBLOCCOTR

        T=(SS-1)*DIMBLOCCOTR+TD
        T_8=T
*     ROB

*------------- Numero sorgente
! Clara fattore di decimazione delle tracce   

        OFF64OFFSET = SGYH64+(T_8-UNO_8)*TRACE_LEN_8*DATADECIMATION_8
        OFF64=OFF64OFFSET+PRIMOBITNS_8
        CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
        CALL GLOB_FREAD(FIDSGY,NSHOT_SEGY,NSHOTNBYTES,1,FILE_ERROR)

*     ROB
#ifdef ALPHA
        CALL swap(NSHOT_SEGY, NSHOTNBYTES,1)         
#endif
        
        NSHOT_SEGY_8=NSHOT_SEGY

        IF (FILE_ERROR .NE. 0) THEN
          WRITE(*,*) 'I/O PE : INPUT - ...fine file '
          GOTO 296
        ENDIF

        NTR_FILE=NTR_FILE+1
        IF (DBRESTART(SS) .EQ. 1 ) GOTO 2988


*------------- Posizione sorgente ____X____

        OFF64=OFF64OFFSET+PRIMOBITXS_8
        CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
        CALL GLOB_FREAD(FIDSGY,XSI,XSNBYTES,1,FILE_ERROR)
#ifdef ALPHA
        CALL swap(XSI, XSNBYTES,1)   
#endif

*------------- Posizione sorgente ____Y____

        OFF64=OFF64OFFSET+PRIMOBITYS_8
        CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
        CALL GLOB_FREAD(FIDSGY,YSI,YSNBYTES,1,FILE_ERROR)
#ifdef ALPHA
        CALL swap(YSI, YSNBYTES,1)
#endif
*------------- Posizione sorgente ____Z____
        IF (PRIMOBITZS .GT. 0) THEN
          OFF64=OFF64OFFSET+PRIMOBITZS_8
          CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
          CALL GLOB_FREAD(FIDSGY,ZSI,ZSNBYTES,1,FILE_ERROR)
#ifdef ALPHA
          CALL swap(ZSI, ZSNBYTES,1)  
#endif
        ELSE
          ZSI=0
        ENDIF


* ------------ Posizione ricevitore ____X____
        OFF64=OFF64OFFSET+PRIMOBITXR_8
        CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
        CALL GLOB_FREAD(FIDSGY,XRI,XRNBYTES,1,FILE_ERROR)
#ifdef ALPHA
        CALL swap(XRI, XRNBYTES,1)
#endif
*------------- Posizione ricevitore ____Y____
        OFF64=OFF64OFFSET+PRIMOBITYR_8
        CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
        CALL GLOB_FREAD(FIDSGY,YRI,YRNBYTES,1,FILE_ERROR)
#ifdef ALPHA
        CALL swap(YRI, YRNBYTES,1)
#endif

*------------- Posizione ricevitore ____Z____
        IF (PRIMOBITZR .GT. 0) THEN
          OFF64=OFF64OFFSET+PRIMOBITZR_8
          CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
          CALL GLOB_FREAD(FIDSGY,ZRI,ZRNBYTES,1,FILE_ERROR)
#ifdef ALPHA
          CALL swap(ZRI, ZRNBYTES,1)
#endif
        ELSE
          ZRI=0
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!        write(6,*)"XRI=",XRI,"  YRI=",YRI,"  XSI=",XSI,"  YSI=",YSI
!        write(6,*)"NSHOT_SEGY_8=",NSHOT_SEGY_8     
        XS=A11*REAL(XSI)+A12*REAL(YSI)+B1
        YS=A21*REAL(XSI)+A22*REAL(YSI)+B2
        ZS=REAL(ZSI)
        
        TR_NS(TD) = NSHOT_SEGY_8
        TR_XS(TD) = XS
        TR_YS(TD) = YS
        TR_ZS(TD) = ZS

        XR=A11*REAL(XRI)+A12*REAL(YRI)+B1
        YR=A21*REAL(XRI)+A22*REAL(YRI)+B2
        ZR=REAL(ZRI)

        TR_XR(TD) = XR
        TR_YR(TD) = YR
        TR_ZR(TD) = ZR
        
!        write(6,*)"XR=",XR,"  YR=",YR,"  XS=",XS,"  YS=",YS

        OFFCUR=INT(SQRT((XS-XR)**2+(YS-YR)**2+(ZS-ZR)**2))

        IF (  ((OFFMIN.LT.0) .AND. (OFFMAX.LT.0))
     $      .OR.((OFFMIN.LT.0) .AND. (OFFCUR.LE.OFFMAX))
     $      .OR.((OFFCUR.GE.OFFMIN) .AND.(OFFMAX.LT.0))
     $      .OR.((OFFCUR.GE.OFFMIN) .AND.(OFFCUR .LE. OFFMAX)) )  THEN
              
             
              

            CALL TROVABIN(BINR_OFFSET(TD), XR, YR, ZR,
     $        ACQ_TYPE,
     $        ACQ_O1, ACQ_O2, ACQ_O3,
     $        ACQ_CA1,ACQ_CA2,ACQ_CA3,
     $        ACQ_CB1,ACQ_CB2,ACQ_CB3,
     $        ACQ_CC1,ACQ_CC2,ACQ_CC3,
     $        ACQ_DA, ACQ_DB, ACQ_NA, ACQ_NB, 
     $        ACQ_DDA,ACQ_DDB,ACQ_NNA,ACQ_NNB,
     $        ACQ_DC, ACQ_NC, ACQ_DDC,
     $        ACQ_NPAN)

            CALL TROVABIN(BINS_OFFSET(TD), XS, YS, ZS,
     $        ACQ_TYPE,
     $        ACQ_O1, ACQ_O2, ACQ_O3,
     $        ACQ_CA1,ACQ_CA2,ACQ_CA3,
     $        ACQ_CB1,ACQ_CB2,ACQ_CB3,
     $        ACQ_CC1,ACQ_CC2,ACQ_CC3,
     $        ACQ_DA, ACQ_DB, ACQ_NA, ACQ_NB, 
     $        ACQ_DDA,ACQ_DDB,ACQ_NNA,ACQ_NNB,
     $        ACQ_DC, ACQ_NC, ACQ_DDC,
     $        ACQ_NPAN)
              
              

        ELSE             
            BINR_OFFSET(TD)=-DUE_8           ! Flag esclusione traccia
            BINS_OFFSET(TD)=-DUE_8
        ENDIF

CCC Clara

! Clara fattore di decimazione delle tracce   
             OFF64=(T_8-UNO_8)*TRACE_LEN_8*DATADECIMATION_8+UNO_8+SGYH64+HEADERLEN_8
        
        IF (FORMATO.EQ.2) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

* --------- lettura della traccia  in formato ieee ----------------------------------

             
             CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
             CALL GLOB_FREAD(FIDSGY,TRACCIA_LETTA,4,NCAMP,FILE_ERROR)
#ifdef ALPHA
             CALL swap(TRACCIA_LETTA, 4, NCAMP)
#endif
        ELSE IF (FORMATO.EQ.1) THEN
* --------- lettura della traccia  in formato ibm -----------------------------------

              
       
             CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
             

             CALL GLOB_FREAD(FIDSGY,TRACCIA_LETTAC,1,NCAMP*4,FILE_ERROR)
             
#ifdef ALPHA
*             CALL swap(TRACCIA_LETTA, 4, NCAMP)
#endif
            
             IF (FILE_ERROR .EQ. 0) THEN
             	             

               CALL IBM2IEEE(TRACCIA_LETTAC,TRACCIA_LETTA,NCAMP)
               
                    

             ENDIF
        ELSE 
* --------- lettura della traccia  in 16bit (formato=3)-----------------------------------

             CALL GLOB_FSEEK64(FIDSGY,OFF64,0,FILE_ERROR)
             CALL GLOB_FREAD(FIDSGY,TRACCIA_LETTASH,2,NCAMP,FILE_ERROR)
#ifdef ALPHA
             CALL swap(TRACCIA_LETTASH, 2, NCAMP)
#endif             
             IF (FILE_ERROR .EQ. 0) THEN
               DO 297 ISH=1, NCAMP
                  TRACCIA_LETTA(ISH) = TRACCIA_LETTASH(ISH)
 297           CONTINUE
             ENDIF
        ENDIF

        IF ( FPESI .EQ. 1) THEN

          OFF64= (PRIMATR_8-UNO_8)*SIZE_REAL_8+(T_8-UNO_8)*SIZE_REAL_8+UNO_8

          CALL GLOB_FSEEK64(FIDPESI,OFF64,0,FILE_ERROR)
          CALL GLOB_FREAD(FIDPESI,TR_WEIGHTS(TD),4,1,FILE_ERROR)

        ELSE

          TR_WEIGHTS(TD)=1

        ENDIF
              
        
        DO 2999 I=1,NCAMP
CCC 03/05/2002 MPI- Traspongo matrice
CCC          TRACES(TD,I)=TRACCIA_LETTA(I)
         TRACES(I,TD)=TRACCIA_LETTA(I)
 2999   CONTINUE
               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 2988   IF (NTR_FILE.GE.NTRACCEFILE) THEN
         GOTO 296
        ENDIF

 294  CONTINUE

 296  CONTINUE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC MPI- Preparo buffer da spedire ai comp PEs
      do 2200 S=1, INDCICLO
         do 2250 TD=1, DIMGRUPPOTR
            TT = (S-1)*DIMGRUPPOTR+TD
            realsend(1,TD,S) = TR_XR(TT)
            realsend(2,TD,S) = TR_YR(TT)
            realsend(3,TD,S) = TR_ZR(TT)
            realsend(4,TD,S) = TR_XS(TT)
            realsend(5,TD,S) = TR_YS(TT)
            realsend(6,TD,S) = TR_ZS(TT)
            realsend(7,TD,S) = TR_WEIGHTS(TT)
            intsend(1,TD,S)  = BINR_OFFSET(TT)
            intsend(2,TD,S)  = BINS_OFFSET(TT)
            intsend(3,TD,S)  = TR_NS(TT)
 2250    continue
 2200 continue
 
              

      END
