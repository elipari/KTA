*---------------------------------------------------------------------
*     SUBROUTINE EXECUTE
*---------------------------------------------------------------------
      subroutine execute_iope(o1,o2,o3,d1,d2,d3,n1,n2,n3,esize,
     &     UPDOWN,
     &     Nshot, xshot, yshot, zshot,
     &     TARGET, Ntgt, Ntgt1, xtgt, ytgt, ztgt,
     &     AA, TT, PPx, PPy, PPz,
     &     OutDir, LenOutDir, OUTPUT, OUT_A, OUT_T,
     &     OUT_UX, OUT_UY, OUT_UZ, VERBOSE,
     &     myrank, size, iope, Npackets, q, r, Pbuffer, requests, statuses,
     &     NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ, 
     &     rangeA, rangeT, rangeCx, rangeCy, rangeCz, rangeA_recv, 
     &     nRangeA, nRangeT, nrangeCx, nrangeCy, nRangeCz,
     &     writeBufferFloat, writeBufferShort, dimWriteBuf, shotSort, QUANT_FLAG, index_cur,
     &     index_A, index_T, index_UX, index_UY, index_UZ) 


      implicit none

      include 'mpif.h'

*     Parametri
      integer*4 MAXSTR
      parameter (MAXSTR=200)

*     Variabili in input
      real*4    o1,o2,o3
      real*4    d1,d2,d3
      integer*4 n1,n2,n3
      integer*4 esize

      integer*4 UPDOWN
      integer*4 VERBOSE

      integer*4 Nshot
      real*4    xshot(Nshot)
      real*4    yshot(Nshot)
      real*4    zshot(Nshot)
      real*4    xs,ys,zs

      integer*4 TARGET
      integer*4 Ntgt, Ntgt1
      integer*4 NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ
      real*4    xtgt(Ntgt1)
      real*4    ytgt(Ntgt1)
      real*4    ztgt(Ntgt1)


      real*4    AA(NtgtAA),   TT(NtgtTT)
      real*4    PPx(NtgtPPX),  PPy(NtgtPPY),  PPz(NtgtPPZ)


      character*(MAXSTR) OutDir
      integer*4 LenOutDir
      integer*4 OUTPUT
      integer*4 OUT_A
      integer*4 OUT_T
      integer*4 OUT_UX,OUT_UY,OUT_UZ


! CAN&BN: gestione della scrittura del database trasposto
!         dimWriteBuf = dimensione (in numero di elementi da 2 byte) del buffer in uscita
!         writeBuffer = buffer in cui vengono parcheggiate le funzioni di Green prima della scrittura su disco
!         shotSort    = array per il sorting dell'uscita (ogni elemento corrisponde ad uno shot e indica
!                       il numero della riga in cui lo shot e' stato effettivamente scritto)
      INTEGER*4  dimWriteBuf
      integer*4  index_cur
      INTEGER*4  shotSort(Nshot)
! CAN&BN: shotInBuf = numero di shot che possono essere parcheggiati nel buffer di scrittura
!         nShotMem  = numero di shot effettivamente parcheggiati nel buffer di scrittura (viene gestito da dataoutput)     
!         nShotWri  = numero degli shot gia' salvati su disco
      INTEGER*4 shotInBuf, nShotMem, nShotWri
      REAL*4     writeBufferFloat(dimWriteBuf)
      INTEGER*2  writeBufferShort(dimWriteBuf*2)
      
      integer*4 size_real,size_short
      integer*2 size_test
      real*4    size_test2
      integer*4 file_error

*     ROB start: variabili impacchettamento 
      integer q 
      integer r
      real*4 Pbuffer(q+1)
      integer NPackets
      integer requests(NPackets)
      integer statuses(MPI_STATUS_SIZE,NPackets)
*     ROB end: variabili impacchettamento

      integer*4 Paddr

      integer*4 nRangeA, nRangeT, nRangeCx, nRangeCy, nRangeCz, range_len
      real*4    rangeA(nRangeA), rangeT(nRangeT), rangeCx(nRangeCx), rangeCy(nRangeCy), rangeCz(nRangeCz)
      real*4    rangeA_recv(nRangeA + nRangeT + nRangeCx + nRangeCy + nRangeCz)
      integer*4 QUANT_FLAG
      integer*4 index_A, index_T, index_UX, index_UY, index_UZ


!     Variabili locali
      integer*4 Nxs,Nys,Nzs
      real*4  memo, n32
      integer*4 ix,iy,iz,is, ii, Nziniz
      integer*4 Nxmin, Nxmax, Nymin, Nymax

      integer*4 it, izt_max
      real*4    zmax, zmin

*     ROB start: indice pacchetto impacchettamento

      integer*4 p
      
*     ROB end: strutture impacchettamento

#ifdef TIMING
CCC   04/06/2002 Variabili per controllo temporale
      real*8 elap, cput, second, comptime, iotime, iotottime
      real*8 mainlooptime, isendtime, isendtottime, waittime, waittottime, recvtime, recvtottime

CCC itostr = Converte integer --> stringa
CCC strlen = Ritorna lunghezza di una stringa
      external itostr, strlen

CCC tottimech = Unita' logica file timing totale
      integer*4 tottimech

CCC cc, itostr, strlen = Variabili utilizzate per costruire nome files timing dei comp PE
CCC timefile*30    = Nome file timing parziale
CCC tottimefile*30 = Nome file timing totale
      integer*4 strlen
      character cc*9, itostr*9, tottimefile*30

#endif

CCC   03/05/2002 MPI- MPI variables CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   07/06/2002 N.B. Le variabili SENZA kind (es. senza *4) sono state dichiarate cosi' per garantire la
CCC   portabilita' degli argomenti delle subroutine MPI su macchine con precisione INTEGER, REAL
CCC   diverse (standard --> un argomento sia INTEGER, non INTEGER*4)

CCC   31/05/2002
CCC   myrank = Rank PE
CCC   size   = Numero totale PE utilizzati
      integer*4 myrank, size

CCC   ierr = Codice errore chiamate MPI
      integer ierr

CCC   iope = size-1 = Rank di I/O PE
      integer iope

CCC   period Periodo dell'Andamento Tempo di computazione shot vs numero shot, andamento a dente di sega
      integer*4 period

CCC   ista(i), iend(i) = Il PE myrank deve elaborare ista(i), ista(i)+1, ...., iend(i),
CCC   ista(i+1), ista(i+1)+1, ...., iend(i+1),
CCC   .... con i=1, 2, ...., mywork
      integer*4 ista(Nshot), iend(Nshot)

CCC   mywork = Numero chunck da elaborare
      integer*4 mywork

CCC   tag     = tag messaggio
CCC   dest    = destinazione send
CCC   status  = status recv
CCC   request = request send noon bloccante caso Npackets = 1
      integer tag, dest, status(MPI_STATUS_SIZE), request

CCC   ich     = indice loop calcolo sui vari chunk di lavoro
CCC   shotID  = ID dello shot
CCC   first   = differenzia la prima iterazione dalle altre
      integer*4 ich, shotID, first



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Inizializzazioni

      range_len = nRangeA + nRangeT + nRangeCx + nRangeCy + nRangeCz
      DO 10  IT = 1, RANGE_LEN
         RANGEA_RECV(IT) = 0.0
 10   CONTINUE

! CAN&BN: inizializzazione 
      shotInBuf = dimWriteBuf / ( index_cur * Ntgt)
      IF ( QUANT_FLAG .EQ. 1) shotInBuf = shotInBuf*2
      nShotMem  = 0
      nShotWri  = 0
!CAN&BN: calcolo dimensione di REAL*4,INTEGER*2
      CALL glob_sizereal4(size_test2, size_real)      
      CALL glob_sizeshortint(size_test, size_short)      
      

#ifdef TIMING
      tottimech = 4120 + myrank

         tottimefile = 'total-time-PE-IO.eik'
      open (tottimech, FILE = tottimefile, STATUS = 'UNKNOWN')
#endif



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Calcolo limiti

*     Se TARGET=2 determinazione della massima quota a cui
*     calcolare il campo d'onda
      if (TARGET .EQ. 2) then

        zmax = o3
        zmin = (n3-1)*d3+o3           ! apr 2002 (aggiunto o3)
        do 41 it=1,Ntgt
          if (ztgt(it) .gt. zmax) then
             zmax = ztgt(it)
          endif
          if (ztgt(it) .lt. zmin) then
             zmin = ztgt(it)
          endif
 41     continue

         if (UPDOWN .eq. 1) then
            izt_max = int((zmax-o3)/d3)+2
         endif

         if (UPDOWN .eq. 2) then
            izt_max = n3-int((zmin-o3)/d3)
         endif

         if (izt_max .gt. n3) then
            izt_max = n3
         endif

         if (izt_max .lt. 1) then
            izt_max = 1
         endif

      else

         izt_max = n3

      endif





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Distribuzione del carico di lavoro fra i computing PE
CCC   Ricerca del periodo
      period = 1
      do 100 while((zshot(period+1) .GT. zshot(period)) .AND. period .LT. Nshot)
         period = period + 1
 100  continue

CCC Divisione del lavoro fra i computing PEs
      call compute_work(myrank, (size-1), Nshot, period, ista, iend, mywork)

      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      write(*,*) "I am ",myrank," and I am starting ..."

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*     CICLO PRINCIPALE PER TUTTI GLI SHOT

#ifdef TIMING
      mainlooptime = second(0)
      isendtottime = 0.0
      waittottime  = 0.0
      recvtottime  = 0.0
      iotottime    = 0.0 
#endif 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Computing PE

      
         do 430 is=1,Nshot
#ifdef TIMING
            cput=second(2)
            elap=second(0)
#endif

#ifdef TIMING
            recvtime    = second(0) 
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Npackets == 1
            if(Npackets .EQ. 1) then            
               call MPI_RECV(AA, q, MPI_REAL4, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
               shotID = status(MPI_TAG)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Npackets != 1
            else
               call MPI_RECV(shotID, INT(1), MPI_INTEGER4, MPI_ANY_SOURCE, INT(0), MPI_COMM_WORLD, status, ierr)
               tag = (shotID - 1) * Npackets + 1
               Paddr = 1
               p = 1
               do 300 while (p .LE. r)
                  call MPI_RECV(Pbuffer, INT(q+1), MPI_REAL4, status(MPI_SOURCE), tag, MPI_COMM_WORLD, 
     &                          status, ierr)
                  p = p + 1
                  tag = tag + 1
*     Paddr = Paddr + q + 1
                  do 3000 ii=1, q+1
                     AA(Paddr) = Pbuffer(ii)
                     Paddr = Paddr + 1
 3000             continue
 300           continue
               do 301 while (p .LE. Npackets)
                  call MPI_RECV(Pbuffer, INT(q), MPI_REAL4, status(MPI_SOURCE), tag, MPI_COMM_WORLD, 
     &                          status, ierr)   
                  p = p + 1
                  tag = tag + 1
*     Paddr = Paddr + q 
                  do 3010 ii=1, q
                     AA(Paddr) = Pbuffer(ii)
                     Paddr = Paddr + 1
 3010             continue
 301           continue
            endif

#ifdef TIMING
            recvtime    = second(0) - recvtime
            recvtottime = recvtottime + recvtime
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Output

#ifdef TIMING
            iotime = second(0)
#endif

            CALL dataoutput(o1,o2,o3,d1,d2,d3,n1,n2,n3,esize,
     &           UPDOWN,
     &           Nshot, xshot, yshot, zshot,
     &           TARGET, Ntgt, 
     &           AA, TT, PPx, PPy, PPz,
     &           OutDir, LenOutDir, OUTPUT, OUT_A, OUT_T,
     &           OUT_UX,OUT_UY,OUT_UZ, shotID, 
     &           NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ,  
     &           rangeA, rangeT,rangeCz, 
     &           writeBufferFloat, writeBufferShort, 
     &           dimWriteBuf,shotSort,nShotMem, shotInBuf, nShotWri, index_cur,
     &           index_A, index_T, index_UX, index_UY, index_UZ, QUANT_FLAG)

#ifdef TIMING
            iotime = second(0) - iotime
            iotottime = iotottime + iotime
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#ifdef TIMING
            elap=second(0)-elap
            cput=second(2)-cput
            write(tottimech,*) "********** myrank = I/O PE"
            write(tottimech,*) "Shot ",shotID," ELAPSED Time  = ",elap
            write(tottimech,*) "Shot ",shotID," CPU  Time     = ",cput
            write(tottimech,*) "Shot ",shotID," Recv time     = ",recvtime
            write(tottimech,*) "PE Shot ",shotID," I/O  time     = ",iotime
#ifdef SGI_ORIGIN
            call flush(tottimech)
#endif
#endif
 430     continue
 







CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#ifdef TIMING
      mainlooptime = second(0) - mainlooptime

         write(tottimech,*) "########## myrank = I/O PE"
         write(tottimech,*) "Total Loop Time = ", mainlooptime
         write(tottimech,*) "Total Recv Time = ", recvtottime
         write(tottimech,*) "Total I/O Time  = ", iotottime

#endif
      call MPI_REDUCE(rangeA, rangeA_recv, range_len, MPI_REAL,
     $                MPI_SUM, iope, MPI_COMM_WORLD, ierr)

      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      write(*,*) "I am ",myrank," and I am finishing ..."

      RETURN
      END

