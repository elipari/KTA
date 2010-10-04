*---------------------------------------------------------------------
*     SUBROUTINE EXECUTE
*---------------------------------------------------------------------
      subroutine execute(o1,o2,o3,d1,d2,d3,n1,n2,n3,esize,n3ver,S,Sx,Sy,
     &     SOGLIAX,SOGLIAY,MODALITA,APERTURA,ANGOLO,
     &     INIZIO,UPDOWN,VERSIONE,AMPMIN,DAMP,
     &     Nshot, xshot, yshot, zshot,
     &     TARGET, Ntgt, Ntgt1, xtgt, ytgt, ztgt,
     &     A,  T,  Px,  Py,  Pz,
     &     AA, TT, PPx, PPy, PPz,
     &     xcorr, ycorr, Acorr, Pxcorr, Pycorr,
     &     Ok2,Posx,Posy,Ok1,
     &     OutDir, LenOutDir, OUTPUT, OUT_A, OUT_T,
     &     OUT_UX, OUT_UY, OUT_UZ, VERBOSE,Tcorr,
     &     myrank, size, iope, Npackets, q, r, Pbuffer, requests, statuses,
     &     NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ,
     &     vettdistx, vettdisty, maxdisty, maxdistx, distxiniz, zmaxdistx,
     &     distyiniz, zmaxdisty, zdistyiniz,     
     &     rangeA, rangeT, rangeCx, rangeCy, rangeCz, rangeA_recv,
     &     nRangeA, nRangeT, nrangeCx, nrangeCy, nRangeCz, QUANT_FLAG )
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
      integer*4 n3ver
      real*4    S(n1,n2,n3),Sx(n1,n2,n3ver),Sy(n1,n2,n3ver)

      real*4    SOGLIAX,SOGLIAY
      integer*4 MODALITA
      integer*4 APERTURA
      real*4    ANGOLO
      integer*4 INIZIO
      integer*4 UPDOWN
      integer*4 VERSIONE
      real*4    AMPMIN
      real*4    DAMP
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

      real*4    A(n1,n2,2), T(n1,n2,2)
      real*4    Px(n1,n2,2),  Py(n1,n2,2),  Pz(n1,n2,2)
      real*4    AA(NtgtAA),   TT(NtgtTT)
      real*4    PPx(NtgtPPX),  PPy(NtgtPPY),  PPz(NtgtPPZ)
      
      real*4    xcorr(n1,n2), ycorr(n1,n2)
      real*4    Acorr(n1,n2), Pxcorr(n1,n2), Pycorr(n1,n2),Tcorr(n1,n2)
      integer*4 Posx(-1:(n1+1),-1:(n2+1),2), Posy(-1:(n1+1),-1:(n2+1),2)
      integer*4 Ok2(n1,n2),Ok1(n1,n2)

      character*(MAXSTR) OutDir
      integer*4 LenOutDir
      integer*4 OUTPUT
      integer*4 OUT_A
      integer*4 OUT_T
      integer*4 OUT_UX,OUT_UY,OUT_UZ

      real*4        vettdistx(n3), vettdisty(n3)      
      integer*4     MAXDISTY, MAXDISTX
      integer*4     ZMAXDISTY, ZMAXDISTX
      integer*4     DISTYINIZ, DISTXINIZ 
      integer*4     ZDISTYINIZ, ZDISTXINIZ     

      integer*4     NZDISTYCOST, NZDISTY, NZDISTX, NZDISTXCOST 
      real*4        DELTADISTY, DELTADISTX

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
      integer*4 QUANT_FLAG, ANGLE

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
         RANGEA(IT) = 0.0
 10   CONTINUE
     
#ifdef TIMING
      tottimech = 4120 + myrank

         cc=itostr(myrank)
         tottimefile = 'total-time-PE-comp-'//cc(1:strlen(cc))//'.eik'

      open (tottimech, FILE = tottimefile, STATUS = 'UNKNOWN')
#endif

* Eventuale INVERSIONE del modello se la propagazione e' dal basso verso l'alto
      if (UPDOWN.eq.2) then
        n32=INT(n3/2)
        DO 11 iz=1,n32
          DO 12 iy=1,n2
            DO 13 ix=1,n1
               memo=S(ix,iy,iz)
               S(ix,iy,iz)=S(ix,iy,n3-iz+1)
               S(ix,iy,n3-iz+1)=memo
13          continue
12        continue
11      continue
      endif

*     Conversione del modello di velocita` in SLOWNESS
      do 20 iz=1,n3
         do 30 iy=1,n2
            do 40 ix=1,n1
               S(ix,iy,iz)=1.0E0/S(ix,iy,iz)
 40         continue
 30      continue
 20   continue

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Calcolo derivate

*     Calcolo delle DERIVATE PARZIALI DELLA SLOWNESS se si e' scelta la
*     versione VELOCE
      if (VERSIONE .eq. 1) then
         CALL derivxyvel(n1,n2,n3,d1,d2,S,Sx,Sy)
      endif

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
            izt_max = int((zmax-o3)/d3)+1
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


! AREA ILLUMINATA

           IF (ZDISTYINIZ .EQ. -1) ZDISTYINIZ = o3
!          NUMERO DI CAMPIONI AD APERTURA COSTANTE           
           NZDISTYCOST=NINT((ZDISTYINIZ-o3)/d3)+1
           
           
           IF ( ZMAXDISTY .NE. -1 ) THEN
!             NUMERO DI CAMPIONI AD APERTURA VARIABILE
              NZDISTY  = NINT((ZMAXDISTY-ZDISTYINIZ)/d3)
              IF (NZDISTY .GT. n3) THEN
           	  NZDISTY = n3
              ENDIF
!             INCREMENTO UNITARIO DELL'APERTURA               
              DELTADISTY = REAL(MAXDISTY-DISTYINIZ)/REAL(NZDISTY) 
           ELSE
              DELTADISTY = 0
           ENDIF
           
           DO 952 ii = 1, n3
               IF ( ii .LE. NZDISTYCOST) THEN
                  VETTDISTY(ii) = DISTYINIZ
               ELSEIF ( ii .LE. (NZDISTY+NZDISTYCOST-1)) THEN
                  VETTDISTY(ii) = VETTDISTY(ii-1) + DELTADISTY 
               ELSE
               	  VETTDISTY(ii) = MAXDISTY
               ENDIF
 952       CONTINUE
 
           
           ZDISTXINIZ = -1
           IF (ZDISTXINIZ .EQ. -1) THEN
           	ZDISTXINIZ = o3
           ENDIF
           NZDISTXCOST=NINT((ZDISTXINIZ-o3)/d3)+1     
           NZDISTX  = NINT((ZMAXDISTX-ZDISTXINIZ)/d3)
           IF (NZDISTX .GT. n3) THEN
           	NZDISTX = n3
           ENDIF
           IF ( ZMAXDISTX .NE. -1 ) THEN
              DELTADISTX = REAL(MAXDISTX-DISTXINIZ)/REAL(NZDISTX)
           ELSE
              DELTADISTX = 0
           ENDIF
           DO 953 ii = 1, n3
               IF ( ii .LE. NZDISTXCOST) THEN
                  VETTDISTX(ii) = DISTXINIZ 
               ELSEIF ( ii .LE. (NZDISTX+NZDISTXCOST-1)) THEN
               	  VETTDISTX(ii) = VETTDISTX(ii-1) + DELTADISTX 
               ELSE
               	  VETTDISTX(ii) = MAXDISTX
               ENDIF
 953       CONTINUE
! Normalizzazione rispetto ai passi di campionamento
           DO 954 ii = 1, n3
               IF ( VETTDISTX(ii).NE.-1) VETTDISTX(ii) = VETTDISTX(ii)/d1
               IF ( VETTDISTY(ii).NE.-1) VETTDISTY(ii) = VETTDISTY(ii)/d2
 954       CONTINUE               
 
! FINE AREA ILLUMINATA 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Distribuzione del carico di lavoro fra i computing PE
CCC   Ricerca del periodo
      period = 1

      if (Nshot .NE. 1) then
         do 100 while((period .LT. Nshot)  .AND. (zshot(period+1).GT. zshot(period)) )
            period = period + 1
 100     continue
      endif

CCC Divisione del lavoro fra i computing PEs
      call compute_work(myrank, size-1, Nshot, period, ista, iend, mywork)

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
                                
         dest  = iope
         
         do 400 ich=1, mywork 
            do 420 is=ista(ich), iend(ich)
#ifdef TIMING
               cput=second(2)
               elap=second(0)
#endif
               IF (VERBOSE .GT. 1) THEN
                  WRITE(*,*) 'myrank = ',myrank,' Elaborazione per lo shot  # ',is,
     &                 ' xs = ', xshot(is),' - ys = ',yshot(is),
     &                 ' - zs = ',zshot(is)
               ENDIF

#ifdef TIMING
               waittime = second(0)
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC 07/06/2002 MPI- Prima iterazione non richiede MPI_WAIT
               if(first. EQ. 1) then
                  call MPI_WAIT(request, status, ierr)
                  if(Npackets .GT. 1) then
                     call MPI_WAITALL(Npackets, requests, statuses, ierr)
                  endif
               else
                  first = 1
               endif

#ifdef TIMING
                waittime = second(0) - waittime
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Inizializzazioni

* Inizializzazione matrici di uscita    (mag 2002)
               do 110 ii=1,NtgtAA
                  AA(ii)  = -2.0E0
 110           continue
               do 111 ii=1,NtgtTT
                  TT(ii)  = -2.0E0
 111           continue
               do 112 ii=1,NtgtPPX
                  PPX(ii)  = -2.0E0
 112           continue
               do 113 ii=1,NtgtPPY
                  PPY(ii)  = -2.0E0
 113           continue
               do 114 ii=1,NtgtPPZ
                  PPZ(ii)  = -2.0E0
 114           continue              

* Conversione delle coordinate dello shot nella piu vicina posizione sulla griglia
               xs=xshot(is)-o1
               ys=yshot(is)-o2
               zs=zshot(is)-o3

               Nxs=NINT(xs/d1)+1
               Nys=NINT(ys/d2)+1
               Nzs=NINT(zs/d3)+1

              if (UPDOWN .eq. 2) then
                 zs = REAL(n3-1)*d3 - zs
                 Nzs= (n3-Nzs) + 1
              endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Fase computazionale

#ifdef TIMING
               comptime = second(0)
#endif

* Nr di piani da inizializzare *oltre* (e sotto) quello della sorgente con indice Nzs
               Nziniz=2
               if ((Nzs.LT.1).OR.(Nzs.GT.n3).OR.
     &            (Nxs.LT.1).OR.(Nxs.GT.n1).OR.
     &            (Nys.LT.1).OR.(Nys.GT.n2)
     &             ) goto 2000

********************************************************************************
********************************************************************************
*     CHIAMATA ALLA ROUTINE CHE INTEGRA IL CAMPO
         CALL iconal3d(o1,o2,o3,d1,d2,d3,n1,n2,n3,n3ver,S,Sx,Sy,
     &                SOGLIAX,SOGLIAY,MODALITA,APERTURA,ANGOLO,
     &                INIZIO,UPDOWN,VERSIONE,AMPMIN,DAMP,
     &                xs,ys,zs,Nxs,Nys,Nzs,
     &                TARGET, Ntgt, Ntgt1, xtgt, ytgt, ztgt,
     &                A,  T,  Px,  Py,  Pz,        
     &                AA, TT, PPx, PPy, PPz,
     &                xcorr, ycorr, Acorr, Pxcorr, Pycorr,Tcorr,
     &                OK1,OK2,Posx,Posy,
     &                Nxmin,Nxmax,Nymin,Nymax,izt_max,is,Nziniz,
     &                OUT_A, OUT_T, OUT_UX, OUT_UY, OUT_UZ,
     &                NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ,
     &                vettdistx, vettdisty)
********************************************************************************
********************************************************************************

 2000          continue
           
!QUANTIZZAZIONE
       ANGLE=0
       IF (UPDOWN .EQ. 1) ANGLE=1
       IF (QUANT_FLAG .EQ. 1) THEN           
           IF (OUT_A .EQ. 1)  CALL quant(AA, Ntgt, is, Nshot, rangeA,0)
           IF (OUT_T .EQ. 1)  CALL quant(TT, Ntgt, is, Nshot, rangeT,0)
           IF (OUT_UX .EQ. 1) CALL quant(PPx, Ntgt, is, Nshot, rangeCx, ANGLE)
           IF (OUT_UY .EQ. 1) CALL quant(PPy, Ntgt, is, Nshot, rangeCy, ANGLE)
           IF (OUT_UZ .EQ. 1) CALL quant(PPz, Ntgt, is, Nshot, rangeCz, ANGLE)
       ENDIF

#ifdef TIMING
               comptime = second(0) - comptime
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Send data to I/O PE

#ifdef TIMING
               isendtime = second(0)
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Npackets == 1
               if(Npackets .EQ. 1) then            
                  tag = is
                  call MPI_ISEND(AA, q, MPI_REAL4, dest, tag, MPI_COMM_WORLD, request, ierr) 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Npackets != 1
               else
                  tag = 0
                  call MPI_ISEND(is, 1, MPI_INTEGER4, dest, tag, MPI_COMM_WORLD, request, ierr)

                  tag = (is - 1) * Npackets + 1
                  Paddr = 1
                  p = 1
                  do 200 while (p .LE. r)
                     call MPI_ISEND(AA(Paddr), int(q+1), MPI_REAL4, dest, tag, MPI_COMM_WORLD, requests(p), ierr)   
                     p = p + 1
                     tag = tag + 1
                     Paddr = Paddr + q + 1
 200              continue
                  do 201 while (p .LE. Npackets)
                     call MPI_ISEND(AA(Paddr), int(q), MPI_REAL4, dest, tag, MPI_COMM_WORLD, requests(p), ierr)   
                     p = p + 1
                     tag = tag + 1
                     Paddr = Paddr + q 
 201              continue
               endif

#ifdef TIMING
               isendtime    = second(0) - isendtime
               isendtottime = isendtottime + isendtime
               waittottime  = waittottime + waittime
#endif
               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#ifdef TIMING
               elap = second(0)-elap
               cput = second(2)-cput
               write(tottimech,*) "********** myrank = ",myrank
               write(tottimech,*) "Shot ",is," ELAPSED Time = ",elap
               write(tottimech,*) "Shot ",is," CPU Time     = ",cput
               write(tottimech,*) "Shot ",is," Comp time    = ",comptime
               write(tottimech,*) "Shot ",is," Isend time   = ",isendtime
               write(tottimech,*) "Shot ",is," Wait time    = ",waittime
#ifdef SGI_ORIGIN
               call flush(tottimech)
#endif
#endif
 420        continue
 400     continue

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   Ultima MPI_WAIT

#ifdef TIMING
         waittime = second(0)
#endif

         call MPI_WAIT(request, status, ierr)
         if(Npackets .GT. 1) then
            call MPI_WAITALL(Npackets, requests, statuses, ierr)
         endif

#ifdef TIMING
         waittime = second(0) - waittime
#endif

#ifdef TIMING
         waittottime  = waittottime + waittime
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#ifdef TIMING
      mainlooptime = second(0) - mainlooptime
         write(tottimech,*) "########## myrank = ",myrank
         write(tottimech,*) "Total Loop  Time = ",mainlooptime
         write(tottimech,*) "Total Isend Time = ",isendtottime
         write(tottimech,*) "Total Wait  Time = ",waittime

#endif

      call MPI_REDUCE(rangeA, rangeA_recv, range_len, MPI_REAL,
     $                MPI_SUM, iope, MPI_COMM_WORLD, ierr)

      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      write(*,*) "I am ",myrank," and I am finishing ..."

      RETURN
      END


!===============================================================================================================
!===============================================================================================================
!===============================================================================================================

      
      subroutine quant( floatvet, Ntgt, is, Nshot, rangeArray, ANGLE)
      
      integer   Ntgt
      real*4    floatvet(Ntgt)
      real*4    high, low
      
      integer nLevel, is, Nshot, ANGLE
      real*4  step, mid, rangeArray(Nshot*2), flt_max


!     Trova valore minimo e massimo solo se ANGLE == 0                        
      IF (ANGLE .EQ. 0) THEN      
         flt_max = 100.0
         low = flt_max
         high  = -2
               
         do 1130 ii=1,Ntgt
             if (floatvet(ii).ge.high) then
             	high=floatvet(ii)
             endif	
             if ((floatvet(ii) .le. low).and.(floatvet(ii) .ne. -2.0)) then
             	low=floatvet(ii)
             endif			                       			
 1130    continue  
      ELSE 
         low   = -1
         high  = +1
      ENDIF      	     
 
 
!     Numero di livelli di quantizzazione      
      nLevel = 65532
      
!     Intervallo di quantizzazione
      step = (high-low)/(nLevel-2)

!     Punto medio fra minimo e massimo      
      mid = (high+low)/2.0

!     Salva mid e step nell'array di range
      rangeArray(is)=mid
      rangeArray(Nshot+is)=step      

!     Quantizzazione      
      do 100 j = 1, Ntgt
          if ( (floatvet(j) .ne. -2.0)) then
             if (step.NE.0.0) THEN
                floatvet(j) = nint( anint((floatvet(j)-mid)/step) )
             else
             	floatvet(j) = 0
             endif
          else
             floatvet(j) = 32767
          endif
100   continue

      return
      end
      
      
