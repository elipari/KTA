      subroutine iconal3d(o1,o2,o3,d1,d2,d3,n1,n2,n3,n3ver,S,Sx,Sy,
     &     SOGLIAX,SOGLIAY,MODALITA,APERTURA,ANGOLO,
     &     INIZIO,UPDOWN,VERSIONE,AMPMIN,DAMP,
     &     xshot,yshot,zshot,Nxs,Nys,Nzs,
     &     TARGET, Ntgt, Ntgt1, xtgt, ytgt, ztgt,
     &     A,  T,  Px,  Py,  Pz,        
     &     AA, TT, PPx, PPy, PPz,
     &     xcorr, ycorr, Acorr, Pxcorr, Pycorr,Tcorr,
     &     OK1,OK2,Posx,Posy,
     &     Nxmin,Nxmax,Nymin,Nymax,izt_max,is,Nziniz,
     &     OUT_A, OUT_T, OUT_UX, OUT_UY, OUT_UZ,
     &     NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ, vettdistx, vettdisty )
*---------------------------------------------------------------------
*     
*     Subroutine che esegue la propagazione del campo in profondita`.
*     Viene integrata lungo le z l'equazione dell'iconale con un metodo alle
*     differenze finite e ad ogni passo vengono rigrigliate le
*     componenti del campo.
*     I traveltime vengono calcolati integrando il campo della slowness e
*     le ampiezze vengono stimate calcolando la divergenza dei tubi
*     di flusso formati da quadruplette di raggi limitrofi.
*     I raggetti che si intersecano vengono scartati secondo un
*     criterio prefissato.
*     
*     INPUT:
*     n1,n2,n3:  dimensioni del cubo di velocita'
*     d1,d2,d3:  passo di campionamento lungo i tre assi (m)
*     xshot,yshot,zshot: posizione dello shot (m)
*     Nxs,Nys,Nzs:       posizione dello shot sulla griglia
*     S:         cubo della slowness (s/m)
*     Sx,Sy:     derivate parziali numeriche della slowness (s/m^2)
*     SOGLIAX/Y: valori della soglia di selezione dei raggetti
*                dati come frazione dei passi di campionamento
*     MODALITA:  modalita' di selezione dei raggetti
*                =1: primo arrivo ; =2: arrivo piu' energetico
*     APERTURA:  apertura del fronte 
*     INIZIO:    apertura dello shot
*     AMPMIN:    ampiezza minima sotto la quale il campo non e'
*                piu` integrato
*     
*     DESCRIZIONE VARIABILI PRINCIPALI:
*     
*     Px:        componente del vettore di slowness in direzione x  
*     Py:        componente del vettore di slowness in direzione y
*     Pz:        componente del vettore di slowness in direzione z
*     deltax:    incremento in ascissa
*     deltay:    incremento in ordinata
*     deltaPx:   incremento del vettore di slowness in direzione x
*     deltaPy:   incremento del vettore di slowness in direzione y
*     Posx:      cubo che memorizza per ogni nodo della griglia le 
*                ascisse di partenza (in campioni) dei raggetti che 
*                sono arrivati in un intorno del nodo stesso. 
*     Posy:      cubo che memorizza per ogni nodo della griglia le 
*                ordinate di partenza (in campioni) dei raggetti che 
*                sono arrivati in un intorno del nodo stesso.
*     xx:        ascissa (in campioni) del nodo nel cui intorno arriva 
*                il raggetto
*     yy:        ordinata(in campioni) del nodo nel cui intorno arriva 
*                il raggetto 
*     Nxmin:     limite inferiore in ascissa del fronte in # di campioni.
*     Nxmax:     limite superiore in ascissa del fronte in # di campioni.                 
*     Nymin:     limite inferiore in ordinata del fronte in # di campioni.
*     Nymax:     limite superiore in ordinata del fronte in # di campioni.   
*     OK1:       matrice che tiene memoria dei raggetti che si sono propagati 
*     OK2:       matrice che tiene memoria dei nodi da cui si puo propagare
*     xcorr:     ascisse dei raggetti nel piano corrente
*     ycorr:     ordinate dei raggetti nel piano corrente
*     Acorr:     ampiezza dei raggetti nel piano corrente (non ancora 
*                rigrigliata)
*     Pxcorr:    componente lungo x dei raggetti nel piano corrente (non
*                ancora rigrigliata)
*     Pycorr:    componente lungo y dei raggetti nel piano corrente (non
*                ancora rigrigliata)     
*     ANGOLO:    angolo di accettazione dei raggetti rispetto all'asse z
*                (angolo di elevazione)
*     
*     OUTPUT:
*     TT,AA,PPx,PPy,PPz:  matrici (cubi) valori calcolati
*     
*     FILE CHIAMANTI:
*     execute.f
*     
*     FILE CHIAMATI:
*     iniz.f:     inizializzazione dei cubi in uscita
*     derivxy.f:  derivazione della slowness
*     selez3d.f:  selezione dei raggetti
*     ampli3d.f:  calcolo delle ampiezze con i tubu di flusso
*     regrid3d.f: esegue la prima fase del rigrigliamento
*     tamp.f:     esegue la seconda fase del rigrigliamento
*     travel3d.f: calcolo dei traveltime
*     
*     Bonolo Ivan - Maggio 1999
*     Giuliano Pepe - febbraio 2002
*     Giancarlo Bernasconi - marzo 2002
*---------------------------------------------------------------------

      implicit none

* Variabili in input
      real*4    o1,o2,o3
      real*4    d1,d2,d3
      integer*4 n1,n2,n3
      integer*4 n3ver
      real*4    S(n1,n2,n3)
      real*4    Sx(n1,n2,n3ver),Sy(n1,n2,n3ver),S1,Sx1,Sy1

      real*4    SOGLIAX,SOGLIAY
      integer*4 MODALITA
      integer*4 APERTURA, APMAX
      real*4    ANGOLO 
      integer*4 INIZIO
      integer*4 UPDOWN
      integer*4 VERSIONE
      real*4    AMPMIN
      real*4    DAMP,dampx,dampy,dampxy

      real*4    xshot,yshot,zshot
      integer*4 Nxs,Nys,Nzs
      integer*4 is

      integer*4 TARGET
      integer*4 Ntgt, Ntgt1
      integer*4 NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ
      real*4    xtgt(Ntgt1),ytgt(Ntgt1),ztgt(Ntgt1)

      integer*4 OUT_A,OUT_T,OUT_UX,OUT_UY,OUT_UZ

      real*4    A(n1,n2,2), T(n1,n2,2)
      real*4    Px(n1,n2,2),  Py(n1,n2,2),  Pz(n1,n2,2),Px1,Py1,Pz1 
      real*4    AA(NtgtAA),   TT(NtgtTT)
      real*4    PPx(NtgtPPX),  PPy(NtgtPPY),  PPz(NtgtPPZ)
 
      real*4    xcorr(n1,n2), ycorr(n1,n2),Tcorr(n1,n2)
      real*4    Acorr(n1,n2), Pxcorr(n1,n2), Pycorr(n1,n2)

      integer*4 Posx(-1:(n1+1),-1:(n2+1),2), Posy(-1:(n1+1),-1:(n2+1),2)
      integer*4 OK1(n1,n2),OK2(n1,n2)

      integer*4 Nxmin,Nxmax,Nymin,Nymax, izt_max,Nziniz

      real*4    vettdistx(n3), vettdisty(n3), vx, vy, xcur, ycur

* Variabili locali
      real*4    xt,yt,zt,zcur,zold
      real*4    xc, yc
      integer*4 m1,m2
      real*4    u1,u2,u3
      real*4    deltaPx,deltaPy,deltaT
      integer*4 ix,iy,iz, ii
      real*4    deltax,deltay
      integer*4 xx,yy
      integer*4 verxmin,verymin,verxmax,verymax
      real*4    oldxmin,oldxmax,oldymin,oldymax
      real*4    newxmin,newxmax,newymin,newymax
      real*4    PZ2,PZ2lim
      real*4    costhlim
      real*4    pi
      integer*4 flag_trim, roi, index_vett, index_vol, cont
      PARAMETER (pi=3.14159265358979E0)

* Soglia di accettazione dei raggetti in base all'inclinazione consentita 
      costhlim=cos(ANGOLO/180.0E0*pi)
           
* Calcolo fattori di damping per le ampiezze lungo le 
* direzioni degli assi e lungo la diagonale      
      if (d2.ge.d1) then 
        dampx=damp
        dampy=1.0E0-(1.0E0-damp)*d2/d1
      else
        dampy=damp
        dampx=1.0E0-(1.0E0-damp)*d1/d2
      endif
      dampxy=1.0E0-(1.0E0-dampx)*sqrt(d1**2+d2**2)/d1
      if (dampx .lt.0.0E0)  dampx =0.0
      if (dampy .lt.0.0E0)  dampy =0.0      
      if (dampxy.lt.0.0E0)  dampxy=0.0

* INIZIALIZZAZIONE del fronte e delle variabili   
      write(*,*) ' Inizializzazione sotto sorgente...'
!Clara inizializzazione a uniforme anche se la sorgente è vicina alla superficie      
      if ((Nzs+Nziniz).GT.n3) then
      	Nziniz=n3-Nzs
      endif
      
      CALL iniz(o1,o2,o3,d1,d2,d3,n1,n2,n3,S,
     &     INIZIO,UPDOWN,
     &     xshot,yshot,zshot,Nxs,Nys,Nzs,
     &     TARGET, Ntgt, Ntgt1, xtgt, ytgt, ztgt,
     &     A,  T,  Px,  Py,  Pz,        
     &     AA, TT, PPx, PPy, PPz,
     &     OK2,is,
     &     Nxmin,Nxmax,Nymin,Nymax,Nziniz,          ! apr 2002
     &     oldxmin,oldxmax,oldymin,oldymax,
     &     verxmin,verxmax,verymin,verymax,
     &     AMPMIN,
     &     OUT_A, OUT_T, OUT_UX, OUT_UY, OUT_UZ,
     &     NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ,
     &     ANGOLO)                                  ! dec 2002
    
      write(*,*) ' ...fine inizializzazione sotto sorgente'

!Clara inizializzazione a uniforme anche se la sorgente è vicina alla superficie
         if ((Nzs+Nziniz).GE.n3) RETURN
* ##############################################################
* ##############################################################
*  Inizio CICLO PRINCIPALE di propagazione
* ##############################################################
* ##############################################################

      write(*,*) ' Inizio ciclo propagazione...'
      
      zcur = real(Nzs+Nziniz-1)*d3

      do 10 iz=Nzs+Nziniz+1,izt_max

         zold = zcur
         zcur = real(iz-1)*d3 
         
         newxmin=oldxmin
         newxmax=oldxmax
         newymin=oldymin
         newymax=oldymax

* ==============================================================
* 0. Inizializzazione strutture dati
* ==============================================================
*     Azzeramento matrici di puntamento   
         do 14 iy=-1,n2+1
            do 13 ix=-1,n1+1
                  Posx(ix,iy,1)=-1
                  Posx(ix,iy,2)=-1
                  Posy(ix,iy,1)=-1
                  Posy(ix,iy,2)=-1
 13         continue
 14      continue
              
* Preparazione matrici per calcolo piano successivo   
         do 140 iy=1,n2
            do 141 ix=1,n1

               T(ix,iy,1)  = T(ix,iy,2)
               A(ix,iy,1)  = A(ix,iy,2)
               Px(ix,iy,1) = Px(ix,iy,2)
               Py(ix,iy,1) = Py(ix,iy,2)
               Pz(ix,iy,1) = Pz(ix,iy,2)

               T(ix,iy,2)  = -2.0E0
               A(ix,iy,2)  = -2.0E0
               Px(ix,iy,2) = -2.0E0
               Py(ix,iy,2) = -2.0E0
               Pz(ix,iy,2) = -2.0E0

               OK1(ix,iy)  = OK2(ix,iy)
               OK2(ix,iy)  = 0

 141        continue 
 140     continue


* Calcolo derivate per piano se si utilizza la versione 
* che minimizza l'occupazione di memoria
         if (VERSIONE .eq. 2) then
            CALL derivxymem(n1,n2,n3,n3ver,d1,d2,S,iz,Sx,Sy)
         endif

* ==============================================================
* 1. PROPAGAZIONE
* ==============================================================
         do 20 iy=Nymin,Nymax
            do 30 ix=Nxmin,Nxmax

               if (OK1(ix,iy).ge.1) then
                  
* 1.1 Determinazione COORDINATE CORRENTI ed ESTREMI DEL FRONTE              

                  Px1 = Px(ix,iy,1)           ! apr 2002 assegnamento scalari      
                  Py1 = Py(ix,iy,1)                   
                  Pz1 = Pz(ix,iy,1)

                  S1  = S(ix,iy,iz-1)
                  if (versione.eq.1) then     ! versione veloce  
                    Sx1 = Sx(ix,iy,iz-1)
                    Sy1 = Sy(ix,iy,iz-1)
                  else                        ! versione min mem
                    Sx1 = Sx(ix,iy,1)
                    Sy1 = Sy(ix,iy,1)
                  endif
                                   
                  deltax=d3*Px1/Pz1
                  deltay=d3*Py1/Pz1

                  xc=REAL(ix-1)*d1 + deltax
                  yc=REAL(iy-1)*d2 + deltay

                  xcorr(ix,iy)=xc                           ! apr 2002
                  ycorr(ix,iy)=yc                           ! apr 2002
                  
                  if (xc.lt.newxmin) then
                     verxmin=1
                     newxmin=xc                             ! apr 2002
                  elseif (xc.gt.newxmax) then
                     verxmax=1
                     newxmax=xc                             ! apr 2002
                  endif
                  if (yc.lt.newymin) then
                     verymin=1
                     newymin=yc                             ! apr 2002
                  elseif (yc.gt.newymax) then
                     verymax=1
                     newymax=yc                             ! apr 2002
                  endif
                    
                    
* 1.2 Costruzione MATRICI DI PUNTAMENTO 

* CONTROLLO sui punti di arrivo dei raggi
                  if ( ( xc.gt.((-2.0E0)*d1)   ) .AND.
     &                 ( yc.gt.((-2.0E0)*d2)   ) .AND.
     &                 ( xc.lt.(real(n1+1)*d1) ) .AND.
     &                 ( yc.lt.(real(n2+1)*d2) ) ) then
                     
* Determinazione della CELLA a cui appartiene
* il punto di arrivo del raggio                   
                     if (xc .ge. 0.0E0) then
                        xx=INT(xc/d1)+1
                     else
                        xx=INT(xc/d1)
                     endif
                     if (yc .ge. 0.0E0) then
                        yy=INT(yc/d2)+1
                     else
                        yy=INT(yc/d2)
                     endif

* MEMORIZZAZIONE nelle matrici di puntamento        
           
                     if (Posx(xx,yy,1) .eq. -1) then
                        Posx(xx,yy,1)=ix
                        Posy(xx,yy,1)=iy
                     elseif (Posx(xx,yy,2) .eq. -1) then
                        Posx(xx,yy,2)=ix
                        Posy(xx,yy,2)=iy
                     endif

                  endif

* 1.3 Determinazione delle COMPONENTI DEL VETTORE DI SLOWNESS             

                  deltaPx=d3*Sx1*S1/Pz1
                  deltaPy=d3*Sy1*S1/Pz1                     

                  Pxcorr(ix,iy)=Px1+deltaPx
                  Pycorr(ix,iy)=Py1+deltaPy

                  deltaT=d3*(S1**2)/Pz1                       ! feb 2002
                  Tcorr(ix,iy)=T(ix,iy,1)+deltaT              ! feb 2002
                  
               endif

 30         continue
 20      continue

* ==============================================================
* 2. SELEZIONE dei raggetti che si incrociano 
* ==============================================================
         CALL selez3d(d1,d2,n1,n2,
     &        SOGLIAX,SOGLIAY,MODALITA,
     &        A, T, 
     &        xcorr, ycorr,
     &        OK1,
     &        Nxmin,Nxmax,Nymin,Nymax)

* ==============================================================
* 3. Calcolo dell'AMPIEZZA
* ==============================================================
         CALL ampli3d(n1,n2,n3,d1,d2,iz,
     &        Nxmin,Nxmax,Nymin,Nymax,
     &        xcorr,ycorr,OK1,A,Acorr,S)

* ==============================================================
* 4. Determinazione delle DIMENSIONI DEL FRONTE 
* ==============================================================
         APMAX=APERTURA
         if (verxmin.eq.1) then
            APMAX=nint(abs(oldxmin-newxmin)/d1+0.3E0) ! dec 2002        
            if (APMAX.GT.APERTURA) APMAX=APERTURA     ! apr 2002        
            Nxmin=Nxmin-APMAX                         ! apr 2002
            if (Nxmin.lt.1) then
               Nxmin=1
            endif
            oldxmin=real(Nxmin-1)*d1
            verxmin=0
         endif
         if (verxmax.eq.1) then
            APMAX=nint(abs(oldxmax-newxmax)/d1+0.3E0) ! dec 2002        
            if (APMAX.GT.APERTURA) APMAX=APERTURA     ! apr 2002  
            Nxmax=Nxmax+APMAX                         ! apr 2002
            if (Nxmax.gt.n1) then
               Nxmax=n1
            endif
            oldxmax=real(Nxmax-1)*d1 
            verxmax=0
         endif
         if (verymin.eq.1) then
            APMAX=nint(abs(oldymin-newymin)/d2+0.3E0) ! dec 2002        
            if (APMAX.GT.APERTURA) APMAX=APERTURA     ! apr 2002  
            Nymin=Nymin-APMAX                         ! apr 2002
            if (Nymin.lt.1) then
               Nymin=1
            endif
            oldymin=real(Nymin-1)*d2
            verymin=0
         endif  
         if (verymax.eq.1) then
            APMAX=nint(abs(oldymax-newymax)/d2+0.3E0) ! dec 2002        
            if (APMAX.GT.APERTURA) APMAX=APERTURA     ! apr 2002  
            Nymax=Nymax+APMAX                         ! apr 2002
            if (Nymax.gt.n2) then
               Nymax=n2
            endif   
            oldymax=real(Nymax-1)*d2
            verymax=0
         endif

*     ==============================================================   
*     5. REGRIDDING         
*     ==============================================================
         CALL regrid3d(n1,n2,d1,d2,
     &        xcorr,ycorr,Pxcorr,Pycorr,Acorr,Tcorr,
     &        OK1,OK2,Nxmin,Nxmax,Nymin,Nymax,
     &        Px,Py,A,T,Posx,Posy)

*     ==============================================================
*     6. ESTRAPOLAZIONE
*     ==============================================================
         CALL tamp(n1,n2,n3,d1,d2,
     &        dampx,dampy,dampxy,
     &        Nxmin,Nxmax,Nymin,Nymax,
     &        Nxs,Nys,Px,Py,A,OK2,T)

*     ==============================================================
*     7. Calcolo della COMPONENTE Pz:
*     ==============================================================
         do 70 iy=Nymin,Nymax 
            do 60 ix=Nxmin,Nxmax

*     controllo della pendenza dei raggi ed evanescenza

               if ( (A(ix,iy,2).gt.0.0E0) .and.
     &              (T(ix,iy,2).gt.0.0E0) .and.
     &              (OK2(ix,iy).ge.1  ) )  then       ! dec 2002 

                 PZ2 = S(ix,iy,iz)**2 - Px(ix,iy,2)**2 - Py(ix,iy,2)**2
                 PZ2lim = ( S(ix,iy,iz) * costhlim )**2

CCC 17/07/2002 Ristrutturato per funzionare su Origin 3800 con -OPT:inline_intrinsics=ON
C                 if ( PZ2.ge.PZ2lim ) then        ! PZ2>=PZ2lim
C                    Pz(ix,iy,2)=sqrt(PZ2)
C                 elseif ( PZ2.gt.0.0E0 ) then     ! 0<PZ2<PZ2lim
C                    Pz(ix,iy,2)=sqrt(PZ2)
C                    OK2(ix,iy)=0
C                 else                             ! PZ2<=0
C                    Px(ix,iy,2) =-2.0E0
C                    Py(ix,iy,2) =-2.0E0
C                    Pz(ix,iy,2) =-2.0E0
C                    T(ix,iy,2)  =-2.0E0
C                    A(ix,iy,2)  = 0.0E0
C                    OK2(ix,iy)  = 0
C                 endif
                 if( PZ2.le.0.0 ) then             ! PZ2<=0
                    Px(ix,iy,2) =-2.0E0
                    Py(ix,iy,2) =-2.0E0
                    Pz(ix,iy,2) =-2.0E0
                    T(ix,iy,2)  =-2.0E0
                    A(ix,iy,2)  = 0.0E0
                    OK2(ix,iy)  = 0
                 elseif ( PZ2.ge.PZ2lim ) then        ! PZ2>=PZ2lim
                    Pz(ix,iy,2)=sqrt(PZ2)
                 else                                 ! 0<PZ2<PZ2lim
                    Pz(ix,iy,2)=sqrt(PZ2)
                    OK2(ix,iy)=0
                 endif
              else
                 OK2(ix,iy) = 0
                 A(ix,iy,2) = 0.0E0
              endif

 60         continue
 70      continue

*     ==============================================================
*     9. Salvataggio grandezze campo
*     ==============================================================

            IF (UPDOWN .eq. 2) THEN
                index_vett=iz-Nzs+1
            ELSE
                index_vett=iz
            ENDIF

*     Salvataggio sui nodi del modello di velocita'
         
         IF (TARGET .EQ. 1) THEN

!********** Salvataggio del piano corrente nei cubi di output
!============================================================
!Clara7 uscita in coseni direttori e non in slowness
            do 110 iy=1,n2
               do 120 ix=1,n1
               
!******************* Verifica se il punto e' nella regione di interesse

!======================================================================    
                   xcur = (ix-1)*d1
                   ycur = (iy-1)*d2
                   if ( iz .gt. n3) write(6,*) 'ERRORE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                   vx = -1.0
                   IF ( vettdistx(n3) .NE. -1) vx = vettdistx(n3)*d1
                   vy = -1.0
                   IF ( vettdisty(n3) .NE. -1) vy = vettdisty(n3)*d2                   
                   CALL ROIFDG( xshot, yshot, xcur, ycur, vx, vy, roi)
                   IF (roi .EQ. 1) cont=cont+1
                   
                   vx = -1.0
                   IF ( vettdistx(index_vett) .NE. -1) vx = vettdistx(index_vett)*d1
                   vy = -1.0
                   IF ( vettdisty(index_vett) .NE. -1) vy = vettdisty(index_vett)*d2

                   CALL ROIFDG( xshot, yshot, xcur, ycur, vx, vy, roi)
                   
                   ii = ix +(iy-1)*n1+(iz-1)*n1*n2
                   IF ((roi .EQ. 1) .AND. (OUT_T.EQ.1)) TT(ii) = T(ix,iy,2)
                   IF ((roi .EQ. 1) .AND. (OUT_A.EQ.1)) AA(ii) = A(ix,iy,2)
                   IF ((roi .EQ. 1) .AND. 
     &                 ((OUT_UX.EQ.1).OR.(OUT_UY.EQ.1).OR.(OUT_UZ.EQ.1))) THEN
                      PPx(ii) = Px(ix,iy,2)
                      IF (PPx(ii).NE.-2.0)  PPx(ii) = PPx(ii)/S(ix,iy,iz)                  
                      PPy(ii) = Py(ix,iy,2)
                      IF (PPy(ii).NE.-2.0)  PPy(ii) = PPy(ii)/S(ix,iy,iz)                  
                      PPz(ii) = Pz(ix,iy,2)
                      IF (PPz(ii).NE.-2.0)  PPz(ii) = PPz(ii)/S(ix,iy,iz)
                   ENDIF
                  
 120           continue
 110        continue

         ENDIF

*     Salvataggio sui punti di un target

         IF (TARGET .EQ. 2) THEN
         
*           zold = real(iz-2)*d3
*           zcur = real(iz-1)*d3

            flag_trim = 0

            do 130 ii=1,Ntgt

               xt = xtgt(ii)-o1
               yt = ytgt(ii)-o2
               zt = ztgt(ii)-o3                
               IF (UPDOWN .eq. 2) zt=real(n3-1)*d3-zt
               
               IF ((zt.GT.zold).and.(zt.LE.zcur)) THEN

                  m1=1+INT(xt/d1)
                  m2=1+INT(yt/d2)

                  if ((m1 .ge. 1 ) .AND. (m2 .ge. 1 ) .AND. 
     &                (m1 .le. n1) .AND. (m2 .le. n2))  then 
                    
!******************* Verifica se il punto e' nella regione di interesse
!======================================================================    
                     vx = -1.0
                     IF ( vettdistx(index_vett) .NE. -1) vx = vettdistx(index_vett)*d1
                     vy = -1.0
                     IF ( vettdisty(index_vett) .NE. -1) vy = vettdisty(index_vett)*d2
                     CALL ROIFDG( xshot, yshot, xt, yt, vx, vy, roi)
                     
                     IF ( roi .EQ. 1) THEN
                     
                        if (m1 .eq. n1) m1=n1-1
                        if (m2 .eq. n2) m2=n2-1

                        u1 = abs( xt - real(m1-1)*d1 ) / d1
                        u2 = abs( yt - real(m2-1)*d2 ) / d2
                        u3 = abs( zt - zold ) / d3

                        CALL interp_tgt(ii,n1, n2, m1, m2, u1, u2, u3, 
     &                       A, T, Px, Py, Pz,
     &                       AA, TT, PPx, PPy, PPz,
     &                       AMPMIN,
     &                       OUT_A, OUT_T, OUT_UX, OUT_UY, OUT_UZ, 1)
     
!                     ELSE
!                     	
!                     	IF (OUT_A.EQ.1) AA(ii) = -2.0
!                     	IF (OUT_T.EQ.1) TT(ii) = -2.0
!                     	IF ((OUT_UX.EQ.1).OR.(OUT_UY.EQ.1).OR.(OUT_UZ.EQ.1)) THEN
!                     	   PPx(ii) = -2.0
!                     	   PPy(ii) = -2.0
!                     	   PPz(ii) = -2.0
!                     	ENDIF

                     ENDIF ! IF (roi .EQ. 1)
                  
                  endif

               ENDIF

 130        continue
            flag_trim = 0

         ENDIF

*     ==============================================================

 10   continue
 

 
      write(*,*) ' ...fine ciclo propagazione...'

*     ##############################################################
*     ##############################################################
*     FINE CICLO PRINCIPALE DI PROPAGAZIONE
*     ##############################################################
*     ##############################################################
 
      RETURN
      END


!=============================================================================================
!=============================================================================================
!=============================================================================================



       subroutine ROIFDG( xs, ys, xp, yp, vettdistx, vettdisty, roi)

!      Questa subroutine stabilisce se un punto e' nella regione di interesse.
!
!      INPUT
!      xs, ys = coordinate x,y della sorgente
!      xp, yp = coordinate x,y del punto da esaminare
!      vettdistx = semiampiezza lungo x della regione di interesse.
!                  Il valore -1.0 indica che la roi e' illimitata lungo x
!      vettdisty = semiampiezza lungo y della regione di interesse
!                  Il valore -1.0 indica che la roi e' illimitata lungo y
!
!      OUTPUT
!      roi = 1 se il punto e' nella regione di interesse, 0 altrimenti

       implicit none
       
       real*4    xs, ys, xp, yp, vettdistx, vettdisty
       integer*4 roi
       
       
       
       roi = 1

       if ( vettdistx .ne. -1) then
       	  if ( abs( xs -xp) .GT. vettdistx) then
       	     roi = 0
       	  endif
       endif
       
       if ( vettdisty .ne. -1) then
       	  if ( abs( ys -yp) .GT. vettdisty) then
       	     roi = 0
       	  endif
       endif
        
       end                   