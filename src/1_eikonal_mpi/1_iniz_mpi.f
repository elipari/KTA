      subroutine iniz(o1,o2,o3,d1,d2,d3,n1,n2,n3,S,
     &           INIZIO,UPDOWN,
     &           xshot,yshot,zshot,Nxs,Nys,Nzs,
     &           TARGET, Ntgt, Ntgt1, xtgt, ytgt, ztgt,
     &           A,  T,  Px,  Py,  Pz,        
     &           AA, TT, PPx, PPy, PPz,
     &           OK2,is,
     &           Nxmin,Nxmax,Nymin,Nymax,Nziniz,
     &           xmin,xmax,ymin,ymax,
     &           verxmin,verxmax,verymin,verymax,
     &           AMPMIN,
     &           OUT_A, OUT_T, OUT_UX, OUT_UY, OUT_UZ,
     &           NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ,
     &           ANGOLO)                             ! dec 2002
     
*---------------------------------------------------------------------
*
* Subroutine che esegue l'inizializzazione dei cubi in uscita.
* Viene inizializzata una zona molto piccola nell'intorno dello shot
* sapendo che poi il campo si espandera' durante la propagazione.
*
* INPUT:
* n1,n2,n3:     dimensioni del modello
* d1,d2,d3:     passi di campionamento (m)
* INIZIO:       INIZIO dello shot
*               =1 : INIZIO a 45 gradi
*               =2 : INIZIO a 60 gradi circa
*               Si puo' aumentare (sempre con valori interi) ma la
*               soluzione nelle zone ad alto offset e' povera.
* xshot,yshot,
* zshot         : coordinate dello shot (m)
* Nxs,Nys,Nzs   : posizione dello shot nella griglia
* S             : cubo della slowness (s/m)
*
* OUTPUT:
* T,A,Px,Py     : cubi inizializzati
* OK2           : matrice che tiene traccia dei raggetti da propagare
* Nxmin,Nxmax,
* Nymin,Nymax   : estremi del fronte da propagare (sul reticolo)
* xmin,xmax,
* ymin,ymax     : estremi del fronte (coordinate reali (m))
* ver*          : variabili booleane per verificare il comportamento 
*                 del fronte
*
* FILE CHIAMANTI:
* iconal3d.f
*
* jan 1999 - Bonolo I.
* mar 2002 - Bernasconi G.
* dec 2002
*---------------------------------------------------------------------

      implicit none
      real*4    o1,o2,o3
      real*4    d1,d2,d3
      integer*4 n1,n2,n3
      real*4    S(n1,n2,n3)

      integer*4 INIZIO
      integer*4 UPDOWN

      real*4    xshot,yshot,zshot
      integer*4 Nxs,Nys,Nzs,Nziniz
      integer*4 ox, oy, ex, ey

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
      integer*4 OUT_A,OUT_T,OUT_UX,OUT_UY,OUT_UZ
 
      integer*4 OK2(n1,n2)
      integer*4 is

      integer*4 Nxmin,Nxmax,Nymin,Nymax
      real*4    xmin,xmax,ymin,ymax
      integer*4 verxmin,verymin,verxmax,verymax
      real*4  AMPMIN, ANGOLO

* Variabili locali
      integer*4 ix,iy,iz,ii,index
      integer*4 m1,m2
      real*4  Sxs,Sys,Szs
      real*4  xt,yt,zt,zcur,zold
      real*4  u1,u2,u3
      real*4  dl,dx,dy,dz
      real*4  Sshot,Scurr,Smean
      real*4  pi, AMP0, hh1

      PARAMETER (pi = 3.14159265358979)
      
      AMP0 = 1.0E0


* ==============================================================
* 0. INIZIALIZZAZIONE delle variabili
* ==============================================================
      do 10 iz=1,2
        do 20 iy=1,n2
          do 30 ix=1,n1
            T(ix,iy,iz)  = -2.0E0
            A(ix,iy,iz)  = -2.0E0
            Px(ix,iy,iz) = -2.0E0
            Py(ix,iy,iz) = -2.0E0
            Pz(ix,iy,iz) = -2.0E0
30        continue
CCC 17/07/2002 Errore --> out-of-bound
CCC          OK2(ix,iy)=0
20      continue
10    continue

CCC 17/07/2002 Correzione out-of-bound
        do 21 iy=1,n2
          do 31 ix=1,n1
             OK2(ix,iy)=0
31        continue
21      continue

* ==============================================================


* ==============================================================
* 1. Controllo uniformita` del modello nell'intorno dello shot
* ==============================================================
      
      if ((Nxs-1.ge.1).AND.(Nxs+1.le.n1).AND.
     &    (Nys-1.ge.1).AND.(Nys+1.le.n2).AND.(Nzs+1.le.n3)) then
            Sxs=(S(Nxs+1,Nys,Nzs)-S(Nxs-1,Nys,Nzs))/(2.0*d1)
            Sys=(S(Nxs,Nys+1,Nzs)-S(Nxs,Nys-1,Nzs))/(2.0*d2)
            Szs=(S(Nxs,Nys,Nzs+1)-S(Nxs,Nys,Nzs))/d3

            if ((Sxs.gt.2.0).or.(Sys.gt.2.0).or.(Szs.gt.2.0)) then
              WRITE(6,*) ' *** WARNING: shot numero ',is
              WRITE(6,*) 'La zona intorno allo shot ha forti gradienti ***'
            endif
      endif
* ==============================================================


* ==============================================================
* 2.0 Inizializzazione estremi del fronte
* ==============================================================
      ox = Nxs-INT(INIZIO/2)
      oy = Nys-INT(INIZIO/2)
      ex = Nxs+INT(INIZIO/2)
      ey = Nys+INT(INIZIO/2)

      if (ox .lt. 1)  ox = 1
      if (oy .lt. 1)  oy = 1
      if (ex .gt. n1) ex = n1
      if (ey .gt. n2) ey = n2

* 2.1 Valore della slowness nell'intorno dello shot
      Sshot=S(Nxs,Nys,Nzs)

* ==============================================================


* ==============================================================
* 3. INIZIALIZZAZIONE analitica CON DEI RAGGI RETTILINEI  
*    dei primi piani nell'intorno dello shot
* ==============================================================

* 3.0 Ciclo sui primi piani sotto la sorgente

       zcur = real(Nzs-2)*d3          ! dec 2002
C      zcur = real(Nzs-Nziniz)*d3

      do 140 iz=Nzs,Nzs+Nziniz

        zold = zcur
        zcur = real(iz-1)*d3          ! dec 2002       
C       zcur = real(iz-Nziniz+1)*d3
 

        do 60 iy=oy,ey 
          do 70 ix=ox,ex

* 3.1 Salvataggio valori precedenti e azzeramento
           
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
            OK2(ix,iy)  = 0

* 3.2 Calcolo distanze

            dx=REAL(ix-1)*d1-xshot
            dy=REAL(iy-1)*d2-yshot
            dz=REAL(iz-1)*d3-zshot

            dl=sqrt(dx**2+dy**2+dz**2)
            
* 3.3 Calcolo valori del campo

            Scurr = S(ix,iy,iz)
            Smean = (Sshot+Scurr)/2.0E0

            T(ix,iy,2) = Smean*dl
	    if (dl.le.0.001E0) then 
               A(ix,iy,2)  = AMP0
               Px(ix,iy,2) = 0.0E0
               Py(ix,iy,2) = 0.0E0
               Pz(ix,iy,2) = Sshot
            else
               if (dl.le.1.0E0) then
                 A(ix,iy,2)= AMP0
               else
                 A(ix,iy,2)= AMP0/dl
               endif
               Px(ix,iy,2) = Smean*dx/dl
               Py(ix,iy,2) = Smean*dy/dl
               Pz(ix,iy,2) = Smean*dz/dl
            endif

!CLARA inizializzazione anche del piano dove c'è la sorgente
            if (iz.eq.Nzs) then

              T(ix,iy,1) = Smean*dl
	      if (dl.le.0.001E0) then 
                A(ix,iy,1)  = AMP0
                Px(ix,iy,1) = 0.0E0
                Py(ix,iy,1) = 0.0E0
                Pz(ix,iy,1) = Sshot
              else
                if (dl.le.1.0E0) then
                  A(ix,iy,1)= AMP0
                else
                  A(ix,iy,1)= AMP0/dl
                endif
                Px(ix,iy,1) = Smean*dx/dl
                Py(ix,iy,1) = Smean*dy/dl
                Pz(ix,iy,1) = Smean*dz/dl
              endif

            endif

            if (TARGET .EQ. 1) THEN
              index = ix+(iy-1)*N1+(iz-1)*N1*N2
              IF (OUT_T.EQ.1) TT(index) = Smean*dl
	      if (dl.le.0.001E0) then                 ! lug 2002
                IF (OUT_A.EQ.1)  AA(index)  = AMP0
                IF ((OUT_UX.EQ.1).OR.(OUT_UY.EQ.1).OR.(OUT_UZ.EQ.1)) THEN
                   PPx(index) = 0.0E0
                   PPy(index) = 0.0E0
                   PPz(index) = Sshot
                endif
                
              else
	        if (dl.le.1.0E0) then
                   IF (OUT_A.EQ.1) AA(index)= AMP0
                else
                   IF (OUT_A.EQ.1) AA(index)= AMP0/dl
                endif
                IF ((OUT_UX.EQ.1).OR.(OUT_UY.EQ.1).OR.(OUT_UZ.EQ.1)) THEN
                   PPx(index) = Smean*dx/dl
                   PPy(index) = Smean*dy/dl
                   PPz(index) = Smean*dz/dl
                endif
              endif
            endif

C          OK2(ix,iy)=5         ! commentata dec 2002

70        continue
60      continue

* 3.4. Salvataggio grandezze del campo

       IF (TARGET .EQ. 2) THEN
         
*         zold = real(iz-2)*d3
*         zcur = real(iz-1)*d3

          do 130 ii=1,Ntgt

             xt = xtgt(ii)-o1
             yt = ytgt(ii)-o2
             zt = ztgt(ii)-o3                
             if (UPDOWN .eq. 2) zt=real(n3-1)*d3-zt
               
             IF (((zt.GT.zold).and.(zt.LE.zcur)).or.(n3.eq.Nzs)) THEN

                m1=1+INT(xt/d1)
                m2=1+INT(yt/d2)

                if ((m1 .ge. 1 ) .AND. (m2 .ge. 1 ) .AND. 
     &              (m1 .le. n1) .AND. (m2 .le. n2))  then 

                   if (m1 .eq. n1) m1=n1-1
                   if (m2 .eq. n2) m2=n2-1

                   u1 = abs( xt - real(m1-1)*d1 ) / d1
                   u2 = abs( yt - real(m2-1)*d2 ) / d2
                   u3 = abs( zt - zold ) / d3
            
                   CALL interp_tgt(ii,n1, n2, m1, m2, u1, u2, u3, 
     &                  A, T, Px, Py, Pz,
     &                  AA, TT, PPx, PPy, PPz,
     &                  AMPMIN,
     &                  OUT_A, OUT_T, OUT_UX, OUT_UY, OUT_UZ, Sshot)

                endif

             ENDIF

 130      continue

        ENDIF

* Fine ciclo sui piani sotto la sorgente
140   continue

* ==============================================================

* ==============================================================
* 4. Settaggio delle variabili relative all'ESTENSIONE FRONTE
* ==============================================================
      hh1= real(Nziniz+1)*d3*tan(ANGOLO*pi/180.E0) ! dec 2002
      ox = Nxs-min0(INT(hh1/d1),int(INIZIO/2))     ! dec 2002
      oy = Nys-min0(INT(hh1/d2),int(INIZIO/2))     ! dec 2002
      ex = Nxs+min0(INT(hh1/d1),int(INIZIO/2))     ! dec 2002
      ey = Nys+min0(INT(hh1/d2),int(INIZIO/2))     ! dec 2002

      if (ox .lt. 1)  ox = 1                       ! dec 2002
      if (oy .lt. 1)  oy = 1                       ! dec 2002
      if (ex .gt. n1) ex = n1                      ! dec 2002
      if (ey .gt. n2) ey = n2                      ! dec 2002

      do 100 iy=oy,ey                              ! dec 2002
      do 100 ix=ox,ex                              ! dec 2002
            OK2(ix,iy)=5                           ! dec 2002
 100  continue                                     ! dec 2002

      Nxmin=ox
      Nxmax=ex
      Nymin=oy
      Nymax=ey

      xmin=REAL(Nxmin-1)*d1
      xmax=REAL(Nxmax-1)*d1
      ymin=REAL(Nymin-1)*d2
      ymax=REAL(Nymax-1)*d2

      verxmin=0 
      verxmax=0
      verymin=0
      verymax=0
* ==============================================================
 
      RETURN
      END
