      subroutine regrid3d(n1,n2,d1,d2,
     &     xcorr,ycorr,Pxcorr,Pycorr,Acorr,Tcorr,
     &     OK1,OK2,Nxmin,Nxmax,Nymin,Nymax,
     &     Px,Py,A,T,Posx,Posy)

*---------------------------------------------------------------------
*     
*     Subroutine che esegue il rigrigliamento dei dati ottenuti
*     con un passo di integrazione alle differenze finite sulla
*     originale griglia del modello: prima fase.
*     In questa fase si rigriglia sulla dimensione del fronte precedente.
*     Viene  eseguita una interpolazione dei valori ottenuti propagando
*     il campo con un metodo bilineare che ottimizza i tempi
*     di calcolo pur mantenendo la stabilita` dell'algoritmo.
*     
*     INPUT:
*     n1,n2:     dimensioni del modello
*     d1,d2:     passi di campionamento (m)
*     x,y:       coordinate correnti (m)
*     Pxcorr:    componenti Px correnti
*     Pycorr:    componenti Py correnti
*     Acorr:     ampiezze correnti
*     OK1:        matrice che tiene traccia dei raggetti da propagare
*     Nxmin,Nxmax,Nymin,Nymax:
*     definizione del fronte su cui rigrigliare
*     Px,Py:     cubi delle componenti del vettore slowness da aggiornare
*     A:         cubo delle ampiezze da aggiornare
*     iz:        quota corrente di integrazione
*     
*     FILE CHIAMANTI:
*     iconal3d.f
*     
*     Bonolo Ivan   - Maggio 1999
*     Bernasconi G. - Aprile 2002
*---------------------------------------------------------------------

      implicit none

*     Variabili in input
      integer*4 n1,n2
      real*4  d1,d2
      
      real*4  xcorr(n1,n2),ycorr(n1,n2)
      real*4  Pxcorr(n1,n2),Pycorr(n1,n2)
      real*4  Acorr(n1,n2),Tcorr(n1,n2)

      integer*4 OK1(n1,n2),OK2(n1,n2)
      integer*4 Nxmin,Nxmax,Nymin,Nymax
      integer*4 Posx( -1 : (n1+1) , -1 : (n2+1) , 2 ), Posx1
      integer*4 Posy( -1 : (n1+1) , -1 : (n2+1) , 2 ), Posy1 
      real*4  Px(n1,n2,2),Py(n1,n2,2)
      real*4  A(n1,n2,2),T(n1,n2,2)
      
*     variabili locali
      integer*4 ix,iy,idx,idy 
      integer*4 locx(4),locy(4)
      integer*4 p1x, p1y, p2x, p2y
      integer*4 qcont  
      integer*4 k1, k2, k3
      integer*4 ax(4,9), ay(4,9)    
      real*4    dist,u1,u2,x0,y0
      integer*4 N

*     parametri
      real*4    DMIN1,DMIN2

* --------------------------------------------------------------------      
*     INIZIALIZZAZIONI

      DMIN1 = AMIN1(d1,d2)*0.002E0   ! distanza per pto vicino
      DMIN2 = AMIN1(d1,d2)*0.05E0    ! distanza per vicinanza asse

* Inizializzazione vettori contenenti la sequenza delle celle in
* cui cercare i punti più vicini
* ax(k1,k2) : k1 indica il quadrante relativo al nodo da interpolare
* k2 indica la cella in cui cercare il punto   
*
* Ascisse ed ordinate relative delle celle che 
* circondano il nodo di griglia segnato in centro
*
*                     ^ y
*                     |
*   |-----|-----|-----|-----|-----|-----|
*   |-3,2 |-2,2 |-1,2 | 0,2 | 1,2 | 2,2 |
*   |-----|-----|-----|-----|-----|-----|
*   |-3,1 |-2,1 |-1,1 | 0,1 | 1,1 | 2,1 |
*   |-----|-----|-----|-----|-----|-----|
*   |-3,0 |-2,0 |-1,0 | 0,0 | 1,0 | 2,0 |   x
* --------------------o---------------------> 
*   |-3,-1|-2,-1|-1,-1| 0,-1| 1,-1| 2,-1|
*   |-----|-----|-----|-----|-----|-----|
*   |-3,-2|-2,-2|-1,-2| 0,-2| 1,-2| 2,-2|
*   |-----|-----|-----|-----|-----|-----|
*   |-3,-3|-2,-3|-1,-3| 0,-3| 1,-3| 2,-3|  
*   |-----|-----|-----|-----|-----|-----|
*
*
*   Quadranti:
*          
*      ^
*    2 | 1
*   ------->
*    3 | 4
*
*
      DATA(ax(1,N),N=1,9) /
     &	  0, 1, 0, 1, 2, 0, 2, 1, 2/

      DATA(ay(1,N),N=1,9) /
     &	  0, 0, 1, 1, 0, 2, 1, 2, 2/

      DATA(ax(2,N),N=1,9) /
     &	  -1, -1, -2, -2, -1, -3, -2, -3, -3/

      DATA(ay(2,N),N=1,9) /
     &	  0, 1, 0, 1, 2, 0, 2, 1, 2/
     
      DATA(ax(3,N),N=1,9) /
     &	  -1, -2, -1, -2, -3, -1, -3, -2, -3/

      DATA(ay(3,N),N=1,9) /
     &	  -1, -1, -2, -2, -1, -3, -2, -3, -3/
     
      DATA(ax(4,N),N=1,9) /
     &	  0, 0, 1, 1, 0, 2, 1, 2, 2/

      DATA(ay(4,N),N=1,9) /
     &	  -1, -2, -1, -2, -3, -1, -3, -2, -3/


* RICERCA dei punti di arrivo dei raggi x il regridding
      do 3 iy=Nymin,Nymax
         do 4 ix=Nxmin,Nxmax

            x0= real(ix-1)*d1
            y0= real(iy-1)*d2
            
*           if (OK1(ix,iy) .ge. 1) then
               
* Inizializzazione dei vettori di memoria delle posizione dei 
* quattro nodi di partenza dei raggi usati nel regridding  
            locx(1)=-1  
            locx(2)=-1  
            locx(3)=-1  
            locx(4)=-1  

            qcont=0               ! contatore quadranti riempiti
            
            do 5 k1=1,4           ! ricerca nei 4 quadranti
              do 6 k2=1,9         ! ricerca nei settori di ogni quadrante

                idx=ix+ax(k1,k2)
                idy=iy+ay(k1,k2)

                if ((idx.ge.-1).AND.(idx.le.(n1+1)).AND.
     &              (idy.ge.-1).AND.(idy.le.(n2+1))) then

                  do 7 k3=1,2

                    Posx1=Posx(idx,idy,k3)
                    Posy1=Posy(idx,idy,k3)

                    if (Posx1.ne.-1) then 
                      if (OK1(Posx1,Posy1).ge.1) then
                        if (k2.eq.1) then      ! verifica punto vicino
                           dist = sqrt( (xcorr(Posx1,Posy1)-x0)**2
     &                                 +(ycorr(Posx1,Posy1)-y0)**2) 
                           if (dist.le.DMIN1) then
*****************************  RICERCA  RIUSCITA - punto vicino
                               A(ix,iy,2)  = Acorr(Posx1,Posy1)
                               Px(ix,iy,2) = Pxcorr(Posx1,Posy1)
                               Py(ix,iy,2) = Pycorr(Posx1,Posy1)
                               T(ix, iy,2) = Tcorr(Posx1,Posy1) 
                               OK2(ix,iy)  = 7
                               goto 1000            
                           endif
                        endif
                        locx(k1)=Posx1
                        locy(k1)=Posy1
                        qcont=qcont+1
                        goto 5                 ! cambia quadrante
                      endif
                    endif
                    
 7                continue
 
                endif

 6            continue
 5          continue

******************************************************************            
* Interpolazione con 4 punti 

            if (qcont.eq.4) then
              CALL interpol(
     &           xcorr(locx(1),locy(1)), xcorr(locx(2),locy(2)),
     &           xcorr(locx(3),locy(3)), xcorr(locx(4),locy(4)),
     &           ycorr(locx(1),locy(1)), ycorr(locx(2),locy(2)),
     &           ycorr(locx(3),locy(3)), ycorr(locx(4),locy(4)),
     &           Pxcorr(locx(1),locy(1)),Pxcorr(locx(2),locy(2)),
     &           Pxcorr(locx(3),locy(3)),Pxcorr(locx(4),locy(4)),
     &           Pycorr(locx(1),locy(1)),Pycorr(locx(2),locy(2)),
     &           Pycorr(locx(3),locy(3)),Pycorr(locx(4),locy(4)),
     &           Acorr(locx(1),locy(1)), Acorr(locx(2),locy(2)),
     &           Acorr(locx(3),locy(3)), Acorr(locx(4),locy(4)),
     &           Tcorr(locx(1),locy(1)), Tcorr(locx(2),locy(2)),
     &           Tcorr(locx(3),locy(3)), Tcorr(locx(4),locy(4)),
     &           x0,y0,Px(ix,iy,2),Py(ix,iy,2),A(ix,iy,2),T(ix,iy,2))

              if (A(ix,iy,2) .gt. 0.0E0) then    ! mar 2002
                 OK2(ix,iy) = 8
                 goto 1000 
              endif
            endif

C==================================================
C==================================================
C==================================================
C==================================================
            goto 1000
C==================================================
C==================================================
C==================================================
C==================================================

******************************************************************            
* Check esistenza punti allineati

            if (qcont.gt.1) then

* Check esistenza due punti allineati lungo asse y
              p1x = -1
              p1y = -1
              p2x = -1
              p2y = -1

              if (locx(1) .ne. -1) then
                if ( ABS(xcorr(locx(1),locy(1))-x0) .le. DMIN2 ) then
                  p1x = locx(1)
                  p1y = locy(1)
                endif
              endif
              if ((locx(2) .ne. -1) .AND. (p1x .eq. -1)) then
                if ( ABS(xcorr(locx(2),locy(2))-x0) .le. DMIN2 ) then
                  p1x = locx(2)
                  p1y = locy(2)
                endif
              endif

              if (locx(3) .ne. -1) then
                if ( ABS(xcorr(locx(3),locy(3))-x0) .le. DMIN2 ) then
                  p2x = locx(3)
                  p2y = locy(3)
                endif
              endif
              if ((locx(4) .ne. -1) .AND. (p2x .eq. -1)) then
                if ( ABS(xcorr(locx(4),locy(4))-x0) .le. DMIN2 ) then
                  p2x = locx(4)
                  p2y = locy(4)
                endif
              endif

              if ((p1x .ne. -1) .and. (p2x .ne. -1)) then
************    RICERCA RIUSCITA - 2 punti allineati lungo asse y
                if (p1y.ne.p2y) then
                  u1 = abs(p1y-y0)/abs(p1y-p2y)
                  u2 = 1.0E0 - u1
                  A(ix,iy,2)  = Acorr(p1x,p1y) *u2 + Acorr(p2x,p2y) *u1
                  Px(ix,iy,2) = Pxcorr(p1x,p1y)*u2 + Pxcorr(p2x,p2y)*u1
                  Py(ix,iy,2) = Pycorr(p1x,p1y)*u2 + Pycorr(p2x,p2y)*u1
                  T(ix,iy,2)  = Tcorr(p1x,p1y) *u2 + Tcorr(p2x,p2y) *u1
                  OK2(ix,iy)  = 10
                  goto 1000
                endif
              endif

* check esistenza due punti allineati lungo asse x
              p1x = -1
              p1y = -1
              p2x = -1
              p2y = -1

              if (locx(1) .ne. -1) then
                if ( ABS(ycorr(locx(1),locy(1))-y0) .le. DMIN2 ) then
                  p1x = locx(1)
                  p1y = locy(1)
                endif
              endif
              if ((locx(4) .ne. -1) .AND. (p1x .eq. -1)) then
                if ( ABS(ycorr(locx(4),locy(4))-y0) .le. DMIN2 ) then
                  p1x = locx(4)
                  p1y = locy(4)
                endif
              endif

              if (locx(3) .ne. -1) then
                if ( ABS(ycorr(locx(3),locy(3))-y0) .le. DMIN2 ) then
                  p2x = locx(3)
                  p2y = locy(3)
                endif
              endif
              if ((locx(2) .ne. -1) .AND. (p2x .eq. -1)) then
                if ( ABS(ycorr(locx(2),locy(2))-y0) .le. DMIN2 ) then
                  p2x = locx(2)
                  p2y = locy(2)
                endif
              endif

              if ((p1x .ne. -1) .and. (p2x .ne. -1)) then
***********     RICERCA RIUSCITA - 2 punti allineati lungo asse x
                if (p1x.ne.p2x) then
                  u1 = abs(p1x-x0)/abs(p1x-p2x)
                  u2 = 1.0E0 - u1
                  A(ix,iy,2)  = Acorr(p1x,p1y) *u2 + Acorr(p2x,p2y) *u1
                  Px(ix,iy,2) = Pxcorr(p1x,p1y)*u2 + Pxcorr(p2x,p2y)*u1
                  Py(ix,iy,2) = Pycorr(p1x,p1y)*u2 + Pycorr(p2x,p2y)*u1
                  T(ix,iy,2)  = Tcorr(p1x,p1y) *u2 + Tcorr(p2x,p2y) *u1
                  OK2(ix,iy)  = 10
                  goto 1000
                endif
              endif
            endif

 1000       continue        
            
*           endif

 4       continue
 3    continue
      
      RETURN
      END 
      
*---------------------------------------------------------------------
*
*---------------------------------------------------------------------      
       subroutine interpol(x1,x2,x3,x4,y1,y2,y3,y4,
     &     Px1,Px2,Px3,Px4,Py1,Py2,Py3,Py4,
     &     A1,A2,A3,A4,T1,T2,T3,T4,
     &     x0,y0,Px0,Py0,A0,T0)
          
* Questa subroutine effettua la rigrigliatura dell'ampiezza e delle 
* componenti Px e Py del vettore di slowness.
* La rigrigliatura avviene con una doppia interpolazione lineare.

*--------------------------------------------------------------------- 
      implicit none 
 
* Variabili in input
      real*4  x0,y0
      real*4  x1,x2,x3,x4
      real*4  y1,y2,y3,y4
      real*4  Px1,Px2,Px3,Px4
      real*4  Py1,Py2,Py3,Py4
      real*4  A1,A2,A3,A4
      real*4  T1,T2,T3,T4
      real*4  Px0,Py0,A0,T0
     
* variabili locali
      real*4  mP1,mP2,u1,u2
      real*4  xP1,yP1,PxP1,PyP1,AP1,TP1
      real*4  xP2,yP2,PxP2,PyP2,AP2,TP2
      real*4  eps
      parameter (eps=0.001E0)
              
* DOPPIA INTERPOLAZIONE LINEARE

**********************************************************
* FASE 1.a
*             |
*         2   |P1   1
*         o---o-----o
*             |
*    ---------0------------
*           x0,y0
*             |      
* y           |-------o 4
* ^      o----oP2   
* |      3    |
* +--> x      |


      if ((abs(x2-x1).gt.eps).and.(abs(x4-x3).gt.eps)) then

* Coefficienti angolari             
          mP1=(y2-y1)/(x2-x1)
          mP2=(y4-y3)/(x4-x3) 
             
* Coordinate dei punti P1 e P2
          xP1=x0
          xP2=x0
          yP1=y1+mP1*(xP1-x1)
          yP2=y3+mP2*(xP2-x3)

* Valori nei punti P1 e P2  
          u1=abs(xP1-x2)/abs(x2-x1)
          u2=abs(1.0E0-u1)     
          PxP1= Px1 * u1 + Px2 * u2
          PyP1= Py1 * u1 + Py2 * u2
          AP1 = A1  * u1 + A2  * u2
          TP1 = T1  * u1 + T2  * u2
             
          u1=abs(xP2-x3)/abs(x4-x3)
          u2=abs(1.0E0-u1)
          PxP2= Px4 * u1 + Px3 * u2
          PyP2= Py4 * u1 + Py3 * u2
          AP2 = A4  * u1 + A3  * u2
          TP2 = T4  * u1 + T3  * u2    

* Valori rigrigliati 
* FASE 2.a       | 
*                o P1 
*                | 
* y         -----0------  
* ^            x0,y0        
* |              |
* +-->x          o P2 
*
 
          if (abs(yP1-yP2).lt.eps) then          ! P1 e P2 coincidono
             u1=0.5E0
             u2=0.5E0              
          else
             u1=abs(y0-yP2)/abs(yP1-yP2)
             u2=abs(1.0E0-u1)
          endif
          Px0= PxP1 * u1 + PxP2 * u2
          Py0= PyP1 * u1 + PyP2 * u2
          A0 = AP1  * u1 + AP2  * u2
          T0 = TP1  * u1 + TP2  * u2   
          
**********************************************************
* FASE 1.b
*             o1
*             |\   
*           2 o \
*            /|  \ P1
*    ---P2-o--0---o--------
*        /  x0,y0  \
*      /      |     \ 
* y   o       |      o 4
* ^   3       |    
* |           |
* +--> x      |

      elseif ((abs(y4-y1).gt.eps).and.(abs(y2-y3).gt.eps)) then

* Coefficienti angolari
           
          mP1=(x4-x1)/(y4-y1)
          mP2=(x2-x3)/(y2-y3) 
             
* Coordinate dei punti P1 e P2
          yP1=y0
          yP2=y0
          xP1=x1+mp1*(yP1-y1)
          xP2=x3+mp2*(yP2-y3)

* Valori nei punti P1 e P2  
          u1=abs(yP1-y4)/abs(y4-y1)
          u2=abs(1.0E0-u1)
          PxP1= Px1 * u1 + Px4 * u2
          PyP1= Py1 * u1 + Py4 * u2
          AP1 = A1  * u1 + A4  * u2
          TP1 = T1  * u1 + T4  * u2
             
          u1=abs(yP2-y3)/abs(y2-y3)
          u2=abs(1.0E0-u1)
          PxP2= Px2 * u1 + Px3 * u2
          PyP2= Py2 * u1 + Py3 * u2
          AP2 = A2  * u1 + A3  * u2
          TP2 = T2  * u1 + T3  * u2 
             
             
* Valori rigrigliati 
* FASE 2.b        
*                  
* y             P2   |   P1
* ^         -----o---0---o------>x
* |                  x0
* +--> x
*
          if ((xP1-xP2).lt.eps) then           ! P1 e P2 coincidono
             u1=0.5E0
             u2=0.5E0              
          else
             u1=abs(x0-xP2)/abs(xP1-xP2)
             u2=abs(1.0E0-u1)
          endif
          Px0= PxP1 * u1 + PxP2 * u2
          Py0= PyP1 * u1 + PyP2 * u2
          A0 = AP1  * u1 + AP2  * u2
          T0 = TP1  * u1 + TP2  * u2   

**********************************************************

      else

         Px0 = -2.0E0
         Py0 = -2.0E0
         A0  =  0.0E0
         T0  = -2.0E0
         
      endif


      RETURN 
      END

