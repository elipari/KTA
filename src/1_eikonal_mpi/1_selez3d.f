      subroutine selez3d(d1,d2,n1,n2,
     &               SOGLIAX,SOGLIAY,MODALITA,
     &               A,  T, 
     &               x, y,
     &               Ok1,
     &               Nxmin,Nxmax,Nymin,Nymax)

*---------------------------------------------------------------------
*
* Subroutine che esegue la selezione dei raggetti che vengono
* propagati al piano inferiore. Il criterio di selezione e`
* la distanza fra le proiezioni dei punti di arrivo sul piano
* corrente. La selezione avviene in base ai valori del
* traveltime o ampiezza alla base del raggetto.
*
* INPUT:
* x:         coordinate x di arrivo dei raggetti (m)
* y:         coordinate y di arrivo dei raggetti (m)
* Ok1:       matrice che tiene traccia dei raggetti da propagare
* MODALITA:  modalita` di selezione dei raggetti
*            =1: primo arrivo ; =2: arrivo piu` energetico
* SOGLIAX/Y: valori della soglia di selezione dei raggetti
*            espresse come frazione dei passi di campionamento
* n1,n2:     dimensioni del piano
* d1,d2:     passo di campionamento lungo gli assi x ed y (m)
* T:         matrice dei traveltime
* A:         matrice delle ampiezze
* Nxmin,Nxmax,Nymin,Nymax:
*            estremi del fronte da propagare
*
* FILE CHIAMANTI:
* iconal3d.f
*
* jun 2002 - G. Bernasconi
* dec 2002
*
*      ^ y
*      |          x
*   ---o--3--5-----> 
*   |  |  |  |
*   6--1--2---
*      |  |  |
*      4------


*--------------------------------------------------------------------- 

      implicit none

      integer*4 n1,n2
      real*4    d1,d2
      real*4    SOGLIAX,SOGLIAY
      integer*4 MODALITA
      real*4    T(n1,n2,2),A(n1,n2,2) 

      real*4    x(n1,n2),y(n1,n2)

      integer*4 Ok1(n1,n2)
      integer*4 Nxmin,Nxmax,Nymin,Nymax
      integer*4 N

* Variabili locali
      integer*4 ix,iy,jx(6),jy(6)
      integer*4 ix1,iy1,k
      integer*4 incrocio
      real*4    sogx,sogy,dmin
      real*4    x1,x2,x3,x4,y1,y2,y3,y4

* DATA declaration

      DATA(jx(N),N=1,6) /
     &     0, 1, 1, 0, 2, -1 /
    
      DATA(jy(N),N=1,6) /
     &     -1, -1, 0, -2, 0, -1 /
    
          
      sogx=SOGLIAX*d1
      sogy=SOGLIAY*d2
      dmin=amin1(d1,d2)*0.3E0   ! dist. min. controllo incrocio rette parallele 
     
      do 60 iy=Nymin+1,Nymax
        y1=real(iy-1)*d2
        do 50 ix=Nxmin,Nxmax
          x1=real(ix-1)*d1

          do 40 k=1,6
          
          if (Ok1(ix,iy).ge.1) then
            x2=x(ix,iy)
            y2=y(ix,iy)
            
            ix1=ix+jx(k)
            iy1=iy+jy(k)
            
            if ( (ix1.LE.Nxmax).AND.(ix1.GE.Nxmin)
     &            .AND. (iy1.ge.Nymin)) then
               if ( Ok1(ix1,iy1).ge.1) then
                 x3=real(ix1-1)*d1
                 y3=real(iy1-1)*d2
                 x4=x(ix1,iy1)
                 y4=y(ix1,iy1)
                 if (incrocio(x1,y1,x2,y2,x3,y3,x4,y4,sogx,sogy,dmin).gt.3) then               
                   if (((T(ix1,iy1,1).lt.T(ix,iy,1)).and.(MODALITA.eq.1))
     &             .OR. ((A(ix1,iy1,1).gt.A(ix,iy,1)).and.(MODALITA.eq.2)))
     &             then	
                      Ok1(ix,iy)=0
                      goto 50          ! cambia punto....
                   else
                      Ok1(ix1,iy1)=0    
                   endif
                 endif
               endif
            endif
          endif
40        continue

50      continue
60    continue

      RETURN
      END
      
C*************************************************************************

      INTEGER*4 FUNCTION incrocio(xA1,yA1,xA2,yA2,xB1,yB1,xB2,yB2,
     &                            sogx,sogy,dmin)

C*************************************************************************
C Sono dati i segmenti A1-A2 e B1-B2, con coordinate 
C A1=[xA1,yA1], A2=[xA2,yA2], B1=[xB1,yB1], B2=[xB2,yB2]
C
C Verifica se segmenti A1-A2 e B1-B2 si incrociano 
C
C L'incrocio si ha se
C - i punti finali A2 e B2 distano meno della soglia nelle direzioni
C     x ed y (sogx e sogy rispettivamente)
C - i segmenti non sono paralleli ed il punto di incrocio si trova 
C     su entrambi i segmenti
C - i segmenti sono paralleli, distano meno di dmin, hanno verso 
C     discorde ed il punto finale di uno si "proietta" all'interno dell'altro
C - i segmenti sono paralleli, distano meno di dmin, hanno verso concorde 
C     ed uno e' "contenuto" all'interno dell'altro
C   
C Uscita:
C incrocio =0  no incrocio
C          =2  rette A1-A2 e B1-B2 non parallele, no incrocio segmenti, 
C              ma pto di incrocio rette su uno dei segmenti
C          =4  rette non parallele con incrocio segmenti
C          =3  rette parallele sotto dmin (3+2 rette orizzontali con "incrocio")
C                                         (3+3 rette non orizz.  con "incrocio")
C          =9  distanza A2 e B2 sotto soglia 
C
C  Rel: 1.0 - dec 2002 - G. Bernasconi
C       2.0 - dec 2002 - G. Bernasconi (aggiunta verifica segmenti degeneri)
C
C*************************************************************************
      
      implicit none

      real*4   sogx,sogy
      real*4   xA1,xA2,xB1,xB2,yA1,yA2,yB1,yB2

C Variabli locali
      real*4   a1,a2,b1,b2,c1,c2
      real*4   dmin,dist,eps
      real*4   delta,deltax,deltay
      real*4   k1,k2,dk2,dk1
      real*4   xA1_2,yA1_2,xA2_2,yA2_2
      real*4   xB1_1,yB1_1,xB2_1,yB2_1
      real*4   xP,yP

      incrocio=0
      eps=0.01E0
      
C------------------------------------------------------------
C Caso 1. Verifica vicinanza punti di arrivo
      
      if ((abs(xA2-xB2).le.sogx).and.(abs(yA2-yB2).le.sogy)) then
          incrocio=9   
          goto 100
      endif
          
C------------------------------------------------------------
C Coefficienti rette A1-A2 e B1-B2      
C A1-A2    a1*x+b1*y+c1 =0
C B1-B2    a2*x+b2*y+c2 =0
          
      a1=yA1-yA2
      b1=xA2-xA1
      c1=xA1*yA2-xA2*yA1
          
      a2=yB1-yB2
      b2=xB2-xB1
      c2=xB1*yB2-xB2*yB1

      k1=a1*a1+b1*b1
      k2=a2*a2+b2*b2

C------------------------------------------------------------
C Caso 2. Segmento A1-A2 degenere (A1=A2)
C         Segmento B1-B2 degenere (B1=B2)

      if ((abs(k1).le.eps).and.(abs(k2).le.eps)) then
          goto 100
      endif

C------------------------------------------------------------
C Coefficienti per verificare parallelismo rette

      delta  =  a1*b2-b1*a2
      deltax = -c1*b2+c2*b1
      deltay = -a1*c2+a2*c1

C------------------------------------------------------------
C Caso 3. Segmento A1-A2 degenere (A1=A2)
C         Segmento B1-B2 normale
C         Distanza di A1(xB1,yB1)=A2 da retta B1-B2
C         dist=abs(a2*xA1+b2*yA1+c2)/sqrt(a2^2+b2^2)

      if (abs(k1).le.eps) then              

C Distanza di A1 dalla retta B1-B2
          dk2=1.0E0/k2
          dist=abs(a2*xA1+b2*yA1+c2)*sqrt(dk2)
          if (dist .le. dmin) then
              incrocio=3
                  
C Proiezioni di A1 su B1-B2
              xA1_2=(b2*b2*xA1-a2*b2*yA1-a2*c2)*dk2    
              yA1_2=(a2*a2*yA1-a2*b2*xA1-b2*c2)*dk2

C retta B1-B2 orizzontale
              if (abs(a2).le.eps) then      

C Proiezione all'interno del segmento B1-B2
                  if     (xB2.gt.xB1) then  
                      if ((xA1_2.le.xB2).and.(xA1_2.ge.xB1)) then
                        incrocio=incrocio+2
                      endif
                  elseif (xB2.lt.xB1) then
                      if ((xA1_2.le.xB1).and.(xA1_2.ge.xB2)) then 
                        incrocio=incrocio+2 
                      endif 
                  endif 
    
C retta B1-B2 non orizzontale                
              else                    
                      
C Proiezione all'interno del segmento B1-B2
                  if     (yB2.gt.yB1) then
                      if ((yA1_2.le.yB2).and.(yA1_2.ge.yB1)) then 
                        incrocio=incrocio+3 
                      endif
                  elseif (yB2.lt.yB1) then
                      if ((yA1_2.le.yB1).and.(yA1_2.ge.yB2)) then 
                        incrocio=incrocio+3 
                      endif
                  endif              
              endif
          endif
          goto 100
      endif

C------------------------------------------------------------
C Caso 4. Segmento A1-A2 normale
C         Segmento B1-B2 degenere (B1=B2)
C         Distanza di B1(xB1,yB1)=B2 da retta A1-A2
C         dist=abs(a1*xB1+b1*yB1+c1)/sqrt(a1^2+b1^2)

      if (abs(k2).le.eps) then
          
C Distanza di B1 dalla retta A1-A2    
          dk1=1.0E0/k1
          dist=abs(a1*xB1+b1*yB1+c1)*sqrt(dk1)
          if (dist .le. dmin) then
              incrocio=3
                  
C Proiezioni di B1 su A1-A2
              xB1_1=(b1*b1*xB1-a1*b1*yB1-a1*c1)*dk1    
              yB1_1=(a1*a1*yB1-a1*b1*xB1-b1*c1)*dk1

C retta A1-A2 orizzontale
              if (abs(a1).le.eps) then      

C Proiezione all'interno del segmento A1-A2
                  if     (xA2.gt.xA1) then  
                      if ((xB1_1.le.xA2).and.(xB1_1.ge.xA1)) then
                        incrocio=incrocio+2
                      endif
                  elseif (xA2.lt.xA1) then
                      if ((xB1_1.le.xA1).and.(xB1_1.ge.xA2)) then 
                        incrocio=incrocio+2 
                      endif 
                  endif     

C retta A1-A2 non orizzontale                
              else                    
                      
C Proiezione all'interno del segmento A1-A2
                  if     (yA2.gt.yA1) then
                      if ((yB1_1.le.yA2).and.(yB1_1.ge.yA1)) then 
                        incrocio=incrocio+3 
                      endif
                  elseif (yA2.lt.yA1) then
                      if ((yB1_1.le.yA1).and.(yB1_1.ge.yA2)) then 
                        incrocio=incrocio+3 
                      endif
                  endif              
              endif
          endif
          goto 100
      endif
          
C------------------------------------------------------------
C Caso 5. Segmento A1-A2 normale
C         Segmento B1-B2 normale
C         Segmenti paralleli (delta=0)
C         Distanza di A1(xA1,yA1) da retta B1-B2
C         dist=abs(a2*xA1+b2*yA1+c2)/sqrt(a2^2+b2^2)
C
      if (abs(delta) .lt. eps) then
              
          dk2=1.0E0/k2
          dist=abs(a2*xA1+b2*yA1+c2)*sqrt(dk2)
              
C Se distanza tra rette e' minore di dmin analizza vari casi
          if (dist .le. dmin) then
              incrocio=3
                  
C Proiezioni di A1 e A2 su retta B1-B2
              xA1_2=(b2*b2*xA1-a2*b2*yA1-a2*c2)*dk2    
              yA1_2=(a2*a2*yA1-a2*b2*xA1-b2*c2)*dk2
                  
              xA2_2=(b2*b2*xA2-a2*b2*yA2-a2*c2)*dk2
              yA2_2=(a2*a2*yA2-a2*b2*xA2-b2*c2)*dk2
                  
C rette parallele orizzontali
              if (abs(a2) .le. eps) then      
      
C Verso discorde e segmenti si "vedono"
                  if ((xB2.gt.xB1) .and. (xA2_2 .lt. xA1_2)) then  
                      if ( ((xA2_2.le.xB2).and.(xA2_2.ge.xB1)) .or. 
     &                     ((xA1_2.le.xB2).and.(xA1_2.ge.xB1)) ) then 
                      incrocio=incrocio+2
                      endif
                  elseif ((xB2.lt.xB1).and.(xA2_2.gt.xA1_2))  then
                      if ( ((xA2_2.le.xB1).and.(xA2_2.ge.xB2)) .or. 
     &                     ((xA1_2.le.xB1).and.(xA1_2.ge.xB2)) ) then 
                      incrocio=incrocio+2 
                      endif 
C Verso concorde ma un raggio "supera" l'altro
                  elseif ((xB2.gt.xB1) .and. (xA2_2 .gt. xA1_2)) then  
                      if ( ((xB1.le.xA1_2).and.(xB2.ge.xA2_2)) .or. 
     &                     ((xB1.ge.xA1_2).and.(xB2.le.xA2_2)) ) then 
                      incrocio=incrocio+2 
                      endif
                  elseif ((xB2.lt.xB1) .and. (xA2_2 .lt. xA1_2)) then  
                      if ( ((xB1.le.xA1_2).and.(xB2.ge.xA2_2)) .or. 
     &                     ((xB1.ge.xA1_2).and.(xB2.le.xA2_2)) ) then 
                      incrocio=incrocio+2 
                      endif
                  endif     
                      
C rette parallele non orizzontali                
              else                    
                      
C Verso discorde e segmenti si "vedono"
                  if     ((yB2.gt.yB1).and.(yA2_2.lt.yA1_2))  then
                      if ( ((yA2_2.le.yB2).and.(yA2_2.ge.yB1)) .or. 
     &                     ((yA1_2.le.yB2).and.(yA1_2.ge.yB1)) ) then 
                      incrocio=incrocio+3 
                      endif
                  elseif ((yB2.lt.yB1).and.(yA2_2.gt.yA1_2))  then
                      if ( ((yA2_2.le.yB1).and.(yA2_2.ge.yB2)) .or. 
     &                     ((yA1_2.le.yB1).and.(yA1_2.ge.yB2)) ) then 
                      incrocio=incrocio+3 
                      endif
C Verso concorde ma un raggio "supera" l'altro
                  elseif ((yB2.gt.yB1) .and. (yA2_2 .gt. yA1_2)) then  
                      if ( ((yB1.le.yA1_2).and.(yB2.ge.yA2_2)) .or. 
     &                     ((yB1.ge.yA1_2).and.(yB2.le.yA2_2)) ) then 
                      incrocio=incrocio+3 
                      endif
                  elseif ((yB2.lt.yB1) .and. (yA2_2 .lt. yA1_2)) then  
                      if ( ((yB1.le.yA1_2).and.(yB2.ge.yA2_2)) .or. 
     &                     ((yB1.ge.yA1_2).and.(yB2.le.yA2_2)) ) then 
                      incrocio=incrocio+3 
                      endif
                  endif              
              endif
          endif
              
      else
      
C------------------------------------------------------------
C Caso 6. Segmento A1-A2 normale
C         Segmento B1-B2 normale
C         Segmenti non paralleli

C Calcola punto di intersezione e poi verifica se si trova 
C al di fuori dei segmenti, su 1 solo segmento oppure su entrambi
         
          xP=deltax/delta
          yP=deltay/delta
              
          if (xA2.gt.xA1)  then
              if ((xP.ge.xA1).and.(xP.le.xA2)) then 
              incrocio=2
              endif
          else
              if ((xP.ge.xA2).and.(xP.le.xA1)) then 
              incrocio=2 
              endif
          endif
           
          if (xB2.gt.xB1)  then
              if ((xP.ge.xB1).and.(xP.le.xB2)) then 
              incrocio=incrocio+2 
              endif
          else
              if ((xP.ge.xB2).and.(xP.le.xB1)) then 
              incrocio=incrocio+2 
              endif
          endif
              
      endif              

         
 100  continue    
      return
      end
