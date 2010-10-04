      subroutine ampli3d(n1,n2,n3,d1,d2,iz,
     &                   Nxmin,Nxmax,Nymin,Nymax,
     &                   x,y,OK1,A,Acorr,S)

*---------------------------------------------------------------------
*
* Subroutine che esegue il calcolo delle ampiezze del campo
* con il metodo dei tubi di flusso. L'ampiezza corrente di un raggio 
* e' determinata valutando la divergenza dei raggi di propagazione
* limitrofi.
*
* INPUT:
* n1,n2,n3    : dimensioni del modello
* d1,d2       : passi di campionamento (m)
* iz          : quota corrente di integrazione
* Nxmin,Nxmax,
* Nymin,Nymax : estremi del fronte  
* x,y         : coordinate correnti (m)
* OK1         : matrice che tiene traccia dei raggetti propagati
* A           : cubo delle ampiezze da aggiornare
* S           : cubo del modello di slowness
* SOGLIA      : max tra SOGLIAX e SOGLIAY, per limitare crescita ampiezze
*
* OUTPUT:
* Acorr:     ampiezze correnti
*
* FILE CHIAMANTI:
* iconal3d.f
*
* NOTE: la subroutine "area" calcola l'area di un quadrilatero
*       date le coordinate cartesiane dei suoi vertici.
*
* Bonolo Ivan - Maggio  1999
* Bernasconi Giancarlo - feb 2002
*---------------------------------------------------------------------

      implicit none

* Variabili in input
      integer*4 n1,n2,n3 
      integer*4 iz
      integer*4 Nxmin,Nxmax,Nymin,Nymax
      integer*4 OK1(n1,n2)
      real*4  d1,d2 
      real*4  A(n1,n2,2), Acorr(n1,n2)
      real*4  x(n1,n2),y(n1,n2)
      real*4  S(n1,n2,n3)

* Variabili locali
      integer*4 ix,iy
      integer*4 xx,yy
      integer*4 OK_A, Na
      real*4  SOGLIA
      real*4  Amean,Afrac,Alim
      real*4  xc,yc
      real*4  sup2
      real*4  sup1_2,sup1_4,sup1_1, sup1       ! feb 2002 
  
* INIZIO ROUTINE

      sup1_1 = d1*d2
      sup1_2 = d1*d2*2.0E0
      sup1_4 = d1*d2*4.0E0
      SOGLIA = 0.25E0             ! dec 2002
     
      Amean = 0.0E0              ! ampiezza media sul piano
      Afrac = 10.0E0             ! Afrac*Amean=ampiezza da clippare
      NA    = 0

* CALCOLO AMPIEZZA
*       TUBO DI FLUSSO A           TUBO DI FLUSSO B 
*
*             o-------o               ----o----
*            /|      /|              /    |   /
*           / | *   / |             o   * |  o
*          /  v    /  v            /|     v / \
*         o-------o                ----o----   \
*        /         \                |  |        _|
*       /           \               v  |
*     |_             _|                v
*

* ZONA INTERNA

      do 10 iy=Nymin+1,Nymax-1
        do 20 ix=Nxmin+1,Nxmax-1

          if (OK1(ix,iy).ge.1) then          ! ==== IF (1)

           OK_A=0
           xc=x(ix,iy)
           yc=y(ix,iy)

* TUBO DI FLUSSO A
           if (OK_A .eq. 0) then             ! ==== IF (2)
             if ((OK1(ix-1,iy-1).ge.1).AND.  ! ==== IF (2a)
     &           (OK1(ix+1,iy+1).ge.1).AND.
     &           (OK1(ix-1,iy+1).ge.1).AND.
     &           (OK1(ix+1,iy-1).ge.1)) then

                CALL area(x(ix-1,iy-1),y(ix-1,iy-1),
     &                    x(ix+1,iy+1),y(ix+1,iy+1),
     &                    x(ix-1,iy+1),y(ix-1,iy+1),
     &                    x(ix+1,iy-1),y(ix+1,iy-1),sup2)
                sup1=sup1_4                             ! feb 2002
                if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002

                  xx=NINT(xc/d1)+1
                  if (xx.LT.1) then
                    xx=1
                  elseif (xx.GT.n1) then
                    xx=n1
                  endif

                  yy=NINT(yc/d2)+1
                  if (yy.LT.1) then
                    yy=1
                  elseif (yy.GT.n2) then
                    yy=n2
                  endif

                  Acorr(ix,iy)=A(ix,iy,1)*
     &                sqrt(sup1/sup2*S(ix,iy,iz-1)/S(xx,yy,iz))
                  OK_A=1
                  Amean=Amean+Acorr(ix,iy)
                  Na=Na+1
                  goto 20
                endif
             endif                          ! ==== ENDIF (2a)
           endif                            ! ==== ENDIF (2)

* TUBO DI FLUSSO B
           if (OK_A .eq. 0) then            ! ==== IF (3)
             if ((OK1(ix-1,iy).ge.1).AND.   ! ==== IF (3a)
     &           (OK1(ix+1,iy).ge.1).AND.
     &           (OK1(ix,iy-1).ge.1).AND.
     &           (OK1(ix,iy+1).ge.1)) then

               CALL area(x(ix-1,iy),y(ix-1,iy),
     &                   x(ix+1,iy),y(ix+1,iy),
     &                   x(ix,iy+1),y(ix,iy+1),
     &                   x(ix,iy-1),y(ix,iy-1),sup2)
               sup1=sup1_2                             ! feb 2002
               if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002

                 xx=NINT(xc/d1)+1
                 if (xx.LT.1) then
                   xx=1
                 elseif (xx.GT.n1) then
                   xx=n1
                 endif

                 yy=NINT(yc/d2)+1
                 if (yy.LT.1) then
                   yy=1
                 elseif (yy.GT.n2) then
                   yy=n2
                 endif

                 Acorr(ix,iy)=A(ix,iy,1)*
     &               sqrt(sup1/sup2*S(ix,iy,iz-1)/S(xx,yy,iz))
                 OK_A=1
                 Amean=Amean+Acorr(ix,iy)
                 Na=Na+1
                 goto 20
               endif

             endif                          ! ==== ENDIF (3a)
           endif                            ! ==== ENDIF (3)

* EVENTUALITA' in cui non si identifica un tubo di flusso
* Considerazione della sola ATTENUAZIONE SPAZIALE LUNGO LE Z          
   
           if (OK_A .eq. 0) then            
              Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
              Amean=Amean+Acorr(ix,iy)
              Na=Na+1
           endif

         endif                          ! ========= ENDIF (1)

20      continue
10    continue            
           
* I QUATTRO ANGOLI
*      
*      SPIGOLO A   SPIGOLO C    SPIGOLO B      SPIGOLO D  
*       |                |       *---o-        - o---*
*       o---o        o---o       |   |           |   |
* y     |   |        |   |       |   |           |   |
* ^     |   |        |   |       o---o           o---o
* +> x  *---o-      -o---*       |                   |

* SPIGOLO A
      ix=Nxmin
      iy=Nymin
      if (OK1(ix,iy).ge.1) then
        OK_A=0
        xc=x(ix,iy)
        yc=y(ix,iy)
        if ((OK1(ix+1,iy+1).ge.1).AND.
     &      (OK1(ix+1,iy).ge.1).AND.
     &      (OK1(ix,iy+1).ge.1)) then
          CALL area(x(ix,iy),y(ix,iy),
     &              x(ix+1,iy+1),y(ix+1,iy+1),
     &              x(ix,iy+1),y(ix,iy+1),
     &              x(ix+1,iy),y(ix+1,iy),sup2)
          sup1=sup1_1                             ! feb 2002 
          if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

            xx=NINT(xc/d1)+1
            if (xx.LT.1) then
              xx=1
            elseif (xx.GT.n1) then
              xx=n1
            endif

            yy=NINT(yc/d2)+1
            if (yy.LT.1) then
              yy=1
            elseif (yy.GT.n2) then
              yy=n2
            endif

            Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                     *S(ix,iy,iz-1)/S(xx,yy,iz))
            OK_A=1
            Amean=Amean+Acorr(ix,iy)
            Na=Na+1
          endif
        endif  
        if (OK_A.eq.0) then  
           Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
           Amean=Amean+Acorr(ix,iy)
           Na=Na+1
        endif
      endif 

* SPIGOLO B     
      ix=Nxmin
      iy=Nymax
      if (OK1(ix,iy).ge.1) then
        xc=x(ix,iy)
        yc=y(ix,iy)
        OK_A=0
        if ((OK1(ix,iy-1).ge.1).AND.
     &      (OK1(ix+1,iy).ge.1).AND.
     &      (OK1(ix+1,iy-1).ge.1) ) then
          CALL area(x(ix,iy-1),y(ix,iy-1),
     &              x(ix+1,iy),y(ix+1,iy),
     &              x(ix,iy),y(ix,iy),
     &              x(ix+1,iy-1),y(ix+1,iy-1),sup2)
          sup1=sup1_1                             ! feb 2002 
          if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

            xx=NINT(xc/d1)+1
            if (xx.LT.1) then
              xx=1
            elseif (xx.GT.n1) then
              xx=n1
            endif

            yy=NINT(yc/d2)+1
            if (yy.LT.1) then
              yy=1
            elseif (yy.GT.n2) then
              yy=n2
            endif

            Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                     *S(ix,iy,iz-1)/S(xx,yy,iz))
            OK_A=1
            Amean=Amean+Acorr(ix,iy)
            Na=Na+1
          endif
        endif 
        if (OK_A.eq.0) then  
          Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
          Amean=Amean+Acorr(ix,iy)
          Na=Na+1
        endif
      endif
      
* SPIGOLO C      
      ix=Nxmax
      iy=Nymin
      if (OK1(ix,iy).ge.1) then
        xc=x(ix,iy)
        yc=y(ix,iy)
        OK_A=0
        if ((OK1(ix-1,iy).ge.1)   .AND.
     &      (OK1(ix,iy+1).ge.1)   .AND.
     &      (OK1(ix-1,iy+1).ge.1)) then
          CALL area(x(ix-1,iy),y(ix-1,iy),
     &              x(ix,iy+1),y(ix,iy+1),
     &              x(ix-1,iy+1),y(ix-1,iy+1),
     &              x(ix,iy),y(ix,iy),sup2)
          sup1=sup1_1                             ! feb 2002 
          if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

            xx=NINT(xc/d1)+1
            if (xx.LT.1) then
              xx=1
            elseif (xx.GT.n1) then
              xx=n1
            endif

            yy=NINT(yc/d2)+1
            if (yy.LT.1) then
              yy=1
            elseif (yy.GT.n2) then
              yy=n2
            endif

            Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                     *S(ix,iy,iz-1)/S(xx,yy,iz))
            OK_A=1  
            Amean=Amean+Acorr(ix,iy)
            Na=Na+1
         endif 
        endif 
        if (OK_A.eq.0) then   
          Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
          Amean=Amean+Acorr(ix,iy)
          Na=Na+1
        endif
      endif
      
* SPIGOLO D      
      ix=Nxmax
      iy=Nymax
      if (OK1(ix,iy).ge.1) then
        xc=x(ix,iy)
        yc=y(ix,iy)
        OK_A=0
        if ((OK1(ix-1,iy-1).ge.1) .AND.
     &      (OK1(ix,iy-1).ge.1)   .AND.
     &      (OK1(ix-1,iy).ge.1))  then
          CALL area(x(ix-1,iy-1),y(ix-1,iy-1),
     &              x(ix,iy),y(ix,iy),
     &              x(ix-1,iy),y(ix-1,iy),
     &              x(ix,iy-1),y(ix,iy-1),sup2)
          sup1=sup1_1                             ! feb 2002 
          if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

            xx=NINT(xc/d1)+1
            if (xx.LT.1) then
              xx=1
            elseif (xx.GT.n1) then
              xx=n1
            endif

            yy=NINT(yc/d2)+1
            if (yy.LT.1) then
              yy=1
            elseif (yy.GT.n2) then
              yy=n2
            endif

            Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                    *S(ix,iy,iz-1)/S(xx,yy,iz))
            OK_A=1  
            Amean=Amean+Acorr(ix,iy)
            Na=Na+1
          endif
        endif 
        if (OK_A.eq.0) then  
           Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
           Amean=Amean+Acorr(ix,iy)
           Na=Na+1
        endif
      endif

* I BORDI ESTERNI               o---o
*                               |   |
*  ^ y      o-------o           *   | 
*  |  x     |       |           |   |
*  +->  ----o---*---o----       o---o
*                               |
* BORDO INFERIORE E SUPERIORE      
      do 30 ix=Nxmin+1,Nxmax-1
        iy=Nymin
        if (OK1(ix,iy).ge.1) then
          OK_A=0
          xc=x(ix,iy)
          yc=y(ix,iy)
          if ((OK1(ix-1,iy).ge.1).AND.
     &        (OK1(ix+1,iy+1).ge.1).AND.
     &        (OK1(ix+1,iy).ge.1).AND.
     &        (OK1(ix-1,iy+1).ge.1)) then
            CALL area(x(ix-1,iy),y(ix-1,iy),
     &                x(ix+1,iy+1),y(ix+1,iy+1),
     &                x(ix-1,iy+1),y(ix-1,iy+1),
     &                x(ix+1,iy),y(ix+1,iy),sup2)
            sup1=sup1_2                             ! feb 2002 
            if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

              xx=NINT(xc/d1)+1
              if (xx.LT.1) then
                xx=1
              elseif (xx.GT.n1) then
                xx=n1
              endif

              yy=NINT(yc/d2)+1
              if (yy.LT.1) then
                yy=1
              elseif (yy.GT.n2) then
                yy=n2
              endif

              Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                     *S(ix,iy,iz-1)/S(xx,yy,iz))
              OK_A=1  
              Amean=Amean+Acorr(ix,iy)
              Na=Na+1
            endif
          endif 
          if (OK_A.eq.0) then  
             Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
             Amean=Amean+Acorr(ix,iy)
             Na=Na+1
          endif
        endif

        iy=Nymax
        if (OK1(ix,iy).ge.1) then
          OK_A=0
          xc=x(ix,iy)
          yc=y(ix,iy)
          if ((OK1(ix-1,iy-1).ge.1)  .AND.
     &        (OK1(ix+1,iy).ge.1)    .AND.
     &        (OK1(ix+1,iy-1).ge.1)  .AND.
     &        (OK1(ix-1,iy).ge.1)  ) then
            CALL area(x(ix-1,iy-1),y(ix-1,iy-1),
     &                x(ix+1,iy),y(ix+1,iy),
     &                x(ix-1,iy),y(ix-1,iy),
     &                x(ix+1,iy-1),y(ix+1,iy-1),sup2)
            sup1=sup1_2                             ! feb 2002 
            if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

              xx=NINT(xc/d1)+1
              if (xx.LT.1) then
                xx=1
              elseif (xx.GT.n1) then
                xx=n1
              endif

              yy=NINT(yc/d2)+1
              if (yy.LT.1) then
                yy=1
              elseif (yy.GT.n2) then
                yy=n2
              endif

              Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                     *S(ix,iy,iz-1)/S(xx,yy,iz))
              OK_A=1  
              Amean=Amean+Acorr(ix,iy)
              Na=Na+1
            endif
          endif 
          if (OK_A.eq.0) then  
             Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
             Amean=Amean+Acorr(ix,iy)
             Na=Na+1
          endif
        endif
30    continue
 
* BORDO DESTRO E SINISTRO 
      do 40 iy=Nymin+1,Nymax-1
        ix=Nxmin
        if (OK1(ix,iy).ge.1) then
          OK_A=0
          xc=x(ix,iy)
          yc=y(ix,iy)
          if ((OK1(ix,iy-1).ge.1).AND.
     &        (OK1(ix+1,iy+1).ge.1).AND.
     &        (OK1(ix+1,iy-1).ge.1).AND.
     &        (OK1(ix,iy+1).ge.1)) then
            CALL area(x(ix,iy-1),y(ix,iy-1),
     &                x(ix+1,iy+1),y(ix+1,iy+1),
     &                x(ix,iy+1),y(ix,iy+1),
     &                x(ix+1,iy-1),y(ix+1,iy-1),sup2)
            sup1=sup1_2                             ! feb 2002 
            if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

              xx=NINT(xc/d1)+1
              if (xx.LT.1) then
                xx=1
              elseif (xx.GT.n1) then
                xx=n1
              endif

              yy=NINT(yc/d2)+1
              if (yy.LT.1) then
                yy=1
              elseif (yy.GT.n2) then
                yy=n2
              endif

              Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                     *S(ix,iy,iz-1)/S(xx,yy,iz))
              OK_A=1  
              Amean=Amean+Acorr(ix,iy)
              Na=Na+1
            endif
          endif 
          if (OK_A.eq.0) then  
             Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
             Amean=Amean+Acorr(ix,iy)
             Na=Na+1
          endif
        endif

        ix=Nxmax
        if (OK1(ix,iy).ge.1) then
          OK_A=0
          xc=x(ix,iy)
          yc=y(ix,iy)
          if ((OK1(ix-1,iy-1).ge.1).AND.
     &        (OK1(ix,iy+1).ge.1).AND.
     &        (OK1(ix,iy-1).ge.1).AND.
     &        (OK1(ix-1,iy+1).ge.1)) then
              CALL area(x(ix-1,iy-1),y(ix-1,iy-1),
     &                x(ix,iy+1),y(ix,iy+1),
     &                x(ix-1,iy+1),y(ix-1,iy+1),
     &                x(ix,iy-1),y(ix,iy-1),sup2)
              sup1=sup1_2                             ! feb 2002 
              if (sup2 .gt. sup1*SOGLIA) then         ! feb 2002 

                xx=NINT(xc/d1)+1
                if (xx.LT.1) then
                  xx=1
                elseif (xx.GT.n1) then
                  xx=n1
                endif
  
                yy=NINT(yc/d2)+1
                if (yy.LT.1) then
                  yy=1
                elseif (yy.GT.n2) then
                  yy=n2
                endif

                Acorr(ix,iy)=A(ix,iy,1)*sqrt(sup1/sup2
     &                     *S(ix,iy,iz-1)/S(xx,yy,iz)) 
                OK_A=1
                Amean=Amean+Acorr(ix,iy)
                Na=Na+1            
             endif
           endif 
           if (OK_A.eq.0) then  
             Acorr(ix,iy)=A(ix,iy,1)*REAL(iz-2)/REAL(iz-1)
             Amean=Amean+Acorr(ix,iy)
             Na=Na+1
           endif
         endif

40    continue

* Test ampiezza sul piano

      Amean=Amean/Na
      Alim=Amean*Afrac
      do 60 iy=Nymin,Nymax
        do 50 ix=Nxmin,Nxmax
          if (Ok1(ix,iy).ge.1) then
             if (Acorr(ix,iy).gt.Alim) then
                Acorr(ix,iy)=Amean
C               Ok1(ix,iy)=0
             endif
          endif
50      continue
60    continue

      RETURN
      END

*---------------------------------------------------------------------
*
*---------------------------------------------------------------------

       subroutine area(x1,y1,x2,y2,x3,y3,x4,y4,Sup)

*
* Subroutine che calcola l'area di un quadrilatero date le
* coordinate cartesiane dei suoi vertici.Essa si avvale della 
* formula di Erone.
*
* INPUT:
* (x1,y1): coordinate del primo vertice
* (x2,y2): coordinate del secondo vertice
* (x3,y3): coordinate del terzo vertice
* (x4,y4): coordinate del quarto vertice
*
* OUTPUT:
* Sup    : area del quadrilatero
*
* VARIABILI LOCALI:
* p1,p2  : semiperimetri
* l**    : distanze tra punti     
*  
*---------------------------------------------------------------------

      implicit none

* Variabili in input
      real*4 x1,y1,x2,y2,x3,y3,x4,y4
      real*4 Sup,sup1,sup2

* Variabili locali
      real*4  l12,l13,l14,l23,l24,p1,p2
       
* CALCOLO AREA
*               l23
*           P3 o----o P2
*              | 1 /|
*           l13|  /2| l24
*              | /  |
*              |/l12|
*           P1 o----o P4
*               l14  

* Lato comune ai due triangoli 123 - 124     
      l12=sqrt((x1-x2)**2+(y1-y2)**2)
      
* Lati distinti dei due triangoli
      l13=sqrt((x1-x3)**2+(y1-y3)**2)
      l23=sqrt((x3-x2)**2+(y3-y2)**2)
      l14=sqrt((x1-x4)**2+(y1-y4)**2)
      l24=sqrt((x2-x4)**2+(y2-y4)**2)
      
* Semiperimetri dei triangoli       
      p1=(l13+l12+l23)/2.0E0
      p2=(l14+l24+l12)/2.0E0
      
* Superficie totale data dalla somma dell'area dei due triangoli
* L'area di un triangolo e' ricavata con la formula di Erone      
      
* Area triangolo 1

       if ( ((p1-l13).gt.0.0E0) .AND. 
     &      ((p1-l12).gt.0.0E0) .AND. 
     &      ((p1-l23).gt.0.0E0))  then
          sup1 = sqrt(p1*(p1-l13)*(p1-l12)*(p1-l23))
       else
          sup1 = 0.0E0
       endif

* Area triangolo 2

       if ( ((p2-l14).gt.0.0E0) .AND. 
     &      ((p2-l24).gt.0.0E0) .AND. 
     &      ((p2-l12).gt.0.0E0))  then
          sup2 = sqrt(p2*(p2-l14)*(p2-l24)*(p2-l12))
       else
          sup2 = 0.0E0
       endif

* Area totale

       Sup=sup1+sup2
  
      RETURN
      END

