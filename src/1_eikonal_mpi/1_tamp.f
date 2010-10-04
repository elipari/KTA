****************************************************************
****************************************************************
      subroutine tamp(n1,n2,n3,d1,d2,
     &     dampx,dampy,dampxy,
     &     Nxmin,Nxmax,Nymin,Nymax,
     &     Nxs,Nys,Px,Py,A,OK2,T)             ! mar 2002
      
*---------------------------------------------------------------
*     
*     Subroutine che esegue il rigrigliamento dei dati ottenuti
*     con un passo di integrazione alle differenze finite sulla
*     originale griglia del modello: seconda fase.
*     In questa fase si esegue il rigrigliamento dei dati ottenuti 
*     con un passo di integrazione alle differenze finite sulla
*     originale griglia del modello nei punti dove l'interpolazione
*     normale (prima fase) non e' applicabile.
*     
*     INPUT:
*     n1,n2,n3      : dimensioni del modello
*     d1,d2         : passi di campionamento
*     Nxmin,Nxmax,
*     Nymin,Nymax   : estremi del fronte su cui rigrigliare
*     Nxs,Nys       : posizione dello shot
*     Px,Py         : cubi delle componenti del vettore slowness da aggiornare
*     A             : cubo delle ampiezze da aggiornare
*     iz            : quota corrente di integrazione
*     OK2           : matrice che tiene traccia delle posizioni dove 
*                     l'interpolazione normale ha fallito
*     
*     FILE CHIAMANTI:
*     iconal3d.f
*     
*     Bonolo Ivan - Maggio 1999
*     Pepe Giuliano - Feb 2002
*     Bernasconi Giancarlo - Mar 2002
*---------------------------------------------------------------------

      implicit none

*     Variabili in input
      integer*4 n1,n2,n3
      real*4    d1,d2
      real*4    dampx,dampy,dampxy
      integer*4 Nxs,Nys
      integer*4 Nxmin,Nymin,Nxmax,Nymax
      real*4    Px(n1,n2,2),Py(n1,n2,2)
      real*4    A(n1,n2,2),T(n1,n2,2)
      integer*4 OK2(n1,n2)
      
*     variabili locali
      integer*4 iy,oy,ey
      integer*4 ix,ox,ex

*     Si tampona nei punti lasciati scoperti dalla prima fase
*     interpolando linearmente i valori limitrofi.
*     Procedo per settori partendo dal centro dove l'informazione
*     dovrebbe essere migliore; controllo prima i punti centrali
*     (cioe' sotto lo shot), poi definisco due strisce nelle due
*     direzioni degli assi larghe 3 campioni; infine procedo con
*     i quattro quadranti. Il bordo estremo viene trattato a parte.

*     MAPPATURA DEL PIANO      CRITERI DI ESTRAPOLAZIONE
*
*                sx
*           +-----x-----+              o              o
*           |q2   x  q1 |             /|             /|
*           |    xxx    |            o |            o |
*           xxxxxxoxxxxxx sy        /| |           /| | 
*     y     |    xxxq0  |          o | |          o | |
*     ^     | q3  x   q4|          | | |          | | |
*     + > x +-----x-----+      ----o-x-o----  ----o-o-x----



*     ==============================================================
*     1. INTORNO DELLO SHOT Q0  
*     ==============================================================
      ox = Nxs-1
      ex = Nxs+1
      oy = Nys-1
      ey = Nys+1
      call xcheck(ox,ex,Nxmin,Nxmax)
      call ycheck(oy,ey,Nymin,Nymax)

      do 10 iy=oy,ey
         do 20 ix=ox,ex

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ1(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 20      continue
 10   continue
*     ==============================================================


*     ==============================================================
*     2. STRISCIA SY1 
*     ==============================================================
      ox = Nxs-1
      ex = Nxs+1
      oy = Nys-2
      ey = Nymin
      call ycheck(oy,ey,Nymin,Nymax)
      call xcheck(ox,ex,Nxmin,Nxmax)

      do 30 iy=oy,ey,-1
         do 40 ix=ox,ex

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ3(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 40      continue
 30   continue
*     ==============================================================
      


*     ==============================================================
*     3. STRISCIA SY2
*     ==============================================================
      ox = Nxs-1
      ex = Nxs+1
      call xcheck(ox,ex,Nxmin,Nxmax)
      oy = Nys+2
      ey = Nymax
      call ycheck(oy,ey,Nymin,Nymax)

      do 45 iy=oy,ey
         do 50 ix = ox,ex

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif 

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ1(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 50      continue
 45   continue
*     ==============================================================



*     ==============================================================
*     STRISCIA SX1
*     ==============================================================
      oy=Nys-1
      ey=Nys+1
      call ycheck(oy,ey,Nymin,Nymax)      
      ox=Nxs-2
      ex=Nxmin
      call xcheck(ox,ex,Nxmin,Nxmax)

      do 60 iy=oy,ey
         do 70 ix=ox,ex,-1

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 70      continue
 60   continue
*     ==============================================================



*     ==============================================================
*     STRISCIA SX2
*     ==============================================================
      oy=Nys-1
      ey=Nys+1
      call ycheck(oy,ey,Nymin,Nymax)      
      ox=Nxs+2
      ex=Nxmax
      call xcheck(ox,ex,Nxmin,Nxmax)

      do 75 iy=oy,ey
         do 80 ix=ox,ex

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ4(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 80      continue
 75   continue
*     ==============================================================



*     ==============================================================
*     SETTORE Q3
*     ==============================================================
      ox=Nxs-2
      ex=Nxmin
      oy=Nys-2
      ey=Nymin
      call xcheck(ox,ex,Nxmin,Nxmax)
      call ycheck(oy,ey,Nymin,Nymax)

      do 90 iy=oy,ey,-1
         do 100 ix=ox,ex,-1 

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif
 2          
            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ3(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 100     continue
 90   continue
*     ==============================================================


      
*     ==============================================================
*     SETTORE Q4
*     ==============================================================
      ox=Nxs+2
      ex=Nxmax
      oy=Nys-2
      ey=Nymin
      call xcheck(ox,ex,Nxmin,Nxmax)
      call ycheck(oy,ey,Nymin,Nymax)

      do 110 iy=oy,ey,-1
         do 120 ix=ox,ex

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ4(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif
            
 120     continue
 110  continue
*     ==============================================================



*     ==============================================================
*     SETTORE Q2
*     ==============================================================
      ox=Nxs-2
      ex=Nxmin
      oy=Nys+2
      ey=Nymax
      call xcheck(ox,ex,Nxmin,Nxmax)
      call ycheck(oy,ey,Nymin,Nymax)

      do 130 iy=oy,ey
         do 140 ix=ox,ex,-1 

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 140     continue
 130  continue
*     ==============================================================

      

*     ==============================================================
*     SETTORE Q1
*     ==============================================================
      ox=Nxs+2
      ex=Nxmax
      oy=Nys+2
      ey=Nymax
      call xcheck(ox,ex,Nxmin,Nxmax)
      call ycheck(oy,ey,Nymin,Nymax)

      do 150 iy=oy,ey
         do 160 ix=ox,ex

            if (OK2(ix,iy).eq.0) then
               CALL interpol2(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,T)
            endif

            if (OK2(ix,iy).eq.0) then
               CALL extrapolQ1(ix,iy,n1,n2,n3,Nxmin,Nxmax,Nymin,Nymax,
     &              Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)
            endif

 160     continue
 150  continue
*     ==============================================================



*     ==============================================================


      RETURN
      END
****************************************************************
****************************************************************




****************************************************************
****************************************************************
      SUBROUTINE interpol2(ix,iy,n1,n2,n3,
     &            Nxmin,Nxmax,Nymin,Nymax,
     &            Px,Py,A,OK2,T)

      implicit none

      integer*4 ix,iy
      integer*4 n1,n2,n3
      integer*4 Nxmin,Nxmax,Nymin,Nymax


      real*4    Px(n1,n2,2),Py(n1,n2,2)
      real*4    A(n1,n2,2),T(n1,n2,2)

      integer*4 OK2(n1,n2)
      integer*4 intorno1,intorno2,intorno3,intorno4,intorno5,intorno6
      integer*4 intorno1a,intorno2a,intorno3a,intorno4a,intorno5a,intorno6a
      
*******Interpolazione
      if (intorno1(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.25E0*(Px(ix+1,iy+1,2)+Px(ix-1,iy-1,2)+
     &                       Px(ix-1,iy+1,2)+Px(ix+1,iy-1,2) )
         Py(ix,iy,2)=0.25E0*(Py(ix+1,iy+1,2)+Py(ix-1,iy-1,2)+
     &                       Py(ix-1,iy+1,2)+Py(ix+1,iy-1,2) )
         A(ix,iy,2) =0.25E0*(A(ix+1,iy+1,2)+ A(ix-1,iy-1,2)+
     &                       A(ix-1,iy+1,2)+ A(ix+1,iy-1,2)  )
         T(ix,iy,2) =0.25E0*(T(ix+1,iy+1,2)+ T(ix-1,iy-1,2)+
     &                       T(ix-1,iy+1,2)+ T(ix+1,iy-1,2)  )
         OK2(ix,iy) = 2

      elseif (intorno2(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.25E0*(Px(ix+1,iy,2)+Px(ix-1,iy,2)+
     &                       Px(ix,iy+1,2)+Px(ix,iy-1,2)  )
         Py(ix,iy,2)=0.25E0*(Py(ix+1,iy,2)+Py(ix-1,iy,2)+
     &                       Py(ix,iy+1,2)+Py(ix,iy-1,2)  )
         A(ix,iy,2) =0.25E0*( A(ix+1,iy,2)+ A(ix-1,iy,2)+
     &                        A(ix,iy+1,2)+ A(ix,iy-1,2)  )
         T(ix,iy,2) =0.25E0*( T(ix+1,iy,2)+ T(ix-1,iy,2)+
     &                        T(ix,iy+1,2)+ T(ix,iy-1,2)  )
         OK2(ix,iy) = 2

      elseif (intorno3(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then      

         Px(ix,iy,2)=0.5E0*(Px(ix-1,iy-1,2)+Px(ix+1,iy+1,2))
         Py(ix,iy,2)=0.5E0*(Py(ix-1,iy-1,2)+Py(ix+1,iy+1,2))
         A(ix,iy,2) =0.5E0*( A(ix-1,iy-1,2)+ A(ix+1,iy+1,2))
         T(ix,iy,2) =0.5E0*( T(ix-1,iy-1,2)+ T(ix+1,iy+1,2))
         OK2(ix,iy) = 2

      elseif (intorno4(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.5E0*(Px(ix-1,iy+1,2)+Px(ix+1,iy-1,2))
         Py(ix,iy,2)=0.5E0*(Py(ix-1,iy+1,2)+Py(ix+1,iy-1,2))
         A(ix,iy,2) =0.5E0*( A(ix-1,iy+1,2)+ A(ix+1,iy-1,2))
         T(ix,iy,2) =0.5E0*( T(ix-1,iy+1,2)+ T(ix+1,iy-1,2))
         OK2(ix,iy) = 2

      elseif (intorno5(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.5E0*(Px(ix-1,iy,2)+Px(ix+1,iy,2))
         Py(ix,iy,2)=0.5E0*(Py(ix-1,iy,2)+Py(ix+1,iy,2))
         A(ix,iy,2) =0.5E0*( A(ix-1,iy,2)+ A(ix+1,iy,2))
         T(ix,iy,2) =0.5E0*( T(ix-1,iy,2)+ T(ix+1,iy,2))
         OK2(ix,iy) = 2

      elseif (intorno6(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.5E0*(Px(ix,iy-1,2)+Px(ix,iy+1,2))
         Py(ix,iy,2)=0.5E0*(Py(ix,iy-1,2)+Py(ix,iy+1,2))
         A(ix,iy,2) =0.5E0*( A(ix,iy-1,2)+ A(ix,iy+1,2)) 
         T(ix,iy,2) =0.5E0*( T(ix,iy-1,2)+ T(ix,iy+1,2)) 
         OK2(ix,iy) = 2

      elseif (intorno1a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.25E0*(Px(ix+2,iy+2,2)+Px(ix-2,iy-2,2)+
     &                       Px(ix-2,iy+2,2)+Px(ix+2,iy-2,2)  )
         Py(ix,iy,2)=0.25E0*(Py(ix+2,iy+2,2)+Py(ix-2,iy-2,2)+
     &                       Py(ix-2,iy+2,2)+Py(ix+2,iy-2,2)  )
         A(ix,iy,2) =0.25E0*( A(ix+2,iy+2,2)+ A(ix-2,iy-2,2)+
     &                        A(ix-2,iy+2,2)+ A(ix+2,iy-2,2)  )       
         T(ix,iy,2) =0.25E0*( T(ix+2,iy+2,2)+ T(ix-2,iy-2,2)+
     &                        T(ix-2,iy+2,2)+ T(ix+2,iy-2,2)  ) 
         OK2(ix,iy) = 2

      elseif (intorno2a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.25E0*(Px(ix+2,iy,2)+Px(ix-2,iy,2)+
     &                       Px(ix,iy+2,2)+Px(ix,iy-2,2)  )
         Py(ix,iy,2)=0.25E0*(Py(ix+2,iy,2)+Py(ix-2,iy,2)+
     &                       Py(ix,iy+2,2)+Py(ix,iy-2,2)  )
         A(ix,iy,2) =0.25E0*( A(ix+2,iy,2)+ A(ix-2,iy,2)+
     &                        A(ix,iy+2,2)+ A(ix,iy-2,2)  )
         T(ix,iy,2) =0.25E0*( T(ix+2,iy,2)+ T(ix-2,iy,2)+
     &                        T(ix,iy+2,2)+ T(ix,iy-2,2)  )
         OK2(ix,iy) = 2

      elseif (intorno3a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then      

         Px(ix,iy,2)=0.5E0*(Px(ix-2,iy-2,2)+Px(ix+2,iy+2,2))
         Py(ix,iy,2)=0.5E0*(Py(ix-2,iy-2,2)+Py(ix+2,iy+2,2))
         A(ix,iy,2) =0.5E0*( A(ix-2,iy-2,2)+ A(ix+2,iy+2,2))
         T(ix,iy,2) =0.5E0*( T(ix-2,iy-2,2)+ T(ix+2,iy+2,2))
         OK2(ix,iy) = 2

      elseif (intorno4a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.5E0*(Px(ix-2,iy+2,2)+Px(ix+2,iy-2,2))
         Py(ix,iy,2)=0.5E0*(Py(ix-2,iy+2,2)+Py(ix+2,iy-2,2))
         A(ix,iy,2) =0.5E0*( A(ix-2,iy+2,2)+ A(ix+2,iy-2,2))
         T(ix,iy,2) =0.5E0*( T(ix-2,iy+2,2)+ T(ix+2,iy-2,2))
         OK2(ix,iy) = 2

      elseif (intorno5a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.5E0*(Px(ix-2,iy,2)+Px(ix+2,iy,2))
         Py(ix,iy,2)=0.5E0*(Py(ix-2,iy,2)+Py(ix+2,iy,2))
         A(ix,iy,2) =0.5E0*( A(ix-2,iy,2)+ A(ix+2,iy,2))
         T(ix,iy,2) =0.5E0*( T(ix-2,iy,2)+ T(ix+2,iy,2))
         OK2(ix,iy) = 2

      elseif (intorno6a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

         Px(ix,iy,2)=0.5E0*(Px(ix,iy-2,2)+Px(ix,iy+2,2))
         Py(ix,iy,2)=0.5E0*(Py(ix,iy-2,2)+Py(ix,iy+2,2))
         A(ix,iy,2) =0.5E0*( A(ix,iy-2,2)+ A(ix,iy+2,2)) 
         T(ix,iy,2) =0.5E0*( T(ix,iy-2,2)+ T(ix,iy+2,2)) 
         OK2(ix,iy) = 2

      endif


      RETURN
      END
****************************************************************
****************************************************************


****************************************************************
****************************************************************
      SUBROUTINE extrapolQ1(ix,iy,n1,n2,n3,
     &     Nxmin,Nxmax,Nymin,Nymax,
     &     Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)

      implicit none

      integer*4 ix,iy
      integer*4 n1,n2,n3
      integer*4 Nxmin,Nxmax,Nymin,Nymax

      real*4    Px(n1,n2,2),Py(n1,n2,2)
      real*4    Pxmed,Pymed
      real*4    A(n1,n2,2),T(n1,n2,2)

      integer*4 OK2(n1,n2)
      real*4 d1,d2,dampx,dampy,dampxy

      integer*4 intorno7,intorno8,intorno9,intorno10,intorno11,intorno12
      integer*4 intorno13, intorno14, intorno15, intorno16, intorno17
      integer*4 intorno18, intorno19, intorno20, intorno21, intorno22

******* Estrapolazione setttore Q1

      if (intorno12(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy-1,2)-Px(ix-2,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy-1,2)-Py(ix-2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy-1,2)-A(ix-2,iy-2,2))*dampxy
        A(ix,iy,2) = A(ix-1,iy-1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy-1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix-1,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix-1,iy-1,2)+Pxmed          *d1+ Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy-1,2)+Px(ix-1,iy-1,2)*d1+ Py(ix-1,iy-1,2)*d2
        OK2(ix,iy) = 1
          
      elseif (intorno13(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy-1,2)-Px(ix,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy-1,2)-Py(ix,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy-1,2)-A(ix,iy-2,2))*dampy
        A(ix,iy,2) = A(ix,iy-1,2)*dampy         
        Pymed      = (Py(ix,iy,2)+Py(ix,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix,iy-1,2)+Pymed        *d2
C       T(ix,iy,2) = T(ix,iy-1,2)+Py(ix,iy-1,2)*d2
        OK2(ix,iy) = 1
        
      elseif (intorno11(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then
      
        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy,2)-Px(ix-2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy,2)-Py(ix-2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy,2)-A(ix-2,iy,2))*dampx
        A(ix,iy,2) = A(ix-1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy,2))/2.0E0       
        T(ix,iy,2) = T(ix-1,iy,2)+Pxmed        *d1
C       T(ix,iy,2) = T(ix-1,iy,2)+Px(ix-1,iy,2)*d1
        OK2(ix,iy) = 1

      elseif (intorno14(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy-1,2)-Px(ix+2,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy-1,2)-Py(ix+2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy-1,2)-A(ix+2,iy-2,2))*dampxy
        A(ix,iy,2) = A(ix+1,iy-1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy-1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix+1,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix+1,iy-1,2)-Pxmed          *d1+ Pymed          *d2          
C       T(ix,iy,2) = T(ix+1,iy-1,2)-Px(ix+1,iy-1,2)*d1+ Py(ix+1,iy-1,2)*d2
        OK2(ix,iy) = 1
          
      elseif (intorno10(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy+1,2)-Px(ix-2,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy+1,2)-Py(ix-2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy+1,2)-A(ix-2,iy+2,2))*dampxy
        A(ix,iy,2) = A(ix-1,iy+1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy+1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix-1,iy+1,2))/2.0E0        
        T(ix,iy,2) = T(ix-1,iy+1,2)+Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy+1,2)+Px(ix-1,iy+1,2)*d1-Py(ix-1,iy+1,2)*d2
        OK2(ix,iy) = 1
          
      elseif (intorno9(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy+1,2)-Px(ix,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy+1,2)-Py(ix,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy+1,2)-A(ix,iy+2,2))*dampy
        A(ix,iy,2) = A(ix,iy+1,2)*dampy
        Pymed      = (Py(ix,iy,2)+Py(ix,iy+1,2))/2.0E0        
        T(ix,iy,2) = T(ix,iy+1,2)-Pymed        *d2
C       T(ix,iy,2) = T(ix,iy+1,2)-Py(ix,iy+1,2)*d2
        OK2(ix,iy) = 1
          
      elseif (intorno7(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy,2)-Px(ix+2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy,2)-Py(ix+2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy,2)-A(ix+2,iy,2))*dampx
        A(ix,iy,2) = A(ix+1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy,2))/2.0E0
        T(ix,iy,2) = T(ix+1,iy,2)-Pxmed        *d1        
C       T(ix,iy,2) = T(ix+1,iy,2)-Px(ix+1,iy,2)*d1
        OK2(ix,iy) = 1

C     elseif (intorno8(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

C       Px(ix,iy,2)= 2.0E0*Px(ix+1,iy+1,2)-Px(ix+2,iy+2,2)
C       Py(ix,iy,2)= 2.0E0*Py(ix+1,iy+1,2)-Py(ix+2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy+1,2)-A(ix+2,iy+2,2))*dampxy
C       A(ix,iy,2) = A(ix+1,iy+1,2)*dampxy
C       Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy+1,2))/2.0E0
C       Pymed      = (Py(ix,iy,2)+Py(ix+1,iy+1,2))/2.0E0         
C       T(ix,iy,2) =  T(ix+1,iy+1,2)-Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) =  T(ix+1,iy+1,2)-Px(ix+1,iy+1,2)*d1-Py(ix+1,iy+1,2)*d2
C       OK2(ix,iy) = 1
                                    
      endif           

      if ( OK2(ix,iy).eq.0 ) then
        Px(ix,iy,2) = -2
        Py(ix,iy,2) = -2
        A(ix,iy,2)  =  0
        T(ix,iy,2)  = -2
      endif  


      RETURN
      END
****************************************************************
****************************************************************



****************************************************************
****************************************************************
      SUBROUTINE extrapolQ4(ix,iy,n1,n2,n3,       ! feb 2002
     &            Nxmin,Nxmax,Nymin,Nymax,
     &            Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)

      implicit none

      integer*4 ix,iy
      integer*4 n1,n2,n3
      integer*4 Nxmin,Nxmax,Nymin,Nymax

      real*4    Px(n1,n2,2),Py(n1,n2,2)
      real*4    Pxmed,Pymed
      real*4    A(n1,n2,2),T(n1,n2,2)

      integer*4 OK2(n1,n2)

      real*4 d1,d2,dampx,dampy,dampxy

      integer*4 intorno7,intorno8,intorno9,intorno10,intorno11,intorno12
      integer*4 intorno13, intorno14, intorno15, intorno16, intorno17
      integer*4 intorno18, intorno19, intorno20, intorno21, intorno22


      if (intorno10(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy+1,2)-Px(ix-2,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy+1,2)-Py(ix-2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy+1,2)-A(ix-2,iy+2,2))*dampxy
        A(ix,iy,2) = A(ix-1,iy+1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy+1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix-1,iy+1,2))/2.0E0        
        T(ix,iy,2) = T(ix-1,iy+1,2)+Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy+1,2)+Px(ix-1,iy+1,2)*d1-Py(ix-1,iy+1,2)*d2
        OK2(ix,iy) = 1
        
      elseif (intorno11(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then
      
        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy,2)-Px(ix-2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy,2)-Py(ix-2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy,2)-A(ix-2,iy,2))*dampx
        A(ix,iy,2) = A(ix-1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy,2))/2.0E0       
        T(ix,iy,2) = T(ix-1,iy,2)+Pxmed        *d1
C       T(ix,iy,2) = T(ix-1,iy,2)+Px(ix-1,iy,2)*d1
        OK2(ix,iy) = 1

      elseif (intorno9(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy+1,2)-Px(ix,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy+1,2)-Py(ix,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy+1,2)-A(ix,iy+2,2))*dampy
        A(ix,iy,2) = A(ix,iy+1,2)*dampy
        Pymed      = (Py(ix,iy,2)+Py(ix,iy+1,2))/2.0E0        
        T(ix,iy,2) = T(ix,iy+1,2)-Pymed        *d2
C       T(ix,iy,2) = T(ix,iy+1,2)-Py(ix,iy+1,2)*d2
        OK2(ix,iy) = 1

      elseif (intorno12(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy-1,2)-Px(ix-2,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy-1,2)-Py(ix-2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy-1,2)-A(ix-2,iy-2,2))*dampxy
        A(ix,iy,2) = A(ix-1,iy-1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy-1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix-1,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix-1,iy-1,2)+Pxmed          *d1+ Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy-1,2)+Px(ix-1,iy-1,2)*d1+ Py(ix-1,iy-1,2)*d2
        OK2(ix,iy) = 1
        
      elseif (intorno8(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy+1,2)-Px(ix+2,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy+1,2)-Py(ix+2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy+1,2)-A(ix+2,iy+2,2))*dampxy
        A(ix,iy,2) = A(ix+1,iy+1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy+1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix+1,iy+1,2))/2.0E0         
        T(ix,iy,2) =  T(ix+1,iy+1,2)-Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) =  T(ix+1,iy+1,2)-Px(ix+1,iy+1,2)*d1-Py(ix+1,iy+1,2)*d2
        OK2(ix,iy) = 1

      elseif (intorno7(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy,2)-Px(ix+2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy,2)-Py(ix+2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy,2)-A(ix+2,iy,2))*dampx
        A(ix,iy,2) = A(ix+1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy,2))/2.0E0
        T(ix,iy,2) = T(ix+1,iy,2)-Pxmed        *d1        
C       T(ix,iy,2) = T(ix+1,iy,2)-Px(ix+1,iy,2)*d1
        OK2(ix,iy) = 1      
            
      elseif (intorno13(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy-1,2)-Px(ix,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy-1,2)-Py(ix,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy-1,2)-A(ix,iy-2,2))*dampy
        A(ix,iy,2) = A(ix,iy-1,2)*dampy         
        Pymed      = (Py(ix,iy,2)+Py(ix,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix,iy-1,2)+Pymed        *d2
C       T(ix,iy,2) = T(ix,iy-1,2)+Py(ix,iy-1,2)*d2
        OK2(ix,iy) = 1

C     elseif (intorno14(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

C       Px(ix,iy,2)= 2.0E0*Px(ix+1,iy-1,2)-Px(ix+2,iy-2,2)
C       Py(ix,iy,2)= 2.0E0*Py(ix+1,iy-1,2)-Py(ix+2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy-1,2)-A(ix+2,iy-2,2))*dampxy
C       A(ix,iy,2) = A(ix+1,iy-1,2)*dampxy
C       Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy-1,2))/2.0E0
C       Pymed      = (Py(ix,iy,2)+Py(ix+1,iy-1,2))/2.0E0
C       T(ix,iy,2) = T(ix+1,iy-1,2)-Pxmed          *d1+ Pymed          *d2          
C       T(ix,iy,2) = T(ix+1,iy-1,2)-Px(ix+1,iy-1,2)*d1+ Py(ix+1,iy-1,2)*d2
C       OK2(ix,iy) = 1
        
      endif           

      if ( OK2(ix,iy).eq.0 ) then
        Px(ix,iy,2)= -2
        Py(ix,iy,2)= -2
        A(ix,iy,2) =  0
        T(ix,iy,2) = -2
      endif  

      RETURN
      END
****************************************************************
****************************************************************



****************************************************************
****************************************************************
      SUBROUTINE extrapolQ3(ix,iy,n1,n2,n3,
     &            Nxmin,Nxmax,Nymin,Nymax,
     &            Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)

      implicit none

      integer*4 ix,iy
      integer*4 n1,n2,n3
      integer*4 Nxmin,Nxmax,Nymin,Nymax

      real*4    Px(n1,n2,2),Py(n1,n2,2)
      real*4    Pxmed,Pymed
      real*4    A(n1,n2,2),T(n1,n2,2)

      integer*4 OK2(n1,n2)

      real*4 d1,d2,dampx,dampy,dampxy

      integer*4 intorno7,intorno8,intorno9,intorno10,intorno11,intorno12
      integer*4 intorno13, intorno14, intorno15, intorno16, intorno17
      integer*4 intorno18, intorno19, intorno20, intorno21, intorno22


      if (intorno8(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy+1,2)-Px(ix+2,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy+1,2)-Py(ix+2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy+1,2)-A(ix+2,iy+2,2))*dampxy
        A(ix,iy,2) = A(ix+1,iy+1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy+1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix+1,iy+1,2))/2.0E0         
        T(ix,iy,2) =  T(ix+1,iy+1,2)-Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) =  T(ix+1,iy+1,2)-Px(ix+1,iy+1,2)*d1-Py(ix+1,iy+1,2)*d2
        OK2(ix,iy) = 1
      
      elseif (intorno9(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy+1,2)-Px(ix,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy+1,2)-Py(ix,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy+1,2)-A(ix,iy+2,2))*dampy
        A(ix,iy,2) = A(ix,iy+1,2)*dampy
        Pymed      = (Py(ix,iy,2)+Py(ix,iy+1,2))/2.0E0        
        T(ix,iy,2) = T(ix,iy+1,2)-Pymed        *d2
C       T(ix,iy,2) = T(ix,iy+1,2)-Py(ix,iy+1,2)*d2
        OK2(ix,iy) = 1
          
      elseif (intorno7(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy,2)-Px(ix+2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy,2)-Py(ix+2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy,2)-A(ix+2,iy,2))*dampx
        A(ix,iy,2) = A(ix+1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy,2))/2.0E0
        T(ix,iy,2) = T(ix+1,iy,2)-Pxmed        *d1        
C       T(ix,iy,2) = T(ix+1,iy,2)-Px(ix+1,iy,2)*d1
        OK2(ix,iy) = 1

      elseif (intorno10(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy+1,2)-Px(ix-2,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy+1,2)-Py(ix-2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy+1,2)-A(ix-2,iy+2,2))*dampxy
        A(ix,iy,2) = A(ix-1,iy+1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy+1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix-1,iy+1,2))/2.0E0        
        T(ix,iy,2) = T(ix-1,iy+1,2)+Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy+1,2)+Px(ix-1,iy+1,2)*d1-Py(ix-1,iy+1,2)*d2
        OK2(ix,iy) = 1

      elseif (intorno14(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy-1,2)-Px(ix+2,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy-1,2)-Py(ix+2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy-1,2)-A(ix+2,iy-2,2))*dampxy
        A(ix,iy,2) = A(ix+1,iy-1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy-1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix+1,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix+1,iy-1,2)-Pxmed          *d1+ Pymed          *d2
C       T(ix,iy,2) = T(ix+1,iy-1,2)-Px(ix+1,iy-1,2)*d1+ Py(ix+1,iy-1,2)*d2
        OK2(ix,iy) = 1

      elseif (intorno13(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy-1,2)-Px(ix,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy-1,2)-Py(ix,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy-1,2)-A(ix,iy-2,2))*dampy
        A(ix,iy,2) = A(ix,iy-1,2)*dampy         
        Pymed      = (Py(ix,iy,2)+Py(ix,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix,iy-1,2)+Pymed        *d2
C       T(ix,iy,2) = T(ix,iy-1,2)+Py(ix,iy-1,2)*d2
        OK2(ix,iy) = 1

      elseif (intorno11(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then
      
        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy,2)-Px(ix-2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy,2)-Py(ix-2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy,2)-A(ix-2,iy,2))*dampx
        A(ix,iy,2) = A(ix-1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy,2))/2.0E0       
        T(ix,iy,2) = T(ix-1,iy,2)+Pxmed        *d1
C       T(ix,iy,2) = T(ix-1,iy,2)+Px(ix-1,iy,2)*d1
        OK2(ix,iy) = 1

C     elseif (intorno12(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

C       Px(ix,iy,2)= 2.0E0*Px(ix-1,iy-1,2)-Px(ix-2,iy-2,2)
C       Py(ix,iy,2)= 2.0E0*Py(ix-1,iy-1,2)-Py(ix-2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy-1,2)-A(ix-2,iy-2,2))*dampxy
C       A(ix,iy,2) = A(ix-1,iy-1,2)*dampxy
C       Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy-1,2))/2.0E0
C       Pymed      = (Py(ix,iy,2)+Py(ix-1,iy-1,2))/2.0E0
C       T(ix,iy,2) = T(ix-1,iy-1,2)+Pxmed          *d1+ Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy-1,2)+Px(ix-1,iy-1,2)*d1+ Py(ix-1,iy-1,2)*d2
C       OK2(ix,iy) = 1
          
      endif           

      if ( OK2(ix,iy).eq.0 ) then
        Px(ix,iy,2)= -2
        Py(ix,iy,2)= -2
        A(ix,iy,2) =  0
        T(ix,iy,2) = -2
      endif  


      RETURN
      END
****************************************************************
****************************************************************


 
****************************************************************
****************************************************************
      SUBROUTINE extrapolQ2(ix,iy,n1,n2,n3,        ! feb 2002
     &            Nxmin,Nxmax,Nymin,Nymax,
     &            Px,Py,A,OK2,dampx,dampy,dampxy,T,d1,d2)

      implicit none

      integer*4 ix,iy
      integer*4 n1,n2,n3
      integer*4 Nxmin,Nxmax,Nymin,Nymax

      real*4    Px(n1,n2,2),Py(n1,n2,2)
      real*4    Pxmed,Pymed      
      real*4    A(n1,n2,2),T(n1,n2,2)

      integer*4 OK2(n1,n2)

      real*4 d1,d2,dampx,dampy,dampxy

      integer*4 intorno7,intorno8,intorno9,intorno10,intorno11,intorno12
      integer*4 intorno13, intorno14, intorno15, intorno16, intorno17
      integer*4 intorno18, intorno19, intorno20, intorno21, intorno22

      if (intorno14(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy-1,2)-Px(ix+2,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy-1,2)-Py(ix+2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy-1,2)-A(ix+2,iy-2,2))*dampxy
        A(ix,iy,2) = A(ix+1,iy-1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy-1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix+1,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix+1,iy-1,2)-Pxmed          *d1+ Pymed          *d2          
C       T(ix,iy,2) = T(ix+1,iy-1,2)-Px(ix+1,iy-1,2)*d1+ Py(ix+1,iy-1,2)*d2
        OK2(ix,iy) = 1
          
      elseif (intorno7(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy,2)-Px(ix+2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy,2)-Py(ix+2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy,2)-A(ix+2,iy,2))*dampx
        A(ix,iy,2) = A(ix+1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy,2))/2.0E0
        T(ix,iy,2) = T(ix+1,iy,2)-Pxmed        *d1        
C       T(ix,iy,2) = T(ix+1,iy,2)-Px(ix+1,iy,2)*d1
        OK2(ix,iy) = 1

      elseif (intorno13(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy-1,2)-Px(ix,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy-1,2)-Py(ix,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy-1,2)-A(ix,iy-2,2))*dampy
        A(ix,iy,2) = A(ix,iy-1,2)*dampy         
        Pymed      = (Py(ix,iy,2)+Py(ix,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix,iy-1,2)+Pymed        *d2
C       T(ix,iy,2) = T(ix,iy-1,2)+Py(ix,iy-1,2)*d2
        OK2(ix,iy) = 1

      elseif (intorno8(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix+1,iy+1,2)-Px(ix+2,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix+1,iy+1,2)-Py(ix+2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix+1,iy+1,2)-A(ix+2,iy+2,2))*dampxy
        A(ix,iy,2) = A(ix+1,iy+1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix+1,iy+1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix+1,iy+1,2))/2.0E0         
        T(ix,iy,2) =  T(ix+1,iy+1,2)-Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) =  T(ix+1,iy+1,2)-Px(ix+1,iy+1,2)*d1-Py(ix+1,iy+1,2)*d2
        OK2(ix,iy) = 1

      elseif (intorno12(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy-1,2)-Px(ix-2,iy-2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy-1,2)-Py(ix-2,iy-2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy-1,2)-A(ix-2,iy-2,2))*dampxy
        A(ix,iy,2) = A(ix-1,iy-1,2)*dampxy
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy-1,2))/2.0E0
        Pymed      = (Py(ix,iy,2)+Py(ix-1,iy-1,2))/2.0E0
        T(ix,iy,2) = T(ix-1,iy-1,2)+Pxmed          *d1+ Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy-1,2)+Px(ix-1,iy-1,2)*d1+ Py(ix-1,iy-1,2)*d2
        OK2(ix,iy) = 1

        
      elseif (intorno11(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then
      
        Px(ix,iy,2)= 2.0E0*Px(ix-1,iy,2)-Px(ix-2,iy,2)
        Py(ix,iy,2)= 2.0E0*Py(ix-1,iy,2)-Py(ix-2,iy,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy,2)-A(ix-2,iy,2))*dampx
        A(ix,iy,2) = A(ix-1,iy,2)*dampx
        Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy,2))/2.0E0       
        T(ix,iy,2) = T(ix-1,iy,2)+Pxmed        *d1
C       T(ix,iy,2) = T(ix-1,iy,2)+Px(ix-1,iy,2)*d1
        OK2(ix,iy) = 1

      elseif (intorno9(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

        Px(ix,iy,2)= 2.0E0*Px(ix,iy+1,2)-Px(ix,iy+2,2)
        Py(ix,iy,2)= 2.0E0*Py(ix,iy+1,2)-Py(ix,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix,iy+1,2)-A(ix,iy+2,2))*dampy
        A(ix,iy,2) = A(ix,iy+1,2)*dampy
        Pymed      = (Py(ix,iy,2)+Py(ix,iy+1,2))/2.0E0        
        T(ix,iy,2) = T(ix,iy+1,2)-Pymed        *d2
C       T(ix,iy,2) = T(ix,iy+1,2)-Py(ix,iy+1,2)*d2
        OK2(ix,iy) = 1
          
C     elseif (intorno10(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2).eq.1) then

C       Px(ix,iy,2)= 2.0E0*Px(ix-1,iy+1,2)-Px(ix-2,iy+2,2)
C       Py(ix,iy,2)= 2.0E0*Py(ix-1,iy+1,2)-Py(ix-2,iy+2,2)
C       A(ix,iy,2) = (2.0E0*A(ix-1,iy+1,2)-A(ix-2,iy+2,2))*dampxy
C       A(ix,iy,2) = A(ix-1,iy+1,2)*dampxy
C       Pxmed      = (Px(ix,iy,2)+Px(ix-1,iy+1,2))/2.0E0
C       Pymed      = (Py(ix,iy,2)+Py(ix-1,iy+1,2))/2.0E0        
C       T(ix,iy,2) = T(ix-1,iy+1,2)+Pxmed          *d1-Pymed          *d2
C       T(ix,iy,2) = T(ix-1,iy+1,2)+Px(ix-1,iy+1,2)*d1-Py(ix-1,iy+1,2)*d2
C       OK2(ix,iy) = 1

      endif           

      if ( OK2(ix,iy).eq.0 ) then
        Px(ix,iy,2)= -2
        Py(ix,iy,2)= -2
        A(ix,iy,2) =  0
        T(ix,iy,2) = -2
      endif  


      RETURN
      END
****************************************************************
****************************************************************



****************************************************************
****************************************************************
      SUBROUTINE xcheck(ox, ex, Nxmin, Nxmax) 

      integer*4 ox,ex     
      integer*4 Nxmin,Nxmax

       if (ox .lt. Nxmin) then
       	  ox = Nxmin
       elseif (ox .gt. Nxmax) then
       	  ox = Nxmax
       endif
 
       if (ex .lt. Nxmin ) then
       	  ex = Nxmin
       elseif (ex .gt. Nxmax ) then
       	  ex = Nxmax
       endif
      
      RETURN
      END 
****************************************************************
****************************************************************

      
****************************************************************
****************************************************************
      SUBROUTINE ycheck(oy, ey, Nymin, Nymax) 
     
      integer*4 oy,ey
      integer*4 Nymin,Nymax
     
       if (oy .lt. Nymin) then
       	  oy = Nymin
       endif
       if (oy .gt. Nymax) then
       	  oy = Nymax
       endif

       if (ey .lt. Nymin ) then
       	  ey = Nymin
       endif
       if (ey .gt. Nymax ) then
       	  ey = Nymax
       endif
       
      RETURN
      END 
****************************************************************
****************************************************************


****************************************************************
****************************************************************
* Controlla se risulta definito l'intorno
* di tipo 1 per il punto di coordinate (ix,iy)
*
*         -o---+---o-
*          |   |   |    +-+
*       iy-+---x---+-   |1|
*          |   |   |    +-+
*         -o---+---o-
*              ix
*
      INTEGER*4 FUNCTION intorno1(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)


      implicit none
           
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno1=0

      if ( ((ix-1).ge.Nxmin).AND.((ix+1).le.Nxmax).AND.
     &     ((iy-1).ge.Nymin).AND.((iy+1).le.Nymax) ) then

         if ( (OK2(ix+1,iy+1).ge.1).AND.(OK2(ix+1,iy-1).ge.1).AND.
     &        (OK2(ix-1,iy-1).ge.1).AND.(OK2(ix-1,iy+1).ge.1) ) then

            intorno1=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************

      

****************************************************************
****************************************************************
* Controlla se risulta definito l'intorno
* di tipo 2 per il punto di coordinate (ix,iy)
*
*         -+---o---+-
*          |   |   |      +-+ 
*       iy-o---x---o-     |2|
*          |   |   |      +-+
*         -+---o---+-
*              ix
*

      INTEGER*4 FUNCTION intorno2(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno2=0

      if ( ((ix-1).ge.Nxmin).AND.((ix+1).le.Nxmax).AND.
     &     ((iy-1).ge.Nymin).AND.((iy+1).le.Nymax) ) then

         if ( (OK2(ix-1,iy).ge.1).AND.(OK2(ix+1,iy).ge.1).AND.
     &        (OK2(ix,iy-1).ge.1).AND.(OK2(ix,iy+1).ge.1) ) then

            intorno2=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 3 per il punto di coordinate (ix,iy)
*
*         -+---+---o-
*          |   |   |      +-+ 
*       iy-+---x---+-     |3|
*          |   |   |      +-+
*         -o---+---+-
*              ix
*

      INTEGER*4 FUNCTION intorno3(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno3=0

      if ( ((ix-1).ge.Nxmin).AND.((ix+1).le.Nxmax).AND.
     &     ((iy-1).ge.Nymin).AND.((iy+1).le.Nymax) ) then

         if ( (OK2(ix-1,iy-1).ge.1).AND.(OK2(ix+1,iy+1).ge.1) ) then

            intorno3=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************



      
*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 4 per il punto di coordinate (ix,iy)
*
*         -o---+---+-
*          |   |   |      +-+ 
*       iy-+---x---+-     |4|
*          |   |   |      +-+
*         -+---+---o-
*              ix
*

      INTEGER*4 FUNCTION intorno4(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno4=0

      if ( ((ix-1).ge.Nxmin).AND.((ix+1).le.Nxmax).AND.
     &     ((iy-1).ge.Nymin).AND.((iy+1).le.Nymax) ) then

         if ( (OK2(ix-1,iy+1).ge.1).AND.(OK2(ix+1,iy-1).ge.1) ) then

            intorno4=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************




*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 5 per il punto di coordinate (ix,iy)
*
*         -+---+---+-
*          |   |   |      +-+ 
*       iy-o---x---o-     |5|
*          |   |   |      +-+
*         -+---+---+-
*              ix
*

      INTEGER*4 FUNCTION intorno5(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno5=0

      if ( ((ix-1).ge.Nxmin).AND.((ix+1).le.Nxmax) ) then

         if ( (OK2(ix-1,iy).ge.1).AND.(OK2(ix+1,iy).ge.1) ) then

            intorno5=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************




*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 6 per il punto di coordinate (ix,iy)
*
*         -+---o---+-
*          |   |   |      +-+ 
*       iy-+---x---+-     |6|
*          |   |   |      +-+
*         -+---o---+-
*              ix
*

      INTEGER*4 FUNCTION intorno6(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno6=0

      if ( ((iy-1).ge.Nymin).AND.((iy+1).le.Nymax) ) then

         if ( (OK2(ix,iy-1).ge.1).AND.(OK2(ix,iy+1).ge.1) ) then

            intorno6=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************



****************************************************************
****************************************************************
* Controlla se risulta definito l'intorno
* di tipo 1a per il punto di coordinate (ix,iy)
*
*         o---+---+---+---o
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |      
*       iy+---+---x---+---+     
*         |   |   |   |   |     
*         +---+---+---+---+
*         |   |   |   |   |
*         o---+---+---+---o
*                ix
*
      INTEGER*4 FUNCTION intorno1a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)


      implicit none
           
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno1a=0

      if ( ((ix-2).ge.Nxmin).AND.((ix+2).le.Nxmax).AND.
     &     ((iy-2).ge.Nymin).AND.((iy+2).le.Nymax) ) then

         if ( (OK2(ix+2,iy+2).ge.1).AND.(OK2(ix+2,iy-2).ge.1).AND.
     &        (OK2(ix-2,iy-2).ge.1).AND.(OK2(ix-2,iy+2).ge.1) ) then

            intorno1a=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************

      

****************************************************************
****************************************************************
* Controlla se risulta definito l'intorno
* di tipo 2a per il punto di coordinate (ix,iy)
*
*         +---+---o---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |      
*      iy o---+---x---+---o     
*         |   |   |   |   |     
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---o---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno2a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno2a=0

      if ( ((ix-2).ge.Nxmin).AND.((ix+2).le.Nxmax).AND.
     &     ((iy-2).ge.Nymin).AND.((iy+2).le.Nymax) ) then

         if ( (OK2(ix-2,iy).ge.1).AND.(OK2(ix+2,iy).ge.1).AND.
     &        (OK2(ix,iy-2).ge.1).AND.(OK2(ix,iy+2).ge.1) ) then

            intorno2a=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 3a per il punto di coordinate (ix,iy)
*
*         +---+---+---+---o
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |      
*      iy +---+---x---+---+     
*         |   |   |   |   |     
*         +---+---+---+---+
*         |   |   |   |   |
*         o---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno3a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno3a=0

      if ( ((ix-2).ge.Nxmin).AND.((ix+2).le.Nxmax).AND.
     &     ((iy-2).ge.Nymin).AND.((iy+2).le.Nymax) ) then

         if ( (OK2(ix-2,iy-2).ge.1).AND.(OK2(ix+2,iy+2).ge.1) ) then

            intorno3a=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************


  
*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 4a per il punto di coordinate (ix,iy)
*
*         o---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |      
*      iy +---+---x---+---+     
*         |   |   |   |   |     
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---o
*                ix
*

      INTEGER*4 FUNCTION intorno4a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno4a=0

      if ( ((ix-2).ge.Nxmin).AND.((ix+2).le.Nxmax).AND.
     &     ((iy-2).ge.Nymin).AND.((iy+2).le.Nymax) ) then

         if ( (OK2(ix-2,iy+2).ge.1).AND.(OK2(ix+2,iy-2).ge.1) ) then

            intorno4a=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************




*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 5a per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |      
*      iy o---+---x---+---o     
*         |   |   |   |   |     
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno5a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno5a=0

      if ( ((ix-2).ge.Nxmin).AND.((ix+2).le.Nxmax) ) then

         if ( (OK2(ix-2,iy).ge.1).AND.(OK2(ix+2,iy).ge.1) ) then

            intorno5a=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************




*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 6a per il punto di coordinate (ix,iy)
*
*         +---+---o---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |      
*      iy +---+---x---+---+     
*         |   |   |   |   |     
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---o---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno6a(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno6a=0

      if ( ((iy-2).ge.Nymin).AND.((iy+2).le.Nymax) ) then

         if ( (OK2(ix,iy-2).ge.1).AND.(OK2(ix,iy+2).ge.1) ) then

            intorno6a=1
         
         endif

      endif

      RETURN
      END
****************************************************************
****************************************************************


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 7 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +-+ 
*       iy+---+---x---o---o     |7|
*         |   |   |   |   |     +-+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno7(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno7=0

      if ( (ix+2).le.Nxmax ) then

         if ( (OK2(ix+1,iy).gt.1).AND.(OK2(ix+2,iy).gt.1) ) then

            intorno7=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 8 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---o
*         |   |   |   |   |
*         +---+---+---o---+
*         |   |   |   |   |     +-+ 
*       iy+---+---x---+---+     |8|
*         |   |   |   |   |     +-+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno8(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno8=0

      if ( ((iy+2).le.Nymax) .AND. ((ix+2).le.Nxmax) ) then

         if ( (OK2(ix+1,iy+1).gt.1).AND.(OK2(ix+2,iy+2).gt.1) ) then

            intorno8=1
         
         endif

      endif

      RETURN
      END



*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 9 per il punto di coordinate (ix,iy)
*
*         +---+---o---+---+
*         |   |   |   |   |
*         +---+---o---+---+
*         |   |   |   |   |     +-+ 
*       iy+---+---x---+---+     |9|
*         |   |   |   |   |     +-+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno9(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno9=0

      if ( (iy+2).le.Nymax) then

         if ( (OK2(ix,iy+1).gt.1).AND.(OK2(ix,iy+2).gt.1) ) then

            intorno9=1
         
         endif

      endif

      RETURN
      END

*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 10 per il punto di coordinate (ix,iy)
*
*         o---+---+---+---+
*         |   |   |   |   |
*         +---o---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |10|
*         |   |   |   |   |     +--+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno10(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno10=0

      if ( ((iy+2).le.Nymax) .AND. ((ix-2).ge.Nxmin) ) then

         if ( (OK2(ix-1,iy+1).gt.1).AND.(OK2(ix-2,iy+2).gt.1) ) then

            intorno10=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 11 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +--+ 
*       iyo---o---x---+---+     |11|
*         |   |   |   |   |     +--+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno11(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno11=0

      if ((ix-2).ge.Nxmin) then

         if ( (OK2(ix-1,iy).gt.1).AND.(OK2(ix-2,iy).gt.1) ) then

            intorno11=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 12 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |12|
*         |   |   |   |   |     +--+
*         +---o---+---+---+
*         |   |   |   |   |
*         o---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno12(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno12=0

      if ( ((iy-2).ge.Nymin) .AND. ((ix-2).ge.Nxmin) ) then

         if ( (OK2(ix-1,iy-1).gt.1).AND.(OK2(ix-2,iy-2).gt.1) ) then

            intorno12=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 13 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |13|
*         |   |   |   |   |     +--+
*         +---+---o---+---+
*         |   |   |   |   |
*         +---+---o---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno13(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno13=0

      if ( (iy-2).ge.Nymin)  then

         if ( (OK2(ix,iy-1).gt.1).AND.(OK2(ix,iy-2).gt.1) ) then

            intorno13=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 14 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |14|
*         |   |   |   |   |     +--+
*         +---+---+---o---+
*         |   |   |   |   |
*         +---+---+---+---o
*                ix
*

      INTEGER*4 FUNCTION intorno14(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno14=0

      if ( ((iy-2).ge.Nymin) .AND. ((ix+2).le.Nxmax) ) then

         if ( (OK2(ix+1,iy-1).gt.1).AND.(OK2(ix+2,iy-2).gt.1) ) then

            intorno14=1
         
         endif

      endif

      RETURN
      END

*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 15 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---o---+     |15|
*         |   |   |   |   |     +--+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno15(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno15=0

      if ((ix+1).le.Nxmax) then

         if (OK2(ix+1,iy).gt.1) then

            intorno15=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 16 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---o---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |16|
*         |   |   |   |   |     +--+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno16(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno16=0

      if ( ((iy+1).le.Nymax) .AND.
     &     ((ix+1).le.Nxmax) ) then

         if (OK2(ix+1,iy+1).gt.1) then

            intorno16=1
         
         endif

      endif

      RETURN
      END



*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 17 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---o---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |17|
*         |   |   |   |   |     +--+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno17(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno17=0

      if ((iy+1).le.Nymax) then

         if (OK2(ix,iy+1).gt.1) then

            intorno17=1
         
         endif

      endif

      RETURN
      END

*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 18 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---o---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |18|
*         |   |   |   |   |     +--+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno18(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno18=0

      if ( ((iy+1).le.Nymax) .AND.
     &     ((ix-1).ge.Nxmin) ) then

         if (OK2(ix-1,iy+1).gt.1) then

            intorno18=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 19 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---o---x---+---+     |19|
*         |   |   |   |   |     +--+
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno19(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno19=0

      if ((ix-1).ge.Nxmin) then

         if (OK2(ix-1,iy).gt.1) then

            intorno19=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 20 per il punto di coordinate (ix,iy)
*
*         +---+---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*         |   |   |   |   |     +--+ 
*       iy+---+---x---+---+     |20|
*         |   |   |   |   |     +--+
*         +---o---+---+---+
*         |   |   |   |   |
*         +---+---+---+---+
*                ix
*

      INTEGER*4 FUNCTION intorno20(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno20=0

      if ( ((iy-1).ge.Nymin) .AND.
     &     ((ix-1).ge.Nxmin) ) then

         if (OK2(ix-1,iy-1).gt.1) then

            intorno20=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 21 per il punto di coordinate (ix,iy)
*
*         +---+---+
*         |   |   |     +--+ 
*       iy+---x---+     |21|
*         |   |   |     +--+
*         +---o---+
*            ix
*

      INTEGER*4 FUNCTION intorno21(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno21=0

      if ((iy-1).ge.Nymin) then

         if (OK2(ix,iy-1).gt.1) then

            intorno21=1
         
         endif

      endif

      RETURN
      END


*************************************************************
* Controlla se risulta definito l'intorno
* di tipo 22 per il punto di coordinate (ix,iy)
*
*         +---+---+
*         |   |   |     +--+ 
*       iy+---x---+     |22|
*         |   |   |     +--+
*         +---+---o
*            ix
*

      INTEGER*4 FUNCTION intorno22(ix,iy,Nxmin,Nxmax,Nymin,Nymax,OK2,n1,n2)
     
      integer*4 ix,iy
      integer*4 Nxmin,Nxmax
      integer*4 Nymin,Nymax
      integer*4 n1,n2
      integer*4 OK2(n1,n2)
     
      intorno14a=0

      if ( ((iy-1).ge.Nymin) .AND.
     &     ((ix+1).le.Nxmax) ) then

         if (OK2(ix+1,iy-1).gt.1) then

            intorno22=1
         
         endif

      endif

      RETURN
      END


