      subroutine derivxyvel(n1,n2,n3,d1,d2,S,Sx,Sy)

*---------------------------------------------------------------------
*
* Derivazione per la versione che minimizza il tempo di calcolo.
* Subroutine che calcola le derivate parziali prime della
* slowness nelle due direzioni x ed y. Utilizzo una semplice
* formula approssimante su 3 campioni (2 ai bordi).
*
* INPUT:
* n1,n2,n3:  dimensioni del cubo di velocita`
* d1,d2,d3:  passo di campionamento lungo i tre assi (m)
* S:         cubo della slowness (s/m)
*
* OUTPUT:
* Sx,Sy:     derivate numeriche della slowness (s/m^2)
*
* FILE CHIAMANTI:
* start.f
*
*  Bonolo Ivan - Maggio 1999
*--------------------------------------------------------------------- 

      implicit none

      integer n1,n2,n3
      real*4  d1,d2
      real*4  S(n1,n2,n3)
      real*4  Sx(n1,n2,n3),Sy(n1,n2,n3)

* Variabili locali
      integer ix,iy,iz

* DERIVAZIONE
* La derivazione viene effettuata con il rapporto incrementale.
*            o 
* S         /|
* ^        o |
* +>      /| |
* x,y    o | |
*        | | |
*   -----o-x-o--------
*        d*

* DERIVATE LUNGO LA DIREZIONE X
* Prime ed ultime colonne
      do 10 iz=1,n3
        do 20 iy=1,n2
          Sx(1,iy,iz)=(S(2,iy,iz)-S(1,iy,iz))/d1
          Sx(n1,iy,iz)=(S(n1,iy,iz)-S(n1-1,iy,iz))/d1
20      continue
10    continue
* Colonne centrali
      do 30 iz=1,n3
        do 40 iy=1,n2
          do 50 ix=2,n1-1
            Sx(ix,iy,iz)=(S(ix+1,iy,iz)-S(ix-1,iy,iz))/(2.0E0*d1)
50        continue
40      continue
30    continue

* DERIVATE LUNGO LA DIREZIONE Y
* Prime ed ultime colonne
      do 60 iz=1,n3
        do 70 ix=1,n1
          Sy(ix,1,iz)=(S(ix,2,iz)-S(ix,1,iz))/d2
          Sy(ix,n2,iz)=(S(ix,n2,iz)-S(ix,n2-1,iz))/d2
70      continue
60    continue
* Colonne centrali
      do 80 iz=1,n3
        do 90 iy=2,n2-1
          do 100 ix=1,n1
            Sy(ix,iy,iz)=(S(ix,iy+1,iz)-S(ix,iy-1,iz))/(2.0E0*d2)
100       continue
90      continue
80    continue

      RETURN
      END

