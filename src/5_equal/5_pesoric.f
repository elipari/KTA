       subroutine pesoric (xric,yric,peso,lung,distx,disty,maxlen)
       implicit none
       integer lung,cellex,celley,i,j,n,cont,maxlen
       integer r1,r2,c1,c2,indx,indy,posx,posy
       real*4 distx,disty,val,xmin,ymin,xmax,ymax
       real*4 xric(maxlen),yric(maxlen),peso(maxlen)
       integer griglia(200,200)
       real*4 area(200,200)
*      *********************************************
*      Versione 13/1/2000
*      autore Ratti Davide
*      Calcola il peso da dare ad ogni ricevitore
*      dello shot posto in XRIC,YRIC.DISTX,DISTY
*      sono le dimensioni della cella della griglia
*      LUNG e' la lunghezza del vettore,MAXLEN e' la
*      lunghezza max(serve x l'allocazione statica della
*      memoria.
*      Il risultato e' passato in PESO
*      Uses:Tooldav.f
*      ********************************************
       call minvett(xric,lung,xmin)
       call maxvett(xric,lung,xmax)
       call minvett(yric,lung,ymin)
       call maxvett(yric,lung,ymax)
       xmin=xmin-(distx/2.0)
       ymin=ymin-(disty/2.0)
*      ymax=ymax+real(disty)/2.0
*       xmax=xmax+real(distx)/2.0
       cellex=int((xmax-xmin)/distx)+1
       celley=int((ymax-ymin)/disty)+1
*      write(*,*) cellex,celley
*       *******************************************
*      INIZIALIZZO L'AREA E LA GRIGLIA
       do 5 i=1,cellex
       do 4 j=1,celley
       area(i,j)=1.0
       griglia(i,j)=0
 4     continue
 5     continue
*      *****************************************
*       INIZIALIZZO IL RISULTATO (PESO)
       do 7 i=1,maxlen
       peso(i)=0.0
 7      continue
*       *******************************
*      RIEMPIO LA GRIGLIA
       do 10 i=1,lung
       posx=int((xric(i)-xmin)/real(distx))+1
       posy=int((yric(i)-ymin)/real(disty))+1
       if (posx.GT.cellex) then
       posx=cellex
       endif
       if (posy.GT.celley) then
       posy=celley
       endif
       griglia(posx,posy)=griglia(posx,posy)+1
 10    continue
*      ******************************
*      SCANSIONE DI TUTTE LE CELLE
       do 41 i=1,cellex
       do 40 j=1,celley
*$       write(*,*) i,j,griglia(i,j)
       if  (griglia(i,j).EQ.0) then
       cont=0
         n=1
*      Ciclo per le celle vuote
*************************************
  12      r1=max(1,i-n)
          r2=min(cellex,i+n)
          c1=max(1,j-n)
          c2=min(celley,j+n)
          do 15 indx=r1,r2
             if (griglia(indx,c1).NE.0) then
             cont=cont+1
             endif
             if (griglia(indx,c2).NE.0) then
             cont=cont+1
             endif
 15       continue
          do 20 indy=(c1+1),(c2-1)
             if (griglia(r1,indy).NE.0) then
             cont=cont+1
             endif
             if (griglia(r2,indy).NE.0) then
             cont=cont+1
             endif
 20       continue
          n=n+1
****************************************
        if (cont.EQ.0) then
       goto 12
        endif
*      aggiorno le aree delle celle toccate

        val=1.0/real(cont)
       do 25 indx=r1,r2
          if (griglia(indx,c1).NE.0) then
          area(indx,c1)=area(indx,c1)+val
          endif
          if (griglia(indx,c2).NE.0) then
          area(indx,c2)=area(indx,c2)+val
          endif
 25    continue
       do 30 indy=(c1+1),(c2-1)
          if (griglia(r1,indy).NE.0) then
          area(r1,indy)=area(r1,indy)+val
          endif
          if (griglia(r2,indy).NE.0) then
          area(r2,indy)=area(r2,indy)+val
          endif
 30    continue
************************************
       endif
 40    continue
 41    continue
***************************************
*      CALCOLO IL PESO DI OGNI RICEVITORE
       do 60 i=1,lung
       posx=int((xric(i)-xmin)/real(distx))+1
       posy=int((yric(i)-ymin)/real(disty))+1

       if (posx.GT.cellex) then
       posx=cellex
       endif
       if (posy.GT.celley) then
       posy=celley
       endif
       peso(i)=area(posx,posy)/real(griglia(posx,posy))
       if (peso(i).GT.cellex*celley*0.1) then
       peso(i)=real(cellex*celley)*0.1
       endif
 60    continue
       return
       end



       subroutine minvett (vett,leng,mini)
       implicit none
       integer leng,i
       real*4 mini
       real*4 vett(leng)
*      ********************************
*      Versione 16/12/99
*      Autore Ratti Davide
*      calcola il minimo di un vettore
*      VETT di lunghezza LENG
*      ********************************
       mini=vett(1)
       do 10 i=1,leng
       if (vett(i).LT.mini) then
       mini=vett(i)
       endif
 10    continue
       return
       end



       subroutine ruota(x,y,xr,yr,A11,A12,A21,A22,B1,B2)
       implicit none
       real*4 x,y,xr,yr
       real*4 A11,A12,A21,A22,B1,B2
*      ********************************
*      Versione 16/12/99
*      Autore Ratti Davide
*      Ruota il punto X,Y di ALFA gradi
*      ********************************


       x=xr*A11+yr*A12+B1
       y=xr*A21+yr*A22+B2
       return
         end




       subroutine maxvett (vett,leng,maxi)
       implicit none
       integer leng,i
       real*4 maxi
       real*4 vett(leng)
*      ********************************
*      Versione 16/12/99
*      Autore Ratti Davide
*      calcola il massimo di un vettore
*      VETT di lunghezza LENG
*      ********************************
       maxi=vett(1)
       do 20 i=1,leng
       if (maxi.LT.vett(i)) then
       maxi=vett(i)
       endif
 20    continue
       return
       end

