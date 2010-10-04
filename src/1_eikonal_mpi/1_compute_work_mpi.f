      subroutine compute_work(myrank, size, Nshot, period, ista, iend, mywork)

CCC 05/06/2002 - Subroutine che, dati Nshot:
CCC 1 - Divide in blocchi di shot di dimensione period e li distribuisce in modalita'
CCC     ciclica fra i comp PES
CCC 2 - Eventuali shot restanti vengono distribuiti singolarmente fra i comp PEs

CCC Input: ID PE, Numero PE, Numero shot, Dimensione blocco elementare
CCC Output: ista(i), iend(i) = Il PE myrank deve elaborare ista(i), ista(i)+1, ...., iend(i),
CCC                                                        ista(i+1), ista(i+1)+1, ...., iend(i+1),
CCC                                                        .... con i=1, 2, ...., mywork

      implicit none

      integer myrank, size, Nshot, period, mywork
      integer i, restNshot, myworkaus, offset
      integer ista(Nshot), iend(Nshot)

CCC 05/06/2002 Primo step

      mywork = INT(Nshot/(period*size))
      do 10 i=1, mywork
         ista(i) = myrank * period + ( (i - 1) * size ) * period + 1 
         iend(i) = ista(i) + period - 1
 10   continue 

CCC 05/06/2002 Secondo step

      restNshot = MOD(Nshot, (period*size))
      myworkaus = INT(restNshot / size)
      if((myrank+1) .LE. MOD(restNshot, size)) then
         myworkaus = myworkaus + 1      
      endif

      if(mywork .NE. 0) then
         offset = period * size * mywork + 1
      else
         offset = 1
      endif

      do 20 i=1, myworkaus
         ista(i+mywork) = offset + myrank + (i - 1) * size
         iend(i+mywork) = ista(i+mywork) 
 20   continue
      mywork = mywork + myworkaus

      end

