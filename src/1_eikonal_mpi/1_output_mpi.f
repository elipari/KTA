*---------------------------------------------------------------------
*     SUBROUTINE DATAOUTPUT
*---------------------------------------------------------------------
      subroutine dataoutput(o1,o2,o3,d1,d2,d3,n1,n2,n3,esize,
     &     UPDOWN,
     &     Nshot, xshot, yshot, zshot,
     &     TARGET, Ntgt, 
     &     A, T, Px, Py, Pz,
     &     OutDir, LenOutDir, OUTPUT, OUT_A, OUT_T,
     &     OUT_UX,OUT_UY,OUT_UZ, is, 
     &     NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ, rangeA, rangeT, rangeCz,
     &     writeBufferFloat, writeBufferShort, 
     &     dimWriteBuf, shotSort, nShotMem, shotInBuf, nShotWri, index_cur,
     &     index_A, index_T, index_UX, index_UY, index_UZ, QUANT_FLAG)


      implicit none

*     Parametri
      integer*4 MAXSTR
      parameter (MAXSTR=200)

*     Variabili in input
      real*4    o1,o2,o3
      real*4    d1,d2,d3
      integer*4 n1,n2,n3
      integer*4 esize

      integer*4 UPDOWN

      integer*4 Nshot
      real*4    xshot(Nshot)
      real*4    yshot(Nshot)
      real*4    zshot(Nshot)

      integer*4 TARGET
      integer*4 Ntgt
CC 10/07/2002 Merging
      integer*4 NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ

      real*4    A(NtgtAA),   T(NtgtTT)
      real*4    Px(NtgtPPX),  Py(NtgtPPY),  Pz(NtgtPPZ)

      character*(MAXSTR) OutDir
      integer*4 LenOutDir
      integer*4 OUTPUT
      integer*4 OUT_A
      integer*4 OUT_T
      integer*4 OUT_UX,OUT_UY,OUT_UZ
      integer*4 index_A, index_T, index_UX, index_UY, index_UZ

      integer*4 is
!CAN&BN: gestione della scrittura SERIALE
      INTEGER*4 dimWriteBuf, shotInBuf, nShotMem, nShotWri, index_cur, QUANT_FLAG
      REAL*4     writeBufferFloat(Ntgt,index_cur*shotInBuf)
      INTEGER*2  writeBufferShort(Ntgt,index_cur*shotInBuf*2)
      INTEGER*4 shotSort(Nshot)

*     Variabili locali
      integer*8 is_8, n1_8, n2_8, n3_8, size_real_8, Ntgt_8, size_sint_8
      real*4 memo
      integer*4 ix,iy,iz,ii
      integer*4 fidT,fidA,fidPx,fidPy,fidPz
      character*8 nome
      integer*4 kl
      integer*4 lenHT, lenDT, lenHA, lenDA
      integer*4 lenHPx, lenDPx, lenHPy, lenDPy
      integer*4 lenHPz, lenDPz
      character*(MAXSTR) HeaderFileT,DataFileT
      character*(MAXSTR) HeaderFileA,DataFileA
      character*(MAXSTR) HeaderFilePx,DataFilePx
      character*(MAXSTR) HeaderFilePy,DataFilePy
      character*(MAXSTR) HeaderFilePz,DataFilePz
      character*(MAXSTR) title, fileName
      
      integer*4 size_real, size_sint
      real*4 size_test
      integer*4 file_error
      
! CAN&BN: gestione della scrittura SERIALE
      INTEGER*4 base, kk, numElem      

*     ROB variabile I/O state

      integer*4 IOstate  
      data IOstate /1/    
      
      SAVE IOstate

*     ROB variabile I/O state

      integer*2 size_test_sint
      real*4 rangeA(Nshot*2), rangeT(Nshot*2), rangeCz(Nshot*2)



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!     Calcolo dimensione di REAL*4
      CALL glob_sizereal4(size_test, size_real)
      
!     Calcolo dimensione di INTEGER*2
      CALL glob_sizeshortint(size_test_sint, size_sint)

! Conversioni in formato integer*8
      n1_8 = n1
      n2_8 = n2
      n3_8 = n3
      is_8 = is
      Ntgt_8 = Ntgt
      size_real_8 = size_real
      size_sint_8 = size_sint

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCC OUTPUT = 2
      if (OUTPUT .eq. 2) then

         kl=LenOutDir

         fidT=2
         HeaderFileT=OutDir(1:kl)//'DBt.H'
         DataFileT=OutDir(1:kl)//'DBt.H@'
         lenHT = kl+5
         lenDT = kl+6

         fidA=3
         HeaderFileA=OutDir(1:kl)//'DBa.H'
         DataFileA=OutDir(1:kl)//'DBa.H@'
         lenHA = kl+5
         lenDA = kl+6


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC OUTPUT = 2, UPDOWN = 1
         if (UPDOWN .EQ. 1) then

            fidPx=4
            HeaderFilePx=OutDir(1:kl)//'DBvx.H'
            DataFilePx=OutDir(1:kl)//'DBvx.H@'
            lenHPx = kl+6
            lenDPx = kl+7

            fidPy=5
            HeaderFilePy=OutDir(1:kl)//'DBvy.H'
            DataFilePy=OutDir(1:kl)//'DBvy.H@'
            lenHPy = kl+6
            lenDPy = kl+7

            fidPz=7
            HeaderFilePz=OutDir(1:kl)//'DBvz.H'
            DataFilePz=OutDir(1:kl)//'DBvz.H@'
            lenHPz = kl+6
            lenDPz = kl+7


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC OUTPUT = 2, UPDOWN = 2
         else

            fidPx=4
            HeaderFilePx=OutDir(1:kl)//'DBux.H'
            DataFilePx=OutDir(1:kl)//'DBux.H@'
            lenHPx = kl+6
            lenDPx = kl+7

            fidPy=5
            HeaderFilePy=OutDir(1:kl)//'DBuy.H'
            DataFilePy=OutDir(1:kl)//'DBuy.H@'
            lenHPy = kl+6
            lenDPy = kl+7

            fidPz=7
            HeaderFilePz=OutDir(1:kl)//'DBuz.H'
            DataFilePz=OutDir(1:kl)//'DBuz.H@'
            lenHPz = kl+6
            lenDPz = kl+7

         endif ! if (UPDOWN .EQ. 1) 

      endif  ! if (OUTPUT .eq. 2)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC (OUTPUT .eq. 1) .or. ( (OUTPUT .eq. 2)
*     Eliminazione dei file con nomi uguali a quelli delle uscite

      if ( (OUTPUT .eq. 1) .or. ( (OUTPUT .eq. 2) .and. (IOstate .eq. 1) ) ) then   

         if  (OUT_A .eq. 1) then
            CALL delFile( fidT, HeaderFileT)
            CALL delFile( fidT, DataFileT)
         endif
         if  (OUT_T .eq. 1) then
            CALL delFile( fidT, HeaderFileA)
            CALL delFile( fidT, DataFileA)
         endif
         if  (OUT_UX .eq. 1) then
            CALL delFile( fidT, HeaderFilePx)
            CALL delFile( fidT, DataFilePx)
         endif
         if  (OUT_UY .eq. 1) then
            CALL delFile( fidT, HeaderFilePy)
            CALL delFile( fidT, DataFilePy)
         endif
         if  (OUT_UZ .eq. 1) then
            CALL delFile( fidT, HeaderFilePz)
            CALL delFile( fidT, DataFilePz)
         endif

      endif

*****************************************************************************
*     SALVATAGGIO FORMATO DATABASE **********************************************
*****************************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC OUTPUT = 2
      if (OUTPUT .eq. 2) then


*     Scrittura Header file dei traveltime
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC OUTPUT = 2, prima scrittura su disco
         if (is .eq. 1) then
            if (OUT_T .eq. 1) then
               title = 'DataBase Traveltime'
               fileName ='./DBt.H@'
               call writeIcoHeader3( HeaderFileT, esize, UPDOWN, Nshot, Ntgt, title, fileName)      
            endif

*     Scrittura Header file delle ampiezze
            if (OUT_A .eq. 1) then
               title = 'DataBase Amplitude'
               fileName ='./DBa.H@'
               call writeIcoHeader3( HeaderFileA, esize, UPDOWN, Nshot, Ntgt, title, fileName)               
            endif
            
*     Scrittura Header file della componente Px
            if (OUT_UX .eq. 1) then               
               title = 'DataBase cos_x1'
               fileName = './DBux.H@'
               if (UPDOWN .eq. 1)  fileName = './DBvx.H@'
               call writeIcoHeader3( HeaderFilePx, esize, UPDOWN, Nshot, Ntgt, title, fileName)               
            endif
            
*     Scrittura Header file della componente Py
            if  (OUT_UY .eq. 1) then               
               title = 'DataBase cos_x2'
               fileName = './DBuy.H@'
               if (UPDOWN .eq. 1)  fileName = './DBvy.H@'
               call writeIcoHeader3( HeaderFilePy, esize, UPDOWN, Nshot, Ntgt, title, fileName)               
            endif
            
*     Scrittura Header file della componente Pz
            if  (OUT_UZ .eq. 1) then               
               title = 'DataBase cos_x3'
               fileName = './DBuz.H@'
               if (UPDOWN .eq. 1)  fileName = './DBvz.H@'
               call writeIcoHeader3( HeaderFilePz, esize, UPDOWN, Nshot, Ntgt, title, fileName)
            endif
            
         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC OUTPUT = 2, fine prima scrittura su disco
         
!     Apertura files
!===================
       if (IOstate .eq. 1) then
       	
          if (OUT_T  .eq. 1) CALL glob_fopen64(index_T,DataFileT,'wb', file_error) ! R64 lug 2001               
          if (OUT_A  .eq. 1) CALL glob_fopen64(index_A,DataFileA,'wb', file_error) ! R64 lug 2001               
          if (OUT_UX .eq. 1) CALL glob_fopen64(index_UX,DataFilePx,'wb',file_error) ! R64 lug 2001               
          if (OUT_UY .eq. 1) CALL glob_fopen64(index_UY,DataFilePy,'wb',file_error) ! R64 lug 2001               
          if (OUT_UZ .eq. 1) CALL glob_fopen64(index_UZ,DataFilePz,'wb',file_error) ! R64 lug 2001

       endif   

*     ---------------------------------------------------------------------------
*     SALVATAGGIO MODELLO INTERO (OUTPUT = 2)
*     ---------------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC OUTPUT = 2, TARGET = 1
       IF ( (TARGET .eq. 1) .AND. (UPDOWN .eq. 2)) THEN            

!          Inversione dei cubi se la propagazione e' dal basso verso l'alto
           DO 133 ix=1,n1
              DO 134 iy=1,n2
                 DO 135 iz=1,INT(n3/2)
                    if(OUT_T .EQ. 1) then
                       memo=T(ix+(iy-1)*n1+(iz-1)*n1*n2)
                       T(ix+(iy-1)*n1+(iz-1)*n1*n2)=T(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)
                       T(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)=memo
                    endif
                    if(OUT_A .EQ. 1) then
                       memo=A(ix+(iy-1)*n1+(iz-1)*n1*n2)
                       A(ix+(iy-1)*n1+(iz-1)*n1*n2)=A(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)
                       A(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)=memo
                    endif
                    if(OUT_UX .EQ. 1) then
                       memo=Px(ix+(iy-1)*n1+(iz-1)*n1*n2)
                       Px(ix+(iy-1)*n1+(iz-1)*n1*n2)=Px(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)
                       Px(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)=memo
                    endif
                    if(OUT_UY .EQ. 1) then
                       memo=Py(ix+(iy-1)*n1+(iz-1)*n1*n2)
                       Py(ix+(iy-1)*n1+(iz-1)*n1*n2)=Py(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)
                       Py(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)=memo
                    endif
                    if(OUT_UZ .EQ. 1) then
                       memo=Pz(ix+(iy-1)*n1+(iz-1)*n1*n2)
                       Pz(ix+(iy-1)*n1+(iz-1)*n1*n2)=Pz(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)
                       Pz(ix+(iy-1)*n1+(n3-iz+1-1)*n1*n2)=memo
                    endif
135              CONTINUE
134           CONTINUE
133        CONTINUE
                                           
       endif  ! if ( (TARGET .eq. 1) .AND. (UPDOWN .eq. 2))
              
!      Salvataggio dell'uscita in formato SERIALE 
!      Parcheggia le funzioni di Green nel buffer di scrittura
!      ======================================================== 
       IF (QUANT_FLAG .eq. 1) THEN
          DO 2000 kk=1,Ntgt
              IF ( OUT_A .EQ. 1)  writeBufferShort( kk, index_A*shotInBuf +nShotMem+1) = NINT(A(kk))
              IF ( OUT_T .EQ. 1)  writeBufferShort( kk, index_T*shotInBuf +nShotMem+1) = NINT(T(kk))
              IF ( OUT_UX .EQ. 1) writeBufferShort( kk, index_UX*shotInBuf+nShotMem+1) = NINT(Px(kk))
              IF ( OUT_UY .EQ. 1) writeBufferShort( kk, index_UY*shotInBuf+nShotMem+1) = NINT(Py(kk))
              IF ( OUT_UZ .EQ. 1) writeBufferShort( kk, index_UZ*shotInBuf+nShotMem+1) = NINT(Pz(kk))
2000      CONTINUE
       ELSE
          DO 2010 kk=1,Ntgt
              IF ( OUT_A .EQ. 1)  writeBufferFloat( kk, index_A*shotInBuf +nShotMem+1) = A(kk)
              IF ( OUT_T .EQ. 1)  writeBufferFloat( kk, index_T*shotInBuf +nShotMem+1) = T(kk)
              IF ( OUT_UX .EQ. 1) writeBufferFloat( kk, index_UX*shotInBuf+nShotMem+1) = Px(kk)
              IF ( OUT_UY .EQ. 1) writeBufferFloat( kk, index_UY*shotInBuf+nShotMem+1) = Py(kk)
              IF ( OUT_UZ .EQ. 1) writeBufferFloat( kk, index_UZ*shotInBuf+nShotMem+1) = Pz(kk)
2010      CONTINUE
       ENDIF
   
!      Incrementa il contatore degli shot parcheggiati nel buffer
!      ==========================================================
       nShotMem = nShotMem + 1
       
!      Scrive il numero dello shot effettivo memorizzato nella posizione corrente
!      ====================================================================================
       IF ( UPDOWN .EQ. 2) THEN
!         Sorting per dbswapper       	
          shotSort(nShotWri + nShotMem) = is
       ELSE
!         Sorting per migratore       	
       	  shotSort(is) = nShotWri + nShotMem
       ENDIF
       
!       Scrittura su disco del contenuto del buffer di scrittura
!       ========================================================
       IF ( (nShotMem.EQ.shotInBuf) .OR. (nShotWri+nShotMem.EQ.Nshot)) THEN

!         Ciclo su tempi, ampiezze e coseno. Ogni grandezza viene scritta su un file differente
!         =====================================================================================            
          DO 2100 kk=0,index_cur-1
          
!            Scrittura del buffer di output
!            ==============================                                     
             numElem = nShotMem*Ntgt
             base    = kk*ShotInBuf+1
             IF ( QUANT_FLAG .EQ. 1) THEN
                CALL glob_fwrite( kk, writeBufferShort( 1, base), size_sint, numElem, file_error)
             ELSE
                CALL glob_fwrite( kk, writeBufferFloat( 1, base), size_real, numElem, file_error)
             ENDIF
                                  
2100      CONTINUE

!         Incrementa il contatore degli shot scritti su disco
!         ====================================================
          nShotWri = nShotWri + nShotMem
          
!          Azzera il contatore degli shot parcheggiati nel buffer
!          =======================================================
          nShotMem = 0
          
       ENDIF  ! IF ( (nShotMem.EQ.shotInBuf) .OR. (nShotWri+nShotMem.EQ.Nshot))
                           
      ENDIF  ! IF (OUTPUT .eq. 2)

!  Chiusura files
       if (IOstate .eq. Nshot)  then            
          if (OUT_T  .eq. 1) CALL glob_fclose(index_T, file_error)
          if (OUT_A  .eq. 1) CALL glob_fclose(index_A, file_error)
          if (OUT_UX .eq. 1) CALL glob_fclose(index_UX,file_error)
          if (OUT_UY .eq. 1) CALL glob_fclose(index_UY,file_error)
          if (OUT_UZ .eq. 1) CALL glob_fclose(index_UZ,file_error)
       endif
                     
      IOstate = IOstate + 1 

*****************************************************************************
*     FINE SALVATAGGIO FORMATO DATABASE *****************************************
*****************************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      RETURN
      END
      


!===============================================================================================================
!===============================================================================================================

      subroutine delFile( fid, fileName)
      
      implicit none
      
*     Parametri
      integer*4 MAXSTR
      parameter (MAXSTR=200)

      integer*4 fid   
      character*(MAXSTR) fileName   
            
      open(fid,NAME=fileName,STATUS='Unknown')
      close(fid,STATUS='Delete')
      
      return
      end
      

!===============================================================================================================

      subroutine writeIcoHeader3( HeaderFile, esize, UPDOWN, Nshot, Ntgt,
     &                           title, fileName)      
               
      implicit none
      
*     Parametri
      integer*4 MAXSTR
      parameter (MAXSTR=200)
      
      integer*4 Ntgt, esize, Nshot, UPDOWN
      character*(MAXSTR) HeaderFile
      character*(MAXSTR) title, fileName
      
               CALL seph_put_string(HeaderFile,'title',title)
               CALL seph_put_int(HeaderFile,'updown',UPDOWN)
               CALL seph_put_int(HeaderFile,'nshot',Nshot)
               CALL seph_put_int(HeaderFile,'ntarget',Ntgt)
               CALL seph_put_int(HeaderFile,'esize',esize)
               CALL seph_put_string(HeaderFile,'in', fileName)

      end