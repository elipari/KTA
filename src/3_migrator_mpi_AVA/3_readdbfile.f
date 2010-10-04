      SUBROUTINE READDBFILE ( FIDDB, OFF64, TG_NT_TOT, RANGEVET, FDG,
     $                        TEMPFDG, FILE_ERROR, QUANT, DIMELEM)
      
      IMPLICIT NONE
      
      
      INTEGER*4 FIDDB
      INTEGER*8 OFF64,OFF64_TEMP
      INTEGER*4 TG_NT_TOT
      REAL*4    RANGEVET(TG_NT_TOT,2)
      REAL*4    FDG(TG_NT_TOT)
      INTEGER*2 TEMPFDG(TG_NT_TOT)
      INTEGER*4 QUANT

      INTEGER*4 I, FILE_ERROR
      REAL*4    MID, STEP               
      INTEGER*4 NLEVEL
      INTEGER*8 DIMELEM, UNO8
      

      NLEVEL = 65536-4       
      UNO8=1
      OFF64_TEMP = OFF64*DIMELEM +UNO8                    
!     POSIZIONAMENTO NEL DATABASE      
      CALL GLOB_FSEEK64(FIDDB,OFF64_TEMP,0,FILE_ERROR)
      
      IF ( (QUANT .EQ. 1) .OR. (QUANT .EQ. 2) ) THEN
!        LETTURA FILE COMPRESSO      	
         CALL GLOB_FREAD( FIDDB, TEMPFDG, 2, TG_NT_TOT, FILE_ERROR)
      ELSE
!        LETTURA FILE NON COMPRESSO      	
      	 CALL GLOB_FREAD( FIDDB, FDG,     4, TG_NT_TOT, FILE_ERROR)
         RETURN
      ENDIF
      
      
      IF ( QUANT .EQ. 1 ) THEN
      	
!        DECOMPRESSIONE BOTTOM-UP      	
         DO 555 I=1,TG_NT_TOT
            MID=RANGEVET(I,1)
            STEP=RANGEVET(I,2)
            IF (TEMPFDG(I).EQ.32767) THEN
               FDG(I)=-2.0
            ELSE
               FDG(I)=STEP*TEMPFDG(I)+MID
            ENDIF
 555     CONTINUE
 
      ELSEIF ( QUANT .EQ. 2) THEN
      
!        DECOMPRESSIONE TOP-DOWN      	
      	 STEP = 2.0/(NLEVEL-2)
         MID  = 0.0
         DO 556 I=1,TG_NT_TOT
            IF (TEMPFDG(I).EQ.32767) THEN
               FDG(I)=-2.0
            ELSE
               FDG(I)=STEP*TEMPFDG(I)+MID
            ENDIF
 556     CONTINUE
 
      ENDIF

      
      RETURN
      END


