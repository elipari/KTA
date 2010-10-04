      SUBROUTINE EXECUTE( FID,
     $                    KEY1_TYPE,   KEY1_INTERP,
     $                    KEY1_NPAN,   KEY1_NT_TOT, KEY1_NNT_TOT,
     $                    KEY1_PAN1,   KEY1_PAN2,   KEY1_PAN3,
     $                    KEY1_NA_MAX, KEY1_NB_MAX, KEY1_NC_MAX,
     $                    KEY1_NNA_MAX,KEY1_NNB_MAX,KEY1_NNC_MAX,
     $                    KEY1_RRA_MAX,KEY1_RRB_MAX,KEY1_RRC_MAX,
     $                    KEY1_O1,      KEY1_O2,      KEY1_O3,
     $                    KEY1_CA1,     KEY1_CA2,     KEY1_CA3,
     $                    KEY1_CB1,     KEY1_CB2,     KEY1_CB3,
     $                    KEY1_CC1,     KEY1_CC2,     KEY1_CC3,
     $                    KEY1_DA, KEY1_DB, KEY1_DC,
     $                    KEY1_NA, KEY1_NB, KEY1_NC,
     $                    KEY1_NT,
     $                    KEY1_DDA, KEY1_DDB, KEY1_DDC,
     $                    KEY1_NNA, KEY1_NNB, KEY1_NNC,
     $                    KEY1_NNT,
     $                    KEY1_W1, KEY1_W2, KEY1_W3, KEY1_W4,
     $                    KEY1_WEIGHTS,
     $                    KEY2_TYPE,   KEY2_INTERP,
     $                    KEY2_NPAN,   KEY2_MPAN, KEY2_NT_TOT, KEY2_NNT_TOT,
     $                    KEY2_PAN1,   KEY2_PAN2,   KEY2_PAN3,
     $                    KEY2_NA_MAX, KEY2_NB_MAX, KEY2_NC_MAX,
     $                    KEY2_NNA_MAX,KEY2_NNB_MAX,KEY2_NNC_MAX,
     $                    KEY2_RRA_MAX,KEY2_RRB_MAX,KEY2_RRC_MAX,
     $                    KEY2_O1,      KEY2_O2,      KEY2_O3,
     $                    KEY2_CA1,     KEY2_CA2,     KEY2_CA3,
     $                    KEY2_CB1,     KEY2_CB2,     KEY2_CB3,
     $                    KEY2_CC1,     KEY2_CC2,     KEY2_CC3,
     $                    KEY2_DA, KEY2_DB, KEY2_DC,
     $                    KEY2_NA, KEY2_NB, KEY2_NC,
     $                    KEY2_NT,
     $                    KEY2_DDA, KEY2_DDB, KEY2_DDC,
     $                    KEY2_NNA, KEY2_NNB, KEY2_NNC,
     $                    KEY2_NNT,
     $                    KEY2_W1, KEY2_W2, KEY2_W3, KEY2_W4,
     $                    KEY2_WEIGHTS,
     $                    KEY2_MTX_IN, KEY2_MTX_OUT   )

      IMPLICIT NONE

      INTEGER*4 FID

      INTEGER*4 KEY1_TYPE,   KEY1_INTERP
      INTEGER*4 KEY1_NPAN,   KEY1_NT_TOT, KEY1_NNT_TOT
      INTEGER*4 KEY1_PAN3, KEY1_PAN1(KEY1_PAN3),   KEY1_PAN2(KEY1_PAN3)

      INTEGER*4 KEY1_NA_MAX, KEY1_NB_MAX, KEY1_NC_MAX
      INTEGER*4 KEY1_NNA_MAX,KEY1_NNB_MAX,KEY1_NNC_MAX
      INTEGER*4 KEY1_RRA_MAX,KEY1_RRB_MAX,KEY1_RRC_MAX

      REAL*4    KEY1_O1(KEY1_NPAN),KEY1_O2(KEY1_NPAN),KEY1_O3(KEY1_NPAN)
      REAL*4    KEY1_CA1(KEY1_NPAN),KEY1_CA2(KEY1_NPAN),KEY1_CA3(KEY1_NPAN)
      REAL*4    KEY1_CB1(KEY1_NPAN),KEY1_CB2(KEY1_NPAN),KEY1_CB3(KEY1_NPAN)
      REAL*4    KEY1_CC1(KEY1_NPAN),KEY1_CC2(KEY1_NPAN),KEY1_CC3(KEY1_NPAN)

      REAL*4    KEY1_DA(KEY1_NPAN), KEY1_DB(KEY1_NPAN), KEY1_DC
      INTEGER*4 KEY1_NA(KEY1_NPAN), KEY1_NB(KEY1_NPAN), KEY1_NC
      INTEGER*4 KEY1_NT(KEY1_NPAN)

      REAL*4    KEY1_DDA(KEY1_NPAN), KEY1_DDB(KEY1_NPAN), KEY1_DDC
      INTEGER*4 KEY1_NNA(KEY1_NPAN), KEY1_NNB(KEY1_NPAN), KEY1_NNC
      INTEGER*4 KEY1_NNT(KEY1_NPAN)

      INTEGER*4 KEY1_W1, KEY1_W2, KEY1_W3, KEY1_W4
      REAL*4    KEY1_WEIGHTS(KEY1_W1, KEY1_W2, KEY1_W3, KEY1_W4)

      INTEGER*4 KEY2_TYPE,   KEY2_INTERP
      INTEGER*4 KEY2_NPAN,   KEY2_MPAN, KEY2_NT_TOT, KEY2_NNT_TOT
      INTEGER*4 KEY2_PAN3, KEY2_PAN1(KEY2_PAN3),   KEY2_PAN2(KEY2_PAN3)

      INTEGER*4 KEY2_NA_MAX, KEY2_NB_MAX, KEY2_NC_MAX
      INTEGER*4 KEY2_NNA_MAX,KEY2_NNB_MAX,KEY2_NNC_MAX
      INTEGER*4 KEY2_RRA_MAX,KEY2_RRB_MAX,KEY2_RRC_MAX

      REAL*4    KEY2_O1(KEY2_NPAN),KEY2_O2(KEY2_NPAN),KEY2_O3(KEY2_NPAN)
      REAL*4    KEY2_CA1(KEY2_NPAN),KEY2_CA2(KEY2_NPAN),KEY2_CA3(KEY2_NPAN)
      REAL*4    KEY2_CB1(KEY2_NPAN),KEY2_CB2(KEY2_NPAN),KEY2_CB3(KEY2_NPAN)
      REAL*4    KEY2_CC1(KEY2_NPAN),KEY2_CC2(KEY2_NPAN),KEY2_CC3(KEY2_NPAN)

      REAL*4    KEY2_DA(KEY2_NPAN), KEY2_DB(KEY2_NPAN), KEY2_DC
      INTEGER*4 KEY2_NA(KEY2_NPAN), KEY2_NB(KEY2_NPAN), KEY2_NC
      INTEGER*4 KEY2_NT(KEY2_NPAN)

      REAL*4    KEY2_DDA(KEY2_NPAN), KEY2_DDB(KEY2_NPAN), KEY2_DDC
      INTEGER*4 KEY2_NNA(KEY2_NPAN), KEY2_NNB(KEY2_NPAN), KEY2_NNC
      INTEGER*4 KEY2_NNT(KEY2_NPAN)

      INTEGER*4 KEY2_W1, KEY2_W2, KEY2_W3, KEY2_W4
      REAL*4    KEY2_WEIGHTS(KEY2_W1, KEY2_W2, KEY2_W3, KEY2_W4)

      REAL*4    KEY2_MTX_IN(KEY2_NT_TOT,KEY1_NT_TOT)
      REAL*4    KEY2_MTX_OUT(KEY2_MPAN)



*     VARIABILI LOCALI
      INTEGER*4 IK1A,IK1B,IK1C
      INTEGER*4 IK2A,IK2B,IK2C

      INTEGER*4 NK1A,NK1B,NK1C
      INTEGER*4 NK2A,NK2B,NK2C

      INTEGER*4 K1SHIFT, K2SHIFT
      INTEGER*4 K2SHIFT_OUT
      INTEGER*4 K2OUT

      INTEGER*4 K1LA, K1LB, K1LC
      INTEGER*4 K1HA, K1HB, K1HC
      INTEGER*4 K1WIA, K1WIB, K1WIC
      INTEGER*4 K1P(8)
      REAL*4    K1W(8)

      INTEGER*4 K2LA, K2LB, K2LC
      INTEGER*4 K2HA, K2HB, K2HC
      INTEGER*4 K2WIA, K2WIB, K2WIC
      INTEGER*4 K2P(8)
      REAL*4    K2W(8)

      INTEGER*4 IK1M, IK2M
      INTEGER*4 NK1M, NK2M

      INTEGER*4 FILE_ERROR


      K1SHIFT = 0
      NK1C = KEY1_PAN3
      DO 1010 IK1C=1,NK1C

        IF (KEY1_TYPE.EQ.2) THEN
          NK1B=KEY1_PAN2(IK1C)
        ELSE
          NK1B=KEY1_PAN2(1)
        ENDIF
        DO 1020 IK1B=1,NK1B

          IF (KEY1_TYPE.EQ.2) THEN
            NK1A=KEY1_PAN1(IK1C)
          ELSE
            NK1A=KEY1_PAN1(1)
          ENDIF
          DO 1030 IK1A=1,NK1A

* ..........per ogni punto della chiave KEY1 vengono determinati
* ..........i vicini utlizzati per interpolare i valori
            IF (KEY1_INTERP .EQ. 1) THEN

            IF (KEY1_TYPE .EQ. 1) THEN

              K1P(1) = IK1C

            ELSEIF (KEY1_TYPE .EQ. 2) THEN

              K1LA=INT(((IK1A-1)*KEY1_DDA(IK1C)) / KEY1_DA(IK1C)) + 1
              IF (K1LA.EQ.KEY1_NA(IK1C)) THEN
                K1LA=K1LA-1
              ENDIF
              K1HA=K1LA+1
              K1LB=INT(((IK1B-1)*KEY1_DDB(IK1C)) / KEY1_DB(IK1C)) + 1
              IF (K1LB.EQ.KEY1_NB(IK1C)) THEN
                K1LB=K1LB-1
              ENDIF
              K1HB=K1LB+1
              K1P(1) = K1LA + (K1LB-1)*KEY1_NA(IK1C) + K1SHIFT
              K1P(2) = K1HA + (K1LB-1)*KEY1_NA(IK1C) + K1SHIFT
              K1P(3) = K1LA + (K1HB-1)*KEY1_NA(IK1C) + K1SHIFT
              K1P(4) = K1HA + (K1HB-1)*KEY1_NA(IK1C) + K1SHIFT

            ELSEIF (KEY1_TYPE .EQ. 3) THEN

              K1LA=INT(((IK1A-1)*KEY1_DDA(1)) / KEY1_DA(1)) + 1
              IF (K1LA.EQ.KEY1_NA(1)) THEN
                K1LA=K1LA-1
              ENDIF
              K1HA=K1LA+1
              K1LB=INT(((IK1B-1)*KEY1_DDB(1)) / KEY1_DB(1)) + 1
              IF (K1LB.EQ.KEY1_NB(1)) THEN
                K1LB=K1LB-1
              ENDIF
              K1HB=K1LB+1
              K1LC=INT(((IK1C-1)*KEY1_DDC   ) / KEY1_DC   ) + 1
              IF (K1LC.EQ.KEY1_NC   ) THEN
                K1LC=K1LC-1
              ENDIF
              K1HC=K1LC+1
              K1P(1) = K1LA + (K1LB-1)*KEY1_NA(1) + (K1LC-1)*KEY1_NA(1)*KEY1_NB(1)
              K1P(2) = K1HA + (K1LB-1)*KEY1_NA(1) + (K1LC-1)*KEY1_NA(1)*KEY1_NB(1)
              K1P(3) = K1LA + (K1HB-1)*KEY1_NA(1) + (K1LC-1)*KEY1_NA(1)*KEY1_NB(1)
              K1P(4) = K1HA + (K1HB-1)*KEY1_NA(1) + (K1LC-1)*KEY1_NA(1)*KEY1_NB(1)
              K1P(5) = K1LA + (K1LB-1)*KEY1_NA(1) + (K1HC-1)*KEY1_NA(1)*KEY1_NB(1)
              K1P(6) = K1HA + (K1LB-1)*KEY1_NA(1) + (K1HC-1)*KEY1_NA(1)*KEY1_NB(1)
              K1P(7) = K1LA + (K1HB-1)*KEY1_NA(1) + (K1HC-1)*KEY1_NA(1)*KEY1_NB(1)
              K1P(8) = K1HA + (K1HB-1)*KEY1_NA(1) + (K1HC-1)*KEY1_NA(1)*KEY1_NB(1)

            ENDIF


            IF (KEY1_TYPE .EQ. 1) THEN

              K1W(1) = 1

            ELSEIF (KEY1_TYPE .EQ. 2) THEN

              K1WIA=INT( ( (IK1A-1)*KEY1_DDA(IK1C)-(K1LA-1)*KEY1_DA(IK1C) ) / KEY1_DDA(IK1C) ) + 1
              K1WIB=INT( ( (IK1B-1)*KEY1_DDB(IK1C)-(K1LB-1)*KEY1_DB(IK1C) ) / KEY1_DDB(IK1C) ) + 1
              K1WIC=IK1C

              K1W(1) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,1)
              K1W(2) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,2)
              K1W(3) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,3)
              K1W(4) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,4)

            ELSEIF (KEY1_TYPE .EQ. 3) THEN

              K1WIA=INT( ( (IK1A-1)*KEY1_DDA(1)-(K1LA-1)*KEY1_DA(1) ) / KEY1_DDA(1) ) + 1
              K1WIB=INT( ( (IK1B-1)*KEY1_DDB(1)-(K1LB-1)*KEY1_DB(1) ) / KEY1_DDB(1) ) + 1
              K1WIC=INT( ( (IK1C-1)*KEY1_DDC   -(K1LC-1)*KEY1_DC    ) / KEY1_DDC    ) + 1

              K1W(1) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,1)
              K1W(2) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,2)
              K1W(3) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,3)
              K1W(4) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,4)
              K1W(5) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,5)
              K1W(6) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,6)
              K1W(7) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,7)
              K1W(8) = KEY1_WEIGHTS(K1WIA,K1WIB,K1WIC,8)

            ENDIF


            IF (KEY1_TYPE .EQ. 1) THEN

              NK1M = 1

            ELSEIF (KEY1_TYPE .EQ. 2) THEN

              NK1M = 4

            ELSEIF (KEY1_TYPE .EQ. 3) THEN

              NK1M = 8

            ENDIF


            ELSE

            IF (KEY1_TYPE .EQ. 1) THEN

              NK1M = 1
              K1P(1) = IK1C
              K1W(1) = 1

            ELSEIF (KEY1_TYPE .EQ. 2) THEN

              NK1M = 1
              K1P(1) = IK1A + (IK1B-1)*NK1A + K1SHIFT
              K1W(1) = 1

            ELSEIF (KEY1_TYPE .EQ. 3) THEN

              NK1M = 1
              K1P(1) = IK1A + (IK1B-1)*NK1A + (IK1C-1)*NK1A*NK1B
              K1W(1) = 1

            ENDIF

            ENDIF


            K2SHIFT = 0
            K2SHIFT_OUT = 0
            NK2C = KEY2_PAN3
            DO 2010 IK2C=1,NK2C

              IF (KEY2_TYPE.EQ.2) THEN
                NK2B=KEY2_PAN2(IK2C)
              ELSE
                NK2B=KEY2_PAN2(1)
              ENDIF
              DO 2020 IK2B=1,NK2B

                IF (KEY2_TYPE.EQ.2) THEN
                  NK2A=KEY2_PAN1(IK2C)
                ELSE
                  NK2A=KEY2_PAN1(1)
                ENDIF
                DO 2030 IK2A=1,NK2A

* ................per ogni punto della chiave KEY2 vengono determinati
* ................i vicini utlizzati per interpolare i valori
                  IF (KEY2_INTERP .EQ. 1) THEN


                  IF (KEY2_TYPE .EQ. 1) THEN

                    K2P(1) = IK2C

                  ELSEIF (KEY2_TYPE .EQ. 2) THEN

                    K2LA=INT(((IK2A-1)*KEY2_DDA(IK2C)) / KEY2_DA(IK2C)) + 1
                    IF (K2LA.EQ.KEY2_NA(IK2C)) THEN
                      K2LA=K2LA-1
                    ENDIF
                    K2HA=K2LA+1
                    K2LB=INT(((IK2B-1)*KEY2_DDB(IK2C)) / KEY2_DB(IK2C)) + 1
                    IF (K2LB.EQ.KEY2_NB(IK2C)) THEN
                      K2LB=K2LB-1
                    ENDIF
                    K2HB=K2LB+1
                    K2P(1) = K2LA + (K2LB-1)*KEY2_NA(IK2C) + K2SHIFT
                    K2P(2) = K2HA + (K2LB-1)*KEY2_NA(IK2C) + K2SHIFT
                    K2P(3) = K2LA + (K2HB-1)*KEY2_NA(IK2C) + K2SHIFT
                    K2P(4) = K2HA + (K2HB-1)*KEY2_NA(IK2C) + K2SHIFT

                  ELSEIF (KEY2_TYPE .EQ. 3) THEN

                    K2LA=INT(((IK2A-1)*KEY2_DDA(1)) / KEY2_DA(1)) + 1
                    IF (K2LA.EQ.KEY2_NA(1)) THEN
                      K2LA=K2LA-1
                    ENDIF
                    K2HA=K2LA+1
                    K2LB=INT(((IK2B-1)*KEY2_DDB(1)) / KEY2_DB(1)) + 1
                    IF (K2LB.EQ.KEY2_NB(1)) THEN
                      K2LB=K2LB-1
                    ENDIF
                    K2HB=K2LB+1
                    K2LC=INT(((IK2C-1)*KEY2_DDC   ) / KEY2_DC   ) + 1
                    IF (K2LC.EQ.KEY2_NC   ) THEN
                      K2LC=K2LC-1
                    ENDIF
                    K2HC=K2LC+1
                    K2P(1) = K2LA + (K2LB-1)*KEY2_NA(1) + (K2LC-1)*KEY2_NA(1)*KEY2_NB(1)
                    K2P(2) = K2HA + (K2LB-1)*KEY2_NA(1) + (K2LC-1)*KEY2_NA(1)*KEY2_NB(1)
                    K2P(3) = K2LA + (K2HB-1)*KEY2_NA(1) + (K2LC-1)*KEY2_NA(1)*KEY2_NB(1)
                    K2P(4) = K2HA + (K2HB-1)*KEY2_NA(1) + (K2LC-1)*KEY2_NA(1)*KEY2_NB(1)
                    K2P(5) = K2LA + (K2LB-1)*KEY2_NA(1) + (K2HC-1)*KEY2_NA(1)*KEY2_NB(1)
                    K2P(6) = K2HA + (K2LB-1)*KEY2_NA(1) + (K2HC-1)*KEY2_NA(1)*KEY2_NB(1)
                    K2P(7) = K2LA + (K2HB-1)*KEY2_NA(1) + (K2HC-1)*KEY2_NA(1)*KEY2_NB(1)
                    K2P(8) = K2HA + (K2HB-1)*KEY2_NA(1) + (K2HC-1)*KEY2_NA(1)*KEY2_NB(1)

                  ENDIF


                  IF (KEY2_TYPE .EQ. 1) THEN

                    K2W(1) = 1

                  ELSEIF (KEY2_TYPE .EQ. 2) THEN

                    K2WIA=INT( ( (IK2A-1)*KEY2_DDA(IK2C)-(K2LA-1)*KEY2_DA(IK2C) ) / KEY2_DDA(IK2C) ) + 1
                    K2WIB=INT( ( (IK2B-1)*KEY2_DDB(IK2C)-(K2LB-1)*KEY2_DB(IK2C) ) / KEY2_DDB(IK2C) ) + 1
                    K2WIC=IK2C

                    K2W(1) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,1)
                    K2W(2) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,2)
                    K2W(3) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,3)
                    K2W(4) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,4)

                  ELSEIF (KEY2_TYPE .EQ. 3) THEN

                    K2WIA=INT( ( (IK2A-1)*KEY2_DDA(1)-(K2LA-1)*KEY2_DA(1) ) / KEY2_DDA(1) ) + 1
                    K2WIB=INT( ( (IK2B-1)*KEY2_DDB(1)-(K2LB-1)*KEY2_DB(1) ) / KEY2_DDB(1) ) + 1
                    K2WIC=INT( ( (IK2C-1)*KEY2_DDC   -(K2LC-1)*KEY2_DC    ) / KEY2_DDC    ) + 1

                    K2W(1) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,1)
                    K2W(2) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,2)
                    K2W(3) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,3)
                    K2W(4) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,4)
                    K2W(5) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,5)
                    K2W(6) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,6)
                    K2W(7) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,7)
                    K2W(8) = KEY2_WEIGHTS(K2WIA,K2WIB,K2WIC,8)

                  ENDIF


                  IF (KEY2_TYPE .EQ. 1) THEN

                    NK2M = 1
                    K2OUT = IK2C

                  ELSEIF (KEY2_TYPE .EQ. 2) THEN

                    NK2M = 4
                    K2OUT = IK2A + (IK2B-1)*NK2A + K2SHIFT_OUT

                  ELSEIF (KEY2_TYPE .EQ. 3) THEN

                    NK2M = 8
                    K2OUT = IK2A + (IK2B-1)*NK2A + (IK2C-1)*NK2A*NK2B

                  ENDIF


                  ELSE


                  IF (KEY2_TYPE .EQ. 1) THEN

                    NK2M = 1
                    K2P(1) = IK2C
                    K2W(1) = 1
                    K2OUT = IK2C

                  ELSEIF (KEY2_TYPE .EQ. 2) THEN

                    NK2M = 1
                    K2P(1) = IK2A + (IK2B-1)*NK2A + K2SHIFT
                    K2W(1) = 1
                    K2OUT = IK2A + (IK2B-1)*NK2A + K2SHIFT_OUT

                  ELSEIF (KEY2_TYPE .EQ. 3) THEN

                    NK2M = 1
                    K2P(1) = IK2A + (IK2B-1)*NK2A + (IK2C-1)*NK2A*NK2B
                    K2W(1) = 1
                    K2OUT = IK2A + (IK2B-1)*NK2A + (IK2C-1)*NK2A*NK2B

                  ENDIF


                  ENDIF


                  KEY2_MTX_OUT(K2OUT) = 0
                  DO 3010 IK1M=1,NK1M

                    DO 3020 IK2M=1,NK2M


                      IF ( ( KEY2_MTX_IN( K2P(IK2M),K1P(IK1M) ) .LE. -2) ) THEN
                        KEY2_MTX_OUT(K2OUT) = -2
                        GOTO 3030
                      ENDIF

                      KEY2_MTX_OUT( K2OUT ) =
     $                  KEY2_MTX_OUT( K2OUT ) +
     $                  KEY2_MTX_IN( K2P(IK2M),K1P(IK1M) )*K1W(IK1M)*K2W(IK2M)


3020                CONTINUE

3010              CONTINUE

3030              CONTINUE



2030            CONTINUE

2020          CONTINUE

              IF (KEY2_TYPE .EQ. 2) THEN
                K2SHIFT = K2SHIFT + KEY2_NA(IK2C)*KEY2_NB(IK2C)
                K2SHIFT_OUT = K2SHIFT_OUT + NK2A*NK2B
              ENDIF

2010        CONTINUE


            CALL GLOB_FSEEK(FID, 1, 2, FILE_ERROR)
            CALL GLOB_FWRITE(FID, KEY2_MTX_OUT, 4, KEY2_MPAN, FILE_ERROR)

1030      CONTINUE

1020    CONTINUE

        IF (KEY1_TYPE .EQ. 2) THEN
          K1SHIFT = K1SHIFT + KEY1_NA(IK1C)*KEY1_NB(IK1C)
        ENDIF

1010  CONTINUE

      RETURN
      END


