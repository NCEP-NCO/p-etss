C For tidal spinup, we needed to remove the static heights (pressure
C gradient rise) from the ocean cells before the spin-up.  We want to
C reintroduce that value after the spin up.
      SUBROUTINE INTLHTSUB
      INCLUDE 'parm.for'
C  
      COMMON /FORCE/ WX(M_,N_),WY(M_,N_),PP(M_,N_),PPX(M_,N_),PPY(M_,N_)
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /STRMSB/ C1,C2,C21,C22,AX,AY,PTENCY,RTENCY
C STIME interferes with C code, so switched to STIME2
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /FLWCPT/ NSQRS,NSQRW,NSQRWC,NPSS,NCUT
      COMMON /DATUM/  SEADTM,DTMLAK
      COMMON /FRST/   X1(50),X12(50)
      COMMON /DUM88/  IIH(4),JJH(4),IHH(4),JHH(4)
      COMMON /DUMMY4/ S(800),C(800),P(800),DELP(800)
      COMMON /SWTCH/  IOPERL(5)
      COMMON /DATUM1/ DTMCHN
      common /TIDE/ ITIDE
      COMMON /XOKE/   XOKE
      CHARACTER*1     XOKE
C      CHARACTER*40     FILNAM
C
      COMMON /GPRT1/  DOLLAR,EBSN
      CHARACTER*2     DOLLAR
      CHARACTER*1     EBSN
C
!JWb
      COMMON /TM2/ ITM2
      ITM2=0
!JWe
      IF (XOKE.EQ.'X') RETURN
      IF (DOLLAR.EQ.'2$') RETURN
      write (*,*) "HERE--- INTLHTSUB"
      CALL TFLUSH

C        INITIALIZE WATER HEIGHTS ACCORDING TO LAKE DATUM
      DO 90 J=1,JMXB1
      DO 90 I=1,IMXB1
         IF (ITREE(I,J).EQ.'2'.OR.ITREE(I,J).EQ.'5'.OR.
     $       ITREE(I+1,J).EQ.'2'.OR.ITREE(I+1,J).EQ.'5'.OR.
     $       ITREE(I,J+1).EQ.'2'.OR.ITREE(I,J+1).EQ.'5'.OR.
     $       ITREE(I+1,J+1).EQ.'2'.OR.ITREE(I+1,J+1).EQ.'5') THEN
           HB(I,J)=.25*(PP(I,J)+PP(I+1,J)+PP(I,J+1)+PP(I+1,J+1))+SEADTM
            HB(I,J)=AMAX1(HB(I,J),-ZB(I,J))
         ELSE

            ZZ=.25*(PP(I,J)+PP(I+1,J)+PP(I,J+1)+PP(I+1,J+1)) +DTMLAK
            HB(I,J)=AMAX1(-ZB(I,J),AMAX1(ZZ,-ZB(I,J)))
         ENDIF
 90   CONTINUE

      RETURN
      END

      SUBROUTINE INTLHTADD
      INCLUDE 'parm.for'
C
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /STRMSB/ C1,C2,C21,C22,AX,AY,PTENCY,RTENCY
C STIME interferes with C code, so switched to STIME2
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /FLWCPT/ NSQRS,NSQRW,NSQRWC,NPSS,NCUT
      COMMON /DATUM/  SEADTM,DTMLAK
      COMMON /FRST/   X1(50),X12(50)
      COMMON /DUM88/  IIH(4),JJH(4),IHH(4),JHH(4)
      COMMON /DUMMY4/ S(800),C(800),P(800),DELP(800)
      COMMON /SWTCH/  IOPERL(5)
      COMMON /DATUM1/ DTMCHN
      common /TIDE/ ITIDE
      COMMON /XOKE/   XOKE
      CHARACTER*1     XOKE
C      CHARACTER*40     FILNAM
C
      COMMON /GPRT1/  DOLLAR,EBSN
      CHARACTER*2     DOLLAR
      CHARACTER*1     EBSN
C
      IF (XOKE.EQ.'X') RETURN
      IF (DOLLAR.EQ.'2$') RETURN
      write (*,*) "HERE--- INTLHTADD"
      CALL TFLUSH

C        INITIALIZE WATER HEIGHTS ACCORDING TO LAKE DATUM
      DO 90 J=1,JMXB1
      DO 90 I=1,IMXB1
      IF (ITREE(I,J).EQ.'2'.OR.ITREE(I,J).EQ.'5') GOTO 95
      IF (ITREE(I+1,J).EQ.'2'.OR.ITREE(I+1,J).EQ.'5') GOTO 95
      IF (ITREE(I,J+1).EQ.'2'.OR.ITREE(I,J+1).EQ.'5') GOTO 95
      IF (ITREE(I+1,J+1).EQ.'2'.OR.ITREE(I+1,J+1).EQ.'5') GOTO 95
C      HB(I,J)=AMAX1(-ZB(I,J),AMAX1(DTMLAK,-ZB(I,J)))
      GOTO 90
C        STATIC HEIGHTS ON OCEAN,SEA OR GULF; NOT ON INLAND WATER BODIES
 95   XR=ELPCL(I)*COSL(J)
      YR=ELPDL(I)*SINL(J)
      X=XR-C1-AX
      Y=YR-C2-AY
      RSQ=X*X+Y*Y
      R1=SQRT(RSQ)/5280.+1.
      K=R1
      R2=K
      DR=R1-R2
      K=MIN0(K,790)
      IF (HB(I,J)+ZB(I,J).NE.0) THEN
         HB(I,J)=HB(I,J)+DELP(K)+DR*(DELP(K+1)-DELP(K))
         HB(I,J)=AMAX1(HB(I,J),-ZB(I,J))
      ENDIF
 90   CONTINUE
      RETURN
      END

      SUBROUTINE MOMNTM2
C        JELESNIANSKI,CHEN   SEPTEMBER 1980 TDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (MOMNTM) COMPUTES FUTURE MOMENTUM COMPONENTS
C           ON ALL INTERIOR GRID POINTS, AFTER FIRST COMPUTING FUTURE
C           SURGE BY SUBROUTINE 'CONTTY'. IT CHECKS IF MOMNTM CAN EXIST
C           AT A GRID POINT; E.G.,  OVERTOPPING THE GRID POINT .
C           FUTURE SURGES UPDATED FROM SUB. 'CONTTY' ARE USED TO COMPUTE
C           HEIGHT GRADIENTS AND TO CHECK FOR ELIGIBLE MOMENTUM POINTS--
C           BACKWARD SCHEME---. CHECKS ARE MADE IF MOMENTUM IS TO BE
C           COMPUTED AT OPEN BOUNDARY POINTS.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C             NCATG = 1 FOR LAKE WINDS, 2 FOR OCEAN WINDS
C              NPLS = 0 REMOVE STATIC PRESSURE TERM IN FORCING(INT.B.C.)
C                   = 1  OTHERWISE
C             JMXB1 = MAX NO. OF J-POINTS, LESS A BOUNDARY POINT
C              JUMP = 1, FULLY FLOODED; 2,PARTIALLY FLOODED (NO STRESS)
C            MS(  ) = BEGIN I-SUBSCRIPT ON A J-LINE
C       ITREE( ,  ) = INDICATOR ARRAY FOR LAKE/OCEAN, TREE/NO TREE
C                     A,B,C,D FOR BNOUDARY TYPES
C        ZBM(  ,  ) = MAXIMUM BARRIER HT AT A SURGE POINT
C       IH(4) JH(4) = SHIFT I/J-SUBSCRIPTS TO 4 SURRNDNG SRGE PTS
C       IP(4) JP(4) = SHIFTS I/J-SUBPTS FROM SURGE PTS TO 4 MOM. CORNERS
C        HB(  ,   ) = SURGE HEIGHTS
C         ZB(  ,  ) = DEPTHS
C             HF(4) = STORAGE FOR 4 SURRNDNG SURGE PTS,
C            HPD(4) = STORAGE FOR 4 SURRNDNG TOTAL DEPTH POINTS
C     UB VB(  ,   ) = COMPONENTS OF TRANSPORT
C           DHX DHY = COMPONENTS OF SURFACE GRADIENT
C            ID DPH = MEAN TOTAL DEPTH
C   AI(300) AR(300) = FRICTION COEFFICIENTS ON TRANSPORTS
C           FXB FYB = COMPONENTS OF DRIVING FORCES
C           X1B(4)  = DEL*CORIOLIS PARM
C            CSHLTR = EXTINCTION COEF OF STRESS FOR WATER LESS THAN 1 FT
C             ISKIP = 1, COMPUTE MOMENTUM ON J=1    BOUNDARY
C                     2, COMPUTE MOMENTUM ON I=2 BOUNDARY
C                     3, COMPUTE MOMENTUM ON J=JMXB BOUNDARY
C                     4, COMPUTE MOMENTUM ON I=IMXB BOUNDARY
C             IJUMP = 1, SHALLOW WATER BOUNDARY COMPUTATIONS
C                     2, INTER DEPTH   BOUNDARY COMPUTATIONS
C    ISBS JSBS(4) = SHIFT SUBSCRIPTS FROM INTERIOR TO BOUNDARY PT
C         ISHF(3,4) = SUBSCIPTS SHIFTS FROM INTERIOR TO 3 CORNER
C                     MOM-POINTS FOR ALL 4 CORNERS
C
C        GENERAL COMMENTS
C           THIS SUBROUTINE RESIDES IN OVERLAY 'CMPUTE'. IT IS CALLED
C           IN BY SUBROUTINE 'CMPUTE'. THE PRESENT SUBROUTINE CALLS
C           IN 2 SUBROUTINES, 'FRCPNT' AND 'MNTMBD'.
C
C
      INCLUDE 'parm.for'
C
      COMMON /FRTH/   IC12,IC19,BT
      COMMON /MF/     DPH,HPD(4),ID
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /EGTH/   DELS,DELT,G,COR
      COMMON /SCND/   AR(900),AI(900),BR(900),BI(900),CR(900),CI(900)
      COMMON /DUMB8/  IP(4),JP(4),IH(4),JH(4)
      COMMON /DUM88/  IIH(4),JJH(4),IHH(4),JHH(4)
      COMMON /DUMB4/  X1B(10)
      COMMON /DUMB4I/ CORL(90)
      COMMON /GPRT1/  DOLLAR,EBSN
      DIMENSION     SLPAI(600),HFX(4),ISBS(4),JSBS(4),ISHF(3,4)
      CHARACTER*2   DOLLAR,SIG_
      CHARACTER*1   EBSN
      DATA ISBS/0,-1,0,1/,  JSBS/-1,0,1,0/
      DATA SIG_/'$'/,SHGT/50./
      DATA ISHF/1,2,4, 1,2,3, 1,3,4, 2,3,4/
C       PRECALCULATED SLOPES OF AI(ID). MAY BE PERMANENTLY STORED.
      DO 10 I=1,599
 10   SLPAI(I)=AI(I+1)-AI(I)
      SLPAI(600)=SLPAI(599)
C
      JEND=JMXB1
      IF (DOLLAR.EQ.SIG_) JEND=JMXB
C
      DO 570 J=2,JEND
C
C         ISKIP SET 3 RANGES FOR J
C
      ISKIP=2
C        FOR CILCULAR ISLAND, NO SIDE-BOUNDARIES (1 OR 3) TO BE TESTED.
C        FOR J=1, U/V  REPEAT THOSE AT J=JMXB (JEND).
      IF (DOLLAR.NE.SIG_) THEN
        IF(J.EQ.2) ISKIP=1
        IF(J.EQ.JEND) ISKIP=3
        ENDIF
C
      IST=MS(J)
      IFN=ME(J)
C        SKIP A J-COLUMN IF MS(J) IS SET TO IMXB OR GREATER.
      IF ((IFN-IST).LT.0) CYCLE ! 570
C
      DO 560 I=IST,IFN
      JUMP=1
C        NO TERRAIN HIGHER THAN 35 FT CAN BE FLOODED.

C     CHANGED BY NSM 11/20/2010 TO ALLOW TERRAIN HIGHER THAN 35FT TO FLOOD
C     DID NOT Accept.  The '4' is now correctly shifted to 56 and above
C     and if it was a '4' for 35..56, it is now a '1'.
      IF (ITREE(I,J).EQ.'4'.OR.ITREE(I,J).EQ.'6') CYCLE ! 560
C      IF (ITREE(I,J).EQ.'6') GO TO 560

C        ITREE IS SET TO 1 OR 3 FOR LAKE WINDS, 2 OR 5 FOR OCEAN WINDS
        IF (ITREE(I,J).EQ.'2'.OR.ITREE(I,J).EQ.'5') THEN
         NCATG=2
         ELSE
         NCATG=1
         ENDIF
      IF (NCATG.EQ.1) GOTO 140
      IF (ITREE(I,J).EQ.'2'.AND.ZBM(I,J).LT.-50.) GOTO 160
C
C      TEST IF SURRNDNG SQRS ARE DRY, PART/FULL FLOOD .
C
 140  CONTINUE
c
      HMN=AMIN1(HB(I,J),HB(I-1,J),HB(I,J-1),HB(I-1,J-1))
      IF (HMN.GT.ZBM(I,J)) GO TO 200
      HMXX=AMAX1(HB(I,J),HB(I-1,J),HB(I,J-1),HB(I-1,J-1))
      IF (HMXX.GT.ZBM(I,J)) GO TO 300
C       NO TRANSPORTS ARE ALLOWED, GO TO CHECK IF NEXT TO BOUNDARY.
      GO TO 220
C        ALL SURROUNDING SQUARES ARE FLOODED, CALCULATE GRADIENT TREM
C        WITH CENTER DIFFERENCES
 200  JUMP=1
      DHX=-HB(I-1,J-1)+HB(I,J-1)-HB(I-1,J)+HB(I,J)
      DHY=-HB(I-1,J-1)-HB(I,J-1)+HB(I-1,J)+HB(I,J)
      GO TO 340
C
C        AT LEAST ONE SQUARE IS NOT OVERTOPPED. USE HYDRULIC HEAD FOR
C        SLOPE ON THAT SQUARE (1/4 EFFECT) AS APPROXMATION
 300  JUMP=2
C      DEFINE A MODIFIED SURFACE GRADIENT FOR A PARTIALLY FLOODED POINT
      HF1=AMAX1(HB(I-1,J-1),ZBM(I,J))
      HF2=AMAX1(HB(I  ,J-1),ZBM(I,J))
      HF3=AMAX1(HB(I-1,J  ),ZBM(I,J))
      HF4=AMAX1(HB(I  ,J  ),ZBM(I,J))
      DHX=-HF1+HF2-HF3+HF4
      DHY=-HF1-HF2+HF3+HF4
C
C        TAKE AVERAGE (D+H), OR TOTAL DEPTH, OF 4 SURROUNDING SQUARES
C        USED TO DEFINE BOTTOM STRESS COEFFICIENTS
 340  CONTINUE
      HPD(1)=HB(I-1,J-1)+ZB(I-1,J-1)
      HPD(2)=HB(I  ,J-1)+ZB(I  ,J-1)
      HPD(3)=HB(I-1,J  )+ZB(I-1,J  )
      HPD(4)=HB(I  ,J  )+ZB(I  ,J  )
      DPH=.25*(HPD(1)+HPD(2)+HPD(3)+HPD(4))
      DPHZ=DPH
      ID=AMIN1(599.,DPHZ)
C
C        FOR VERY THIN SHEET OF WATER ON BARE TERRAIN, BOTTOM FRICTIONS
C        ARE ARTIFICIALLY INCREASED WITH A CUBIC FORMULA
      IF (DPH.GT.5.) GO TO 405
C        IT IS UNNECESSARY IF PARTIALLY FLOODED.
      IF (JUMP.EQ.2) GO TO 400
C
C        REVISE DPH AND ID
      DO 380 K=1,4
      IF(HPD(K).GE.5.) GO TO 370
C        USE ALTERNATIVE X-1+(1-X/5)**3 = X*(.4+.12*X-.008*X**2)
      HPD(K)=HPD(K)*(.4+.12*HPD(K)-.008*HPD(K)*HPD(K))
      GO TO 380
 370  HPD(K)=HPD(K)-1.
 380  CONTINUE
C
      DPH=.25*(HPD(1)+HPD(2)+HPD(3)+HPD(4))
      DPHZ=DPH
      ID=AMIN1(599.,DPHZ)
 400  ID=MAX0(1,ID)
C
C+++++++ TAKE AVERAGE 'AI(TOTAL DEPTH)' ON 4 SQUARES++++++++++++++++++++
C        EXTRAPOLATION IF DPH IS LESS THAN 1 FT
 405  AINTRP=0.
      DO 430 K=1,4
      DPHZ=HPD(K)
      IDD=MIN(599.,DPHZ)
      IDD=MAX0(IDD,1)
C        EXTRAPOLATION RESULTS IF DEPTH IS LESS THAN 1 FT.
      DPHI=IDD
      AITR=AI(IDD)+SLPAI(IDD)*(DPHZ-DPHI)
 420  AINTRP=AINTRP+AITR
 430  CONTINUE
      AINTRP=.25*AINTRP
      GOTO 450
C
C        ALL SURROUNDING SQUARES ARE FLOODED
 160  CONTINUE
      JUMP=1
      DHX=-HB(I-1,J-1)+HB(I,J-1)-HB(I-1,J)+HB(I,J)
      DHY=-HB(I-1,J-1)-HB(I,J-1)+HB(I-1,J)+HB(I,J)
      DPH=.25*(HB(I-1,J-1)+HB(I,J-1)+HB(I-1,J)+HB(I,J)+
     1         ZB(I-1,J-1)+ZB(I,J-1)+ZB(I-1,J)+ZB(I,J))
      DPHZ=MIN(599.,DPH)
      ID=MIN(599.,DPHZ)
      DPHI=ID
      AINTRP=AI(ID)+SLPAI(ID)*(DPHZ-DPHI)
C
 450  AII=1.+X1B(4)*AINTRP
C
C      CORIOLIS PARAMETER DEPENDS ON THE THE LATITUDE OF THE GRID.
      LLAT=YLT(I,J)
      ZCOR=CORL(LLAT+1)+(YLT(I,J)-LLAT)*(CORL(LLAT+2)-CORL(LLAT+1))
      ARJ=AR(ID)*X1B(4)*ZCOR
C
C
      AIU=AII*UB(I,J)
      AIV=AII*VB(I,J)
      FCT=X1B(6)*DPH
C
C        IF PARTIALLY FOODED, NO STRESS IS APPLIED FOR CURRENT TIME.
      IF (JUMP.EQ.2) GOTO 490
C        NO FORCINGS
 490  XTTX=0.
      XTTY=0.
!JWb
      FXB=0.
      FYB=0.
!JWe
C
 480  CONTINUE
C
C       TEMPORARILY STORE PRESENT U/V TO BE USED FOR BOUNDARY POINTS
      UBJ=UB(I,J)
      VBJ=VB(I,J)
C        COMPUTE MOMENTUM ON INTERIOR POINT, SIELECKI'S SCHEME FOR
C        CORIOLIS TERM IS USED.
      UB(I,J)=AIU-FCT*(BR(ID)*DHX-BI(ID)*DHY)+ARJ*VB(I,J)+XTTX
      VB(I,J)=AIV-FCT*(BR(ID)*DHY+BI(ID)*DHX)-ARJ*UB(I,J)+XTTY
C
      GO TO 500
C
C        TRANSPORT POINT NOT OVERTOPPED IN FUTURE
 220  UB(I,J)=0.
      VB(I,J)=0.
C ------------------------------------------------------------------
C        CHECK IF MOMENTUM COMPUTATIONS REQUIRED ON BOUNDARY
C        ISKIP=1, LEFT BOUNDARY(J=1)
C             =2, AND I=2, NEXT TO TOP BOUNDARY
C             =3, RIGHT BOUNDARY
C             =4, AND I=IMXB1,NEXT TO BOTTOM BOUNDARY
C        IJUMP=1, INTERMEDIATE WATER; =2, SHALLOW WATER
C
 500  IJUMP=1
!
!
!
!
      IF (ISKIP == 1) THEN
         IF (I.NE.ME(J)) THEN
            IF (I.EQ.MS(J)) THEN
               ICOR = 1
               GO TO 600
            END IF
         ELSE
            ICOR = 2
            IF (I.EQ.MS(J)) THEN
               ICOR = 1
               GO TO 600
            END IF
         END IF
      ELSEIF (ISKIP == 2) THEN
         IF (I /= MS(J)) THEN
            IF (I == ME(J)) THEN
               ISKIP = 4
            ELSE
!               GO TO 560 
               CYCLE
            ENDIF
         ENDIF
      ELSEIF (ISKIP == 3) THEN
         IF (I /= ME(J)) THEN
            IF (I == MS(J)) THEN
               ICOR = 3
               GO TO 600
            END IF
         ELSE
            ICOR = 4
            IF (I == MS(J)) THEN
               ICOR = 3
               GO TO 600
            END IF
         END IF
      END IF
!            
!
!

      IA=ISBS(ISKIP)
      JA=JSBS(ISKIP)
      ISB=I+IA
      JSB=J+JA
C        IF AT (I,J),TRANSPORTS ARE ZERO,SET TRANSPORTS AT BOUNDY ZEROS.
      IF(UB(I,J).EQ.0.) THEN
      UB(ISB,JSB)=0.
      VB(ISB,JSB)=0.
        CYCLE ! 560
        ENDIF
C        PRE-SET ITREEALONG BOUNDARIES; =4  FOR LAND (SKIP COMP.)
C                                       =9  FOR SHALLOW WATER
C                                       =8  FOR INTERMEDIATE DEPTHS
C                                       =6  FOR DEEP WATER (SKIP COMP.)
      IF(ITREE(ISB,JSB).EQ.'6'.OR.ITREE(ISB,JSB).EQ.'4') CYCLE ! 560
      IF(ITREE(ISB,JSB).EQ.'8' ) IJUMP=2
C      WRITE(*,*) '  MNTMBD ISB,JSB',ISB,JSB,IJUMP
      CALL MNTMBD2(JUMP,IJUMP,ISB,JSB,ARJ,AII,I,J,UBJ,VBJ,
     1   FXB,FYB,NCATG,CSHLTR,NPLS)
      CYCLE ! 560
C        THREE MOMENTUM POINTS MUST BE CHECKED FOR COMPUTATIONS FOR
C          CORNER POINTS.
C        B   *                              *    B
C      L O   *                              *    O
C      E U   *                              *  R U
C      F N  1.     .(I,J)         (I,J).    .4 I N
C      T D   *                              *  G D
C        A   *  +(I,J-1)            (I,J) + *  H A
C        R   *                              *  T R
C        Y  2.*****.****  BOUNDARY ****.****.3   Y
C                  3                    2
C
 600  DO 610 KK=1,3
      K=ISHF(KK,ICOR)
      ISB=I+IH(ICOR)+IP(K)
      JSB=J+JH(ICOR)+JP(K)
C        IF AT (I,J),TRANSPORTS ARE ZERO,SET TRANSPORTS AT BOUNDY ZEROS.
      IF (UB(I,J).EQ.0.) THEN ! 630
         UB(ISB,JSB)=0.
         VB(ISB,JSB)=0.
      ELSE
         IF(ITREE(ISB,JSB).EQ.'6'.OR.ITREE(ISB,JSB).EQ.'4') EXIT ! 560
         IF(ITREE(ISB,JSB).EQ.'8' ) IJUMP=2
         CALL MNTMBD2(JUMP,IJUMP,ISB,JSB,ARJ,AII,I,J,UBJ,VBJ,
     1        FXB,FYB,NCATG,CSHLTR,NPLS)
      ENDIF
 610  CONTINUE
C -------------------------------------------------------------------
 560  CONTINUE
 570  CONTINUE
 580  CONTINUE
C       FOR CIRCULAR ISLAND, PERIODIC BOUNDARY CONDITIONS ARE USED.
      IF (DOLLAR.EQ.'$') THEN
        DO 700 I=1,IMXB
        UB(I,1)=UB(I,JMXB)
 700    VB(I,1)=VB(I,JMXB)
       ENDIF
      RETURN
       END

      SUBROUTINE MNTMBD2(JUMP,IJUMP,ISB,JSB,ARJ,AII,I,J,UBJ,VBJ,
     1   FXB,FYB,NCATG,CSHLTR,NPLS)
C        JELESNIANSKI   OCTOBER 1980   TDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (MNTMBD) COMPUTES TRANSPORTS ON MOMENTUM
C           BOUNDARY POINTS, BETWEEN LAND AND DEEP WATERS. IN SHALLOW
C           WATERS THE BOUNDARY CONDITION EQUATES SURFACE GRADIENT AT
C           BOUNDARY AND NEAREST INTERIOR POINT.  IN INTERMEDIATE
C           WATERS THE BOUNDARY CONDITION IGNORES TIME VARIATIONS AND
C           EQUATES SURGE GRADIENT AS STATIC HEIGHT GRADIENT AT A
C           BOUNDARY POINT.
C           BELOW IS ARRANGEMENT OF MOMENTUM BOUNDARY LINE
C           AND NEAREST INTERIOR LINE.
C
C          .    .(I,J)          .    .(I,J)      .    . INTERIOR LINE
C                                                *    *
C            +                    +              * +  *
C                                                *    *
C          .    .<---IJUMP=2--->.    .< IJUMP=1 >.****. BOUNDARY LINE
C               (ISB,JSB)          (ISB,JSB)
C  DEEP--->      <INTERMEDIATE >      <SHALLOW  >     <---LAND-----
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C             ISKIP = 1 TO COMPUTE MOMENTUM ON J=1    BOUNDARY
C                     2 TO COMPUTE MOMENTUM ON I=IMXB BOUNDARY
C                     3 TO COMPUTE MOMENTUM ON J=JMXB BOUNDARY
C         ISB JSB = REVISED SUBSCRIPTS FOR A BOUNDARY POINT
C     UB VB(  ,   ) = TRANSPORTS
C              JUMP = 1 FOR SURFACE FORCING,2 FOR NO SURFACE FORCING
C             IJUMP = 1 FOR SHALLOW WATER BOUNDARY COMPUTATIONS
C                     2 FOR INTER DEPTH   BOUNDARY COMPUTATIONS
C           FXB FYB = COMPONENTS OF DRIVING FORCES
C       AIT AII AIU = PRE-COMPUTED VALUES IN 'MOMNTM'
C       AIV ARJ XTT = DITTO
C            AINTRP = DITTO
C           X1B(10) = FIXED CONSTANTS
C           UBJ VBJ = TRANSPORTS AT (I,J) AT PRESENT TIME,SAVED BEFORE
C                     UPDATED
C
C        GENERAL COMMENTS
C           THIS SUBROUTINE RESIDES IN OVERLAY 'CMPUTE'. IT IS CALLED
C           IN BY SUBROUTINE 'MOMNTM'. SUBROUTINE 'MOMNTM' IS CALLED
C          IN BY SUBROUTINE 'CMPUTE'. THIS SUBROUTINE ALSO CALLS IN
C           SUBROUTINE 'STRESS'.
C
      INCLUDE 'parm.for'
C
      COMMON /EGTH/   DELS,DELT,G,COR
      COMMON /DUMB4/  X1B(10)
      COMMON /SCND/   AR(900),AI(900),BR(900),BI(900),CR(900),CI(900)
C
      AIUL=AII*(UB(ISB,JSB)-UBJ)
      AIVB=AII*(VB(ISB,JSB)-VBJ)
C       TEST FOR PARTIAL FLOODING (NO FORCING).
      IF (JUMP.EQ.2) THEN
          XTTX=0.
          XTTY=0.
          ! 140
          UB(ISB,JSB)=UB(I,J)+AIUL+ARJ*(VB(ISB,JSB)-VBJ)    +XTTX
          VB(ISB,JSB)=VB(I,J)+AIVB-ARJ*(UB(ISB,JSB)-UB(I,J))+XTTY
          ! 170
      ELSE
C           TEST FOR SHALLOW WATER BOUNDARY AND INTERMEDIATE WATER BRY.
         IF (IJUMP.EQ.2) THEN
         ! 160
            NPLS=0
            CSHLTR=1.
            FXB=0.
            FYB=0.
C 
            UB(ISB,JSB)=AII*UB(ISB,JSB)+ARJ*VB(ISB,JSB)+ DELT*FXB
            VB(ISB,JSB)=AII*VB(ISB,JSB)-ARJ*UB(ISB,JSB)+ DELT*FYB

         ELSE
C   
            XTTX=0.
            XTTY=0.
            UB(ISB,JSB)=UB(I,J)+AIUL+ARJ*(VB(ISB,JSB)-VBJ)    +XTTX
            VB(ISB,JSB)=VB(I,J)+AIVB-ARJ*(UB(ISB,JSB)-UB(I,J))+XTTY
         ENDIF
      ENDIF
C
C        COMPUTE MOMENTUM ON SHALLOW DEPTH BOUNDARIES. SURFACE GRADIENT
C        TERMS CANCELED BY ASSUMED B.C.
C        NOTE: USE SIELECKI'S SCHEME FOR CORIOLIS TERM.
C              VBJ = V-COMPONENT AT PRESENT TIME AT I,J
C              UB(I,J),VB(I,J) = U,V IN FUTURE TIME
C
C        NO FORCIING WHEN JUMP=2
C
C        COMPUTE MOMENTUM ON INTERMEDIATE DEPTH BOUNDARIES
C        ELIMINATE STATIC PRESSURE TERM IN FRCPNT(NPLS=0).
  170 RETURN
       END

      SUBROUTINE FLW1DM2
C        JELESNIANSKI   SEPTEMBER 1980 TDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (FLW1DM) COMPUTES 1-DIM FLOW ACROSS CHANNEL
C           SIDES OF INTERLOCKING SQUARES. WATER PASSES ONLY THRU 'OPEN'
C           SIDES OF A 1-DIM FLOW CHANNEL, SEE DIAGRAM BELOW:
C
C                .****.****.****.****.        -FLOW     *NO FLOW
C                -    -    -    -    *        -THRU     *THRU
C                - +  - +  - +  - +  *        -CHANNEL  *CHANNEL
C                -    -    -    -    *        -SIDE     *SIDE
C                -****.****.****.----.
C                               *    *        (OPEN)    (CLOSED)
C                               * +  *
C                               *    *
C                               .----.
C           THE 'LOOP' OF THE CONTINUITY EQUATION FILLS/DEPLETES
C           TWO ADJOINING SQUARES WITH FLOW THRU AN 'OPEN' SIDE,
C           ONE SIDE AT A TIME. IF WATER DROPS BELOW A SQUARE
C           FROM THE ACTION OF AN 'OPEN' SIDE, THEN THE FLOW THRU
C           THE SIDE IS REDUCED TO EXHAUST WATER ONLY TO A DRY
C           SQUARE.
C
C           EACH 'OPEN' SIDE HAS AN INTERIOR AND AN EXTERIOR SQUARE.
C           SUBROUTINE 'CRDRD2' READS THE SUBSCRIPTS FOR INTERIOR
C           SQUARES AND SETS UP PROCEDURES TO ISOLATE THE
C           ADJOINING EXTERIOR SQUARES AND 'OPEN' SIDES VIA
C           'COMMON/DUMB18/'. THE PRESENT SUBROUTINE ISOLATES THE TWO
C           INTERIOR/EXTERIOR SQUARES AND THE ADJOINING SIDE FOR
C           1-DIM FLOW. A 1-DIM FLOW SQUARE CAN BE EITHER AN INT/EXT
C           SQUARE, OR BOTH, 1-4 TIMES, DEPENDING ON THE AMOUNT OF
C           'OPEN' SIDES BOUNDING A SQUARE.
C
C           I,J SHIFTS ABOUT AN (I,J) INTERIOR SQUARE FOR 4 POSSIBLE
C              EXTERIOR SQUARES ARE:
C
C                            .////.
C                            IHH=0/
C                            / 4+ /
C                            JHH=1/
C                       .////.////.////.
C                      IHH=-1/ I  /IHH=1
C                       / 1+ / +  / 2+ /
C                      JHH=0 / J  /JHH=0
C                       .////.////.////.
C                            IHH=0/
C                            / 3+ /
C                            JHH=-1
C                            .////.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C           AI(800) = BOTTOM FRICTION, AT ONE FOOT INTERVALS
C             NSQRW = NO OF SQUARES WITH 1-DIM FLOW POSSIBLE
C  ISQR(L) JSQRL(L) = I/J-SUBSCRIPT FOR SQUARE WITH 1-DIM FLOW
C        HB(  ,   ) = SURGE FIELD
C        ISIDE(300) = SIDE OF FLOW RELATIVE TO INTERIOR 1-DIM FLOW SQRE
C     IHH(4) JHH(4) = SHIFT I/J-SUBSCRIPT FOR CONTIGUOUS SQUARE
C         HINT HEXT = INTERIOR/EXTERIOR SURGE FOR SFC GRADIENT
C       HINT1 HEXT1 = INTERIOR/EXTERIOR SURGE HEIGHTS ON SQUARES
C        ZBMIN(300) = LOWEST BARRIER VALUE AT END OF AN 'OPEN' SIDE
C                ZL = DUMMY VALUE FOR ZBMIN(300)
C         ZB(  ,  ) = DEPTH FIELD
C        HWEIR(300) = HIGHEST DEPTH  OF INT/EXT SQUARES, OR WEIR HEIGHT
C              HOVT = DUMMY VALUE FOR HWEIR(300)
C     FLWSQR(300  ) = TRANSPORT ACROSS AN 'OPEN' SIDE, TWO TIME LEVELS
C        GRIDR2(  ) = JACOBEAN FOR POLAR COORDINATES
C           SIGA(4) = SIGN FOR FLOW ACROSS SIDES OF SQUARES
C          DPH1D(2) = DEPTH+SURGE AT INT/EXT SQUARES, PRESENT TIME
C     CHNGHI CHNGHE = SURGE INCREASE ON INT/EXT SQRES, FROM 'OPEN' SIDE
C       DEPLI DEPLE = TOTAL DEPTH OF INT/EXT SQRES, FUTURE TIME
C               CII = ACCUMULATED WATER FROM 'OPEN' SIDES, INTERIOR SQR
C              SUBE = SURGE FROM EXT SQR TO PREVENT WATER BLW INT SQR
C         )    CEE = ACCUMULATED WATER FROM 'OPEN' SIDES, EXTERIOR SQR
C              SUBI = SURGE FROM INT SQR TO PREVENT WATER BLW EXT SQR
C            ID DPH = MEAN TOTAL DEPTH OF INT/EXT SQUARES, PRESENT TIME
C               DZL = HEIGHT OF HWEIR ABOVE MEAN DEPTHS
C           FXB FYB = COMPONENTS OF SURFACE STRESS
C            X1B(N) = FIXED COEFFICIENTS FOR FINITE DIFFERENCING
C   BR(300) BI(300) = FRICTION COEFFICIENTS AT 1-FOOT INTERVALS
C              JUMP = 2 FOR STRESS COMPUTATIONS, 1 FOR NO STRESS
C
C        GENERAL COMMENTS
C           THIS IS A VERSION FOR 2-STEP SCHEME (JULY 1981).
C           THIS SUBROUTINE RESIDES IN OVERLAY 'CMPUTE'. IT IS CALLED
C           IN BY SUBROUTINE 'CMPUTE'. SUBROUTINE 'FLW1DM' CALLS IN
C
C           SOME (NOT ALL) ELEVTNS ON INT/EXT SQRES AT PRESENT TIME ARE:
C
C                              T=ZL=ZBMIN(300)
C                              I
C                              I
C                              I
C                              I
C                              I***********>HINT=HB(I,JC)
C              HEXT<***********I      /
C                        /     I      /DPH1D(1)=HINT+ZB
C                        /     I      /
C                DPH1D(2)/     I----------->-ZB(I,J)
C                        /     I   /
C                        /     I   /
C                -ZB<----------I   /
C                          /   I   /
C                          /   I   /
C                          /   I   /
C                          /   I   /
C                 DEPTH=ZB=V   I   V=ZB=DEPTH
C             <----------------I----------------->NGVD DATUM
C                (EXTERIOR)        (INTERIOR)
C
      INCLUDE 'parm.for'
C
      COMMON /EGTH/   DELS,DELT,G,COR
      COMMON /SCND/   AR(900),AI(900),BR(900),BI(900),CR(900),CI(900)
      COMMON /DUMB4/  X1B(10)
      COMMON /DUMB8/  IP(4),JP(4),IH(4),JH(4)
      COMMON /DUM88/  IIH(4),JJH(4),IHH(4),JHH(4)
      COMMON /MF/     DPH,HPD(4),ID
      COMMON /FLWCPT/ NSQRS,NSQRW,NSQRWC,NPSS,NCUT
C
      DIMENSION       SIGA(4),DPH1D(2)
C
      DATA SIGA/-1.,1.,-1.,1./
C
      DO 100 L=1,NSQRWC
      I=ISQR(L)
      J=JSQR(L)
      K=ISIDE(L)
      II=I+IHH(K)
      JJ=J+JHH(K)
      HSUB(I,J)=0.
      HSUB(II,JJ)=0.
 100  F1DACT(L)='F'
C
CXXXXXXXXX BEGIN 1ST 'LOOP'. TREAT 2-DIM FLOW (FROM 'CONTTY') ONTO
CXXXXXXXXX A 1-DIM SQUARE AS A SOURCE.
C
      DO 510 L=1,NSQRWC
C
      IF (FLWSQR(L).EQ.0.) GO TO 510
C        ISOLATE A PARTICULAR SQUARE AND ITS SURFACE HEIGHT
      I=ISQR(L)
      J=JSQR(L)
C
C        DETERMINE WHICH OF 4 SIDES HAS FLOW AND ISOLATE ADJACENT SQUARE
C        AND ITS SURFACE HEIGHT
      K=ISIDE(L)
      II=I+IHH(K)
      JJ=J+JHH(K)
C
C        START CONTINUITY EQUATION FOR ONE SIDE OF A SQUARE
C        COMPUTE TOTAL FLOW IN/OUT OF INTERIOR SQUARE
 110  Z=ELPDL2(I)+(ELPCL2(I)-ELPDL2(I))*SINL2(J)
      Z1=ELPDL2(II)+(ELPCL2(II)-ELPDL2(II))*SINL2(JJ)
      CHNGHI=-SIGA(K)*X1B(5)*FLWSQR(L)/Z
      CHNGHE=-CHNGHI*Z/Z1
C
C     DETERMINE 1-DIM, OR EXPANSION, FLOW
      KCUT=1
      IF (L.GT.NSQRW) KCUT=2
!------------------------------------------------------------
!     rewrite the following codes to remove the computed GOTO
!     Huiqing.Liu /MDL Feb. 2015
!------------------------------------------------------------
!--------------------------------------------------------------
! Old codes
!---------------------------------------------------------------

!      GO TO (120,130), KCUT
C
C     CHECK FOR EXISTENCE OF BANKS
! 120  IF (DELCUT(L).GE.1.) GO TO 470
!      GO TO 140
! 130  LL=L-NSQRW
!      IF (CUTLI(LL).GE.1..AND.CUTLE(LL).GE.1.) GO TO 420
!C
!C      TWO CASES EXIST: OUTFLOW VERSUS INFLOW
!C      CASE 1: CHNGHI NEG (OUTFLOW), CHNGHE POS (INFLOW),  LEAP=1
!C      CASE 2: CHNBHI POS (INFLOW),  CHNGHE NEG (OUTFLOW), LEAP=2
! 140   LEAP=1
!--------------------------------------------------------------
! New codes
!---------------------------------------------------------------
       IF (KCUT == 2) THEN
          LL=L-NSQRW
          IF (CUTLI(LL).GE.1..AND.CUTLE(LL).GE.1.) THEN
             GO TO 420
          ELSE
             LEAP = 1
          END IF
       ELSE
         IF (DELCUT(L).GE.1.) THEN
            GO TO 470
         ELSE
            LEAP = 1
         END IF
       END IF
!
!                 

       IF (CHNGHI.GT.0.) LEAP=2
!
!
!------------------------------------------------------------
!     rewrite the following codes to remove the computed GOTO
!     Huiqing.Liu /MDL Feb. 2015
!------------------------------------------------------------
!--------------------------------------------------------------
! Old codes
!---------------------------------------------------------------

!------------------------------------------------------------                   
!       GO TO (192,194),LEAP
! 192   CHNG=CHNGHI
!       CSTRT=CHNGHI
!        HT=HB(I,J)
!        BK=BANK(L,1)
!        IF(KCUT.EQ.2) CUTW=CUTLI(LL)
!        GO TO (200,300),LEAP
! 194   CHNG=CHNGHE
!       CSTRT=CHNGHE
!       HT=HB(II,JJ)
!       BK=BANK(L,2)
!       IF(KCUT.EQ.2) CUTW=CUTLE(LL)
!       GO TO (300,200),LEAP
!C******LOGIC FOR NEGATIVE WATER CHANGES (OUTFLOW)
! 200   IF (HT.GT.BK) GO TO 230
!        GO TO (210,220),KCUT
!C       INITIAL WATER BELOW BANK AND DESCENDING
! 210   CHNG=CHNG
!       GO TO 280
! 220   CHNG=CHNG*CUTL(LL)/CUTW
!       GO TO 272
!C      INITIAL WATER ABOVE BANK AND DESCENDING
! 230   DELB=HT-BK
!       GO TO (240,260),KCUT
!C      WATER DOES NOT DESCEND BELOW BANK, PURE 1-D FLOW
! 240   IF(-CHNG*DELCUT(L).GT.DELB) GO TO 250
!       CHNG=CHNG*DELCUT(L)
!       GO TO 272
!C      WATER DESCENDS BELOW BANK, PURE 1-D FLOW
! 250   CHNG=CHNG-DELB+DELB/DELCUT(L)
!       GO TO 272
!C      WATER DOES NOT DESCEND BELOW BANK, CUT-TYPE FLOW
! 260   IF (-CHNG*CUTL(LL).GT.DELB) GO TO 270
!       CHNG=CHNG*CUTL(LL)
!       GO TO 272
!C      WATER DESCENDS BELOW BANK, CUT-TYPE FLOW
! 270   CHNG=(CHNG*CUTL(LL)+DELB)/CUTW-DELB
! 272   CONTINUE
! 280   GO TO (290,295),LEAP
! 290   CHNGHI=CHNG
!       HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)
!       GO TO 194
! 295   CHNGHE=CHNG
!       HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
!       GO TO 192
!C$$$$$$LOGIC FOR POSITIVE WATER CHANGES (INFLOW)
! 300   IF (HT.LT.BK) GO TO 330
!       GO TO (310,320),KCUT
!C      INITIAL WATER ABOVE BANK AND ASCENDING
! 310   CHNG=CHNG*DELCUT(L)
!       GO TO 372
! 320   CHNG=CHNG*CUTL(LL)
!       GO TO 372
!C      INITIAL WATER BELOW BANK AND ASCENDING
! 330   DELB=BK-HT
!       GO TO (340,360),KCUT
!C      WATER DOES NOT ASCEND ABOVE BANK, PURE 1-D FLOW
! 340   IF (CHNG.GT.DELB) GO TO 350
!       CHNG=CHNG
!       GO TO 380
!C      WATER ASCENDS ABOVE BANK, PURE 1-D FLOW
! 350   CHNG=DELB+(CHNG-DELB)*DELCUT(L)
!       GO TO 372
!C      WATER DOES NOT ASCEND ABOVE BANK, CUT-TYPE FLOW
! 360   IF (CHNG*CUTL(LL).GT.DELB*CUTW) GO TO 370
!       CHNG=CHNG*CUTL(LL)/CUTW
!       GO TO 372
!C      WATER ASCENDS ABOVE BANK, CUT-TYPE FLOW
! 370   CHNG=CHNG*CUTL(LL)-DELB*CUTW+DELB
! 372   CONTINUE
! 380   GO TO (390,395),LEAP
! 390   CHNGHE=CHNG
!       HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
!       GO TO 470
! 395   CHNGHI=CHNG
!       HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)
!       GO TO 470
!--------------------------------------------------------------
! New codes
!---------------------------------------------------------------
!       IF (LEAP == 1) THEN
       IF (LEAP /= 2) THEN
          CHNG=CHNGHI !192
          CSTRT=CHNGHI
          HT=HB(I,J)
          BK=BANK(L,1)
          IF(KCUT.EQ.2) CUTW=CUTLI(LL)
          ! 200
          IF (HT.GT.BK) THEN ! 200
             DELB=HT-BK !230
!             IF (KCUT == 1) THEN  !230
             IF (KCUT /= 2) THEN ! For safe consideration
                IF (-CHNG*DELCUT(L).GT.DELB) THEN !240
                   CHNG=CHNG-DELB+DELB/DELCUT(L)
                ELSE
                   CHNG=CHNG*DELCUT(L)
                END IF
                ! 272 
                   CHNGHI=CHNG !290
                   HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)

                   CHNG=CHNGHE ! 194
                   CSTRT=CHNGHE
                   HT=HB(II,JJ)
                   BK=BANK(L,2)
                   IF(KCUT.EQ.2) CUTW=CUTLE(LL)
                    !300
                   IF (HT.LT.BK) THEN !The value of HT is changed
                      DELB=BK-HT !330
                      IF (CHNG.GT.DELB) THEN !340
                         CHNG=DELB+(CHNG-DELB)*DELCUT(L) !350
                         ! 372/380
                      ELSE
                         CHNG=CHNG
                         ! 372/380
                      END IF

                   ELSE
                      CHNG=CHNG*DELCUT(L) !310
                      ! 372/380
                   END IF
!                   CHNGHE=CHNG !390
!                   HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
!                   GO TO 470
             ELSEIF (KCUT == 2) THEN !230
                IF (-CHNG*CUTL(LL).GT.DELB) THEN  ! 260
                   CHNG=(CHNG*CUTL(LL)+DELB)/CUTW-DELB
                ELSE
                   CHNG=CHNG*CUTL(LL)
                END IF
                   !272
                   CHNGHI=CHNG  !290
                   HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)

                   CHNG=CHNGHE ! 194
                   CSTRT=CHNGHE
                   HT=HB(II,JJ)
                   BK=BANK(L,2)
                   IF(KCUT.EQ.2) CUTW=CUTLE(LL)
                   !300
                   IF (HT.LT.BK) THEN !300
                      DELB=BK-HT !330
                      IF (CHNG*CUTL(LL).GT.DELB*CUTW) THEN !360
                         CHNG=CHNG*CUTL(LL)-DELB*CUTW+DELB !370
                         !372
                      ELSE
                         CHNG=CHNG*CUTL(LL)/CUTW
                         !372
                      END IF
                   ELSE
                      CHNG=CHNG*CUTL(LL) ! 320
                      !372
                   END IF
                   !372
!                   CHNGHE=CHNG   !390
!                   HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
!                   GO TO 470
             END IF
          ELSE !HT <= BK  !200
!             IF (KCUT == 1) THEN  !200
             IF (KCUT /= 2) THEN  !200
                CHNG=CHNG ! 210

                CHNGHI=CHNG ! 290
                HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)

                CHNG=CHNGHE ! 194
                CSTRT=CHNGHE
                HT=HB(II,JJ)
                BK=BANK(L,2)
                IF (KCUT.EQ.2) CUTW=CUTLE(LL)
                 ! 300
                IF (HT.LT.BK) THEN !300
                   DELB=BK-HT !330
                   IF (CHNG.GT.DELB) THEN !340
                      CHNG=DELB+(CHNG-DELB)*DELCUT(L) !350
                   ELSE
                      CHNG=CHNG
                   END IF
                   ! 372 !380
                ELSE
                   CHNG=CHNG*DELCUT(L) !310
                   !372
                END IF

!                CHNGHE=CHNG !390
!                HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
!                GO TO 470
             ELSEIF (KCUT == 2) THEN !200
                CHNG=CHNG*CUTL(LL)/CUTW ! 220
                ! 272
                CHNGHI=CHNG ! 290
                HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)

                CHNG=CHNGHE ! 194
                CSTRT=CHNGHE
                HT=HB(II,JJ)
                BK=BANK(L,2)
                IF (KCUT.EQ.2) CUTW=CUTLE(LL)

                IF (HT.LT.BK) THEN !300 
                   DELB=BK-HT !330
                   IF (CHNG*CUTL(LL).GT.DELB*CUTW) THEN !360
                      CHNG=CHNG*CUTL(LL)-DELB*CUTW+DELB !370
                   ELSE
                      CHNG=CHNG*CUTL(LL)/CUTW
                   END IF
                !372
                ELSE
                   CHNG=CHNG*CUTL(LL) !320
                !372
                END IF

!                CHNGHE=CHNG !390
!                HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
!                GO TO 470
             END IF
          END IF

          CHNGHE=CHNG !390
          HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
          GO TO 470

       ELSEIF (LEAP == 2) THEN
          CHNG=CHNGHE ! 194
          CSTRT=CHNGHE
          HT=HB(II,JJ)
          BK=BANK(L,2)
          IF(KCUT.EQ.2) CUTW=CUTLE(LL)
          IF (HT.GT.BK) THEN ! 200
             DELB=HT-BK !230
!             IF (KCUT == 1) THEN  !230
             IF (KCUT /= 2) THEN ! For safe consideration
                IF (-CHNG*DELCUT(L).GT.DELB) THEN !240
                   CHNG=CHNG-DELB+DELB/DELCUT(L) !250
                ELSE
                   CHNG=CHNG*DELCUT(L)
                END IF

                   !272
                   CHNGHE=CHNG ! 295
                   HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)

                   CHNG=CHNGHI ! 192
                   CSTRT=CHNGHI
                   HT=HB(I,J)
                   BK=BANK(L,1)
                   IF(KCUT.EQ.2) CUTW=CUTLI(LL)
                   ! 300
                   IF (HT.LT.BK) THEN !300
                      DELB=BK-HT !330
                      IF (CHNG.GT.DELB) THEN !340
                         CHNG=DELB+(CHNG-DELB)*DELCUT(L) !350
                         !372
                      ELSE
                         CHNG=CHNG
                         !380
                      END IF
                   ELSE
                      CHNG=CHNG*DELCUT(L) !310
                   END IF
                   ! 372

!                   CHNGHI=CHNG ! 395
!                   HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)
!                   GO TO 470
             ELSEIF (KCUT == 2) THEN !230
                IF (-CHNG*CUTL(LL).GT.DELB) THEN  ! 260
                   CHNG=(CHNG*CUTL(LL)+DELB)/CUTW-DELB !270
                ELSE
                   CHNG=CHNG*CUTL(LL)
                END IF
                   !272
                   CHNGHE=CHNG ! 295
                   HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)

                   CHNG=CHNGHI ! 192
                   CSTRT=CHNGHI
                   HT=HB(I,J)
                   BK=BANK(L,1)
                   IF(KCUT.EQ.2) CUTW=CUTLI(LL)
                   ! 300
                   IF (HT.LT.BK) THEN !300
                      DELB=BK-HT !330
                      IF (CHNG*CUTL(LL).GT.DELB*CUTW) THEN !360
                         CHNG=CHNG*CUTL(LL)-DELB*CUTW+DELB !370
                         !372
                      ELSE
                         CHNG=CHNG*CUTL(LL)/CUTW
                         !372
                      END IF
                   ELSE
                      CHNG=CHNG*CUTL(LL) !320                   
                         !372
                   END IF
                   !372
!                   CHNGHI=CHNG ! 395
!                   HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)
!                   GO TO 470
             END IF
          ELSE !200
!             IF (KCUT == 1) THEN
             IF (KCUT /= 2) THEN
                CHNG=CHNG !210
                !272

                CHNGHE=CHNG !295
                HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)

                CHNG=CHNGHI !192
                CSTRT=CHNGHI
                HT=HB(I,J)
                BK=BANK(L,1)
                IF(KCUT.EQ.2) CUTW=CUTLI(LL)

                IF (HT.LT.BK) THEN ! 300
                   DELB=BK-HT !330
                   IF (CHNG.GT.DELB) THEN !340
                      CHNG=DELB+(CHNG-DELB)*DELCUT(L) !350
                   ELSE
                      CHNG=CHNG
                   END IF
                ELSE
                   CHNG=CHNG*DELCUT(L) !310
                END IF

                !372
!                CHNGHI=CHNG !395
!                HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)
!                GO TO 470
             ELSEIF (KCUT == 2) THEN !200
                CHNG=CHNG*CUTL(LL)/CUTW !220
                !280

                CHNGHE=CHNG !295
                HSUB(II,JJ)=HSUB(II,JJ)+ABS(CSTRT-CHNG)
                CHNG=CHNGHI !192 
                CSTRT=CHNGHI
                HT=HB(I,J)
                BK=BANK(L,1)
                IF(KCUT.EQ.2) CUTW=CUTLI(LL)

                IF (HT.LT.BK) THEN ! 300
                   DELB=BK-HT !330
                   IF (CHNG*CUTL(LL).GT.DELB*CUTW) THEN !360
                      CHNG=CHNG*CUTL(LL)-DELB*CUTW+DELB !370
                   ELSE
                      CHNG=CHNG*CUTL(LL)/CUTW
                   END IF
                ELSE
                   CHNG=CHNG*CUTL(LL) !320
                END IF
                ! 372
!                CHNGHI=CHNG !395
!                HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)
!                GO TO 470
             END IF
          END IF

          CHNGHI=CHNG ! 395
          HSUB(I,J)=HSUB(I,J)+ABS(CSTRT-CHNG)
          GO TO 470

       END IF
!       GO TO 470
!------------------------------------------------------------


 420   CHNGHI=CHNGHI*CUTL(LL)/CUTLI(LL)
       CHNGHE=CHNGHE*CUTL(LL)/CUTLE(LL)
 470   HBJ=HB(I,J)+CHNGHI
      HBJJ=HB(II,JJ)+CHNGHE
C        FINISH CONTINUITY EQUATION FOR ONE SIDE OF A SQUARE BY
C        TESTING IF WATER LIES BELOW SQUARES
 480  DEPLI=HBJ+ZB(I,J)
      DEPLE=HBJJ+ZB(II,JJ)
      IF (DEPLI.GE.0.) GO TO 490
C        WATER LIES BELOW INTERIOR SQUARE (DEPLI IS NEGATIVE)
      CII=CHNGHI-DEPLI
      SUBE=-CII*Z/Z1
      HB(I,J)=-ZB(I,J)
      HB(II,JJ)=AMAX1(HB(II,JJ)-CHNGHE+SUBE,-ZB(II,JJ))
      FLWSQR(L)=FLWSQR(L)*CII/CHNGHI
      GO TO 510
C        WATER LIES BELOW EXTERIOR SQUARE (DEPLE IS NEGATIVE)
 490  IF(DEPLE.GE.0.) GO TO 500
      CEE=CHNGHE-DEPLE
      SUBI=-CEE*Z1/Z
      HB(I,J)=AMAX1(HB(I,J)-CHNGHI+SUBI,-ZB(I,J))
      HB(II,JJ)=-ZB(II,JJ)
      FLWSQR(L)=FLWSQR(L)*CEE/CHNGHE
      GO TO 510
C
C        BOTH INTERIOR AND EXTERIOR SQUARES HAVE WATER ABOVE TERRAIN
C        AFTER UPDATES.
 500  HB(I,J)=HBJ
      HB(II,JJ)=HBJJ
C
      F1DACT(L)='T'
 510  CONTINUE
C***********************************************************************
C        END OF HEIGHT UPDATES BY COMPUTED FLOW ACCROSS SIDES
C***********************************************************************
C
C        USE UPDATED HEIGHTS COMPUTE FLOW ACCROSS SIDES.
C
C        FRICTION CHANGE BETWEEN 1-2 FEET OF TOTAL DEPTH, USED
C        FOR EXTRAPOLATION FROM 1 TO 0 FT.
C        SET PARAMETERS FOR FRCPNT ARGUMENTS, LAKE WINDS, WITH STATIC
C        PRESSURE FORCE.
      SLPA=AI(2)-AI(1)
      NCATG=1
      NPLS=1
C
      DO 630 L=1,NSQRWC
C        ISOLATE A PARTICULAR SQUARE AND ITS SURFACE HEIGHT
      I=ISQR(L)
      J=JSQR(L)
      HINT=HB(I,J)
C        DETERMINE WHICH OF 4 SIDES HAS FLOW AND ISOLATE ADJACENT SQUARE
C        AND ITS SURFACE HEIGHT
      K=ISIDE(L)
      II=I+IHH(K)
      JJ=J+JHH(K)
      HEXT=HB(II,JJ)
C        ISOLATE MINIMUM ZBM ON SIDE CONNECTING TWO SQUARES
      ZL=ZBMIN(L)
C        TEST IF SFC LIES ABOVE ZL, E.G., 2-DIM FLOW IN BOTH SQUARES
      IF (AMIN1(HINT,HEXT).GE.ZL) GO TO 520
C        TEST IF BOTH SQUARES ARE DRY
      IF (HINT.EQ.-ZB(I,J).AND.HEXT.EQ.-ZB(II,JJ)) GO TO 520
C        DETERMINE HT OF WEIR OR HIGHEST DEPTH OF TWO SQUARES
      HOVT=HWEIR(L)
C        TEST IF ANY SQUARE(S) OVERTOP THE WEIR AT PRESENT TIME
      IF(AMAX1(HINT,HEXT).GT.HOVT) GO TO 530
 520  FLWSQR(L)=0.
      HSUB(I,J)=0.
      HSUB(II,JJ)=0.
      GO TO 630
 530  CONTINUE
C        COMPUTE MOMENTUM ON ONE SIDE OF A SQUARE
      HINT1=HB(I,J)
      HEXT1=HB(II,JJ)
      HINT=AMIN1(AMAX1(HINT1,HOVT),ZL)
      HEXT=AMIN1(AMAX1(HEXT1,HOVT),ZL)
      JUMP=2
      IF (AMAX1(HINT,HEXT).GE.ZL) JUMP=1
      HEAD=HEXT-HINT
      HEAD=SIGA(K)*HEAD
      DPH1D(1)=HINT+ZB(I,J)
      DPH1D(2)=HEXT+ZB(II,JJ)
      DPH=.5*(DPH1D(1)+DPH1D(2))
CCCCCCCCCCC Arthur fix of dph close to 0.
      if (dph.lt.0.00000001) then
        FLWSQR(L)=0.
        HSUB(I,J)=0.
        HSUB(II,JJ)=0.
        GO TO 630
      end if
CCCCCCCCCCC Arthur end fix of dph close to 0.
      ID=DPH
      ID=AMIN1(599.,DPH)
      ID=MAX0(ID,1)
      BIOBR=BI(ID)/BR(ID)
      IF (NSQRW.EQ.NSQRS) GOTO 1492
          IF (L.GT.NSQRS.AND.L.LE.NSQRW) BIOBR=0.
 1492 CONTINUE
      HCOFF=BR(ID)+BI(ID)*BIOBR
      FLWT=-X1B(1)*HCOFF*DPH*HEAD
         IF (JUMP.EQ.1)  GOTO 589
C
!JWC        AVERAGE SHELTERING COEFF IF WATER BECOME LESS THAN 1 FT ABOVE
!JWC        BED,OR 1 FT BELOW CHANNEL BANK,OR BARRIER HEIGHT.
!JW      IF (KTREE(L).EQ.'T') THEN
!JW         CSHLTR=(DPH/50.)**2
!JW       ELSE
!JW         CSHINT=1.-DIM(1.,DIM(HINT1,HOVT))
!JW         CSHEXT=1.-DIM(1.,DIM(HEXT1,HOVT))
!JW         CSH2I=1.-DIM(1.,DIM(ZL,HINT1))
!JW         CSH2E=1.-DIM(1.,DIM(ZL,HEXT1))
!JW         CSHLTR=.5*AMIN1(CSHINT+CSHEXT,CSH2I+CSH2E)
!JW       ENDIF
!JWC
!JW      III=I
!JW      JJJ=J
!JW      GO TO (116,112,113,114),K
!JWC        COMPUTE (X,Y) POSITION OF FORCE POINTS ON POLAR GRID
!JW 112  III=I+1
!JW 116  XR0=COSL(JJJ)
!JW      YR0=SINL(JJJ)
!JW      ELPCZ=ELPCT(III)
!JW      ELPDZ=ELPDT(III)
!JW      GO TO 115
!JW 114  JJJ=J+1
!JW 113  XR0=COST(JJJ)
!JW      YR0=SINT(JJJ)
!JW      ELPCZ=ELPCL(III)
!JW      ELPDZ=ELPDL(III)
!JW 115  CONTINUE
!JW      XR=XR0*ELPCZ
!JW      YR=YR0*ELPDZ
!JWC
!JW      CALL FRCPNT(XR,YR,FXB,FYB,NCATG,CSHLTR,NPLS)
!JWC        CHANGE FORCING FROM (X,Y) SYSTEM TO IMAGE PLANE
!JW      XR=XR0*ELPDZ
!JW      YR=YR0*ELPCZ
!JW      FXBP=XR*FXB+YR*FYB
!JW      FYBP=XR*FYB-YR*FXB
!JWC        CORRECT STRESS DIRECTION ACCORDING TO ACTURAL FLOW
!JW      IF(COS1D(L).NE.1.) THEN
!JW      FXB=COS1D(L)*FXBP-SIN1D(L)*FYBP
!JW      FYB=SIN1D(L)*FXBP+COS1D(L)*FYBP
!JW      ELSE
!JW      FXB=FXBP
!JW      FYB=FYBP
!JW      ENDIF
!JWC
!JW      FRC1=.75*FXB
!JW      FRC2=.75*FYB
!JW      IF (K.EQ.3.OR.K.EQ.4) THEN
!JW      Z=FRC2
!JW      FRC2=-FRC1
!JW      FRC1=Z
!JW      ENDIF
!JWC
!JW      FRC12T= DELT*(FRC1+BIOBR*FRC2)
!JWC           ENDED STRESS COMPUTATIONS.
!JW      GOTO 590
C
C        INITIALIZE FLOW WITH GRAVITY AND WITHOUT SURFACE STRESS
 589  FRC12T=0.
C
 590   CONTINUE
C        TAKE AVERAGE OF 'AI' ON TWO SQUARES
      AINTRP=0.
      DO 620 KK=1,2
      IDD=AMIN1(299.,DPH1D(KK))
      IF (IDD.GT.1) GO TO 600
      AITR=AI(1)+SLPA*(DPH1D(KK)-1.)
      GO TO 610
 600  SLPAI=AI(IDD+1)-AI(IDD)
      DPHI=IDD
      AITR=AI(IDD)+SLPAI*(DPH1D(KK)-DPHI)
 610  AINTRP=AINTRP+AITR
 620  CONTINUE
      AINTRP=.5*AINTRP
      FLWCOF=X1B(4)*(AINTRP-AR(ID)*BIOBR)+1.
      FLW=FLWT+FLWCOF*FLWSQR(L)+FRC12T
      FWD=HSUB(II,JJ)-HSUB(I,J)
      IF (FWD.EQ.0.) GO TO 628
      FD2=-SIGA(K)*FWD*X1B(2)/(DPH*DPH)
      FD2=ABS(FD2)
      Z=ELPDL2(I)+(ELPCL2(I)-ELPDL2(I))*SINL2(J)
      Z1=ELPDL2(II)+(ELPCL2(II)-ELPDL2(II))*SINL2(JJ)
      XY=.5*(1./Z+1./Z1)
      FD2=FD2*XY
      FLW1=FLW
      DO 627 ITR=1,20
      FLW1=FLW/(1.+FD2*ABS(FLW1))
 627  CONTINUE
      FLWSQR(L)=FLW1
      GO TO 630
 628  FLWSQR(L)=FLW
 630  CONTINUE
C++++++++END MOMENTUM COMPUTATIONS FOR 1-DIM FLOW+++++++++++++++++++++++
C
C--------END MAIN 'LOOP'------------------------------------------------
      RETURN
       END

      SUBROUTINE TMSTP2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE CALLS ALL THE FORTRAN SUBROUTINES WHICH ARE NEEDED TO
C DO ONE TIME STEP.
C
C ORIGINALLY THIS WAS DONE IN CMPUTE BETWEEN 100 AND 999.
C THE CODE IS THE SAME EXCEPT CUT AND PASTED HERE.
C
C THIS ALSO RETURNS TO C THE VARIOUS VARIABLES NEEDED SO THAT
C IT CAN UPDATE THE GRAPHICS.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      include 'parm.for'

      COMMON /GPRT2/  EBSN1,EBSN2
      CHARACTER*1  EBSN1,EBSN2
      COMMON /FFTH/   ITIME, MHALT
      COMMON /FLWCPT/ NSQRS,NSQRW,NSQRWC,NPSS,NCUT
      COMMON /OPTS/   NOFLD,NOF1D
      CHARACTER*1     NOFLD,NOF1D
      COMMON /PRHSY/  IPRHR,JUMPR,IPRT
      COMMON /PRTSV/  IPRTSV,IPRTAD
C STIME interferes with C code, so switched to STIME2
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /DELTH/  NDLTHR,NDLTH2,DLTHR,DLTH2
      COMMON /CESAV/  IPN(250),IPL(250),KHSPT,IPN2(250),IPL2(250),
     $                KHSPT2
      COMMON /GPRT1/  DOLLAR,EBSN
      CHARACTER*1 EBSN
      CHARACTER*2 DOLLAR
      COMMON /EGTH/   DELS,DELT,G,COR
      COMMON /WTEMP/  YDELP,ZDELP,PNN,YC24,ZC24
      COMMON /STRMSB/ C1,C2,C21,C22,AX,AY,PTENCY,RTENCY
      COMMON /POLAR/  DEGREE,RMOUTH,AZMTH,DELA
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /SMTH/   ISMTH
      COMMON /GPRT/   STA
      CHARACTER*16  STA
!JWb
      COMMON /TM2/ ITM2
      ITM2=ITM2+1
!JWe

!C CODE TO IMPLEMENT ONE TIME STEP..
  100 INCSM=20
      IF (EBSN1.EQ.'+'.OR.EBSN1.EQ.'=') INCSM=1
      IF (EBSN1.EQ.'!'.OR.EBSN1.EQ.'#') INCSM=5
      IF (EBSN1.EQ.'@'.OR.EBSN1.EQ.'%') INCSM=10
      IF (EBSN1.EQ.'&') INCSM=20
      IF (EBSN1.EQ.'^'.OR.EBSN1.EQ.'*') INCSM=30
      ISMPT2G=1
      IF (EBSN1.EQ.'+') ISMPT2G=0
      IF (EBSN1.EQ.'&') ISMPT2G=0
      IF (EBSN1.EQ.'#') ISMPT2G=0
      IF (EBSN1.EQ.'%') ISMPT2G=0
      IF (EBSN1.EQ.'*') ISMPT2G=0
!      CALL TTIMER
!      xtime=itime
!c
c  continuity
      CALL CONTTY
!c
!C       NO CORNER SMOOTHING for PNB BASIN
!
!C      IF (EBSN1.EQ.'&'.or.EBSN1.EQ.'+') GOTO 113
      IF (ISMPT2G.EQ.0) GOTO 113
      IF (ITM2.GT.5.AND.INCSM.NE.1)  CALL SMPT2G
 113  CONTINUE
!c
!c
!c
      CALL BDRYHT
!c      CALL PCGWR (2.)
      IF (NPSS.NE.0) CALL CHANNL
      IF (NSQRWC.NE.0.and.nof1d.ne.'+') CALL FLW1DM2

  180 CONTINUE
!C
!C       IF (KEY1.EQ.0) GOTO 1112
!C
!c      IF(ITIME+1.EQ.NHRAD.or.EBSN1.EQ.'+') THEN
!      IF(mod(ITIME+1,INCSM).EQ.0.or.EBSN1.EQ.'+') THEN
!JWb
      IF(mod(ITM2+1,INCSM).EQ.0.or.EBSN1.EQ.'+') THEN
!JWe
       IF(ISMTH.EQ.1) THEN
        IF (DOLLAR.EQ.'$') THEN
         CALL FLTER2
        ELSE
         CALL FILTER
        ENDIF
       ELSE
        IF (DOLLAR.EQ.'$') THEN
         CALL FLTER2
        ELSE
         IF (STA.EQ."CH2 (2008)") THEN
          CALL XFILTER(1)
         ELSE
          CALL XFILTER(2)
         ENDIF
        ENDIF
       ENDIF
      ENDIF
!c
!c  momentum equation
      CALL MOMNTM2
!C
!      CALL STMVAL
!C END OF COMPUTE FOR ONE TIME STEP
      RETURN
      END
