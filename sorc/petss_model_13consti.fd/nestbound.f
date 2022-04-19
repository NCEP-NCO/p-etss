!*****************************************************************************/
! * def_nesting_boun  --- Huiqing Liu / MDL
! *
! * DESCRIPTION
! *    New subroutine to determine the nesting boundary points for nesting runs
! *    Or for Tide Version 3.1 runs (put tidal forcing to all ocean boundary
! *    grids not only to deep ocean grids)  
! *    Replace the variable NBCPT (static height boundary points) and
! *    update the index of boundary points ISH and JSH
! *    Any Itree equals 6 (deep water),8 (intermediate water) and 9 (shallow 
! *    water) will be selected for input boundary from previous nesting output
! *    
! *     
! *     
! *
! * HISTORY
! *    11/20/2013 Written by  --- Huiqing Liu / MDL
! *
! * NOTES
!*    
! *****************************************************************************/       
      SUBROUTINE def_nesting_boun
      INCLUDE 'parm.for'
      PARAMETER (NBCPTS=12000)
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /BCPTS/  TIDESH(NBCPTS),BOUN_NEST(NBCPTS),
     *                NBCPT,ISH(NBCPTS),JSH(NBCPTS)
      INTEGER I,J,N,NESTBOUN

      N=0
      DO I=1,IMXB1
      DO J=1,JMXB1
       NESTBOUN=0
       IF(ITREE(I,J).EQ.'6'.OR.ITREE(I,J).EQ.'8'.OR.
     $ ITREE(I,J).EQ.'9')THEN
          NESTBOUN=1
       ELSEIF(ITREE(I+1,J).EQ.'6'.OR.ITREE(I+1,J).EQ.'8'.OR.
     $ ITREE(I+1,J).EQ.'9')THEN
          NESTBOUN=1        
       ELSEIF(ITREE(I,J+1).EQ.'6'.OR.ITREE(I,J+1).EQ.'8'.OR.
     $ ITREE(I,J+1).EQ.'9')THEN
          NESTBOUN=1
       ENDIF

       IF(NESTBOUN.EQ.1.AND.ZB(I,J).GT.0.0)THEN
C       
        N=N+1
        ISH(N)=I
        JSH(N)=J
        KSKP(I,J)='0'
       ENDIF
      
       IF (N.GT.NBCPTS) THEN
          WRITE (*,*) 'Number of boundary points exceeded',N,NBCPTS
!          CALL FLUSH (6)
          STOP 
       ENDIF
      
      ENDDO
      ENDDO
      NBCPT=N
      write(*,*)'NBCPT=',NBCPT
      
      END
      
!*****************************************************************************/
! * BDRYHT_NEST  --- Huiqing Liu / MDL
! *
! * DESCRIPTION
! *
! * Borrowed from BDRYHT BY REPLACING THE STATIC HEIGHT TO NESTING BOUNDARY
! * OUTPUT FROM PREVIOUS OUTER DOMAIN RUNs
! *
! * HISTORY
! *    11/20/2013 Written by  --- Huiqing Liu / MDL
! *
! * NOTES
!*    
! *****************************************************************************/        
      SUBROUTINE BDRYHT_NEST
C        JELESNIANSKI   SEPTEMBER 1980 TDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (BDRYHT) COMPUTES STATIC HEIGHTS ON DEEP
C           WATER BOUNDARIES ONLY, SEE DIAGRAM BELOW:
C
C                             ITREE( , )
C                              =
C                           B.9   .
C                           O
C                           U  +
C                           N                  .*********.
C                           D.8   .            *         *
C                         | A*                 *         *
C                         / R* +               *         *
C                         / Y*                 *    +    * COSL,SINL
C                         /  .6    .            *         *
C                      DEEP  /    /            *         *
C                     WATERS /    /            *         *
C                         /  /    /            .*********.
C                         /  /    /                 |
C                         /  .6    .----.    .
C                         /  *
C                         /  * +         +
C                         /  *
C                         V  .****.----.****.BOUNDARY---->
C                            6    6    8    9     4= ITREE(I,J)
C                                 DEEP
C                            <----WATERS---->
C
C
C           DEEP WATER CAN BE ON ONE, TWO OR NEITHER OF THE TWO SIDE
C           BOUNDARIES, BUT IT WILL EXIST AT LEAST ON A SEGMENT OF
C           THE CIRCLE WITH LARGEST RADIUS OF THE POLAR GRID.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C             AX AY = COMPS OF TOTAL STRM MOTION, ADVANCED IN 'STMVAL'
C             C1 C2 = INITIAL COMPS OF STORM, SET IN 'INTVAL'
C COSL(  ) SINL(  ) = CO-SINE OF ANGLE, RAYS TO X-AXIS, (HEIGHT POINTS
C             IMXB1 = MAX I-SUBSCRIPT FOR HEIGHT POINTS
C            SEADTM = INITIAL HEIGHT OF THE SEA (NO STATIC HEIGHTS)
C         DELP(800) = STATIC HEIGHTS AT MILE INTERVALS FROM STORM CENTER
C        HB(  ,   ) = SURGE HEIGHTS
C        ITREE( , ) = A,   ON AT LEAST ONE BOUNDARY CORNER AS STATIC
C                     HEIGHT IS USED
C
C        GENERAL COMMENTS
C           THIS SUBROUTINE RESIDES IN OVERLAY 'CMPUTE'. IT IS CALLED
C           IN BY SUBROUTINE 'CMPUTE'.
C      THIS SUBROUTINE IS CALLED IN BY SUBROUTINE 'CONTTY'
C      WHICH RESIDES IN OVERLAY 'CMPUTE'
C
      INCLUDE 'parm.for'
C
      PARAMETER (NBCPTS=12000)
      COMMON /FORCE/ WX(M_,N_),WY(M_,N_),PP(M_,N_),PPX(M_,N_),PPY(M_,N_)
      COMMON /BCPTS/  TIDESH(NBCPTS),BOUN_NEST(NBCPTS),
     *                NBCPT,ISH(NBCPTS),JSH(NBCPTS)

      COMMON /DATUM/  SEADTM,DTMLAK
C
C       BOUNDARY SQUARES DETERMINED IN SUBROUTINE DEPSFC
C
      DO 100 N=1,NBCPT
      I=ISH(N)
      J=JSH(N)
!      HB(I,J)=.25*(PP(I,J)+PP(I+1,J)+PP(I,J+1)+PP(I+1,J+1))
!     1          +SEADTM*0.
!      HB(I,J)=HB(I,J)+BOUN_NEST(N)
      HB(I,J)=BOUN_NEST(N)+TIDESH(N)+SEADTM

  100 CONTINUE
C
C      STATIC HEIGHTS ON SIDE BOUNDARIES
C
c      WRITE(*,*)'max value of boun',maxval(boun_nest)
      RETURN
       END
      
       
       
!*****************************************************************************/
! * tmstp2_all  --- Huiqing Liu / MDL
! *
! * DESCRIPTION
! * Borrowed from spinup.f by-----Huiqing Liu/MDL Nov. 2013 
! * Replace calling sub MOMNTM2 by calling sub MOMNTM2_ALL for Tide V3.1      
! * spinup runs without adding wind and pressure gradient forcing
! *
! *
! * HISTORY
! *    11/20/2013 Written by  --- Huiqing Liu / MDL
! *
! * NOTES
!*    
! *****************************************************************************/          
      SUBROUTINE TMSTP2_ALL
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
!      IF (ISMPT2G.EQ.0) GOTO 113
!
      IF (ISMPT2G.NE.0) THEN
         IF (ITM2.GT.5.AND.INCSM.NE.1)  CALL SMPT2G
      END IF
!
!
 113  CONTINUE
!c
!c
!c
      CALL BDRYHT
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
      CALL MOMNTM2_ALL
!C
!      CALL STMVAL
!C END OF COMPUTE FOR ONE TIME STEP
      RETURN
      END   
      
!*****************************************************************************/
! * momntm2_all  --- Huiqing Liu / MDL
! *
! * DESCRIPTION
! * Borrowed from spinup.f by-----Huiqing Liu/MDL Nov. 2013 
! * remove calculating the U,V transport on boundary grids     
! * It is not necessary to call sub MNTMBD2 any longer for calculating U,V 
! * transport in shallow depth water and intermidiate depth water grids for 
! * Tide V3.1 spinup runs    
! *
! * HISTORY
! *    11/20/2013 Written by  --- Huiqing Liu / MDL
! *
! * NOTES
!*    
! *****************************************************************************/             
            SUBROUTINE MOMNTM2_ALL

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
      COMMON /SCND/   AR(600),AI(600),BR(600),BI(600),CR(600),CI(600)
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
!hliu      IF ((IFN-IST).LT.0) GO TO 570
      IF ((IFN-IST).LT.0) cycle
C
      DO 560 I=IST,IFN
      JUMP=1
C        NO TERRAIN HIGHER THAN 35 FT CAN BE FLOODED.

C     CHANGED BY NSM 11/20/2010 TO ALLOW TERRAIN HIGHER THAN 35FT TO FLOOD
C     DID NOT Accept.  The '4' is now correctly shifted to 56 and above
C     and if it was a '4' for 35..56, it is now a '1'.
!hliu      IF (ITREE(I,J).EQ.'4'.OR.ITREE(I,J).EQ.'6') GO TO 560
      IF (ITREE(I,J).EQ.'4'.OR.ITREE(I,J).EQ.'6') cycle
C      IF (ITREE(I,J).EQ.'6') GO TO 560

C        ITREE IS SET TO 1 OR 3 FOR LAKE WINDS, 2 OR 5 FOR OCEAN WINDS
        IF (ITREE(I,J).EQ.'2'.OR.ITREE(I,J).EQ.'5') THEN
         NCATG=2
         ELSE
         NCATG=1
         ENDIF
!--------------------------------------------------
! rewrite codes to remove GO TO statments
! Huiqing.Liu /MDL March. 2015
!--------------------------------------------------

!      IF (NCATG.EQ.1) GOTO 140
      IF (NCATG.NE.1.AND.ITREE(I,J).EQ.'2'.AND.ZBM(I,J).LT.-50.) THEN   ! 140
!            GOTO 160
            JUMP=1
            DHX=-HB(I-1,J-1)+HB(I,J-1)-HB(I-1,J)+HB(I,J)
            DHY=-HB(I-1,J-1)-HB(I,J-1)+HB(I-1,J)+HB(I,J)
            DPH=.25*(HB(I-1,J-1)+HB(I,J-1)+HB(I-1,J)+HB(I,J)+
     1         ZB(I-1,J-1)+ZB(I,J-1)+ZB(I-1,J)+ZB(I,J))
            DPHZ=MIN(599.,DPH)
            ID=MIN(599.,DPHZ)
            DPHI=ID
            AINTRP=AI(ID)+SLPAI(ID)*(DPHZ-DPHI)
      ELSE
C
C      TEST IF SURRNDNG SQRS ARE DRY, PART/FULL FLOOD .
C
c
      HMN=AMIN1(HB(I,J),HB(I-1,J),HB(I,J-1),HB(I-1,J-1))
!
!      IF (HMN.GT.ZBM(I,J)) THEN
      IF (HMN.LE.ZBM(I,J)) THEN
         HMXX=AMAX1(HB(I,J),HB(I-1,J),HB(I,J-1),HB(I-1,J-1))

!         IF (HMXX.GT.ZBM(I,J)) THEN 
         IF (HMXX.LE.ZBM(I,J)) THEN 
            GO TO 220
!            UB(I,J)=0.
!            VB(I,J)=0.
         ELSE
!            GO TO 300
            JUMP=2
C      DEFINE A MODIFIED SURFACE GRADIENT FOR A PARTIALLY FLOODED POINT
            HF1=AMAX1(HB(I-1,J-1),ZBM(I,J))
            HF2=AMAX1(HB(I  ,J-1),ZBM(I,J))
            HF3=AMAX1(HB(I-1,J  ),ZBM(I,J))
            HF4=AMAX1(HB(I  ,J  ),ZBM(I,J))
            DHX=-HF1+HF2-HF3+HF4
            DHY=-HF1-HF2+HF3+HF4
         END IF

      ELSE

         JUMP=1
         DHX=-HB(I-1,J-1)+HB(I,J-1)-HB(I-1,J)+HB(I,J)
         DHY=-HB(I-1,J-1)-HB(I,J-1)+HB(I-1,J)+HB(I,J)
!         GO TO 340

      END IF
!
!
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
!
      IF (DPH.LE.5.) THEN
         IF (JUMP.NE.2) THEN
            DO K=1,4
               IF(HPD(K).GE.5.) THEN
                  HPD(K)=HPD(K)-1.
               ELSE
C        USE ALTERNATIVE X-1+(1-X/5)**3 = X*(.4+.12*X-.008*X**2)
                  HPD(K)=HPD(K)*(.4+.12*HPD(K)-.008*HPD(K)*HPD(K))
               END IF
            END DO
C
            DPH=.25*(HPD(1)+HPD(2)+HPD(3)+HPD(4))
            DPHZ=DPH
            ID=AMIN1(599.,DPHZ)
         END IF
         ID=MAX0(1,ID)
         
      END IF
!
!
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

      END IF
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
!      IF (JUMP.EQ.2) GOTO 490
!JWC Huiqing Liu/MDL remove comment-----------------------------------------------------------------------
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
!------------------------------------------------------------
!     rewrite the following codes to remove the computed GOTO
!     Huiqing.Liu /MDL Feb. 2015
!------------------------------------------------------------
!--------------------------------------------------------------
! Old codes
!---------------------------------------------------------------

!--------------------------------------------------------------
! New codes
!---------------------------------------------------------------
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
        GOTO 560
        ENDIF
C        PRE-SET ITREEALONG BOUNDARIES; =4  FOR LAND (SKIP COMP.)
C                                       =9  FOR SHALLOW WATER
C                                       =8  FOR INTERMEDIATE DEPTHS
C                                       =6  FOR DEEP WATER (SKIP COMP.)
      IF(ITREE(ISB,JSB).EQ.'6'.OR.ITREE(ISB,JSB).EQ.'4') GO TO 560
c     Added by Huiqing Liu ---MDL Nov. 2013
c      IF(ITREE(ISB,JSB).EQ.'8'.OR.ITREE(ISB,JSB).EQ.'9') GO TO 560
c     Ended
      IF(ITREE(ISB,JSB).EQ.'8' ) IJUMP=2
C      WRITE(*,*) '  MNTMBD ISB,JSB',ISB,JSB,IJUMP
c      CALL MNTMBD2(JUMP,IJUMP,ISB,JSB,ARJ,AII,I,J,UBJ,VBJ,
c     1   FXB,FYB,NCATG,CSHLTR,NPLS)
      GO TO 560
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
      IF(UB(I,J).EQ.0.) GOTO 630
C
      IF(ITREE(ISB,JSB).EQ.'6'.OR.ITREE(ISB,JSB).EQ.'4') GO TO 560
c     Added by Huiqing Liu ---MDL Nov. 2013
c      IF(ITREE(ISB,JSB).EQ.'8'.OR.ITREE(ISB,JSB).EQ.'9') GO TO 560
c     Ended
      IF(ITREE(ISB,JSB).EQ.'8' ) IJUMP=2
c      CALL MNTMBD2(JUMP,IJUMP,ISB,JSB,ARJ,AII,I,J,UBJ,VBJ,
c     1   FXB,FYB,NCATG,CSHLTR,NPLS)
      GOTO 610
 630  UB(ISB,JSB)=0.
      VB(ISB,JSB)=0.
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
