C note C-code also calls: llx2pq(&lt_pnt1,&lg_pnt1,&i,&j);
C note get rid of program main in runslhg.f...

!
!*****************************************************************************
! * module char_length and ARRAY(as1,as2,as3,as4) 
! *
! * Huiqing Liu / MDL
! *
! * PURPOSE
! *    Created a dynamic assigned character arrays to avoid -check bound error
! * passing from c language procedure (slosh2.c) 
! *
! * ARGUMENTS
! *    as*   = String length of character array passing from c code(Input)
! *
! * HISTORY
! *  10/2015 Huiqing Liu (MDL): Created
! *
! * NOTES
! *   
! *****************************************************************************
! *

      module char_length
!      implicit none
      integer :: as1,as2,as3,as4
      end module

      subroutine ARRAY (asize1,asize2,asize3,asize4)
      use char_length
      integer, intent(in) :: asize1,asize2,asize3,asize4
      as1 = asize1
      as2 = asize2
      as3 = asize3
      as4 = asize4
      end subroutine

      SUBROUTINE INITAL (MMHALT,IIMXB,JJMXB,ZZB,MBHR,MMIN,MBDY,MBNT,
     1                   MYR,FFLE5,FFLE9,FFLE91,FFLE40,DDELT,MF_TID)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE CALLS ALL THE FORTRAN SUBROUTINES WHICH ARE NEEDED TO
C INITIALIZE THE FORTRAN VARIABLES BEFORE WE ENTER INTO THE COMPUTE LOOP.
C
C ORIGINALLY THIS WAS DONE IN PROGRAM MAIN AND CMPUTE.
C THE CODE IS THE SAME EXCEPT CUT AND PASTED HERE.
C
C THIS ALSO RETURNS TO C THE VARIOUS INITIAL VARIABLES NEEDED SO THAT
C IT CAN PROPERLY CONTROL THE EXECUTION OF THE PROGRAM.
c
C
C Updated by----- Huiqing Liu /MDL Nov. 2013
C
c Added one more parameter MF_TID (tide_nest) to control where we put tide 
c or nesting boundary information,only deep water(tide_nest=1) or all open water 
c boundary (tide_nest=0 default value)
c---------------------------------------------------------------------------/

      use char_length

      include 'parm.for'
      REAL ZZB(M_,N_)
!      CHARACTER*257 FFLE5,FFLE9,FFLE91,FFLE40
!--------------------------------------------------------------------
! Huiqing Liu/MDL Oct. 2015 
!
! Created a dynamic assigned character arrays to avoid -check bound
! error passing from c language procedure (slosh2.c) 
!
      CHARACTER(len=as1),intent(in):: FFLE5
      CHARACTER(len=as2),intent(in):: FFLE9
      CHARACTER(len=as3),intent(in):: FFLE91
      CHARACTER(len=as4),intent(in):: FFLE40
!----------------------------------------------------------------------
!
!      COMMON /WNDENS/ MGEFS_CMCE
! Huiqing Liu/MDL March. 2018
! COMM BLOCK for GEFS (MGEFS_CMCE = 1) or CMCE (2) or ECMWF (3)
      COMMON /IDENT/  AIDENT(40),DACLOK(7)
      COMMON /CESAV/  IPN(250),IPL(250),KHSPT,IPN2(250),IPL2(250),
     $                KHSPT2
      COMMON /PRHSY/  IPRHR,JUMPR,IPRT
      COMMON /KEYS/ KEY1
C STIME interferes with C code, so switched to STIME2
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /FFTH/   ITIME,MHALT
      COMMON /EGTH/   DELS,DELT,G,COR
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /FLES/ FLE5,FLE9,FLE8,FLE91,FLE99,FLE10,FLE20,FLE30,FLE1
!
! Huiqing.Liu /MDL June 2014 ETSS 2.0 wind control window info.
!
      COMMON /IFT11 / LATIN1,LATIN2,LONIN1,LONIN2
      COMMON /DATINC/ XLAT1,XLON1,DELLT,DELLN,IMXC,JMXC
      COMMON /DATEE/ IDATE1(4) !Holding the date information of start time in setcp and stmvl1

!
      CHARACTER*256 FLE5,FLE9,FLE8,FLE91,FLE99,FLE10,FLE20,FLE30,FLE1
      CHARACTER*256 FLE40
      common /landfl/ lftime
      CHARACTER*80   lftime
      CHARACTER*3 CMNT,TMNT(12)

      INTEGER NDYMNT(12)
      DATA NDYMNT/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA TMNT/'JAN','FEB','MAR','APR','MAY','JUN',
     1            'JUL','AUG','SEP','OCT','NOV','DEC'/

C CODE FOR PROGRAM MAIN STARTS.
C
C      READ (*,'(A)') FLE5
      FLE5 = FFLE5
      WRITE(*,*)'   TRK and BASIN FILE DTA',FFLE5, FFLE9
C      READ (*,'(A)') FLE9
      FLE9 = FFLE9
      KEY1=1
C        ARCHIVE SURGE ENVELOP TO A FILE
C      READ (*,'(A)') FLE91
      FLE91 = FFLE91
C
c      CALL STRMPR
C
C      FLE40 = 'FT40'
      FLE40 = FFLE40

CCCCC Arthur: Removed "FT40" option 8/2/2004
CCCC      CALL EXTRPT (FLE40)
CCCCC
CC      WRITE (*,*)' READ IN LAT/LONG OF THE GRIDS'
C      READ (*,'(A)') FLE99
C Commented out FLE99 which was the name of the .llx file.
C      FLE99 = FFLE99
        
c----------------------------------------------------------------
C  Added by----- Huiqing Liu /MDL Nov. 2013
C  To prepare the model input,parameters and determine the number
C  of static height boundary grid:
C  (1) f_tide V3.1 or nesting runs----all ocean boundary grids
C  (2) other cases only deep water grids 
c----------------------------------------------------------------
c      CALL INITLZ

!
! Huiqing.Liu /MDL June 2014 ETSS2.0
!
      CALL INITLZ1(MF_TID)
!      write(*,*) 'After Initlz1'
!      call tflush
!      CALL INITLZ
!
! Huiqing.Liu /MDL
!
!
!      ENDIF
c----------------------------------------------------------------
c
C CODE Located at top of CMPUTE... Initialize here, do rest in time step.
C
C    SELECTION OF SNAPSH FROM 'C' OPTION.
C     OPEN (77,FILE='SN001',FORM='UNFORMATTED')
c
c   include 'cmpgr1.for'
C
c Arthur: This part goes into intrface.f (but we don't need conditional.
C     IF (MHALT.EQ.0) GOTO 1111
c
      JUMPR=1
C      INCSM=20                  Arthur: Moved into loop...
C      IF (EBSN1.EQ.'+') INCSM=1       : local variable needs to be in timestep
c
c
!
! Huiqing.Liu /MDL modified to ETSS which need read GFS wind instead of
! using parmametric winds June 2014
!
!        CALL SETCMP

       XLAT1=LATIN1
       XLON1=LONIN1
! Combine 3 subroutine into one subroutine to read gefs/cmce/ecmwf wind
       CALL SETCP (0)
!
! Huiqing.Liu /MDL
!

        CALL INTLHT
CC        CALL RDLTLG
C
!      IF (KHSPT.EQ.0) THEN
C      OPEN (10,FILE='$$fle10',FORM='UNFORMATTED')
C      OPEN (20,FILE='$$fle20',FORM='UNFORMATTED')
C      OPEN (30,FILE='$$fle30',FORM='UNFORMATTED')
C      WRITE(10) AIDENT
C      WRITE(20) AIDENT
C      WRITE(30) AIDENT
!      ELSE
!      OPEN (13,FILE='$$fle13',FORM='UNFORMATTED')
!      OPEN (14,FILE='$$fle14',FORM='UNFORMATTED')
!      OPEN (15,FILE='$$fle15',FORM='UNFORMATTED')
!      WRITE(13) AIDENT
!      WRITE(14) AIDENT
!      WRITE(15) AIDENT
!      ENDIF
C
C     End of compute and initall... return stuff to C.
C     CODE TO RETURN VALUES TO C CODE
C
      MMHALT = MHALT
C start of time parse in fortran
! Huiqing.Liu /MDL set the date of model for rex and tide
!
!       LFTIME='HR1800 21 MAY 2014'

!      READ(LFTIME,2020) MHR,MMIN,MDAY,CMNT,MYR
! 2020 FORMAT(2X,2I2.2,1X,I2.2,1X,A3,1X,I4)
      MHR = IDATE1(4)
      MMIN = 0
      MDAY = IDATE1(3)
      CMNT = TMNT(IDATE1(2))
      MYR = IDATE1(1)
      write(*,*)'mhr=,mday,myr,cmnt',mhr,mday,cmnt,myr
      call tflush
!
!
      IF(MOD(MYR,4).EQ.0) NDYMNT(2)=29
      IF(MOD(MYR,100).EQ.0) NDYMNT(2)=28
      IF(MOD(MYR,400).EQ.0) NDYMNT(2)=29
!      DO MBNT=1,12
!        IF(CMNT.EQ.TMNT(MBNT)) GOTO 2021
!      END DO
! 2021 NTMLNG=JHR-IBGNT
!      NTMLNG=JHR-IBGNT
      mbnt=IDATE1(2)
      ntmlng=0
      MBHR=MHR-NTMLNG
      MBDY=MDAY+MBHR/24
      MBHR=MOD(MBHR,24)
      IF (MBHR.LT.0) THEN
        MBDY=MBDY-1
        MBHR=24+MBHR
      ENDIF
      IF (MBDY.LT.1) THEN
        MBNT=MBNT-1
        IF(MBNT.LT.1) THEN
          MYR=MYR-1
          MBNT=MBNT+12
        ENDIF
        MBDY=NDYMNT(MBNT)+MBDY
      ENDIF
C Arthur Added... 5/26/2005
      IF (MBDY.GT.NDYMNT(MBNT)) THEN
        MBDY=MBDY-NDYMNT(MBNT)
        MBNT=MBNT+1
        IF(MBNT.GT.12) THEN
          MYR=MYR+1
          MBNT=MBNT-12
        ENDIF
      ENDIF
C end of time parse in fortran

      IIMXB = IMXB
      JJMXB = JMXB
C The actual computation grid is jmxb-1 imxb-1
      DO J=1,JMXB
         DO I=1,IMXB
            ZZB(I,J)=ZB(I,J)
         END DO
      END DO
C Pass back the delt to C.
      DDELT = DELT
      RETURN
      END


      SUBROUTINE WAV
      COMMON /WTEMP/  YDELP,ZDELP,PNN,YC24,ZC24
      REAL YDELP,ZDELP,PNN,YC24,ZC24
      COMMON /DUMY44/ SW(800),CW(800),W1218(2),WMAX(2)
      REAL SW,CW,W1218,WMAX
      COMMON /STRMPS/ X(999),Y(999),P(999),R(999),DIR(999),SP(999)
      REAL X,Y,P,R,DIR,SP
C STIME interferes with C code, so switched to STIME2
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      INTEGER ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      REAL VM,RM,DP,VF,HN,TS
C UNITS: YC24 IS IN STATUTE MILES
C UNITS: YDELP IS IN MILBARS
C UNITS: VF, WMAX ARE IN MPH
CC      WRITE (*,*) 'Rmax: ',YC24, ' statute miles'
CC      WRITE (*,*) 'dP: ',YDELP, ' mb'
CC      WRITE (*,*) 'Vf: ',SP(ITMADV -1), ' mph'
CC      WRITE (*,*) 'VMax: ',WMAX(2) + 0.5*SP(ITMADV -1), ' mph'
      VM = WMAX(2) + 0.5*SP(ITMADV -1)
C     WRITE (*,*) 'Rmax: ',(YC24 / 1.15), ' nautical miles'
C     WRITE (*,*) 'dP: ',(YDELP * 0.029565), ' in'
C     WRITE (*,*) 'Vf: ',(SP(ITMADV -1) / 1.15), ' knots'
C     WRITE (*,*) 'Vmax: ',(VM / 1.15), ' knots'
      RM = YC24 / 1.15
      DP = YDELP *0.029565
      VF = SP(ITMADV -1) / 1.15
      VM = VM / 1.15
      HN = 16.5 * EXP((RM * DP)/100)
      HN=HN*(1+(0.208*VF)/(SQRT(VM)))
CC      WRITE (*,*)
CC      WRITE (*,*) 'H. = ', HN, ' ft'
      TS = 8.6 * EXP((RM * DP)/200)
      TS=TS*(1+(0.104*VF)/SQRT(VM))
CC      WRITE (*,*) 'Ts = ', TS, ' sec'
CC      WRITE (*,*)
      RETURN
      END

      SUBROUTINE TMSTEP(IITIME,MMHALT,HHB,DDELT,XXLT,XXLG,YYDELP,YYC24,
     1WSPEED,WDIRCT,FLAG,SMOOTH,FPASS,ITIDE,INEST,IREX,IBOUN,TF_HR)
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
C Updated by Huiqing Liu/MDL in Jan.,2014 for adding boundary option(IBOUN)
C and damping time (TF) used by partial clamped radiation boundary
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      use parm_gfs1
      use parm_gfs2

      include 'parm.for'
      REAL HHB(M_,N_)
c REAL   HHHB(M_,N_)
      INTEGER*2 FLAG,SMOOTH,FPASS

!      COMMON /WNDENS/ MGEFS_CMCE
! Huiqing Liu/MDL March. 2018
! COMM BLOCK for GEFS (MGEFS_CMCE = 1) or CMCE (2) or ECMWF (3)

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
cBeg current output by Brian
      COMMON /CURRENT/ CurrentMag(M_,N_),U_Current(M_,N_),
     1                 V_Current(M_,N_)
CEnd current output by Brian
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

C CODE TO IMPLEMENT ONE TIME STEP..
! Huiqing.Liu /MDL Debug smooth frequency
  100 INCSM=20
!  100 INCSM=10
!
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
      CALL TTIMER
      xtime=itime
c
c  continuity
c
C      CALL CPU_TIME(ELAPSE)
C      WRITE (*,*) "5a", ELAPSE
C      CALL TFLUSH
! Huiqing.Liu /MDL Jan. 2015 speeding model in T1 mode
!------------------------------------------------------
! Added Skip Model Run if Tide model is T1 (tide only)!
! ITIDE=-1                                            !
!------------------------------------------------------
      if(itide.ne.-1)then !Huiqing.Liu /MDL if tide version 1 
      CALL CONTTY
      endif
c
C       NO CORNER SMOOTHING for PNB BASIN
c

C      IF (EBSN1.EQ.'&'.or.EBSN1.EQ.'+') GOTO 113
!------------------------------------------------------------
!     rewrite the following codes to remove the GOTO Statements
!     Huiqing.Liu /MDL March. 2015
!------------------------------------------------------------

!
!
!      IF (ISMPT2G.EQ.0) GOTO 113
!
      IF (ISMPT2G.NE.0) THEN
         IF (ITIME.GT.5.AND.INCSM.NE.1)  CALL SMPT2G
      END IF
!
!
 113  CONTINUE
c
c
c
cd Debug Huiqing Liu /MDL New Boundary Condition
      IF (INEST.EQ.2.OR.INEST.EQ.21) THEN 
         CALL BDRYHT_NEST
      ELSE 
         SELECT CASE (IBOUN)
            CASE (0)
                 CALL BDRYHT_ETSS
            CASE (1)
                 CALL PCGWR (TF_HR)
         END SELECT
      ENDIF
cd Debug Huiqing Liu /MDL New Boundary Condition
      IF (NPSS.NE.0) CALL CHANNL
      IF (NSQRWC.NE.0.and.nof1d.ne.'+') CALL FLW1DM
      CALL HMXSV
C Create C-COPY OF HB TO HHB.. DO THIS RIGHT AFTER HMXSV
C HMXSV is where the envelope calculations are done, so
C we want them as close together as possible.
      IF(FPASS.EQ.1) THEN
        DO J=1,JMXB
          DO I=1,IMXB
C This is so that HHB is never greater than HMX
            IF (HMX(I,J).LT.HB(I,J)) THEN
              HHB(I,J)=HMX(I,J)
            ELSE
              HHB(I,J)=HB(I,J)
            ENDIF
          END DO
        END DO
      ENDIF
C
      IPRHR=3
!
!
!      IF (ITIME.NE.IPRTSV) GO TO 180
      IF (ITIME.EQ.IPRTSV) THEN
!

         IF(ITIME.EQ.NHRAD) IPRHR=1
! Getting rid of output at half hour Huiqing.Liu April. 2018
!         IF(ITIME.EQ.NHRAD-NDLTHR/2) IPRHR=2
         IPRTSV=IPRTSV+IPRTAD
!
! Huiqing.Liu /MDL modified to ETSS which need stations and whole grid
! output at every half hour interval
! NHRAD--Number of steps within 1 hour
! NDLTHR--Number of step within 1 hour
!
         IF (IPRHR.NE.3) CALL HISTRY1
!      write(*,*) 'After HISTRY1'
!      call tflush
      END IF
!

c
  180 CONTINUE
C
C       IF (KEY1.EQ.0) GOTO 1112
C
c      IF(ITIME+1.EQ.NHRAD.or.EBSN1.EQ.'+') THEN
! Huiqing.Liu June 2014 Switch to apply smoothing every time step, which
! is same as ETSS does
!      IF(mod(ITIME+1,INCSM).EQ.0.or.EBSN1.EQ.'+') THEN
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
!      ENDIF
c
c  momentum equation
c
C      CALL CPU_TIME(ELAPSE)
C      WRITE (*,*) "5j", ELAPSE
C      CALL TFLUSH
! Huiqing.Liu /MDL Jan. 2015 speeding model in T1 mode
!------------------------------------------------------
! Added Skip Model Run if Tide model is T1 (tide only)!
! ITIDE=-1                                            !
!------------------------------------------------------       
       if(itide.ne.-1)then !Huiqing.Liu /MDL Jan. 2015 if tide T1
       CALL MOMNTM (ITIDE,INEST,IREX)
       endif
c      CALL MOMNTM
C      CALL CPU_TIME(ELAPSE)
C      WRITE (*,*) "5k", ELAPSE
C      CALL TFLUSH
C
!
! Huiqing.Liu /MDL modified to ETSS which need read GFS wind instead of
! using parmametric winds June 2014
! Combine 3 subroutine into one subroutine to read gefs/cmce/ecmwf wind
!
!      CALL STMVAL
      CALL STMVL(0)
!
! Huiqing.Liu /MDL
!

c
C GRAPHICS
C
c    include 'cmpgr2.for'
C
C END OF COMPUTE FOR ONE TIME STEP

C ARTHUR: PROCEDURE FOR CS.
C NOTE: ITMADV HAS BEEN ADVANCED FOR NEXT HOUR IN CALL TO STMVAL
      IF (FLAG.EQ.1) THEN
        CALL WAV
      ENDIF

C END CODE FOR ONE TIME STEP

C CODE TO RETURN TO C-CODE THE REST OF THE VALUES IT NEEDS.
      IITIME = ITIME
      MMHALT = MHALT
      DDELT = DELT
!      YYDELP = YDELP
      YYDELP = 10. !Huiqing.Liu delp of trk
!      YYC24 = YC24
      YYC24 = 10. !Huiqing.Liu size of trk
!      XA=(C1+AX)/5280.
!      YA=(C2+AY)/5280.
!      CALL XY2LLX(XA,YA,XXLT,XXLG)
      XXLT=10. !Huiqing.Liu lat of trk
      XXLG=10. !Huiqing.Liu lon of trk
!      WSPEED=C21
      WSPEED=10. !Huiqing.Liu wnd speed of trk
C Direction is 0 for north. (I think this is meteorological direction)
C WDIRCT is returned in degrees.
!      WDIRCT=(AZMTH-(C22)*180./3.14159265358979)
      WDIRCT=0. !Huiqing.Liu wnd direction of trk
!
! Huiqing.Liu /MDL June 2014 ETSS2.0 write out grid output files      
!
      IF (ITIME.EQ.MHALT) THEN
      REWIND (81)
      REWIND (82)
      REWIND (83)
      REWIND (84)
      REWIND (85)
      REWIND (86)
      REWIND (87) !Temporary file to hold Second station output Oct. 2014
      REWIND (88) !Temporary file to hold water level AGL output Mar. 2016

      CALL FLEGEN
      CALL FLEGEN2

!      CALL W3TAGE('EXT_6H')

      CLOSE (96)
      CLOSE (81)
      CLOSE (82)
      CLOSE (83)
      CLOSE (84)
      CLOSE (85)
      CLOSE (86)
      CLOSE (11)
      CLOSE (14)
!      CLOSE (19)
      CLOSE (21)
!      CLOSE (25)
      CLOSE (33)
      CLOSE (34)
      CLOSE (52)
      CLOSE (53)
      CLOSE (15) !Second station list
      CLOSE (55) !File to hold second station output
      CLOSE (87) 
      CLOSE (88) 
      CLOSE (54) 
! H.Liu Allocated dimension of wind arrays 04/2020
      deallocate (IU)
      deallocate (IV)
      deallocate (IP)
      deallocate (IUG)
      deallocate (IVG)
      deallocate (XIU)
      deallocate (XIV)
      deallocate (XIP)
      deallocate (XIUG)
      deallocate (XIVG)

      ENDIF
!
! Huiqing.Liu /MDL 
!
      RETURN
      END

      SUBROUTINE CLNUP (HHB, iENV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE CALLS THE FORTRAN SUBROUTINES NEEDED TO SAVE THE
C MAX ENVELOP, AND TO CLEAN UP EXECUTION OF THE CODE.
C
C ORIGINALLY THIS WAS DONE AT THE END OF CMPUTE (AFTER 999) AND AT THE
C END OF PROGRAM MAIN.
C THE CODE IS THE SAME EXCEPT CUT AND PASTED HERE.
C
C THIS ALSO RETURNS TO C THE VARIOUS VARIABLES NEEDED SO THAT
C IT CAN DISPLAY THE MAX ENVELOP.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      include "parm.for"
      REAL HHB(M_,N_)
      COMMON /FFTH/   ITIME, MHALT
      COMMON /KEYS/ KEY1
      COMMON /FLES/ FLE5,FLE9,FLE8,FLE91,FLE99,FLE10,FLE20,FLE30,FLE1
      CHARACTER*256 FLE5,FLE9,FLE8,FLE91,FLE99,FLE10,FLE20,FLE30,FLE1
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2

C FINISHING UP THE CMPUTE CODE
CC      write (*,*)'   finished at time step=',itime
CC      write (*,*)'   mhalt=' ,mhalt
c      CALL FILTRP
      CALL FHMXSV
      if (ienv.eq.1) CALL ARCHIV

C
C      GOTO 1112
C 1111 call rdhmx
C 1112 CONTINUE
c
c   include 'cmpgr4.for'
C
C    FINISHING UP WHAT WAS LEFT IN MAIN PROGRAM
!------------------------------------------------------------
!     rewrite the following codes to remove the GOTO Statements
!     Huiqing.Liu /MDL March. 2015
!------------------------------------------------------------

!
!      IF (KEY1.EQ.0) GOTO 1000
!
!
C
CC       WRITE(*,*)'   FINISHED SAVING SURGE ENVELOP (FT*10) TO DISK.'
CC       WRITE(*,'(A)') '  FILE NAME = '
CC       WRITE(*,'(A)')  FLE91
CC       WRITE(*,*)'   TIME-HISTRIES data ARE SAVED ON'
CC       WRITE(*,'(A)') FLE10
CC       WRITE(*,'(A)') FLE20
CC       WRITE(*,'(A)')  FLE30
CC       WRITE(*,*) ' RESPECTIVELY FOR SURGE, WIND SPEED, AND WIND DIR.'
CC       WRITE(*,*) '  IN FEET, MPH, METEOR. DIRECTION (DEG.)'
C
 1000 CONTINUE
c
c       CALL PLOTS1
C
C      CALL GETDAT(MYR,MMON,MDAY)
C      CALL GETTIM(MHR,MMIN,MSEC,MI100)
C
C      WRITE(*,100) MMON,MDAY,MYR,MHR,MMIN,MSEC,MI100
C 100  FORMAT(1H0,'         CALCULATIONS ENDED AT DAY - TIME ',
C    1   2(I2.2,':'),I4.4,'--',2(I2.2,':'),I2.2,'.',I2.2//)

C CODE TO RETURN VALUES TO C-CODE.
      DO J=1,JMXB
         DO I=1,IMXB
            HHB(I,J)=HMX(I,J)
         END DO
      END DO
      RETURN
      END

      SUBROUTINE XY2LLX(XA,YA,XLT,XLG)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /BSN/    PHI,ALTO,ALNO,PHI1,ALT1,ALN1,ALT1C
      REAL PHI,ALTO,ALNO,PHI1,ALT1,ALN1,ALT1C
      COMMON /ORIGN/  XMOUTH,YMOUTH
      REAL XMOUTH,YMOUTH

C PASSED PARAMETERS
      REAL XA,YA,XLT,XLG

C LOCAL VARIABLES
      REAL CLTO,SLTO,XCORD,YCORD,RLL,ENU,PSI,SPSI,CPSI,CNU
      REAL DLAT,DLONG

      CLTO=COS(ALT1C)
      SLTO=SIN(ALT1C)
      XCORD=(XA-XMOUTH)*5280./6080.21
      YCORD=(YA-YMOUTH)*5280./6080.21
      RLL=SQRT(XCORD**2+YCORD**2)
      IF (RLL.LT.1.E-5) THEN
        ENU=PHI1
      ELSE
        ENU=ATAN2(XCORD,YCORD)+PHI1
      ENDIF
      PSI=2.*ATAN(.5*RLL/3437.87)
      SPSI=SIN(PSI)
      CPSI=COS(PSI)
      CNU=COS(ENU)
      DLAT=ACOS(CLTO*CPSI+SPSI*SLTO*CNU)
      DLONG=ALN1-ATAN2(SPSI*SIN(ENU),CPSI*SLTO-SPSI*CNU*CLTO)
      XLT=90.-DLAT/1.74532925E-2
      XLG=DLONG/1.74532925E-2
      return
      end

      SUBROUTINE STATICHT (I,J,ANS)
C        JELESNIANSKI   SEPTEMBER 1980 TDL   IBM 360/195
C        Taylor         May 2013 MDL
C
C        PURPOSE
C           Borrowed most of this code from BDRYHT to compute the static
C        height for deep water cells where we want to force the tide:
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
      INCLUDE 'parm.for'
C
      COMMON /DUMMY4/ S(800),C(800),P(800),DELP(800)
      COMMON /STRMSB/ C1,C2,C21,C22,AX,AY,PTENCY,RTENCY
      COMMON /DATUM/  SEADTM,DTMLAK
C
C       BOUNDARY SQUARES DETERMINED IN SUBROUTINE DEPSFC
C
      XR=ELPCL(I)*COSL(J)
      YR=ELPDL(I)*SINL(J)
      X=XR-C1-AX
      Y=YR-C2-AY
      RSQ=X*X+Y*Y
      R1=SQRT(RSQ)/5280.+1.
      K=R1
      R2=K
      DR=R1-R2
      K=MIN0(K,790)
      ANS=DELP(K)+DR*(DELP(K+1)-DELP(K))+SEADTM
      RETURN
      END

      SUBROUTINE ISSUBGRID (I,J,IANS)
C        Taylor         May 2013 MDL
C
C        PURPOSE
C           Determine if a cell is a subgrid cell.
      include "parm.for"
      COMMON /FLWCPT/ NSQRS,NSQRW,NSQRWC,NPSS,NCUT

      if (i.eq.155.and.j.eq.3) then
        write (*,*) "Checking on 155, 3, Here"
      endif

      DO L=1,NSQRWC
        IF (ISQR(L).EQ.I.AND.JSQR(L).EQ.J) THEN
          IF (L.LE.NSQRW) THEN
C Check banks
            IF (DELCUT(L).LT.1.) THEN
              IANS = 1
              RETURN
            ENDIF
          ELSE
C Check cuts
            LL = L - NSQRW
            IF (CUTLI(LL).LT.1..OR.CUTLE(LL).LT.1.) THEN
              IANS = 1
              RETURN
            ENDIF
          ENDIF
        ENDIF
      END DO

      IANS = 0
      RETURN
      END
