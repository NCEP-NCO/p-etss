C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C ABSTRACT: EXTRACT SURFACE P,U, AND V FROM AVN GRIB FILES
C
C PROGRAM HISTORY LOG:
!c       03/2018--Huiqing Liu /MDL Modified to handle both CMCE and GEFS
!c       05/2018--Huiqing Liu /MDL Optimize the codes to reduce I/O
C USAGE:
C   INPUT FILES:
C      /COM/AVN/PROD/AVN.YYMMDD/GBLAV.THHZ.PGRBIFXX -- FORT.ZZ
C              - GRIB INDEX FILE FOR XX HOUR PROJECTION
C      /COM/AVN/PROD/AVN.YYMMDD/GBLAV.THHZ.PGRBFXX -- FORT.YY
C              - GRIB DATA FILE FOR XX HOUR PROJECTION
C          WHERE:
C            TODAY - DATE (YYMMDD) AND CYCLE (HH)
C               XX - 0,3,6,...,48
C               YY = 12+XX/3
C           AND ZZ = 32+XX/3
C
C      FORT.30  - FT11.EGAW  INPUT WINDOW INFORMATION FOR ALL BASIN
C
C
C   OUTPUT FILES:
C     FORT.51,52,53,54  - AVN_PUV.HHX (OLD GRIBHH)
C                         WINDOWED AVN SURFACE P-U-V FOR BASIN 'X'
C   TEMPORARY FILES:
C     FORT.81,82,83 - TEMPORARY WORK SPACE FOR P,U,V
C
C   LOG FILES:
C     FORT.96 -  SDS LOG FILE FOR ANY ERROR IN GRIB_PUV AND GRIB_EX RUN. 
C  
C SUBPROGRAM CALLED:
C    GETGB    - W3LIB
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN77 and fortran 90
C   MACHINE:  C90
C
C$$$
!     fortran 90
!
      subroutine wind_hind_cmc

      use grib_mod

      implicit none

      type (gribfield) :: Gfld
      logical          :: Unpack=.true.
      integer          :: K,Jksp,Jpdtn,Jxx,Iounit,Latin1,Latin2,Lonin1
      integer          :: Lunit,Kf,Idd,Iunit,Iret,Jdisc,Jgdtn,Jskp
      integer          :: Iprj,I,Iyear,Imonth,Iday,Numbasin,Ii,L,J,Ixx
      integer          :: Lonin2,Ilonul1,Ilonul2,Ihour,Ipint
      integer          :: Idatapnt,Iret1,Iret2,Iret3,Ic2to1
      integer          :: Irt,Idoub,Jdoub,ires
      integer          :: Iloc,Jloc

      integer,dimension(200)              :: Jids,Jpdt,Jgdt
      real,dimension(:),allocatable       :: P,U,V
      integer*2,dimension(:,:),allocatable:: Iint2p,Iint2u,Iint2v

      real :: res_wnd,half_lat,half_lon


      character (len=80)  :: Title,Title1
      character (len=255) :: Filei,fileb
      character (len=11)  :: Envvar
      character (len=1)   :: Achr,Aaa

      character (len=3),dimension (12) ::  Amon

      DATA AMON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1    'OCT','NOV','DEC'/

C     W3TAGB needs (Name, Julian day (see next line), seconds, org) 
C         tclsh : puts [clock format [clock seconds] -format "%j"]
      CALL W3TAGB('C10_GEN',2012,0341,0000,'OST25')
C
C
!-----------------------------------------------------------
      res_wnd=0.5 !GFS wind resolution (unit degree)
      ires=1/res_wnd
      idd=(180*ires+1)*(360*ires) ! 259920
! 0.5 by 0.5 degree 361 by 720 (-90 to 90 and 0 to 359.5)
      allocate (P(idd))
      allocate (U(idd))
      allocate (V(idd))

!------------------------------------------------------------
C     OPEN UNIT NUMBERS
C         Input P(ressure)/U/V files (full field)  extracted from
C         GRIB2 and stored in one file (kind of NDFD like).

C         Log file for any errors in run
      CALL GETENV('FORT96',FILEI)
      OPEN(96,FILE=FILEI)
C         Input basin dimmenension file
      CALL GETENV('FORT30',FILEI)
      OPEN(30,FILE=FILEI)
C         Output P(ressure)/U/V files Sampled to SLOSH grid.
      CALL GETENV('FORT51',FILEI)
      OPEN(51,FILE=FILEI,FORM='UNFORMATTED')
      CALL GETENV('FORT52',FILEI)
      OPEN(52,FILE=FILEI,FORM='UNFORMATTED')
      CALL GETENV('FORT53',FILEI)
      OPEN(53,FILE=FILEI,FORM='UNFORMATTED')
      CALL GETENV('FORT54',FILEI)
      OPEN(54,FILE=FILEI,FORM='UNFORMATTED')
      CALL GETENV('FORT55',FILEI)
      OPEN(55,FILE=FILEI,FORM='UNFORMATTED')
      CALL GETENV('FORT56',FILEI)
      OPEN(56,FILE=FILEI,FORM='UNFORMATTED')

C  EXTRACT PAST SURFACE PUV FROM AVN GRIB2 FILES FOR EVERY 12 HOUR ANALYSIS. 

      DO IUNIT=1,5
      LUNIT=IUNIT+10
      ENVVAR='FORT  '
      WRITE(ENVVAR(5:6),FMT='(I2)') LUNIT
      CALL GETENV(ENVVAR,FILEB)
      CALL BAOPENR(LUNIT,FILEB,IRET)

!  fortran 90
      jdisc=-1
      jids=-9999
!      jpdtn=-1
      jgdtn=-1
      jgdt=-9999
      jpdt=-9999
!
      jskp=0
      jpdtn=1 !PRODUCT template number 4.1
!

C  COMPUTE THE PROJECTION WE SHOULD BE WORKING WITH
      iprj=0

C  GET MSL PRESSURE AS REAL ARRAY P

!     Search for PRESS at MSL by production template 4.0
!
!      jpdt(1:15)=(/ 3,1,2,0,81,0,0,1,0,101,0,0,255,0,0 /)
      jpdt(1)=003
      jpdt(2)=001
      jpdt(10)=101
      jpdt(12)=0

      call getgb2(lunit,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,k,gfld,iret)
      p = gfld%fld
      kf = gfld%ndpts
      WRITE(*,*) KF,IRET
      WRITE(*,*) gfld%idsect(9),iprj
      IRET1=IRET

C  GET 10-METER WIND COMPONENT U

!     Search for U at 10 m by production template 4.0
!

!      jpdt(1:15)=(/ 2,2,2,0,81,0,0,1,0,103,0,10,255,0,0 /)
      jpdt(1)=002
      jpdt(2)=002
      jpdt(10)=103
      jpdt(12)=10

      call getgb2(lunit,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,k,gfld,iret)
      u = gfld%fld
!
      kf = gfld%ndpts

      IRET2=IRET
C
      IF (IRET.NE.0) WRITE (*,*)'  U ERR', IRET

C  GET 10-METER WIND COMPONENT V

!     Search for V at 10 m by production template 4.0
!

!      jpdt(1:15)=(/ 2,3,2,0,81,0,0,1,0,103,0,10,255,0,0 /)
      jpdt(1)=002
      jpdt(2)=003
      jpdt(10)=103
      jpdt(12)=10
!
      call getgb2(lunit,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,k,gfld,iret)
       v = gfld%fld
!
      kf = gfld%ndpts


      IF (IRET.NE.0) PRINT*,' V ERR', IRET
      IRET3=IRET

      if (Iret1.ne.0.or.Iret2.ne.0.or.Iret3.ne.0) then
         exit
      end if

C
      CALL BACLOSE (LUNIT,IRT)
C
C  OPEN WINDOW INFORMATION AND SET UPPER-LEFT CORNER OF 35 DEGREE WINDOW
C     READ NUMBER OF BASINS
      rewind(30)
      READ (30,*) NUMBASIN
      
      DO II=1,NUMBASIN
      IOUNIT=50+II
      READ (30,*) LATIN1,LATIN2
      READ (30,'(A1)') ACHR
      READ (30,*) LONIN1,LONIN2
      READ (30,*) 
      IF(ACHR.EQ.'W') THEN
        ILONUL1=360-LONIN1
        ILONUL2=360-LONIN2
      ELSE
        ILONUL1=LONIN1
        ILONUL2=LONIN2
      ENDIF

      IXX=(LATIN1-LATIN2)*ires + 1  ! 0.5 by 0.5 degree
      JXX=(ILONUL2-ILONUL1)*ires + 1 ! 0.5 by 0.5 degree
!
!
!      write(*,*)'ixd=',ixd,'jxd=',jxd
      allocate (IINT2P(IXX,JXX))
      allocate (IINT2U(IXX,JXX))
      allocate (IINT2V(IXX,JXX))
      IINT2P=0
      IINT2U=0
      IINT2V=0
!
!
C  PROCESS FOR ONE PROJECTION
      write (*,*)' BASIN =',II,IXX,JXX
      IC2TO1=0 
      Ihour = Gfld%idsect(9)
      Ipint = Iprj
      Iyear = Gfld%idsect(6)
      Imonth = Gfld%idsect(7)
      Iday = Gfld%idsect(8)
  
C  GENERATE HEADER FOR OUTPUT FILE
      TITLE(1:17)=' GRIB 10-METER   '
      WRITE (TITLE(18:),8000) IYEAR,IDAY,AMON(IMONTH),IHOUR,IPINT
      TITLE1(10:)='   COORDINATE (1,1) OF EXTRACTED FILE'
      WRITE (TITLE1(1:9),'(1X,I4,I4)') LATIN1,LONIN1
      WRITE (IOUNIT) IXX,JXX
      WRITE (IOUNIT) TITLE,TITLE1
C
8000  FORMAT (I4,' ',I2,' ',A3,'.',1X,I2.2,'Z FT=  ',I2.2,' AVN')

      iloc = 0
      do i=latin1,latin2,-1
      do idoub = 1, ires
        half_lat = i - (idoub-1)*res_wnd
        iloc = iloc + 1
        if (iloc.gt.ixx) exit
        jloc = 0
        do j=ilonul1,ilonul2
        do jdoub = 1, ires
        half_lon = j + (jdoub-1)*res_wnd
        jloc = jloc +1
        if (jloc.gt.jxx) exit
! CMCE is from -90 to 90 but GEFS is from 90 to -90 for lat
!        idatapnt=(90*ires-int(ires*half_lat))*360*ires+
!     $            int(half_lon*ires)+1
        idatapnt=(int(ires*half_lat)+90*ires)*360*ires+
     $            int(half_lon*ires)+1
        iint2u(iloc,jloc)=10*u(idatapnt)
        iint2v(iloc,jloc)=10*v(idatapnt)
        iint2p(iloc,jloc)=int(0.1*p(idatapnt))-10000
        enddo
        enddo
      enddo
      enddo
C
      WRITE (IOUNIT) ((IINT2U(I,J),I=1,IXX),J=1,JXX)
      WRITE (IOUNIT) ((IINT2V(I,J),I=1,IXX),J=1,JXX)
      WRITE (IOUNIT) ((IINT2P(I,J),I=1,IXX),J=1,JXX)

      write (*,*) ' IC2TO1 =',IC2TO1      
!      write(*,*)'ixd2=',iloc,'jxd2=',jloc
      deallocate (IINT2P)
      deallocate (IINT2U)
      deallocate (IINT2V)
      ENDDO
      enddo
      close (96)
      close (30)
      close(51)
      close(52)
      close(53)
      close(54)
      close(55)
      close(56)
      close(30)

      call gf_free (gfld)
      deallocate (P)
      deallocate (U)
      deallocate (V)
      CALL W3TAGE('C10_GEN')
!      STOP
       END
