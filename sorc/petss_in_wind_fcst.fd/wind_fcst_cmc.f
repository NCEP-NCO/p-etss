C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
!-----------------------------------------------------------
C Copy the codes from wind_hind.f90 and modified to read 
C Canidian ensemble winds (CMCE)
C
C
C PROGRAM HISTORY LOG:
C!
C!  03-18     Huiqing Liu Ace Info/MDL switch to subroutine to handle both CMC and GEFS
C!  05-18     Huiqing Liu /MDL Optimize the codes to reduce I/O
C!-----------------------------------------------------------

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
C      FORT.30 MAPPED TO FORT.84 (5/10/2007)
C
C
C   OUTPUT FILES:
C     FORT.51,52,53,54  - AVN_PUV.HHX (OLD GRIBHH)
C                         WINDOWED AVN SURFACE P-U-V FOR BASIN 'X'
C     FORT.51.52.53.54 MAPPED to FORT.85.86.87.88 (5/10/2007)
C
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
C   LANGUAGE: FORTRAN77 and fortran90
C   MACHINE:  WCOSS
C
C$$$

!-----------------------------------------------------------
      subroutine wind_fcst_cmc (wndshr)
!     fortran 90 
!
      use grib_mod

      implicit none

      integer          :: wndshr   ! winds projection hours 102 or 108 -hr
      type (gribfield) :: gfld
      logical          :: Unpack=.true.
      integer          :: K,Jksp,Jpdtn,Iounit,Latin1,Latin2,Lonin1
      integer          :: Lunit,Kf,Iunit,Iret,Jdisc,Jgdtn,Jskp
      integer          :: Iprj,Iyear,Imonth,Iday,Numbasin,Ii,L,I,J
      integer          :: Lonin2,Ilonul1,Ilonul2,Ihour,Ipint
      integer          :: Idatapnt,Idd,Ixx,Jxx,ires,Idoub,Jdoub
      integer          :: Iloc,Jloc
      real             :: res_wnd,half_lat,half_lon

      integer,dimension(200) :: jids,jpdt,jgdt
      real,dimension(:),allocatable:: P,U,V
      integer*2,dimension(:,:),allocatable:: IINT2P,IINT2U,IINT2V

!      INTEGER LUNIT,KF
!      REAL P(259920),U(259920),V(259920) ! 0.5 by 0.5 degree
!
!    .5 by .5 degree
!      INTEGER*2 IINT2P(71,161),IINT2U(71,161),IINT2V(71,161) 
      character (len=80)  :: Title,Title1
      character (len=255) :: Fil_51,Filei,fileb
      character (len=11)  :: Envvar
      character (len=1)   :: Achr,Aaa

      character (len=3),dimension (12) ::  Amon
      DATA AMON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1    'OCT','NOV','DEC'/
C
C     W3TAGB needs (Name, Julian day (see next line), seconds, org)
C         tclsh : puts [clock format [clock seconds] -format "%j"]
      CALL W3TAGB('MDL_CY_PUV10',2012,0341,0000,'OST25')
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
      CALL GETENV('FORT49',FILEI)
      OPEN(49,FILE=FILEI)
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

C  EXTRACT SURFACE PUV FROM AVN GRIB FILES FOR EVERY 3 HOUR PROJECTION.

!      do iunit=1,35    ! 102-hr
      do iunit=1,wndshr/3+1
         lunit = iunit + 10
         ENVVAR='FORT  '
         write(Envvar(5:6),FMT='(I2)') Lunit
         call getenv(Envvar,Fileb)
         call baopenr(Lunit,Fileb,Iret)
         write(*,*) Lunit,Fileb,Iret

!  fortran 90
         jdisc=-1
         jids=-9999
         jgdtn=-1
         jgdt=-9999
         jpdt=-9999
!
         jskp=0
         jpdtn=1 !PRODUCT template number 4.N
!

C  COMPUTE THE PROJECTION WE SHOULD BE WORKING WITH      
         iprj=(IUNIT-1)*3

C  GET MSL PRESSURE AS REAL ARRAY P

!     Search for PRESS at MSL by production template 4.0
!
         jpdt(1)=003
         jpdt(2)=001
         jpdt(10)=101
         jpdt(12)=0

         call getgb2(lunit,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,k,gfld,iret)
         p = gfld%fld
!
         KF = gfld%ndpts
!
         IF(IRET.NE.0) exit

!
C  GET 10-METER WIND COMPONENT U
!     Search for U at 10 m by production template 4.0
!
        jpdt(1)=002
        jpdt(2)=002
        jpdt(10)=103
        jpdt(12)=10
!

        call getgb2(lunit,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,k,gfld,iret)
        u = gfld%fld
!     
        KF = gfld%ndpts

        IF(IRET.NE.0) exit

!
C  GET 10-METER WIND COMPONENT V
!     Search for V at 10 m by production template 4.0
!
        jpdt(1)=002
        jpdt(2)=003
        jpdt(10)=103
        jpdt(12)=10
!
        call getgb2(lunit,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,k,gfld,iret)
        v = gfld%fld
!       
        KF = gfld%ndpts
        IF(IRET.NE.0) exit
!
!C  GENERATE HEADER FOR OUTPUT FILE
        close (lunit)

        rewind(49)
        read (49,*) Numbasin
        do Ii = 1, Numbasin
           Iounit = 50 + Ii
           read (49,*) Latin1,Latin2
           read (49,'(A1)') Achr
           read (49,*) Lonin1,Lonin2
           read (49,*)
           if(Achr == 'W') then
              Ilonul1 = 360 - Lonin1
              Ilonul2 = 360 - Lonin2
           else
              Ilonul1 = Lonin1
              Ilonul2 = Lonin2
           endif

           IXX=(LATIN1-LATIN2)*ires + 1  ! 0.5 by 0.5 degree
           JXX=(ILONUL2-ILONUL1)*ires + 1! 0.5 by 0.5 degree

           allocate (IINT2P(IXX,JXX))
           allocate (IINT2U(IXX,JXX))
           allocate (IINT2V(IXX,JXX))
           IINT2P=0
           IINT2U=0
           IINT2V=0

           iyear = gfld%idsect(6)
           imonth = gfld%idsect(7)
           iday = gfld%idsect(8)
           Ihour = Gfld%idsect(9)
           Ipint = Iprj
C  GENERATE HEADER FOR OUTPUT FILE
C
           TITLE(1:17)=' GRIB 10-METER   '
           WRITE (TITLE(18:),8000) IYEAR,IDAY,AMON(IMONTH),IHOUR,IPINT
           TITLE1(10:)='   COORDINATE (1,1) OF EXTRACTED FILE'
           WRITE (TITLE1(1:9),'(1X,I4,I4)') LATIN1,LONIN1
           WRITE (IOUNIT) IXX,JXX
           WRITE (IOUNIT) TITLE,TITLE1
8000       FORMAT (I4,' ',I2,' ',A3,'.',1X,I2.2,'Z FT=  ',I2.2,' AVN')

!-----------------------------------------------------
 
C  PROCESS FOR ONE PROJECTION
           write(*,*) ' BASIN =',Ii,Ixx,Jxx,Iprj
! 0.5 by 0.5 degree (361*720 DIM; -90 to 90 and 0 to 359.5 Lat and Lon)
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
!              idatapnt=(90*ires-int(ires*half_lat))*360*ires+
!     $            int(half_lon*ires)+1
               idatapnt=(int(ires*half_lat)+90*ires)*360*ires+
     $                   int(half_lon*ires)+1
               iint2u(iloc,jloc)=10*u(idatapnt)
               iint2v(iloc,jloc)=10*v(idatapnt)
               iint2p(iloc,jloc)=int(0.1*p(idatapnt))-10000
            enddo
            enddo
           enddo
           enddo
!
           WRITE (IOUNIT) ((IINT2U(I,J),I=1,IXX),J=1,JXX)
           WRITE (IOUNIT) ((IINT2V(I,J),I=1,IXX),J=1,JXX)
           WRITE (IOUNIT) ((IINT2P(I,J),I=1,IXX),J=1,JXX)

!      write(*,*)'ixd2=',iloc,'jxd2=',jloc
           deallocate (IINT2P)
           deallocate (IINT2U)
           deallocate (IINT2V)
        end do
      end do

      IF (IRET.EQ.0) THEN
        AAA='Y'
        WRITE (96,801) AAA
      ELSE
        AAA='N'
        WRITE (96,801) AAA,IRET,'=RETURN CODE'
      ENDIF
801   FORMAT(A1,I5,A15)
      CLOSE(96)
      CLOSE(49)
      CLOSE(51)
      CLOSE(52)
      CLOSE(53)
      CLOSE(54)
      CLOSE(55)
      CLOSE(56)

!
      call gf_free(gfld)
      deallocate (P)
      deallocate (U)
      deallocate (V)

!
      CALL W3TAGE('MDL_CY_PUV10')

      STOP
       END

