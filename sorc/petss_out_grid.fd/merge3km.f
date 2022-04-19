      SUBROUTINE merge3km (AREA,DTM)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c       MERGING OF THE MODEL GRIDDED DATA TO 3km ALASKA NDFD GRID
!c
!c    ARGUMENTS:
!c       READ DATA FROM SSGRID.CCB(00a for example)
!c       SKIP THE HALF HOUR RECORDS
!c       CALL map_proj ROUTINE TO GET THE PROJECTION I,J INDEX
!c       AND CREATE THE NDFD GRIB2 FILE
!c
!c    INPUT FILES:
!c       FORT.11  - CONTROL FILE FOR AREA AND CYCLE
!c       FORT.12  - CONTROL FILE FOR BASIN SIZE
!c       FORT.13  - SURGE HIGHT BASIN1 (SSGRID.CCB (CC = CYCLE, B=BASIN)
!c       FORT.14  - SURGE HIGHT BASIN2
!c       FORT.15  - SURGE HIGHT BASIN3 (NO USE FOR ALASKA AREA)
!c       FORT.16  - MAP PROJECTION GRIDS conus.bin OR alaska.bin
!c       FORT.17  - CONTROL FILE CONTAINING THE DATE AND TIME
!c
!c    OUTPUTFILES:
!c       FORT.53  - SURGE ARCHIVE FOR BIG MAP (GRIB2)
!c
!c    VARIABLES:
!c      INPUT
!c       AREA  == con (CONUS) OR ala (ALASKA)
!c       BASIN == E,W,G  OR  A,Z,K
!c       NXMAX == MAXIMUM OF NX IN ALASKA AND CONUS AREA
!c       NYMAX == MAXIMUM OF NY IN ALASKA AND CONUS AREA
!c       IMAX  == MAXIMUM OF IMXB IN ALL BASINS
!c       JMAX  == MAXIMUM OF JMXB IN ALL BASINS
!c       ATMP  == TEMPERORY ARRAY FOR FILTERING HALF HOUR DATA
!c          HB == SOURCE ARRAY
!c      OUTPUT
!c          HA == DESTINATION ARRAY
!c    AUTHORS:
!c       Modelers /MDL, Arthur, Taylor, Huiqing Liu /MDL
!c           
!c    HISTORY:
!c       03/1995--Modelers /MDL Created the routine
!c       10/2014--Huiqing Liu /MDL Updated the routine to read 2.5/3km mask 
!c                                 files
!c       02/2015--Huiqing Liu /MDL Updated to use dynamicall allocated
!c                                 array instead of statically allocated arrays
!c       01/2016--Huiqing Liu /MDL Updated the routine to read different res
!c                                 mask files
!c       01/2017--Huiqing Liu /MDL Put the routine to a independent fortran file
!c       02/2017--Huiqing Liu /MDL Added header block
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      USE Mask_Array

      CHARACTER*3     AREA,DTM
      CHARACTER*1     BASIN
      CHARACTER*255   FIL_11,FIL_12,FIL_13,FIL_14,FIL_15,FIL_16
      CHARACTER*255   FIL_17,FIL_53,FIL_54

      integer      :: Lcgrib
      integer      :: I,J,K,L,M,Mm,Hour,Imax,Jmax,N,Mhour
      integer      :: Ryear,Rmonth,Rday,Rhour,Rmin,Rsec
      integer      :: Ifcsthr
      integer      :: Ibsn1,Ibsn2,Ibsn3,Perc1,Perc2
 
      integer,dimension(:),allocatable  :: Imxb,Jmxb
      real,dimension(:,:),allocatable   :: Atmp,HaMax,Hamax_6hr,Ha_tmp
      real,dimension(:,:,:),allocatable :: Hb

!C     W3TAGB needs (Name, Julian day (see next line), seconds, org)
!C         tclsh : puts [clock format [clock seconds] -format "%j"]
      CALL W3TAGB('GRIDMERGE',2014,162,00,'OST25')

!C     OPEN UNIT NUMBERS
!C     FORT.12  - CONTROL FILE FOR BASIN SIZE
      CALL GETENV('FORT12',FIL_12)
      OPEN(12,FILE=FIL_12)

      READ(12,*) MM

!C     FORT.13  - SURGE HIGHT BASIN1 (SSGRID.CCB (CC = CYCLE, B=BASIN)
      CALL GETENV('FORT13',FIL_13)
      OPEN(13,FILE=FIL_13,FORM='UNFORMATTED')
!C     FORT.14  - SURGE HIGHT BASIN2
      CALL GETENV('FORT14',FIL_14)
      OPEN(14,FILE=FIL_14,FORM='UNFORMATTED')
!C     FORT.15  - SURGE HIGHT BASIN3 (NOT USED FOR ALASKA AREA)
      IF(MM.EQ.3) THEN
        CALL GETENV('FORT15',FIL_15)
        OPEN(15,FILE=FIL_15,FORM='UNFORMATTED')
      ENDIF
!C     FORT.16  - MAP PROJECTION GRIDS conus.bin OR alaska.bin
      CALL GETENV('FORT16',FIL_16)
      OPEN(16,FILE=FIL_16,FORM='UNFORMATTED')
!C     FORT.17  - CONTROL FILE CONTAINING THE DATE AND TIME
      CALL GETENV('FORT17',FIL_17)
      OPEN(17,FILE=FIL_17)
!C     FORT.53  - ssforecast.CCB    SURGE ARCHIVE FOR BIG MAP
      CALL GETENV('FORT53',FIL_53)
      OPEN(53,FILE=FIL_53,FORM='UNFORMATTED')
      CALL GETENV('FORT54',FIL_54)
      OPEN(54,FILE=FIL_54,FORM='UNFORMATTED')

!C****** READ AREA AND CYCLE FROM CONTROL.TXT, UNIT 11********
     
      IF(AREA=='con') THEN
         WRITE(*,*) 'CONUS area'
      ELSE IF(AREA=='ala') THEN
         WRITE(*,*) 'ALASKA area'
      ELSE
        WRITE(*,*) 'ERROR == NO MATCH AREA!'
      END IF

!c----------------------------------------------------
!c Allocate Dimension to Arrays
!c---------------------------------------------------- 
      HOUR=103
!c      NGRIBM=12958000
      NGDSTMPL=22
!c      IPDSTMPLEN=15
      IDRSTMPLEN=16

      ALLOCATE (Imxb(Mm))
      ALLOCATE (Jmxb(Mm))
!c      ALLOCATE (Cgrib(Ngribm))
!c-----------------------------------------------------

!C FILL THESE WITH CORRECT REFERENCE DATE.
      READ(17,'(I4,2I2,1X,I2)') RYEAR, RMONTH, RDAY, RHOUR
      RMIN=0
      RSEC=0

!C******  READ IMXB,JMXB FROM ETS_GridMerge.area UNIT 12 ************
      DO 110 M=1,MM
         READ(12,*) BASIN
         READ(12,*) IMXB(M),JMXB(M)
 110  CONTINUE

!c----------------------------------------------------
!c Allocate Dimension to Arrays
!c----------------------------------------------------
      IMAX = MAXVAL (IMXB)
      JMAX = MAXVAL (JMXB)
      ALLOCATE (ATMP(IMAX,JMAX))
      ALLOCATE (HB(MM,IMAX,JMAX))
!C***** CALL THE MAP PROJECTION PROGRAM **************
!C***** TO LOCATE THE INDEX FOR BIG MAP AT FILL DATA*****

      CALL MAP_PROJ(MM,1)

      Nxnymax= Nx * Ny
      ALLOCATE (Ha(Nx,Ny))
      ALLOCATE (Ha_tmp(Nx,Ny))
      ALLOCATE (HaMax(Nx,Ny))
      ALLOCATE (HaMax_6hr(Nx,Ny))

      IF (Nx == 1073 .or. Nx == 825) THEN !5/6km grid
         NGRIBM = 2958000
         Res_con = 5079406
         Res_ala = 5953125
      ELSE
         NGRIBM = 12958000 !2.5/3km grid
         Res_con = 2539703
         Res_ala = 2976560
      END IF
      ALLOCATE (Cgrib(Ngribm))

      Ha = 9999.
      HaMax = 9999. !Initialize all grid value to 9999
      HaMax_6hr = 9999.
      Ha_tmp = 9999.

!C****** READ HOURLY STORM SURGE DATA***********
!c Add the maximum value to one grib2 file HOUR+1
!c---------------------------------------------------

      DO 120 N=1,HOUR
        DO 130 M=1,MM
          IF(N==1) READ(12+M) MHOUR
          READ(12+M) ((HB(M,I,J),I=1,IMXB(M)),J=1,JMXB(M))
 130    CONTINUE

        HA = 9999 !Initialize all grid value to 9999

!C******* DO MAP PROJECTION ****************
        DO 140 I=1,NX
          DO 150 J=1,NY
            IF(MASK(I,J)==0) THEN
              HA(I,J)= 9999
            ELSE IF (MASK(I,J)==1) THEN
              IF (IVAL(1,I,J) >= 1.and.JVAL(1,I,J) >=1.and.
     1            IVAL(1,I,J)<=IMXB(1).and.JVAL(1,I,J)<=JMXB(1)) then
                  HA(I,J)=HB(1,IVAL(1,I,J),JVAL(1,I,J))
              ENDIF
            ELSE IF (MASK(I,J)==2) THEN
              HA(I,J)=HB(2,IVAL(2,I,J),JVAL(2,I,J))
            END IF
            if (ha(i,j).gt.100) ha(i,j)=9999
            IF (HA(I,J).GT.9999.or.HA(I,J).LT.-9999) THEN
              WRITE (*,*) "Bad merge: (Hour, Value, Mask) =", 
     1                N, HA(I,J), MASK(I,J)
              HA(I,J) = 9999
            END IF
!c----------------------------------------
!c find the maximum value of time series
!c---------------------------------------- 
           if ( n == 1) then
              HaMax(i,j)=Ha(i,j)
              HaMax_6hr(i,j)=Ha(i,j)
           else
              if (Ha(i,j)<=100.and.Ha(i,j)>HaMax(i,j).or.
     1         HaMax(i,j)>100.and.Ha(i,j)<=100) then
               HaMax(i,j)=Ha(i,j)
              endif
              if (Ha(i,j)<=100.and.Ha(i,j)>HaMax_6hr(i,j).or.
     1         HaMax_6hr(i,j)>100.and.Ha(i,j)<=100) then
               HaMax_6hr(i,j)=Ha(i,j)
              endif


           endif



 150      CONTINUE
 140    CONTINUE


!C SET IFCSTHR TO THE CURRENT HOUR (THROUGH THE LOOP)

!c hourly to hour 102
      IFCSTHR=N-1
      if (DTM == 'DAT'.and. (n-1) > 0) then ! NAVD88 generate 1-hr inc

         CALL MKGRIB(LCGRIB,RYEAR,RMONTH,RDAY,RHOUR,RMIN,RSEC,
     1            AREA,IFCSTHR,1,2,DTM)
         WRITE(53) (CGRIB(K),K=1,LCGRIB)
      endif

!c 6-hr incremental and cumulative groups to hour 102       

      if (mod(n-1,6) == 0 .and. (n-1) > 0 ) then

         Ha_tmp=Ha ! Copy 6-hr incremental value to temporary array,
!c                    which will be reassigned back to HaMax_6hr
!c e.g. 0-6, 0-12, 0-18,... 0-102
         Ha=HaMax
         CALL MKGRIB(LCGRIB,RYEAR,RMONTH,RDAY,RHOUR,RMIN,RSEC,
     1            AREA,IFCSTHR,6,192,DTM)
         WRITE(54) (CGRIB(K),K=1,LCGRIB)
!c e.g. 0-6, 6-12, 12-18,... 96-102
         Ha=Hamax_6hr
         Hamax_6hr=Ha_tmp

         if (DTM == 'AGL') then ! AGL generate 6-hr inc

            CALL MKGRIB(LCGRIB,RYEAR,RMONTH,RDAY,RHOUR,RMIN,RSEC,
     1            AREA,IFCSTHR,6,2,DTM)
            WRITE(53) (CGRIB(K),K=1,LCGRIB)

         endif

      endif
           
      write(*,*)'hour=',n,'LCGRIB=',LCGRIB
!C      WRITE(*,*) K, ", ", L, ", ", HA(K,L)
 120  CONTINUE
!c--------------------------------------------------------------------------------
!c Assgin HaMax to Ha at Last loop (hour+1) in order to write HaMax to
!c Grib2 file
!c--------------------------------------------------------------------------------
      Ha=HaMax
!c      IFCSTHR=hour-1
!c      CALL MKGRIB(LCGRIB,RYEAR,RMONTH,RDAY,RHOUR,RMIN,RSEC,
!c     1            AREA,IFCSTHR,1,1)
!c      WRITE(54) (CGRIB(K),K=1,LCGRIB)

!C W3TAGE can either take PROG, or PROG,KYR,JD,LF,ORG
!C    in the later case you may need the bacio library.
!c-----------------------------------------------------
!c Deallocate Arrays
!c-----------------------------------------------------
      close(53)
      close(54)
      DEALLOCATE (IMXB)
      DEALLOCATE (JMXB)
      DEALLOCATE (CGRIB)
      DEALLOCATE (HA)
      DEALLOCATE (HA_TMP)
      DEALLOCATE (HaMax)
      DEALLOCATE (HaMax_6hr)
      DEALLOCATE (MASK)
      DEALLOCATE (IVAL)
      DEALLOCATE (JVAL)
      DEALLOCATE (HB)
      DEALLOCATE (ATMP)

      CALL W3TAGE('petss_out_grid')
      END
