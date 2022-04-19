      SUBROUTINE merge2p5km (AREA,DTM)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c       MERGING OF THE MODEL GRIDDED DATA TO 2.5km ALASKA NDFD GRID BY USING
!c       TROPICAL and EXTRA-TOPICAL GRIDS
!c      
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
!c       Modelers /MDL, Arthur Taylor, Huiqing Liu /MDL
!c           
!c    HISTORY:
!c       03/1995--Modelers /MDL Created the routine
!c       10/2014--Huiqing Liu /MDL Updated the routine to read 2.5/3km mask 
!c                                 files
!c       02/2015--Huiqing Liu /MDL Updated to use dynamicall allocated
!c                                 array instead of statically allocated arrays
!c       08/2015--Huiqing Liu /MDL Updated to use tropical and extra-tropical grids
!c       01/2016--Huiqing Liu /MDL Updated the routine to read different res
!c                                 mask files
!c       01/2017--Huiqing Liu /MDL Put the routine to a independent fortran file
!c       02/2017--Huiqing Liu /MDL Added header block
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      USE Mask_Array
      
      implicit none

      CHARACTER*3     AREA,DTM
      CHARACTER*1     BASIN
      CHARACTER*255   FIL_12,FIL_49
      CHARACTER*255   FIL_48,FIL_53,FIL_10,FIL_54
      CHARACTER*255   FIL_13,FIL_14,FIL_15
      CHARACTER*255   FIL_16,FIL_17,FIL_18,FIL_19,FIL_20
      CHARACTER*255   FIL_21,FIL_22,FIL_23,FIL_24,FIL_25
      CHARACTER*255   FIL_26,FIL_27,FIL_28,FIL_29,FIL_30
      CHARACTER*255   FIL_31,FIL_32,FIL_33,FIL_34,FIL_35
      CHARACTER*255   FIL_36,FIL_37,FIL_38,FIL_39,FIL_40
      CHARACTER*255   FIL_41,FIL_42,FIL_43,FIL_44

      integer      :: Lcgrib
      integer      :: I,J,K,L,M,Mm,Hour,Imax,Jmax,N,Mhour,Ii
      integer      :: Ryear,Rmonth,Rday,Rhour,Rmin,Rsec
      integer      :: Ifcsthr
      integer      :: Ibsn1,Ibsn2,Ibsn3,Perc1,Perc2

      integer,dimension(:),allocatable  :: Imxb,Jmxb,Mask_F
      real,dimension(:,:),allocatable   :: HaMax,Hamax_6hr,Ha_tmp
      real,dimension(:,:,:),allocatable :: Hb

!C     W3TAGB needs (Name, Julian day (see next line), seconds, org)
!C         tclsh : puts [clock format [clock seconds] -format "%j"]
      CALL W3TAGB('GRIDMERGE',2014,162,00,'OST25')

!C     OPEN UNIT NUMBERS
!C     FORT.10  - CONTROL FILE FOR WHICH TROPICAL BASINS ARE USED
      CALL GETENV('FORT10',FIL_10)
      OPEN(10,FILE=FIL_10)

      IF(AREA=='con') THEN
        MM=32
      ELSE IF(AREA=='ala') THEN
        MM=3
      ELSE
        WRITE(*,*) 'ERROR == NO MATCH AREA!'
      END IF

!C     FORT.12  - CONTROL FILE FOR BASIN SIZE
      CALL GETENV('FORT12',FIL_12)
      OPEN(12,FILE=FIL_12)
      CALL GETENV('FORT49',FIL_49)
      OPEN(49,FILE=FIL_49,FORM='UNFORMATTED')
!C     FORT.48  - CONTROL FILE CONTAINING THE DATE AND TIME
      CALL GETENV('FORT48',FIL_48)
      OPEN(48,FILE=FIL_48)
!C     FORT.53  - ssforecast.CCB    SURGE ARCHIVE FOR BIG MAP
      CALL GETENV('FORT53',FIL_53)
      OPEN(53,FILE=FIL_53,FORM='UNFORMATTED')
      CALL GETENV('FORT54',FIL_54)
      OPEN(54,FILE=FIL_54,FORM='UNFORMATTED')

!c     3 extra-tropical basins

!C     FORT.13  - SURGE HIGHT BASIN1 (SSGRID.CCB (CC = CYCLE, B=BASIN)
      CALL GETENV('FORT13',FIL_13)
      OPEN(13,FILE=FIL_13,FORM='UNFORMATTED')
!C     FORT.14  - SURGE HIGHT BASIN2
      CALL GETENV('FORT14',FIL_14)
      OPEN(14,FILE=FIL_14,FORM='UNFORMATTED')
!C     FORT.15  - SURGE HIGHT BASIN3 (NOT USED FOR ALASKA AREA)
      CALL GETENV('FORT15',FIL_15)
      OPEN(15,FILE=FIL_15,FORM='UNFORMATTED')

!c    29 Tropical basins

      CALL GETENV('FORT16',FIL_16)
      OPEN(16,FILE=FIL_16,FORM='UNFORMATTED')

      CALL GETENV('FORT17',FIL_17)
      OPEN(17,FILE=FIL_17,FORM='UNFORMATTED')

      CALL GETENV('FORT18',FIL_18)
      OPEN(18,FILE=FIL_18,FORM='UNFORMATTED')

      CALL GETENV('FORT19',FIL_19)
      OPEN(19,FILE=FIL_19,FORM='UNFORMATTED')

      CALL GETENV('FORT20',FIL_20)
      OPEN(20,FILE=FIL_20,FORM='UNFORMATTED')

      CALL GETENV('FORT21',FIL_21)
      OPEN(21,FILE=FIL_21,FORM='UNFORMATTED')

      CALL GETENV('FORT22',FIL_22)
      OPEN(22,FILE=FIL_22,FORM='UNFORMATTED')

      CALL GETENV('FORT23',FIL_23)
      OPEN(23,FILE=FIL_23,FORM='UNFORMATTED')

      CALL GETENV('FORT24',FIL_24)
      OPEN(24,FILE=FIL_24,FORM='UNFORMATTED')

      CALL GETENV('FORT25',FIL_25)
      OPEN(25,FILE=FIL_25,FORM='UNFORMATTED')

      CALL GETENV('FORT26',FIL_26)
      OPEN(26,FILE=FIL_26,FORM='UNFORMATTED')

      CALL GETENV('FORT27',FIL_27)
      OPEN(27,FILE=FIL_27,FORM='UNFORMATTED')

      CALL GETENV('FORT28',FIL_28)
      OPEN(28,FILE=FIL_28,FORM='UNFORMATTED')

      CALL GETENV('FORT29',FIL_29)
      OPEN(29,FILE=FIL_29,FORM='UNFORMATTED')

      CALL GETENV('FORT30',FIL_30)
      OPEN(30,FILE=FIL_30,FORM='UNFORMATTED')

      CALL GETENV('FORT31',FIL_31)
      OPEN(31,FILE=FIL_31,FORM='UNFORMATTED')

      CALL GETENV('FORT32',FIL_32)
      OPEN(32,FILE=FIL_32,FORM='UNFORMATTED')

      CALL GETENV('FORT33',FIL_33)
      OPEN(33,FILE=FIL_33,FORM='UNFORMATTED')

      CALL GETENV('FORT34',FIL_34)
      OPEN(34,FILE=FIL_34,FORM='UNFORMATTED')

      CALL GETENV('FORT35',FIL_35)
      OPEN(35,FILE=FIL_35,FORM='UNFORMATTED')

      CALL GETENV('FORT36',FIL_36)
      OPEN(36,FILE=FIL_36,FORM='UNFORMATTED')

      CALL GETENV('FORT37',FIL_37)
      OPEN(37,FILE=FIL_37,FORM='UNFORMATTED')

      CALL GETENV('FORT38',FIL_38)
      OPEN(38,FILE=FIL_38,FORM='UNFORMATTED')

      CALL GETENV('FORT39',FIL_39)
      OPEN(39,FILE=FIL_39,FORM='UNFORMATTED')

      CALL GETENV('FORT40',FIL_40)
      OPEN(40,FILE=FIL_40,FORM='UNFORMATTED')

      CALL GETENV('FORT41',FIL_41)
      OPEN(41,FILE=FIL_41,FORM='UNFORMATTED')

      CALL GETENV('FORT42',FIL_42)
      OPEN(42,FILE=FIL_42,FORM='UNFORMATTED')

      CALL GETENV('FORT43',FIL_43)
      OPEN(43,FILE=FIL_43,FORM='UNFORMATTED')

      CALL GETENV('FORT44',FIL_44)
      OPEN(44,FILE=FIL_44,FORM='UNFORMATTED')


!c----------------------------------------------------
!c Allocate Dimension to Arrays
!c---------------------------------------------------- 
      HOUR=103
      NGDSTMPL=22
!c      IPDSTMPLEN=15
      IDRSTMPLEN=16

      ALLOCATE (Imxb(Mm))
      ALLOCATE (Jmxb(Mm))
      ALLOCATE (Mask_f(Mm))

!C FILL THESE WITH CORRECT REFERENCE DATE.
      READ(48,'(I4,2I2,1X,I2)') RYEAR, RMONTH, RDAY, RHOUR
      RMIN=0
      RSEC=0

!C******  READ IMXB,JMXB FROM ETS_GridMerge.area UNIT 12 ************
      DO 110 M=1,MM
         READ(12,*) BASIN
         READ(12,*) IMXB(M),JMXB(M)
         MASK_F(M)=1
         IF(AREA=='con')READ(10,*)MASK_F(M)
         
         IF(MASK_F(M).NE.0)WRITE(*,*)'merge_basin=',BASIN
 110  CONTINUE
!c----------------------------------------------------
!c Allocate Dimension to Arrays
!c----------------------------------------------------
      IMAX = MAXVAL (IMXB)
      JMAX = MAXVAL (JMXB)
      ALLOCATE (HB(MM,IMAX,JMAX))

!C***** CALL THE MAP PROJECTION PROGRAM **************
!C***** TO LOCATE THE INDEX FOR BIG MAP AT FILL DATA*****

      CALL MAP_PROJ(MM,2)

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


      Nxnymax= Nx * Ny
      ALLOCATE (Ha(Nx,Ny))
      ALLOCATE (Ha_tmp(Nx,Ny))
      ALLOCATE (HaMax(Nx,Ny))
      ALLOCATE (HaMax_6hr(Nx,Ny))
!C****** READ HOURLY STORM SURGE DATA***********
!c Add the maximum value to one grib2 file HOUR+1
!c-----------------------------------------------
      Ha = 9999.
      HaMax = 9999. !Initialize all grid value to 9999
      HaMax_6hr = 9999.
      Ha_tmp = 9999.

      DO 120 N=1,HOUR

        DO 130 M=1,MM
          IF(AREA=='con'.AND.MASK_F(M).NE.0)THEN
             IF(N==1) READ(12+M) MHOUR
             READ(12+M) ((HB(M,I,J),I=1,IMXB(M)),J=1,JMXB(M))
          ENDIF
 130    CONTINUE
!c        write(*,*)'maxvalue of mask1=',maxval(mask(1,:,:))
!c       READ TIDE ONLY RESULTS FOR 2 EXTRA-TROPICAL BASINS (EEX2 and
!c       EGM3)

!C******* DO MAP PROJECTION ****************
        DO 140 I=1,NX
          DO 150 J=1,NY
            HA(I,J)= 9999 !Initialize all grid value to 9999

            if (mask(i,j) < 3.and.Perc(i,j)==1) then !Extra-tropical basins if necessary

               IF (MASK(I,J)==1) THEN ! EEX2 extra-tropical basin

                  HA(I,J)=HB(1,IVAL(1,I,J),JVAL(1,I,J))

               ELSE IF (MASK(I,J)==2) THEN !EGM3 extra-tropical basin
 
                  HA(I,J)=HB(2,IVAL(2,I,J),JVAL(2,I,J))
               ENDIF
            elseif (MASK(I,J)==4) THEN !EWC2 western extra-tropical basin

                   HA(I,J)=HB(3,IVAL(3,I,J),JVAL(3,I,J))

            elseif (mask(i,j) > 10 .and. mask(i,j) < 40) then ! 29 Tropical basins without overlap
              
               ibsn1=mask(i,j)-7 ! e.g. pn2 is 4th member in HB(*,i,j) with mask value 11
               HA(I,J)=HB(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))

            elseif (mask(i,j)>100.and.mask(i,j)<10000) then ! 29 Tropical basins with 2 overlap

               ibsn1=mask(i,j)/100
               ibsn2=mask(i,j)-ibsn1*100
               ibsn1=ibsn1-7
               ibsn2=ibsn2-7

               if (Perc(i,j)>100) then
                  write(*,*)'Perc =',Perc(i,j),'is not allowed'
                  stop
               endif 

               if (hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))>100) then !one of them is dry then ignore this

                 ! ha(i,j)=hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))
                 ha(i,j)=9999

               elseif (hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))>100) 
     $         then

                 ! ha(i,j)=hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))
                 ha(i,j)=9999

               else                                                    !Both of them are wet 
                   HA(I,J)=HB(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))*
     1                     Perc(i,j)*.01 + 
     2                     HB(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))*
     3                     (1-Perc(i,j)*.01) 
               endif

            elseif (mask(i,j)>10000) THEN ! 29 Tropical basins with 3 overlap

               ibsn1=mask(i,j)/10000
               ibsn2=(mask(i,j)-ibsn1*10000)/100
               ibsn3=mask(i,j)-ibsn1*10000-ibsn2*100
               ibsn1=ibsn1-7
               ibsn2=ibsn2-7
               ibsn3=ibsn3-7
  
               Perc1=Perc(i,j)/100
               Perc2=Perc(i,j)-Perc1*100

               if (Perc1>100 .or. Perc2 > 100) then
                  write(*,*)'Perc1 and Perc2 =',Perc1, Perc2
                  stop 
               endif 

               if (hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))>100.and.
     1             hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))>100.and.
     2             hb(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))>100) then

                  ha(i,j)=9999

               elseif (hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))>100
     1           .and.hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))>100)then

                  ha(i,j)=hb(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))

               elseif (hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))>100
     1           .and.hb(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))>100)then

                  ha(i,j)=hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))

               elseif (hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))>100
     1           .and.hb(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))>100)then

                  ha(i,j)=hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))

              elseif (hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))>100)then

                  ha(i,j)=hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))*0.5
     1                   +hb(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))*0.5

              elseif (hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))>100)then

                  ha(i,j)=hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))*0.5
     1                   +hb(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))*0.5

              elseif (hb(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))>100)then

                  ha(i,j)=hb(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))*0.5
     1                   +hb(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))*0.5
               else
                  HA(I,J)=HB(ibsn1,IVAL(ibsn1,I,J),JVAL(ibsn1,I,J))*
     1                 Perc1*.01 + 
     2                 HB(ibsn2,IVAL(ibsn2,I,J),JVAL(ibsn2,I,J))*
     3                 Perc2*.01 +
     2                 HB(ibsn3,IVAL(ibsn3,I,J),JVAL(ibsn3,I,J))*
     3                 (1-Perc1*.01-Perc2*.01)

               endif
           endif

            if (ha(i,j).gt.100) ha(i,j)=9999
            IF (HA(I,J).GT.9999.or.HA(I,J).LT.-9999) THEN
              WRITE (*,*) "Bad merge: (Hour, Value, Mask) =", 
     1               N, HA(I,J), MASK(I,J)
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
     1            HaMax(i,j)>100.and.Ha(i,j)<=100) then
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
!c e.g. 0-6, 0-12, 0-18,...,0-102
         Ha=HaMax
         CALL MKGRIB(LCGRIB,RYEAR,RMONTH,RDAY,RHOUR,RMIN,RSEC,
     1            AREA,IFCSTHR,6,192,DTM)
         WRITE(54) (CGRIB(K),K=1,LCGRIB)
!c e.g. 0-6, 6-12, 12-18,...., 96-102
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
!c      Ha=HaMax
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
      DEALLOCATE (MASK_F)
      DEALLOCATE (CGRIB)
      DEALLOCATE (HA)
      DEALLOCATE (HA_TMP)
      DEALLOCATE (HaMax)
      DEALLOCATE (HaMax_6hr)
      DEALLOCATE (MASK)
      DEALLOCATE (IVAL)
      DEALLOCATE (JVAL)
      DEALLOCATE (HB)

      CALL W3TAGE('GRIDMERGE')
      END
