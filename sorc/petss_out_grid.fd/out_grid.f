!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    PURPOSE:
!c 
!c       MAIN PROGRAM TO POST PROCESSING OF THE ETSS GRIDDED DATA
!c
!c    ARGUMENTS:
!c    
!c       POST PROCESSING OF THE ETSS OUTPUT DATA READ DATA FROM SSGRID.xxx AND 
!c       MERGED INTO NDFD HOURLY GRIB2 FILES SKIP THE HALF HOUR RECORDS 
!c
!c       METHOD: DIVIDE THIS INTO THREE CATEGORIES
!c         (1) Extract surge values from tropical grids,extra-tropical grids
!c             and merged into CONUS grids in 2.5km (East Coast, 
!c             GULF of Mexico, NEP/West Coast)
!c         (2) Extract surge values from extropical grids  and merged into ALSKA 
!c             grids in 3km (BBC,GULF of AK/NEP grids)
!c         (3) Extract surge values from tropical grids,extra-tropical grids
!c             and merged into CONUS grids in 625m (East Coast and
!c             GULF of Mexico)
!c
!c    INPUT FILES:
!c       FORT.10  - AREA
!c
!c    OUTPUTFILES:
!c
!c
!c    VARIABLES:
!c
!c      INPUT
!c       prod     == stn/exc/pro
!c
!c     OUTPUT
!c
!c    AUTHORS:
!c       Huiqing Liu /MDL Jan. 2015 
!c           
!c    HISTORY:
!c       01/2015--Huiqing Liu /MDL Created routine
!c       02/2015--Huiqing Liu /MDL Put the Mask related array to a module 
!c       01/2016--Huiqing Liu /MDL Extended to 102 hours 
!c       02/2017--Huiqing Liu /MDL Added header block
!c 
!c    NOTES: 
!c       INCLUDEING MODULE SUBSORT,WHICH WILL BE USED BY OTHER THREE SUBROUTINES
!c 
!c---------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      MODULE Mask_Array

      INTEGER:: Nx,Ny,Nxnymax,Ngribm,Ngdstmpl,Ipdstmplen,Idrstmplen
      INTEGER,DIMENSION(:,:,:),ALLOCATABLE:: Ival,Jval
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: Mask,Perc
      INTEGER,DIMENSION(:),ALLOCATABLE:: Pid
      REAL,DIMENSION(:,:),ALLOCATABLE:: Ha
      CHARACTER*1,DIMENSION(:),ALLOCATABLE:: Cgrib
      INTEGER:: Res_con,Res_ala,Nrec1
      CHARACTER*10    FLEEXT
      CHARACTER*11    PROBP

      END MODULE Mask_Array

      PROGRAM petss_out_grid

      USE Mask_Array

      implicit none

      CHARACTER*255 FIL_11
      CHARACTER*3 AREA, DTM
      CHARACTER*1 TROPICAL
      integer              :: startH,endH

!c     FORT.11  - CONTROL FILE FOR AREA AND PRODUCT TYPE,NESTING
      CALL GETENV('FORT11',FIL_11)
      OPEN(11,FILE=FIL_11)

      READ(11,*) AREA, FLEEXT, TROPICAL, PROBP, DTM, startH,endH
      WRITE (*,*) 'Merging grid: ', AREA, PROBP, DTM, startH,endH
!c-----------------------------------------------------
!c Different products use different prod tables, which
!c will have different length.
!c----------------------------------------------------- 
      IF (PROBP=='10p'.or.PROBP=='20p'.or.PROBP=='30p'.or.
     $    PROBP=='40p'.or.PROBP=='50p'.or.PROBP=='90p') THEN
          IPDSTMPLEN=32
      ELSE IF (PROBP=='max'.or.PROBP=='mean'.or.
     $    PROBP=='min') THEN
          IPDSTMPLEN=31
      ELSE
          IPDSTMPLEN=38
      ENDIF

      IF (TROPICAL=='Y') THEN
         CALL merge2p5km (AREA,DTM) ! Merging from extropical and tropical grids
      ELSEIF (TROPICAL=='N') THEN
         CALL merge3km (AREA,DTM) ! Merging only from extropical grids
      ELSE
         CALL merge625m (AREA,DTM,startH,endH) ! Merging 625m NDFD grids
      ENDIF

      END

