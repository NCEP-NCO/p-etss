      PROGRAM petss_out_stn
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    PURPOSE:
!c 
!c       MAIN PROGRAM TO POST PROCESSING OF THE MODEL STATION DATA
!c
!c    ARGUMENTS:
!c    
!c       POST PROCESSING OF THE MODEL OUTPUT DATA READ DATA *.txt 
!c
!c       METHOD: DIVIDE THIS INTO THREE CATEGORIES
!c         (1) Extract surge values from tropical grids (East Coast and GULF of Mexico)
!c         (2) Extract surge values from extropical grids (West Coast,
!c             GULF of AK and NEP grids)
!c         (3) Extract surge values from extropical grids but output to
!c             separate files in order to keep consistence with old format
!c             text file (BBC grids)
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
!c
!c     OUTPUT
!c
!c    AUTHORS:
!c       Huiqing Liu /MDL Jan. 2017 
!c           
!c    HISTORY:
!c       01/2017--Huiqing Liu /MDL Created routine
!c       02/2017--Huiqing Liu /MDL Added header block
!c 
!c    NOTES: 
!c 
!c---------------------------------------------------
      
      implicit none

      CHARACTER*255 FIL_IN
      CHARACTER*1 AREA
      INTEGER SPINUP,FCSTHR


      CALL GETENV('FORT10',FIL_IN)
      OPEN(10,FILE=FIL_IN)

      READ(10,*) AREA
      WRITE (*,*) 'Area is: ', AREA

      CALL GETENV('FORT14',FIL_IN)
      OPEN(14,FILE=FIL_IN)

      READ(14,*) SPINUP,FCSTHR ! SpinUp hr and forecast hr

      IF (AREA=='w'.or.AREA=='k'.or.AREA=='n') THEN
         CALL extrCoarseRes(area,Spinup) ! Extract surge values from extropical grids
      ELSEIF (AREA=='e'.or.AREA=='g') THEN
         CALL  extrFineRes(Spinup)! Extract surge values from tropical grids
      ELSEIF (AREA=='m')THEN   ! For eBBC grids
         CALL extrCoarseRes_eBBC(Spinup)
      ELSE
         write(*,*)'Wrong basin only support e,g,w,k,m,n'
         
      ENDIF

      END

