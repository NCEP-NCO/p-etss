!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    PURPOSE:
!c 
!c       MAIN PROGRAM TO READ THE WIND DATA (GEFS OR CMCE)
!c
!c    ARGUMENTS:
!c    
!c
!c    INPUT FILES:
!c       FORT.31  - PROD
!c
!c    OUTPUTFILES:
!c
!c
!c    VARIABLES:
!c
!c      INPUT
!c       prod     == gefs/cmce
!c
!c     OUTPUT
!c
!c    AUTHORS:
!c       Huiqing Liu Ace Info/MDL MARCH 2018
!c           
!c    HISTORY:
!c 
!c    NOTES: 
!c 
!c---------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      PROGRAM wind_fcst


      implicit none

      CHARACTER*255 FIL_10
      CHARACTER*4 PROD
      CHARACTER*2 cyc
      integer wndshr


      CALL GETENV('FORT10',FIL_10)
      OPEN(10,FILE=FIL_10)

      READ(10,*) PROD,cyc
      WRITE (*,*) 'Reading Wind Data from: ', PROD
      CLOSE(10)
      if (cyc=='06' .or. cyc=='18')then
         wndshr = 108
      else
         wndshr = 102
      endif
!c-----------------------------------------------------
!c Different products use different prod tables, which
!c will have different length.
!c----------------------------------------------------- 
      IF (PROD=='gefs') THEN
         CALL wind_fcst_gefs(0.5) ! Reading wind from GEFS
      ELSEIF (PROD=='gfnw') THEN
         CALL wind_fcst_gefs(0.25) ! Reading wind from newGEFS
      ELSEIF (PROD=='cmce') THEN
         CALL wind_fcst_cmc(wndshr) ! Reading wind from CMCE 
      ELSE
         WRITE (*,*)'Reading Wind Data from: ',PROD,' is not supportted'
      ENDIF

      END

