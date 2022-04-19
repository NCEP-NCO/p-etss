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

      PROGRAM wind_hind


      implicit none

      CHARACTER*255 FIL_31
      CHARACTER*4 PROD

!c     FORT.11  - CONTROL FILE FOR AREA AND PRODUCT TYPE,NESTING
      CALL GETENV('FORT31',FIL_31)
      OPEN(31,FILE=FIL_31)

      READ(31,*) PROD
      WRITE (*,*) 'Reading Wind Data from: ', PROD
      CLOSE(31)
!c-----------------------------------------------------
!c Different products use different prod tables, which
!c will have different length.
!c----------------------------------------------------- 
      IF (PROD=='gefs') THEN
         CALL wind_hind_gefs(10,0.5) ! deal with GEFS
      ELSEIF (PROD=='gfnw') THEN
         CALL wind_hind_gefs(10,0.25) ! deal with newGEFS
      ELSEIF (PROD=='gfnr') THEN
         CALL wind_hind_gefs(5,0.25) ! deal with newGEFS retrospective
      ELSEIF (PROD=='cmce') THEN
         CALL wind_hind_cmc ! deal with CMCE
      ELSE
         WRITE (*,*)'Reading Wind Data from: ',PROD,' is not supportted'
      ENDIF

      END

