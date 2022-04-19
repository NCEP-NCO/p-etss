      SUBROUTINE extrFineRes(Spinup)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c
!c       THIS SUBROUTINE IS USED TO EXTRACT HOURLY SURGE VALUES FOR STATIONS 
!c       LOCATED IN eEX2/eGM3 grids using tropical basins to store at 
!c       etss*est/gom.txt
!c
!c    ARGUMENTS:
!c
!c    INPUT FILES:
!c       FORT.16  - AVNPUV.HH  AVN FORECAST
!c       FORT.17  - SURGE.HH    SURGE ARCHIVE
!c
!c    OUTPUTFILES:
!c       FORT.58  - MDLSURGE.OUT  AFOS FORMAT SURGE DATA
!c
!c    VARIABLES:
!c      INPUT
!c        AREA == con (CONUS) OR ala (ALASKA)
!c       BASIN == E,W,G  OR  A,Z,K
!c      HISDTA == SOURCE ARRAY
!c      OUTPUT
!c        IHIS == DESTINATION ARRAY
!c    AUTHORS: 
!c       CHEN /MDL, Arthur Taylor, Huiqing Liu /MDL
!c           
!c    HISTORY:
!c       10/1994--CHEN /MDL Created the routine
!c       08/2015--Huiqing Liu /MDL Updated the routine to deal with wst,goa
!c       01/2016--Huiqing Liu /MDL Updated the routine to use allocatable array
!c       01/2017--Huiqing Liu /MDL Put the routine to a independent fortran file
!c       02/2017--Huiqing Liu /MDL Added header block
!c       10/2018--Huiqing Liu /MDL Added water cells minimum negative value to 
!c                                 avoid drying out.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none
      character (len=48)                 :: sta
      integer                            :: Spinup,ipn,ipl,iset
 
      integer :: Num_Bsn,Imx,Jmx,I_Bsn,Npts,Npts2,Mhour,Iii,Jjj,Ntime
      integer :: Itm,Itime,Ime,Jsta,Num_Hisdta,I,J
      real    :: Hisdta_T1,Hisdta_T

      real,allocatable,dimension(:,:) :: hisdta, hisdta2
      real,allocatable,dimension(:)      :: wlMin, wlDepth
      integer,allocatable,dimension(:,:) :: ihis, ihis2

      CHARACTER*40 PATH
      CHARACTER*80 TTLCRD(2)

      character (len=30),allocatable :: stnnam (:)
      character (len=32),allocatable :: stnnam2(:)

      CHARACTER*18 TTL1
      CHARACTER*72 TTL2,TTL3
      CHARACTER*96 TTL3_2
      CHARACTER*1 AAA
      CHARACTER*255 FIL_96,FIL_11,FIL_16,FIL_17,FIL_58,FIL_59,FIL_32
      CHARACTER*255 FIL_18,FIL_19,FIL_20,FIL_21,FIL_22,FIL_23,FIL_24
      CHARACTER*255 FIL_25,FIL_26,FIL_27,FIL_28,FIL_29,FIL_30,FIL_31
      CHARACTER*255 FIL_34,FIL_35,FIL_36,FIL_37,FIL_38,FIL_39,
     $              FIL_40,FIL_41,FIL_42,FIL_43,FIL_44,FIL_45,
     $              FIL_46,FIL_47,FIL_48,FIL_49,FIL_12,FIL_13,FIL_IN

!C     W3TAGB needs (Name, Julian day (see next line), seconds, org)
!C         tclsh : puts [clock format [clock seconds] -format "%j"]
      CALL W3TAGB('MDLSURGE',2012,0341,0000,'OST25') 

      CALL GETENV('FORT96',FIL_96)
      OPEN (96,FILE=FIL_96)
      CALL GETENV('FORT11',FIL_11)
      OPEN (11,FILE=FIL_11)
      CALL GETENV('FORT12',FIL_12)
      OPEN (12,FILE=FIL_12,FORM='UNFORMATTED')
      CALL GETENV('FORT13',FIL_13)
      OPEN (13,FILE=FIL_13,FORM='UNFORMATTED')
      CALL GETENV('FORT16',FIL_16)
      OPEN (16,FILE=FIL_16,FORM='UNFORMATTED')
      CALL GETENV('FORT59',FIL_59)
      OPEN (59,FILE=FIL_59)
      CALL GETENV('FORT58',FIL_58)
      OPEN (58,FILE=FIL_58)

!C  INITIALIZE TITLES

       READ (96,'(A1)') AAA
       IF (AAA.EQ.'N') THEN
       CALL W3TAGE('petss_out_stn')
       STOP
       ENDIF

!C      SENT SURGE OUTPUT TO AFOS SYSTEM
!C     CALL W3AG15('FT18F001', 'AFOS    ', KRETC)

!C      TTL1='FQUS23 KWBC 000000'
      READ (11,801) TTL1
801   FORMAT(/,/,/,A18)
!c      TTL2=
!c     1 'GFS BASED STORM SURGE (IN TENTH OF FT)'//
!c     2 '     NOT VALID FOR TROPICAL STORMS'
       READ (11,802) TTL2
802    FORMAT (A72)
!c--------------------------------------------------------------
!c Added by Huiqing.Liu Oct. 2014
!c read number of basins to output station time series
!c--------------------------------------------------------------

       READ (11,*) NUM_BSN

       SELECT CASE (NUM_BSN)
       CASE (1)
            CALL GETENV('FORT17',FIL_17)
            OPEN (17,FILE=FIL_17,FORM='UNFORMATTED')
            CALL GETENV('FORT34',FIL_34)
            OPEN (34,FILE=FIL_34,FORM='UNFORMATTED')
       CASE (13)
            CALL GETENV('FORT17',FIL_17)
            OPEN (17,FILE=FIL_17,FORM='UNFORMATTED')
            CALL GETENV('FORT18',FIL_18)
            OPEN (18,FILE=FIL_18,FORM='UNFORMATTED')
            CALL GETENV('FORT19',FIL_19)
            OPEN (19,FILE=FIL_19,FORM='UNFORMATTED')
            CALL GETENV('FORT20',FIL_20)
            OPEN (20,FILE=FIL_20,FORM='UNFORMATTED')
            CALL GETENV('FORT21',FIL_21)
            OPEN (21,FILE=FIL_21,FORM='UNFORMATTED')
            CALL GETENV('FORT22',FIL_22)
            OPEN (22,FILE=FIL_22,FORM='UNFORMATTED')
            CALL GETENV('FORT23',FIL_23)
            OPEN (23,FILE=FIL_23,FORM='UNFORMATTED')
            CALL GETENV('FORT24',FIL_24)
            OPEN (24,FILE=FIL_24,FORM='UNFORMATTED')
            CALL GETENV('FORT25',FIL_25)
            OPEN (25,FILE=FIL_25,FORM='UNFORMATTED')
            CALL GETENV('FORT26',FIL_26)
            OPEN (26,FILE=FIL_26,FORM='UNFORMATTED')
            CALL GETENV('FORT27',FIL_27)
            OPEN (27,FILE=FIL_27,FORM='UNFORMATTED')
            CALL GETENV('FORT28',FIL_28)
            OPEN (28,FILE=FIL_28,FORM='UNFORMATTED')
            CALL GETENV('FORT29',FIL_29)
            OPEN (29,FILE=FIL_29,FORM='UNFORMATTED')
            CALL GETENV('FORT34',FIL_34)
            OPEN (34,FILE=FIL_34,FORM='UNFORMATTED')
            CALL GETENV('FORT35',FIL_35)
            OPEN (35,FILE=FIL_35,FORM='UNFORMATTED')
            CALL GETENV('FORT36',FIL_36)
            OPEN (36,FILE=FIL_36,FORM='UNFORMATTED')
            CALL GETENV('FORT37',FIL_37)
            OPEN (37,FILE=FIL_37,FORM='UNFORMATTED')
            CALL GETENV('FORT38',FIL_38)
            OPEN (38,FILE=FIL_38,FORM='UNFORMATTED')
            CALL GETENV('FORT39',FIL_39)
            OPEN (39,FILE=FIL_39,FORM='UNFORMATTED')
            CALL GETENV('FORT40',FIL_40)
            OPEN (40,FILE=FIL_40,FORM='UNFORMATTED')
            CALL GETENV('FORT41',FIL_41)
            OPEN (41,FILE=FIL_41,FORM='UNFORMATTED')
            CALL GETENV('FORT42',FIL_42)
            OPEN (42,FILE=FIL_42,FORM='UNFORMATTED')
            CALL GETENV('FORT43',FIL_43)
            OPEN (43,FILE=FIL_43,FORM='UNFORMATTED')
            CALL GETENV('FORT44',FIL_44)
            OPEN (44,FILE=FIL_44,FORM='UNFORMATTED')
            CALL GETENV('FORT45',FIL_45)
            OPEN (45,FILE=FIL_45,FORM='UNFORMATTED')
            CALL GETENV('FORT46',FIL_46)
            OPEN (46,FILE=FIL_46,FORM='UNFORMATTED')

            CALL GETENV('FORT67',FIL_IN)
            OPEN (67,FILE=FIL_IN)
            CALL GETENV('FORT68',FIL_IN)
            OPEN (68,FILE=FIL_IN)
            CALL GETENV('FORT69',FIL_IN)
            OPEN (69,FILE=FIL_IN)
            CALL GETENV('FORT70',FIL_IN)
            OPEN (70,FILE=FIL_IN)
            CALL GETENV('FORT71',FIL_IN)
            OPEN (71,FILE=FIL_IN)
            CALL GETENV('FORT72',FIL_IN)
            OPEN (72,FILE=FIL_IN)
            CALL GETENV('FORT73',FIL_IN)
            OPEN (73,FILE=FIL_IN)
            CALL GETENV('FORT74',FIL_IN)
            OPEN (74,FILE=FIL_IN)
            CALL GETENV('FORT75',FIL_IN)
            OPEN (75,FILE=FIL_IN)
            CALL GETENV('FORT76',FIL_IN)
            OPEN (76,FILE=FIL_IN)
            CALL GETENV('FORT77',FIL_IN)
            OPEN (77,FILE=FIL_IN)
            CALL GETENV('FORT78',FIL_IN)
            OPEN (78,FILE=FIL_IN)
            CALL GETENV('FORT79',FIL_IN)
            OPEN (79,FILE=FIL_IN)

       CASE (15)
            CALL GETENV('FORT17',FIL_17)
            OPEN (17,FILE=FIL_17,FORM='UNFORMATTED')
            CALL GETENV('FORT18',FIL_18)
            OPEN (18,FILE=FIL_18,FORM='UNFORMATTED')
            CALL GETENV('FORT19',FIL_19)
            OPEN (19,FILE=FIL_19,FORM='UNFORMATTED')
            CALL GETENV('FORT20',FIL_20)
            OPEN (20,FILE=FIL_20,FORM='UNFORMATTED')
            CALL GETENV('FORT21',FIL_21)
            OPEN (21,FILE=FIL_21,FORM='UNFORMATTED')
            CALL GETENV('FORT22',FIL_22)
            OPEN (22,FILE=FIL_22,FORM='UNFORMATTED')
            CALL GETENV('FORT23',FIL_23)
            OPEN (23,FILE=FIL_23,FORM='UNFORMATTED')
            CALL GETENV('FORT24',FIL_24)
            OPEN (24,FILE=FIL_24,FORM='UNFORMATTED')
            CALL GETENV('FORT25',FIL_25)
            OPEN (25,FILE=FIL_25,FORM='UNFORMATTED')
            CALL GETENV('FORT26',FIL_26)
            OPEN (26,FILE=FIL_26,FORM='UNFORMATTED')
            CALL GETENV('FORT27',FIL_27)
            OPEN (27,FILE=FIL_27,FORM='UNFORMATTED')
            CALL GETENV('FORT28',FIL_28)
            OPEN (28,FILE=FIL_28,FORM='UNFORMATTED')
            CALL GETENV('FORT29',FIL_29)
            OPEN (29,FILE=FIL_29,FORM='UNFORMATTED')
            CALL GETENV('FORT30',FIL_30) 
            OPEN (30,FILE=FIL_30,FORM='UNFORMATTED')
            CALL GETENV('FORT31',FIL_31) 
            OPEN (31,FILE=FIL_31,FORM='UNFORMATTED')
            CALL GETENV('FORT34',FIL_34)
            OPEN (34,FILE=FIL_34,FORM='UNFORMATTED')
            CALL GETENV('FORT35',FIL_35)
            OPEN (35,FILE=FIL_35,FORM='UNFORMATTED')
            CALL GETENV('FORT36',FIL_36)
            OPEN (36,FILE=FIL_36,FORM='UNFORMATTED')
            CALL GETENV('FORT37',FIL_37)
            OPEN (37,FILE=FIL_37,FORM='UNFORMATTED')
            CALL GETENV('FORT38',FIL_38)
            OPEN (38,FILE=FIL_38,FORM='UNFORMATTED')
            CALL GETENV('FORT39',FIL_39)
            OPEN (39,FILE=FIL_39,FORM='UNFORMATTED')
            CALL GETENV('FORT40',FIL_40)
            OPEN (40,FILE=FIL_40,FORM='UNFORMATTED')
            CALL GETENV('FORT41',FIL_41)
            OPEN (41,FILE=FIL_41,FORM='UNFORMATTED')
            CALL GETENV('FORT42',FIL_42)
            OPEN (42,FILE=FIL_42,FORM='UNFORMATTED')
            CALL GETENV('FORT43',FIL_43)
            OPEN (43,FILE=FIL_43,FORM='UNFORMATTED')
            CALL GETENV('FORT44',FIL_44)
            OPEN (44,FILE=FIL_44,FORM='UNFORMATTED')
            CALL GETENV('FORT45',FIL_45)
            OPEN (45,FILE=FIL_45,FORM='UNFORMATTED')
            CALL GETENV('FORT46',FIL_46)
            OPEN (46,FILE=FIL_46,FORM='UNFORMATTED')
            CALL GETENV('FORT47',FIL_47)
            OPEN (47,FILE=FIL_47,FORM='UNFORMATTED')
            CALL GETENV('FORT48',FIL_48)
            OPEN (48,FILE=FIL_48,FORM='UNFORMATTED')

            CALL GETENV('FORT67',FIL_IN)
            OPEN (67,FILE=FIL_IN)
            CALL GETENV('FORT68',FIL_IN)
            OPEN (68,FILE=FIL_IN)
            CALL GETENV('FORT69',FIL_IN)
            OPEN (69,FILE=FIL_IN)
            CALL GETENV('FORT70',FIL_IN)
            OPEN (70,FILE=FIL_IN)
            CALL GETENV('FORT71',FIL_IN)
            OPEN (71,FILE=FIL_IN)
            CALL GETENV('FORT72',FIL_IN)
            OPEN (72,FILE=FIL_IN)
            CALL GETENV('FORT73',FIL_IN)
            OPEN (73,FILE=FIL_IN)
            CALL GETENV('FORT74',FIL_IN)
            OPEN (74,FILE=FIL_IN)
            CALL GETENV('FORT75',FIL_IN)
            OPEN (75,FILE=FIL_IN)
            CALL GETENV('FORT76',FIL_IN)
            OPEN (76,FILE=FIL_IN)
            CALL GETENV('FORT77',FIL_IN)
            OPEN (77,FILE=FIL_IN)
            CALL GETENV('FORT78',FIL_IN)
            OPEN (78,FILE=FIL_IN)
            CALL GETENV('FORT79',FIL_IN)
            OPEN (79,FILE=FIL_IN)
            CALL GETENV('FORT80',FIL_IN)
            OPEN (80,FILE=FIL_IN)
            CALL GETENV('FORT81',FIL_IN)
            OPEN (81,FILE=FIL_IN)

       CASE DEFAULT
            CALL GETENV('FORT17',FIL_17)
            OPEN (17,FILE=FIL_17,FORM='UNFORMATTED')
            CALL GETENV('FORT34',FIL_34)
            OPEN (34,FILE=FIL_34,FORM='UNFORMATTED')
       END SELECT

!C  READ TITLE CARD AND EXTRACT INFORMATION
!C  FROM FT16

      READ(16) IMX,JMX
      READ(16) (TTLCRD(I),I=1,2)
      CLOSE(16)
      TTL1(13:14)=TTLCRD(1)(23:24)
      IF(TTL1(13:13).EQ.' ') TTL1(13:13)='0'
      TTL1(15:16)=TTLCRD(1)(31:32)
      IF(TTL1(15:15).EQ.' ') TTL1(15:15)='0'
      IF(TTL1(15:16).EQ.'00') THEN
      TTL3=
     1 '01Z            06Z              '//
     2 ' 12Z               18Z               00Z'
      TTL3_2=
     1 ' 01Z                 06Z                    '//
     2 ' 12Z                     18Z                     00Z'
      ELSE IF(TTL1(15:16).EQ.'12') THEN
      TTL3=
     1 '13Z            18Z              '//
     2 ' 00Z               06Z               12Z'
      TTL3_2=
     1 ' 13Z                 18Z                    '//
     2 ' 00Z                     06Z                     12Z'
      ELSE IF(TTL1(15:16).EQ.'06') THEN
      TTL3=
     1 '07Z            12Z              '//
     2 ' 18Z               00Z               06Z'
      TTL3_2=
     1 ' 07Z                 12Z                    '//
     2 ' 18Z                     00Z                     06Z'
      ELSE IF(TTL1(15:16).EQ.'18') THEN
      TTL3=
     1 '19Z            00Z              '//
     2 ' 06Z               12Z               18Z'
      TTL3_2=
     1 ' 19Z                 00Z                    '//
     2 ' 06Z                     12Z                     18Z'
      ENDIF

!C  READ IN NUMBER OF STATIONS (=43) AND TOTAL HOUR (=96)
!C  FROM FT17

!c-------------------------------------------------------------
!c Added by Huiqing.Liu Oct. 2014 
!c Postprocessing station output from multible tropical basins
!c-------------------------------------------------------------
      READ(12) NPTS,MHOUR
      READ(13) NPTS2,MHOUR
      allocate (stnnam(npts))
      allocate (stnnam2(npts2))

      allocate (wlMin(npts2))
      allocate (wlDepth(npts2))
      wlMin = 0.
      wlDepth = 0.

      DO I=1,NPTS
         READ(12) STNNAM(I),III,JJJ
      ENDDO
      DO I=1,NPTS2
         READ(13) STNNAM2(I),III,JJJ
      ENDDO

      allocate (hisdta (npts,num_bsn))
      allocate (hisdta2 (npts2,num_bsn))
      allocate (ihis(npts,mhour))
      allocate (ihis2(npts2,mhour))

      DO I_BSN=1,NUM_BSN
         READ (66+I_BSN,'(I3)') NPTS2

         DO I=1,NPTS2
            READ (66+I_BSN,'(A48,2I4,I2,f6.0)') STA,IPN,IPL,iset,
     $                                          wlDepth(I)
            if (wlDepth(I).gt.wlMin(I)) wlMin(I) = wlDepth(I)
         ENDDO
      ENDDO

      ITM=0
      NTIME=MHOUR*1
!c      write(*,*)'MHOUR=',MHOUR
      DO ITIME=1,NTIME 
         DO I_BSN=1,NUM_BSN
            READ(16+I_BSN) IME,(HISDTA(JSTA,I_BSN),JSTA=1,NPTS)
            READ(33+I_BSN) IME,(HISDTA2(JSTA,I_BSN),JSTA=1,NPTS2)
         ENDDO
! Write station output after model spinup hours
!         IF(ITIME.GE.96) THEN
         IF(ITIME.GE.Spinup*1) THEN
!         IF(MOD(ITIME,2).EQ.0) THEN
         ITM=ITM+1
         DO JSTA=1,NPTS
            HISDTA_T=0.
            NUM_HISDTA=0
!c Pick up the maximum value
!c            HISDTA_T1=HISDTA(JSTA,NUM_BSN)

            DO I_BSN=1,NUM_BSN
               IF(HISDTA(JSTA,I_BSN).NE.99)THEN
                   HISDTA_T=HISDTA_T+HISDTA(JSTA,I_BSN)
                   NUM_HISDTA=NUM_HISDTA+1
!c Pick up the maximum value
!c                 IF(HISDTA(JSTA,I_BSN).GT.HISDTA_T1.OR.
!c     $              HISDTA_T1.EQ.99)THEN
!c                    HISDTA_T1=HISDTA(JSTA,I_BSN)
!c                 ENDIF

               ENDIF
            ENDDO
!c Average the value
            IF(NUM_HISDTA.GE.1)THEN
               HISDTA_T=HISDTA_T/NUM_HISDTA
               IHIS(JSTA,ITM)=10.*HISDTA_T+.5 
            ELSE
!c               IHIS(JSTA,ITM)=10.*(HISDTA(JSTA,NUM_BSN)+
!c     $                        HISDTA(JSTA,NUM_BSN+1))+.5
            ENDIF
!c---------------------
!c Pick up the maximum value
!c            IHIS(JSTA,ITM)=10.*HISDTA_T1+.5
!c-------

            IF(IHIS(JSTA,ITM).LE.-100) IHIS(JSTA,ITM)=-99
         ENDDO 

         DO JSTA=1,NPTS2
            HISDTA_T=0. 
            NUM_HISDTA=0 
            HISDTA_T1=HISDTA2(JSTA,NUM_BSN)
            DO I_BSN=1,NUM_BSN 
               IF(HISDTA2(JSTA,I_BSN).NE.99)THEN
                   HISDTA_T=HISDTA_T+HISDTA2(JSTA,I_BSN)
                   NUM_HISDTA=NUM_HISDTA+1
!c Pick up the maximum value
!c                 IF(HISDTA2(JSTA,I_BSN).GT.HISDTA_T1.OR.
!c     $              HISDTA_T1.EQ.99)THEN
!c                    HISDTA_T1=HISDTA2(JSTA,I_BSN)
!c                 ENDIF

               ENDIF 
            ENDDO
            IF(NUM_HISDTA.GE.1)THEN
              HISDTA_T=HISDTA_T/NUM_HISDTA
               IHIS2(JSTA,ITM)=10.*HISDTA_T+.5
            ELSE 
!c               IHIS2(JSTA,ITM)=10.*(HISDTA2(JSTA,NUM_BSN)+
!c     $                        HISDTA2(JSTA,NUM_BSN+1))+.5
            ENDIF
!c---------------------
!c Pick up the maximum value
!c            IHIS2(JSTA,ITM)=10.*HISDTA_T1+.5
!c-----------

          IF (IHIS2(JSTA,ITM).LE.-100.and.wlMin(jsta).le.60) then
             IHIS2(JSTA,ITM)=-9999
          endif

!c----------------------
          if (ihis2(jsta,itm).ge.600.or.ihis2(jsta,itm).le.-9999)then  ! Cells are dry and water level is set to -1 * water depth
             ihis2(jsta,itm) = -10.0 * wlMin(jsta) - 0.5
          endif
!c----------------------

         ENDDO

         ENDIF 
!         ENDIF
      ENDDO
      DO I_BSN=1,NUM_BSN
         CLOSE(16+I_BSN) 
         CLOSE(33+I_BSN) 
         CLOSE(66+I_BSN) 
         
      ENDDO

!C  WRITE OUT TO AN AFOS TRANSMISSION FILE.

      WRITE(58,'(A18)') TTL1
      WRITE(58,'(A72)') TTL2
      WRITE(58,'(A72)') TTL3
      WRITE(59,'(A18)') TTL1
      WRITE(59,'(A72)') TTL2
      WRITE(59,'(A96)') TTL3_2
      DO 400 J=1,NPTS
      WRITE(58,'(1X,A20,48X,I3)') STNNAM(J)(11:30),IHIS(J,1)
      WRITE(58,1000) (IHIS(J,I),I=2,25)
      WRITE(58,1000) (IHIS(J,I),I=26,49)
      WRITE(58,1000) (IHIS(J,I),I=50,73)
      WRITE(58,1000) (IHIS(J,I),I=74,97)
      WRITE(58,1000) (IHIS(J,I),I=98,103)
 400  CONTINUE
      DO  J=1,NPTS2
      WRITE(59,'(1X,A32,59X,I4)') STNNAM2(J)(1:32),IHIS2(J,1)
      WRITE(59,1001) (IHIS2(J,I),I=2,25)
      WRITE(59,1001) (IHIS2(J,I),I=26,49)
      WRITE(59,1001) (IHIS2(J,I),I=50,73)
      WRITE(59,1001) (IHIS2(J,I),I=74,97)
      WRITE(59,1001) (IHIS2(J,I),I=98,103)
      ENDDO
 1000 FORMAT(24I3)
 1001 FORMAT(24I4)
      CLOSE(58)
      CLOSE(59)

      deallocate (hisdta)
      deallocate (hisdta2)
      deallocate (ihis)
      deallocate (ihis2)
      deallocate (stnnam)
      deallocate (stnnam2)
      deallocate (wlMin)
      deallocate (wlDepth)

      CALL W3TAGE('petss_out_stn')
      STOP
      END
