      subroutine stn_sort_all (ens_num)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c
!c       POST PROCESSING OF THE P-ETSS OUTPUT STATION DATA TO GENERATE
!c       SURGE AND TIDE HEIGHT ABOVE NAVD-88 BY 10%,90% AND MAX,MIN AND MEAN  
!c
!c    ARGUMENTS:
!c    
!c       READ DATA FROM ENSEMBLE RUNS STATION OUTPUT etss.{bsn}.txt
!c       GENERATE 10%,90%,MAXIMUM,MINNIMUM AND MEAN OF CONTROL AND ENSEMBLE RUNS
!c
!c    INPUT FILES:
!c       FORT.10-30  - STATION OUTPUT FROM 20 ENS MEMEBERS
!c
!c    OUTPUTFILES:
!c       FORT.61-65  - STATION OUTPUT TEXT PRODUTS OF 10%,90%,MAXIMUM,MINNIMUM 
!c                     AND MEAN
!c
!c
!c    VARIABLES:
!c
!c      INPUT
!c       IHIS     == SURGE HEIGHT
!c     OUTPUT
!c       IHIS_??  == 10%,90%,MAXIMUM,MINNIMUM AND MEAN
!c
!c    AUTHORS:
!c       Huiqing Liu /MDL Jan. 2016 
!c           
!c    HISTORY:
!c       01/2016--Huiqing Liu /MDL Created the routine
!c       01/2017--Huiqing Liu /MDL Added header block
!c       02/2018--Huiqing Liu /MDL Updated to handle 42 NAEFS ens members
!c       03/2020--Huiqing Liu /MDL Updated to handle 52 NewNAEFS + 31 NewGEFS ens members
!c
!c
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use subsort
!  Sort array subroutine from post_sort.f90


      implicit none
      
      integer             :: ens_num

      character (len=18)  :: TTL1
      character (len=72)  :: TTL2
      character (len=96)  :: TTL3
      character (len=255) :: Filei
      character (len=11)  :: Envvar

      integer             :: NPTS,fcr_num,ihis_f,IDF,i,j,m,iunit,lunit

      character (len=32),dimension(:),allocatable:: STNNAM2
      integer,dimension(:,:),allocatable         :: IHIS_max,IHIS_min
      integer,dimension(:,:),allocatable         :: IHIS_10p,IHIS_90p
      integer,dimension(:,:),allocatable         :: IHIS_20p,IHIS_80p
      integer,dimension(:,:),allocatable         :: IHIS_30p,IHIS_70p
      integer,dimension(:,:),allocatable         :: IHIS_40p,IHIS_60p
      integer,dimension(:,:,:),allocatable       :: IHIS
      real,dimension(:),allocatable              :: IHIS_sort
      real,dimension(:,:),allocatable            :: IHIS_mean,IHIS_50p

      ihis = 0
      ihis_f = 0
      fcr_num = 103

      do iunit = 1, ens_num
         lunit = iunit + 109
         envvar = 'FORT  '
         write(envvar(5:7),FMT='(I3)') Lunit
         call getenv(Envvar,Filei)
         open (lunit,file=Filei)
      enddo

      call getenv('FORT31',Filei)
      open (31,file=Filei)
      call getenv('FORT61',Filei)
      open (61,file=Filei)
      call getenv('FORT62',Filei)
      open (62,file=Filei)
      call getenv('FORT63',Filei)
      open (63,file=Filei)

      call getenv('FORT64',Filei)
      open (64,file=Filei)
      call getenv('FORT65',Filei)
      open (65,file=Filei)
      call getenv('FORT66',Filei)
      open (66,file=Filei)
      call getenv('FORT67',Filei)
      open (67,file=Filei)
      call getenv('FORT68',Filei)
      open (68,file=Filei)
      call getenv('FORT69',Filei)
      open (69,file=Filei)
      call getenv('FORT70',Filei)
      open (70,file=Filei)
      call getenv('FORT71',Filei)
      open (71,file=Filei)
      call getenv('FORT72',Filei)
      open (72,file=Filei)


      write(*,*)'1st step'
      
      read(31,*)NPTS
      write(*,*)'STATION NUMBER IS:',NPTS
      close(31)
      
      allocate (stnnam2(npts))
      allocate (ihis(ens_num,npts,fcr_num))
      allocate (ihis_max(npts,fcr_num))
      allocate (ihis_min(npts,fcr_num))
      allocate (ihis_mean(npts,fcr_num))
      allocate (ihis_10p(npts,fcr_num))
      allocate (ihis_90p(npts,fcr_num))
      allocate (ihis_20p(npts,fcr_num))
      allocate (ihis_80p(npts,fcr_num))
      allocate (ihis_30p(npts,fcr_num))
      allocate (ihis_70p(npts,fcr_num))
      allocate (ihis_40p(npts,fcr_num))
      allocate (ihis_60p(npts,fcr_num))
      allocate (ihis_50p(npts,fcr_num))
      allocate (IHIS_sort(ens_num))

      do IDF=0,ens_num-1

         read(110+IDF,'(a18)') TTL1
         read(110+IDF,'(a72)') TTL2
         read(110+IDF,'(a96)') TTL3
         if (IDF==0) then
            write(61,'(A18)') TTL1
            write(62,'(A18)') TTL1
            write(63,'(A18)') TTL1
            write(64,'(A18)') TTL1
            write(65,'(A18)') TTL1
            write(66,'(A18)') TTL1
            write(67,'(A18)') TTL1
            write(68,'(A18)') TTL1
            write(69,'(A18)') TTL1
            write(70,'(A18)') TTL1
            write(71,'(A18)') TTL1
            write(72,'(A18)') TTL1

            write(61,'(A72)') TTL2
            write(62,'(A72)') TTL2
            write(63,'(A72)') TTL2
            write(64,'(A72)') TTL2
            write(65,'(A72)') TTL2
            write(66,'(A18)') TTL2
            write(67,'(A18)') TTL2
            write(68,'(A18)') TTL2
            write(69,'(A18)') TTL2
            write(70,'(A18)') TTL2
            write(71,'(A18)') TTL2
            write(72,'(A18)') TTL2

            write(61,'(A72)') TTL3 
            write(62,'(A72)') TTL3
            write(63,'(A72)') TTL3
            write(64,'(A72)') TTL3
            write(65,'(A72)') TTL3
            write(66,'(A18)') TTL3
            write(67,'(A18)') TTL3
            write(68,'(A18)') TTL3
            write(69,'(A18)') TTL3
            write(70,'(A18)') TTL3
            write(71,'(A18)') TTL3
            write(72,'(A18)') TTL3

          !  write(*,*)'fst'

         endif
           
         do j=1,NPTS

            read(110+IDF,'(1X,a32,59x,i4)') STNNAM2(j),IHIS(IDF+1,j,1)
            read(110+IDF,1001) (IHIS(IDF+1,j,i),i=2,25)
            read(110+IDF,1001) (IHIS(IDF+1,j,i),i=26,49)
            read(110+IDF,1001) (IHIS(IDF+1,j,i),i=50,73)
            read(110+IDF,1001) (IHIS(IDF+1,j,i),i=74,97)
            read(110+IDF,1001) (IHIS(IDF+1,j,i),i=98,103)
         
        enddo
      close(110+IDF)

      enddo

      write(*,*) '------ok------'
      do j=1,NPTS
      do i=1,fcr_num

           IHIS_max(j,i) = 990 ! --Initialization
           IHIS_min(j,i) = 990
           IHIS_mean(j,i) = 990
           IHIS_10p(j,i) = 990
           IHIS_20p(j,i) = 990
           IHIS_30p(j,i) = 990
           IHIS_40p(j,i) = 990
           IHIS_50p(j,i) = 990
           IHIS_60p(j,i) = 990
           IHIS_70p(j,i) = 990
           IHIS_80p(j,i) = 990
           IHIS_90p(j,i) = 990

         do IDF=0,ens_num-1
            IHIS_sort(IDF+1) = IHIS(IDF+1,j,i)*1.0
         enddo

         call sort(IHIS_sort,ens_num)
         
         if (IHIS_sort(ens_num) < 600 ) then ! A) All memebers are wet

            IHIS_max(j,i) = IHIS_sort(ens_num)
            IHIS_min(j,i) = IHIS_sort(1)
            IHIS_mean(j,i) = sum(IHIS_sort)/(1.0*ens_num)
            if (ens_num == 21) then
               IHIS_10p(j,i) = IHIS_sort(20)
               IHIS_20p(j,i) = IHIS_sort(ens_num-3)
               IHIS_30p(j,i) = IHIS_sort(ens_num-5)
               IHIS_40p(j,i) = IHIS_sort(ens_num-7)
               IHIS_50p(j,i) = IHIS_sort(11)
               IHIS_60p(j,i) = IHIS_sort(9)
               IHIS_70p(j,i) = IHIS_sort(7)
               IHIS_80p(j,i) = IHIS_sort(5)
               IHIS_90p(j,i) = IHIS_sort(3)
            elseif (ens_num == 31) then
               IHIS_10p(j,i) = IHIS_sort(29)
               IHIS_20p(j,i) = IHIS_sort(26)
               IHIS_30p(j,i) = IHIS_sort(23)
               IHIS_40p(j,i) = IHIS_sort(20)
               IHIS_50p(j,i) = IHIS_sort(16)
               IHIS_60p(j,i) = IHIS_sort(13)
               IHIS_70p(j,i) = IHIS_sort(10)
               IHIS_80p(j,i) = IHIS_sort(7)
               IHIS_90p(j,i) = IHIS_sort(4)
            elseif (ens_num == 42) then
               IHIS_10p(j,i) = IHIS_sort(39)
               IHIS_20p(j,i) = IHIS_sort(ens_num-7)
               IHIS_30p(j,i) = IHIS_sort(ens_num-12)
               IHIS_40p(j,i) = IHIS_sort(ens_num-16)
               IHIS_50p(j,i) = IHIS_sort(21)
               IHIS_60p(j,i) = IHIS_sort(17)
               IHIS_70p(j,i) = IHIS_sort(13)
               IHIS_80p(j,i) = IHIS_sort(9)
               IHIS_90p(j,i) = IHIS_sort(5)
            elseif (ens_num == 52) then
               IHIS_10p(j,i) = IHIS_sort(47)
               IHIS_20p(j,i) = IHIS_sort(ens_num-9)
               IHIS_30p(j,i) = IHIS_sort(ens_num-14)
               IHIS_40p(j,i) = IHIS_sort(ens_num-19)
               IHIS_50p(j,i) = IHIS_sort(26)
               IHIS_60p(j,i) = IHIS_sort(21)
               IHIS_70p(j,i) = IHIS_sort(16)
               IHIS_80p(j,i) = IHIS_sort(11)
               IHIS_90p(j,i) = IHIS_sort(6)
            endif

         else                             ! B) Partial members are wet or all dry
            
            do m=1,ens_num
               if (IHIS_sort(m) > 600) exit ! Locate the first number of member is dry
            enddo
            
            if (m == 1) then ! --0 All dry
               IHIS_max(j,i) = IHIS_sort(ens_num) 
               IHIS_min(j,i) = IHIS_sort(ens_num) 
               IHIS_10p(j,i) = IHIS_sort(ens_num) 
               IHIS_20p(j,i) = IHIS_sort(ens_num)
               IHIS_30p(j,i) = IHIS_sort(ens_num)
               IHIS_40p(j,i) = IHIS_sort(ens_num)
               IHIS_50p(j,i) = IHIS_sort(ens_num)
               IHIS_60p(j,i) = IHIS_sort(ens_num)
               IHIS_70p(j,i) = IHIS_sort(ens_num)
               IHIS_80p(j,i) = IHIS_sort(ens_num)
               IHIS_90p(j,i) = IHIS_sort(ens_num) 
               IHIS_mean(j,i) = IHIS_sort(ens_num) 
            else
!            if ((m-1) >= 1) then
               if ((m-1) == 1) then ! --1) only one member is wet
                  IHIS_max(j,i) = IHIS_sort(1) ! Only ens max is wet, all others are dry
                  IHIS_min(j,i) = IHIS_sort(ens_num)
                  IHIS_10p(j,i) = IHIS_sort(ens_num)
                  IHIS_90p(j,i) = IHIS_sort(ens_num)
               else                 ! --2) more than one member are wet 
                  IHIS_max(j,i) = IHIS_sort(m-1)
                  IHIS_min(j,i) = IHIS_sort(ens_num)
                  if (ens_num == 21) then
                    if ((m-2) >= 1)then
                      IHIS_10p(j,i) = IHIS_sort(m-2)
                      if ((m-19) >= 1)then
                         IHIS_90p(i,j) = IHIS_sort(m-19)
                      endif
                    endif
                  elseif (ens_num == 31) then
                    if ((m-3) >= 1)then
                       IHIS_10p(j,i) = IHIS_sort(m-3)
                      if ((m-28) >= 1)then
                         IHIS_90p(i,j) = IHIS_sort(m-28)
                      endif
                    endif
                  elseif (ens_num == 42) then
                    if ((m-4) >= 1)then
                       IHIS_10p(j,i) = IHIS_sort(m-4)
                      if ((m-39) >= 1)then
                         IHIS_90p(i,j) = IHIS_sort(m-39)
                      endif
                    endif
                  elseif (ens_num == 52) then
                    if ((m-6) >= 1)then
                       IHIS_10p(j,i) = IHIS_sort(m-6)
                      if ((m-45) >= 1)then
                         IHIS_90p(i,j) = IHIS_sort(m-45)
                      endif
                    endif
                  endif
               endif
               where (IHIS_sort > 600)
                   IHIS_sort = 0.
               endwhere
               IHIS_mean(j,i) = sum(IHIS_sort)/(m-1)

            endif

         endif 
         if (IHIS_max(j,i) > 600) IHIS_max(j,i) = 9999
         if (IHIS_min(j,i) > 600) IHIS_min(j,i) = 9999
         if (IHIS_mean(j,i) > 600) IHIS_mean(j,i) = 9999
         if (IHIS_10p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_20p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_30p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_40p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_50p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_60p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_70p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_80p(j,i) > 600) IHIS_10p(j,i) = 9999
         if (IHIS_90p(j,i) > 600) IHIS_90p(j,i) = 9999
      enddo
         write(61,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_max(j,1)
         write(61,1001) (IHIS_max(j,i),i=2,25)
         write(61,1001) (IHIS_max(j,i),i=26,49)
         write(61,1001) (IHIS_max(j,i),i=50,73)
         write(61,1001) (IHIS_max(j,i),i=74,97)
         write(61,1001) (IHIS_max(j,i),i=98,103)
        
         write(62,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_min(j,1)
         write(62,1001) (IHIS_min(j,i),i=2,25)
         write(62,1001) (IHIS_min(j,i),i=26,49)
         write(62,1001) (IHIS_min(j,i),i=50,73)
         write(62,1001) (IHIS_min(j,i),i=74,97)
         write(62,1001) (IHIS_min(j,i),i=98,103)

         write(63,'(1x,a32,59x,i4)') STNNAM2(j),NINT(IHIS_mean(j,1))
         write(63,1001) (NINT(IHIS_mean(j,i)),i=2,25)
         write(63,1001) (NINT(IHIS_mean(j,i)),i=26,49)
         write(63,1001) (NINT(IHIS_mean(j,i)),i=50,73)
         write(63,1001) (NINT(IHIS_mean(j,i)),i=74,97)
         write(63,1001) (NINT(IHIS_mean(j,i)),i=98,103)

         write(64,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_10p(j,1)
         write(64,1001) (IHIS_10p(j,i),i=2,25)
         write(64,1001) (IHIS_10p(j,i),i=26,49)
         write(64,1001) (IHIS_10p(j,i),i=50,73)
         write(64,1001) (IHIS_10p(j,i),i=74,97)
         write(64,1001) (IHIS_10p(j,i),i=98,103)

         write(65,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_90p(j,1)
         write(65,1001) (IHIS_90p(j,i),i=2,25)
         write(65,1001) (IHIS_90p(j,i),i=26,49)
         write(65,1001) (IHIS_90p(j,i),i=50,73)
         write(65,1001) (IHIS_90p(j,i),i=74,97)
         write(65,1001) (IHIS_90p(j,i),i=98,103)

         write(66,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_20p(j,1)
         write(66,1001) (IHIS_20p(j,i),i=2,25)
         write(66,1001) (IHIS_20p(j,i),i=26,49)
         write(66,1001) (IHIS_20p(j,i),i=50,73)
         write(66,1001) (IHIS_20p(j,i),i=74,97)
         write(66,1001) (IHIS_20p(j,i),i=98,103)
 
         write(67,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_30p(j,1)
         write(67,1001) (IHIS_30p(j,i),i=2,25)
         write(67,1001) (IHIS_30p(j,i),i=26,49)
         write(67,1001) (IHIS_30p(j,i),i=50,73)
         write(67,1001) (IHIS_30p(j,i),i=74,97)
         write(67,1001) (IHIS_30p(j,i),i=98,103)

         write(68,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_40p(j,1)
         write(68,1001) (IHIS_40p(j,i),i=2,25)
         write(68,1001) (IHIS_40p(j,i),i=26,49)
         write(68,1001) (IHIS_40p(j,i),i=50,73)
         write(68,1001) (IHIS_40p(j,i),i=74,97)
         write(68,1001) (IHIS_40p(j,i),i=98,103)
 
         write(69,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_50p(j,1)
         write(69,1001) (IHIS_50p(j,i),i=2,25)
         write(69,1001) (IHIS_50p(j,i),i=26,49)
         write(69,1001) (IHIS_50p(j,i),i=50,73)
         write(69,1001) (IHIS_50p(j,i),i=74,97)
         write(69,1001) (IHIS_50p(j,i),i=98,103)

         write(70,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_60p(j,1)
         write(70,1001) (IHIS_60p(j,i),i=2,25)
         write(70,1001) (IHIS_60p(j,i),i=26,49)
         write(70,1001) (IHIS_60p(j,i),i=50,73)
         write(70,1001) (IHIS_60p(j,i),i=74,97)
         write(70,1001) (IHIS_60p(j,i),i=98,103)
 
         write(71,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_70p(j,1)
         write(71,1001) (IHIS_70p(j,i),i=2,25)
         write(71,1001) (IHIS_70p(j,i),i=26,49)
         write(71,1001) (IHIS_70p(j,i),i=50,73)
         write(71,1001) (IHIS_70p(j,i),i=74,97)
         write(71,1001) (IHIS_70p(j,i),i=98,103)

         write(72,'(1x,a32,59x,i4)') STNNAM2(j),IHIS_80p(j,1)
         write(72,1001) (IHIS_80p(j,i),i=2,25)
         write(72,1001) (IHIS_80p(j,i),i=26,49)
         write(72,1001) (IHIS_80p(j,i),i=50,73)
         write(72,1001) (IHIS_80p(j,i),i=74,97)
         write(72,1001) (IHIS_80p(j,i),i=98,103)

      enddo
 1001 format(24i4)
 1002 format(97i4)
 1003 format(24f6.1)
      deallocate(IHIS_max)
      deallocate(IHIS_min)
      deallocate(IHIS_mean)
      deallocate(IHIS_10p)
      deallocate(IHIS_20p)
      deallocate(IHIS_30p)
      deallocate(IHIS_40p)
      deallocate(IHIS_50p)
      deallocate(IHIS_60p)
      deallocate(IHIS_70p)
      deallocate(IHIS_80p)
      deallocate(IHIS_90p)
      deallocate(IHIS)
      deallocate(IHIS_sort)
      close(61)
      close(62)
      close(63)

      end

