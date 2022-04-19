      subroutine grid_probabl_all (mmember,cyc)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c
!c       POST PROCESSING OF THE P-ETSS OUTPUT GRIDDED DATA TO GENERATE
!c       PROBABILITY OF SURGE AND TIDE HEIGHT GREATER THAN 1,2,3,6,10,13,16 FT  
!c
!c    ARGUMENTS:
!c    
!c       READ DATA FROM SSGRID.CCB(00a for example)
!c       GENERATE MAXIMUM,MINNIMUM AND MEAN OF CONTROL AND ENSEMBLE RUNS
!c
!c    INPUT FILES:
!c       FORT.12     - CONTROL FILE FOR BASIN SIZE
!c       FORT.13     - SURGE HIGHT FROM CONTROL RUN
!c       FORT.14-33  - SURGE HIGHT FROM 20 ENS MEMEBERS
!c
!c    OUTPUTFILES:
!c       FORT.51-59  - PROBABILITY OF SURGE AND TIDE HEIGHT GREATER THAN 1,2,3,6,
!C                     10,13,16 FT
!c
!c
!c    VARIABLES:
!c
!c      INPUT
!c       IMXB  == IMXB IN ALL BASINS
!c       JMXB  == JMXB IN ALL BASINS
!c       ATMP  == TEMPERORY ARRAY FOR FILTERING HALF HOUR DATA
!c         HB  == SURGE ARRAY
!c     OUTPUT
!c       HB_??  == PROBABILITY OF SURGE AND TIDE HEIGHT GREATER THAN
!c
!c    AUTHORS:
!c       Huiqing Liu /MDL Jan. 2016 
!c           
!c    HISTORY:
!c       01/2016--Huiqing Liu /MDL Created the routine
!c       07/2016--Huiqing Liu /MDL Added 10,13 and 16 ft and removed 9 ft
!c                                 Corresponding (equivalent) to 1,2,3,4,5 
!c                                 meters (5 category storm surge events)
!c       01/2017--Huiqing Liu /MDL Added header block
!c       06/2017--Huiqing Liu /MDL Added 0,4,5,7,8,9 ft
!c       02/2018--Huiqing Liu /MDL Updated to handle 42 NAEFS ens members
!c       06/2018--Huiqing Liu /MDL Added 15 ft
!c       03/2020--Huiqing Liu /MDL Updated to handle 52 NewNAEFS + 31 NewGEFS ens members
!c
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use subsort
!  Sort array subroutine from post_sort.f90

      implicit none

      integer             :: mmember,cyc

      character (len=255) :: FIL_12,FILEI,FIL_34
      character (len=255) :: FIL_51,FIL_52,FIL_53,FIL_54,FIL_55
      character (len=255) :: FIL_56,FIL_57,FIL_IN
      character (len=11)  :: Envvar
      integer             :: i,j,m,n,imxb,jmxb,hour,mhour,mmax
      integer             :: i_dumy,dry_num,iunit,lunit,membstrt
      integer,dimension(:,:),allocatable :: i_boun 
     
      real,dimension(:,:),allocatable  :: chance_1ft,chance_1ft_max
      real,dimension(:,:),allocatable  :: chance_2ft,chance_2ft_max
      real,dimension(:,:),allocatable  :: chance_3ft,chance_3ft_max
      real,dimension(:,:),allocatable  :: chance_6ft,chance_6ft_max
      real,dimension(:,:),allocatable  :: chance_10ft,chance_10ft_max
      real,dimension(:,:),allocatable  :: chance_13ft,chance_13ft_max
      real,dimension(:,:),allocatable  :: chance_16ft,chance_16ft_max

      real,dimension(:,:),allocatable  :: chance_4ft,chance_4ft_max
      real,dimension(:,:),allocatable  :: chance_5ft,chance_5ft_max
      real,dimension(:,:),allocatable  :: chance_7ft,chance_7ft_max
      real,dimension(:,:),allocatable  :: chance_8ft,chance_8ft_max
      real,dimension(:,:),allocatable  :: chance_9ft,chance_9ft_max
      real,dimension(:,:),allocatable  :: chance_0ft,chance_0ft_max

      real,dimension(:,:),allocatable  :: chance_15ft,chance_15ft_max

      real,dimension(:,:),allocatable    :: hb_tide
      real,dimension(:),  allocatable    :: hb_sort
      real,dimension(:,:,:),allocatable  :: hb
      real                               :: z_dumy
      character                          :: tide,basinNEP



!     OPEN UNIT NUMBERS

!     FORT.12  - CONTROL FILE FOR BASIN SIZE
      call getenv('FORT12',FIL_12)
      open(12,file=FIL_12)
!     FORT.51  - Probability of surge/tide height greater than 1ft
!     FORT.52  - Probability of surge/tide height greater than 2ft
!     FORT.53  - Probability of surge/tide height greater than 3ft
!     FORT.54  - Probability of surge/tide height greater than 6ft
!     FORT.55  - Probability of surge/tide height greater than 10ft (9ft)
!     FORT.56  - Probability of surge/tide height greater than 13ft
!     FORT.57  - Probability of surge/tide height greater than 16ft

!     FORT.58  - Probability of surge/tide height greater than 0ft
!     FORT.59  - Probability of surge/tide height greater than 4ft
!     FORT.60  - Probability of surge/tide height greater than 5ft
!     FORT.61  - Probability of surge/tide height greater than 7ft
!     FORT.62  - Probability of surge/tide height greater than 8ft
!     FORT.63  - Probability of surge/tide height greater than 9ft

!     FORT.64  - Probability of surge/tide height greater than 15ft

      call getenv('FORT51',FIL_51)
      open(51,file=FIL_51,form='unformatted')
      call getenv('FORT52',FIL_52)
      open(52,file=FIL_52,form='unformatted')
      call getenv('FORT53',FIL_53)
      open(53,file=FIL_53,form='unformatted')
      call getenv('FORT54',FIL_54)
      open(54,file=FIL_54,form='unformatted')
      call getenv('FORT55',FIL_55)
      open(55,file=FIL_55,form='unformatted')
      call getenv('FORT56',FIL_56)
      open(56,file=FIL_56,form='unformatted')
      call getenv('FORT57',FIL_57)
      open(57,file=FIL_57,form='unformatted')


      call getenv('FORT58',FIL_IN)
      open(58,file=FIL_IN,form='unformatted')
      call getenv('FORT59',FIL_IN)
      open(59,file=FIL_IN,form='unformatted')
      call getenv('FORT60',FIL_IN)
      open(60,file=FIL_IN,form='unformatted')
      call getenv('FORT61',FIL_IN)
      open(61,file=FIL_IN,form='unformatted')
      call getenv('FORT62',FIL_IN)
      open(62,file=FIL_IN,form='unformatted')
      call getenv('FORT63',FIL_IN)
      open(63,file=FIL_IN,form='unformatted')

      call getenv('FORT64',FIL_IN)
      open(64,file=FIL_IN,form='unformatted')

      call getenv('FORT11',FILEI)
      open(11,file=FILEI,form='unformatted')

!     FORT.113-154  - SURGE HIGHT of 42 ENS RUNS
!     FORT.113-205  - SURGE HIGHT of 93 ENS RUNS

      do iunit = 1,mmember
         lunit = iunit + 112
         envvar = 'FORT  '
         write(envvar(5:7),FMT='(I3)') Lunit
         call getenv(Envvar,Filei)
         open (lunit,file=Filei,form='unformatted')
      enddo
 
      read(12,*) imxb,jmxb,tide,basinNEP
      close (12)

      if (tide=='Y') then

         call getenv('FORT34',FIL_34)
         open(34,file=FIL_34,form='unformatted')

      endif

      hour = 103

      allocate (hb (mmember,imxb,jmxb))

      allocate (chance_0ft (imxb,jmxb))
      allocate (chance_0ft_max (imxb,jmxb))
      allocate (chance_4ft (imxb,jmxb))
      allocate (chance_4ft_max (imxb,jmxb))
      allocate (chance_5ft (imxb,jmxb))
      allocate (chance_5ft_max (imxb,jmxb))
      allocate (chance_7ft (imxb,jmxb))
      allocate (chance_7ft_max (imxb,jmxb))
      allocate (chance_8ft (imxb,jmxb))
      allocate (chance_8ft_max (imxb,jmxb))
      allocate (chance_9ft (imxb,jmxb))
      allocate (chance_9ft_max (imxb,jmxb))
      allocate (chance_15ft (imxb,jmxb))
      allocate (chance_15ft_max (imxb,jmxb))

      allocate (chance_1ft (imxb,jmxb))
      allocate (chance_1ft_max (imxb,jmxb))
      allocate (chance_2ft (imxb,jmxb))
      allocate (chance_2ft_max (imxb,jmxb))
      allocate (chance_3ft (imxb,jmxb))
      allocate (chance_3ft_max (imxb,jmxb))
      allocate (chance_6ft (imxb,jmxb))
      allocate (chance_6ft_max (imxb,jmxb))

      allocate (chance_10ft (imxb,jmxb))
      allocate (chance_10ft_max (imxb,jmxb))
      allocate (chance_13ft (imxb,jmxb))
      allocate (chance_13ft_max (imxb,jmxb))
      allocate (chance_16ft (imxb,jmxb))
      allocate (chance_16ft_max (imxb,jmxb))

      allocate (hb_sort (mmember))
      allocate (hb_tide(imxb,jmxb))
      allocate (i_boun (imxb,jmxb))

      if (basinNEP=='Y') then
         open(101,file='grid_depth_nep.txt')
         do i=1,IMXB-1
         do j=1,JMXB-1
            read(101,'(2I5,F11.2,I2)')i_dumy,i_dumy,z_dumy,i_boun(i,j)
         enddo
         enddo
         close(101)
      endif

      read(11)mhour
      close(11)
      hb_tide=0.
      do n=1,47
        do m=1,mmember
           read(112+m) ((hb(m,i,j),i=1,imxb),j=1,jmxb)
        enddo
        if (tide=='Y')then
           read(34) ((hb_tide(i,j),i=1,imxb),j=1,jmxb)
        endif      
      enddo
!------------------------------------------------------------------------------
! 06 and 18z for CMC and ECMWF ensemble memebers (22->) need postPhone extra 6 hours
      if (mmember .ne. 21.and.mmember .ne. 31) then
! Not for GEFS only runs
         if (mmember .eq. 42 .or. mmember .eq. 93) then
            membstrt = 22
         else if (mmember .eq. 52 .or. mmember .eq. 103) then
            membstrt = 32
         else if (mmember .eq. 51) then
            membstrt = 1 ! Only ECMWF 
         endif

         if (cyc == 6 .or. cyc == 18) then
!
! Huiqing.Liu /MDL March 20202
            if (mmember .eq. 51) mhour = mhour - 6
! mhour is read from FORT11 ssgrid* file
! Only ECMWF onlyrun get mhour from ecmwf ssgrid which is 108 not 102hr
! Others run including NAEFS runs get mhour from gefs ssgrid, which is
! 102 hr
!
            do n = 1, 6
               do m = membstrt, mmember
                  read(112+m) ((hb(m,i,j),i=1,imxb),j=1,jmxb)
               enddo
               if (tide=='Y')then
                  read(34) ((hb_tide(i,j),i=1,imxb),j=1,jmxb)
               endif
            enddo
          endif
      endif
!------------------------------------------------------------------------------
      do n=1,mhour-47

        do m=1,mmember
           read(112+m) ((hb(m,i,j),i=1,imxb),j=1,jmxb)
        enddo
        if (tide=='Y')then
           read(34) ((hb_tide(i,j),i=1,imxb),j=1,jmxb)
        endif

        do i=1,imxb
        do j=1,jmxb

           dry_num = 0
           do m=1,mmember
              hb_sort(m)=hb(m,i,j)+hb_tide(i,j)
              if (hb_sort(m) > 60) dry_num = dry_num +1
           enddo

           if (dry_num < mmember) then 
               !call quicksort(hb_sort,1,mmember)
              if (basinNEP=='Y') then
                 if (i_boun(i,j).ne.1)then
                    call sort (hb_sort,mmember)
                 else 
                    cycle
                 endif
              else
                 call sort (hb_sort,mmember)
              endif
           else
              cycle  ! All dry cells
           endif
!---------------------------------------------
! Initializing probablity of all cells 
!--------------------------------------------
           chance_0ft(i,j) = 9999.
           chance_1ft(i,j) = 9999.
           chance_2ft(i,j) = 9999.
           chance_3ft(i,j) = 9999.
           chance_4ft(i,j) = 9999.
           chance_5ft(i,j) = 9999.
           chance_6ft(i,j) = 9999.
           chance_7ft(i,j) = 9999.
           chance_8ft(i,j) = 9999.
           chance_9ft(i,j) = 9999.
           chance_10ft(i,j) = 9999.
           chance_13ft(i,j) = 9999.
           chance_15ft(i,j) = 9999.
           chance_16ft(i,j) = 9999.
           
           if (hb_sort(mmember) < 60) then ! I)--All ensemble memebers are wet

            if (hb_sort(mmember) > 0.) then
             do m=1,mmember
                if (hb_sort(m) > 0) exit
             enddo
             chance_0ft(i,j) = (mmember-m+1)*1.0/mmember*100.
             if (hb_sort(mmember) >= 1.) then
                do m=1,mmember 
                   if (hb_sort(m) >= 1) exit 
                enddo
                chance_1ft(i,j) = (mmember-m+1)*1.0/mmember*100.
                if (hb_sort(mmember) >= 2.) then
                   do m=1,mmember
                      if (hb_sort(m) >= 2) exit
                   enddo
                   chance_2ft(i,j) = (mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 3.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 3) exit
                      enddo
                      chance_3ft(i,j) = (mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 4.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 4) exit
                      enddo
                      chance_4ft(i,j) = (mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 5.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 5) exit
                      enddo
                      chance_5ft(i,j) = (mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 6.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 6) exit
                      enddo
                      chance_6ft(i,j) = (mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 7.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 7) exit
                      enddo
                      chance_7ft(i,j) = (mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 8.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 8) exit
                      enddo
                      chance_8ft(i,j) = (mmember-m+1)*1.0/mmember*100.

                   if (hb_sort(mmember) >= 9.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 9) exit
                      enddo
                      chance_9ft(i,j) =(mmember-m+1)*1.0/mmember*100.

                   if (hb_sort(mmember) >= 10.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 10) exit
                      enddo
                      chance_10ft(i,j) =(mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 13.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 13) exit
                      enddo 
                      chance_13ft(i,j) =(mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 15.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 15) exit
                      enddo         
                      chance_15ft(i,j) =(mmember-m+1)*1.0/mmember*100.
                   if (hb_sort(mmember) >= 16.) then
                      do m=1,mmember
                         if (hb_sort(m) >= 16) exit
                      enddo
                      chance_16ft(i,j) =(mmember-m+1)*1.0/mmember*100.
                   endif
                   endif
                   endif
                   endif
                   endif
                   endif
                   endif
                   endif
                   endif
                   endif
                 endif
               endif
             endif
             endif 

           else                            ! II) -- Partial members are wet

             do m=1,mmember
                if (hb_sort(m) > 60) exit ! Find the first number of member is dry
             enddo
             mmax = m-1
             if (mmax > 0) then ! Some ensemble members are wet and some are dry 

              if (hb_sort(mmax) > 0) then
                do m=1,mmax
                   if (hb_sort(m) > 0) exit
                enddo
                chance_0ft(i,j) = (mmax-m+1)*1.0/mmember*100.
              if (hb_sort(mmax) >= 1.) then
                do m=1,mmax
                   if (hb_sort(m) >= 1) exit
                enddo
                chance_1ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                if (hb_sort(mmax) >= 2.) then
                   do m=1,mmax
                      if (hb_sort(m) >= 2) exit
                   enddo
                   chance_2ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                   if (hb_sort(mmax) >= 3.) then
                      do m=1,mmax
                         if (hb_sort(m) >= 3) exit
                      enddo
                      chance_3ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                   if (hb_sort(mmax) >= 4.) then
                      do m=1,mmax
                         if (hb_sort(m) >= 4) exit
                      enddo
                      chance_4ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                   if (hb_sort(mmax) >= 5.) then
                      do m=1,mmax
                         if (hb_sort(m) >= 5) exit
                      enddo
                      chance_5ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 6.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 6) exit
                         enddo
                         chance_6ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 7.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 7) exit
                         enddo
                         chance_7ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 8.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 8) exit
                         enddo
                         chance_8ft(i,j) = (mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 9.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 9) exit
                         enddo
                         chance_9ft(i,j) =(mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 10.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 10) exit
                         enddo
                         chance_10ft(i,j) =(mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 13.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 13) exit
                         enddo
                         chance_13ft(i,j) =(mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 15.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 15) exit
                         enddo
                         chance_15ft(i,j) =(mmax-m+1)*1.0/mmember*100.
                      if (hb_sort(mmax) >= 16.) then
                         do m=1,mmax
                            if (hb_sort(m) >= 16) exit
                         enddo
                         chance_16ft(i,j) =(mmax-m+1)*1.0/mmember*100.
                      endif
                      endif
                      endif
                      endif
                      endif
                   endif
                   endif
                   endif 
                  endif 
                 endif 
                endif
               endif
              endif
              endif
!            else !#All dry
! III)            We need nothing to do when all cells are dry (9999. default value)
             endif

           endif
           
!----------------------------------------
! find the maximum probability value of 103 hours     
!---------------------------------------- 
           if ( n == 1) then
              chance_0ft_max(i,j) = chance_0ft(i,j)
              chance_1ft_max(i,j) = chance_1ft(i,j)
              chance_2ft_max(i,j) = chance_2ft(i,j)
              chance_3ft_max(i,j) = chance_3ft(i,j)
              chance_4ft_max(i,j) = chance_4ft(i,j)
              chance_5ft_max(i,j) = chance_5ft(i,j)
              chance_6ft_max(i,j) = chance_6ft(i,j)
              chance_7ft_max(i,j) = chance_7ft(i,j)
              chance_8ft_max(i,j) = chance_8ft(i,j)
              chance_9ft_max(i,j) = chance_9ft(i,j)
              chance_10ft_max(i,j) = chance_10ft(i,j)
              chance_13ft_max(i,j) = chance_13ft(i,j)
              chance_15ft_max(i,j) = chance_13ft(i,j)
              chance_16ft_max(i,j) = chance_16ft(i,j)
           else

              if (chance_0ft(i,j) > chance_0ft_max(i,j).and. &
                  chance_0ft(i,j) <= 100.or.chance_0ft_max(i,j) > 100 &
                  .and.chance_0ft(i,j) <= 100) &
                  chance_0ft_max(i,j) = chance_0ft(i,j)

              if (chance_1ft(i,j) > chance_1ft_max(i,j).and. &
                  chance_1ft(i,j) <= 100.or.chance_1ft_max(i,j) > 100 &
                  .and.chance_1ft(i,j) <= 100) &
                  chance_1ft_max(i,j) = chance_1ft(i,j)

              if (chance_2ft(i,j) > chance_2ft_max(i,j).and. &
                  chance_2ft(i,j) <= 100.or.chance_2ft_max(i,j) > 100 &
                  .and.chance_2ft(i,j) <= 100) &
                  chance_2ft_max(i,j) = chance_2ft(i,j)

              if (chance_3ft(i,j) > chance_3ft_max(i,j).and. &
                  chance_3ft(i,j) <= 100.or.chance_3ft_max(i,j) > 100 &
                  .and.chance_3ft(i,j) <= 100) &
                  chance_3ft_max(i,j) = chance_3ft(i,j)

              if (chance_4ft(i,j) > chance_4ft_max(i,j).and. &
                  chance_4ft(i,j) <= 100.or.chance_4ft_max(i,j) > 100 &
                  .and.chance_4ft(i,j) <= 100) &
                  chance_4ft_max(i,j) = chance_4ft(i,j)

              if (chance_5ft(i,j) > chance_5ft_max(i,j).and. &
                  chance_5ft(i,j) <= 100.or.chance_5ft_max(i,j) > 100 &
                  .and.chance_5ft(i,j) <= 100) &
                  chance_5ft_max(i,j) = chance_5ft(i,j)

              if (chance_6ft(i,j) > chance_6ft_max(i,j).and. &
                  chance_6ft(i,j) <= 100.or.chance_6ft_max(i,j) > 100 &
                  .and.chance_6ft(i,j) <= 100) &
                  chance_6ft_max(i,j) = chance_6ft(i,j)

              if (chance_7ft(i,j) > chance_7ft_max(i,j).and. &
                  chance_7ft(i,j) <= 100.or.chance_7ft_max(i,j) > 100 &
                  .and.chance_7ft(i,j) <= 100) &
                  chance_7ft_max(i,j) = chance_7ft(i,j)

              if (chance_8ft(i,j) > chance_8ft_max(i,j).and. &
                  chance_8ft(i,j) <= 100.or.chance_8ft_max(i,j) > 100 &
                  .and.chance_8ft(i,j) <= 100) &
                  chance_8ft_max(i,j) = chance_8ft(i,j)

              if (chance_9ft(i,j) > chance_9ft_max(i,j).and. &
                  chance_9ft(i,j) <= 100.or.chance_9ft_max(i,j) > 100 &
                  .and.chance_9ft(i,j) <= 100) &
                  chance_9ft_max(i,j) = chance_9ft(i,j)

              if (chance_10ft(i,j) > chance_10ft_max(i,j).and. &
                 chance_10ft(i,j) <= 100.or.chance_10ft_max(i,j) > 100 &
                  .and.chance_10ft(i,j) <= 100) &
                  chance_10ft_max(i,j) = chance_10ft(i,j)

              if (chance_13ft(i,j) > chance_13ft_max(i,j).and. & 
                 chance_13ft(i,j) <= 100.or.chance_13ft_max(i,j) > 100 &
                  .and.chance_13ft(i,j) <= 100) &
                  chance_13ft_max(i,j) = chance_13ft(i,j)

              if (chance_15ft(i,j) > chance_15ft_max(i,j).and. &
                 chance_15ft(i,j) <= 100.or.chance_15ft_max(i,j) > 100 &
                  .and.chance_15ft(i,j) <= 100) &
                  chance_15ft_max(i,j) = chance_15ft(i,j)

              if (chance_16ft(i,j) > chance_16ft_max(i,j).and. & 
                 chance_16ft(i,j) <= 100.or.chance_16ft_max(i,j) > 100 &
                  .and.chance_16ft(i,j) <= 100) &
                  chance_16ft_max(i,j) = chance_16ft(i,j)

           endif   
        enddo
        enddo
        if (n == 1) then
           write(51) mhour
           write(52) mhour
           write(53) mhour
           write(54) mhour
           write(55) mhour
           write(56) mhour
           write(57) mhour
           write(58) mhour
           write(59) mhour
           write(60) mhour
           write(61) mhour
           write(62) mhour
           write(63) mhour
           write(64) mhour
        endif

        write(51) ((chance_1ft(i,j),i=1,imxb),j=1,jmxb)
        write(52) ((chance_2ft(i,j),i=1,imxb),j=1,jmxb)
        write(53) ((chance_3ft(i,j),i=1,imxb),j=1,jmxb)
        write(54) ((chance_6ft(i,j),i=1,imxb),j=1,jmxb)
        write(55) ((chance_10ft(i,j),i=1,imxb),j=1,jmxb)
        write(56) ((chance_13ft(i,j),i=1,imxb),j=1,jmxb)
        write(57) ((chance_16ft(i,j),i=1,imxb),j=1,jmxb)

        write(58) ((chance_0ft(i,j),i=1,imxb),j=1,jmxb)
        write(59) ((chance_4ft(i,j),i=1,imxb),j=1,jmxb)
        write(60) ((chance_5ft(i,j),i=1,imxb),j=1,jmxb)
        write(61) ((chance_7ft(i,j),i=1,imxb),j=1,jmxb)
        write(62) ((chance_8ft(i,j),i=1,imxb),j=1,jmxb)
        write(63) ((chance_9ft(i,j),i=1,imxb),j=1,jmxb)
        write(64) ((chance_15ft(i,j),i=1,imxb),j=1,jmxb)

      enddo
!-----------------------------------------------------------------------
! write the maximum value of 103 hour to the last record (104th) of ssgrid
!-----------------------------------------------------------------------
      write(51) ((chance_1ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(52) ((chance_2ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(53) ((chance_3ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(54) ((chance_6ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(55) ((chance_10ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(56) ((chance_13ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(57) ((chance_16ft_max(i,j),i=1,imxb),j=1,jmxb)

      write(58) ((chance_0ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(59) ((chance_4ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(60) ((chance_5ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(61) ((chance_7ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(62) ((chance_8ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(63) ((chance_9ft_max(i,j),i=1,imxb),j=1,jmxb)
      write(64) ((chance_15ft_max(i,j),i=1,imxb),j=1,jmxb)


      close(51)
      close(52)
      close(53)
      close(54)
      close(55)
      close(56)
      close(57)
      close(58)
      close(59)
      close(60)
      close(61)
      close(62)
      close(63)
      close(64)
      do m=1,mmember
         close(112+m)
      enddo
      if(tide=='Y')then
        close(34)
      endif
      deallocate (hb)

      deallocate (chance_0ft)
      deallocate (chance_0ft_max)
      deallocate (chance_4ft)
      deallocate (chance_4ft_max)
      deallocate (chance_5ft)
      deallocate (chance_5ft_max)
      deallocate (chance_7ft)
      deallocate (chance_7ft_max)
      deallocate (chance_8ft)
      deallocate (chance_8ft_max)
      deallocate (chance_9ft)
      deallocate (chance_9ft_max)
      deallocate (chance_15ft)
      deallocate (chance_15ft_max)


      deallocate (chance_1ft)
      deallocate (chance_1ft_max)
      deallocate (chance_2ft)
      deallocate (chance_2ft_max)
      deallocate (chance_3ft)
      deallocate (chance_3ft_max)
      deallocate (chance_6ft)
      deallocate (chance_6ft_max)
      deallocate (chance_10ft)
      deallocate (chance_10ft_max)
      deallocate (chance_13ft)
      deallocate (chance_13ft_max)
      deallocate (chance_16ft)
      deallocate (chance_16ft_max)
      deallocate (hb_sort)
      deallocate (hb_tide)
      deallocate (i_boun)

      end

   
