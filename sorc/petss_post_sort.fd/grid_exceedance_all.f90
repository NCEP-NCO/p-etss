      subroutine grid_exceedance_all (mmember,cyc)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c
!c       POST PROCESSING OF THE P-ETSS OUTPUT GRIDDED DATA TO GENERATE
!c       SURGE AND TIDE HEIGHT EXCEDDED BY 10%,20%,30%,40%,50%,90%  
!c       AND ENSEMBLE MAX,MIN,MEAN
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
!c       FORT.51-59  - ENSEMBLE max,mean,min,10%,90%,20%,30%...50%
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
!c       HB_??  == MAXIMUM,MEAN,MIN,10%,90%,20%,30%...50% SURGE ARRAY
!c
!c    AUTHORS:
!c       Huiqing Liu /MDL Jan. 2016 
!c           
!c    HISTORY:
!c       01/2016--Huiqing Liu /MDL Created the routine
!c       07/2016--Huiqing Liu /MDL Added 20%,30%,40% and 50%
!c       01/2017--Huiqing Liu /MDL Added header block
!c       02/2018--Huiqing Liu /MDL Updated to handle 42 NAEFS ens members
!c       03/2020--Huiqing Liu /MDL Updated to handle 52 NewNAEFS + 31 NewGEFS ens members
!c
!c
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      use subsort
! Sort array subroutine from post_sort.f90
      implicit none

      integer             :: mmember,cyc

      character (len=255) :: FIL_12,FIL_34,FILEI
      character (len=255) :: FIL_51,FIL_52,FIL_53,FIL_54,FIL_55
      character (len=255) :: FIL_56,FIL_57,FIL_58,FIL_59
      character (len=11)  :: Envvar
      integer             :: i,j,m,n,imxb,jmxb,hour,mhour
      integer             :: i_dumy,dry_num,iunit,lunit,membstrt
      integer,dimension(:,:),allocatable :: i_boun
 
      real,dimension(:,:),allocatable    :: hb_10p,hb_90p
      real,dimension(:,:),allocatable    :: hb_20p,hb_30p,hb_40p,hb_50p
      real,dimension(:,:),allocatable    :: hb_max,hb_min,hb_mean 
      real,dimension(:,:),allocatable    :: hb_maxT,hb_minT,hb_meanT 
      real,dimension(:,:),allocatable    :: hb_10pT,hb_90pT,hb_tide
      real,dimension(:,:),allocatable    :: hb_20pT,hb_30pT,hb_40pT
      real,dimension(:,:),allocatable    :: hb_50pT
      real,dimension(:),allocatable      :: hb_sort
      real,dimension(:,:,:),allocatable  :: hb
      real                               :: z_dumy
      character (len=1)                  :: tide,basinNEP
!-----------------------------------------------------------------


!    OPEN UNIT NUMBERS

!     FORT.12  - CONTROL FILE FOR BASIN SIZE

      call getenv('FORT12',FIL_12)
      open(12,file=FIL_12)

!     FORT.51-59  - ENSEMBLE max,mean,min,10%,90%,20%,30%...50%

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
      call getenv('FORT58',FIL_58)
      open(58,file=FIL_58,form='unformatted')
      call getenv('FORT59',FIL_59)
      open(59,file=FIL_59,form='unformatted')

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
      allocate (hb_max (imxb,jmxb))
      allocate (hb_min (imxb,jmxb))
      allocate (hb_mean (imxb,jmxb))
      allocate (hb_10p (imxb,jmxb))
      allocate (hb_20p (imxb,jmxb))
      allocate (hb_30p (imxb,jmxb))
      allocate (hb_40p (imxb,jmxb))
      allocate (hb_50p (imxb,jmxb))
      allocate (hb_90p (imxb,jmxb))
      allocate (hb_maxT (imxb,jmxb))
      allocate (hb_minT (imxb,jmxb))
      allocate (hb_meanT (imxb,jmxb))
      allocate (hb_10pT (imxb,jmxb))
      allocate (hb_20pT (imxb,jmxb))
      allocate (hb_30pT (imxb,jmxb))
      allocate (hb_40pT (imxb,jmxb))
      allocate (hb_50pT (imxb,jmxb))
      allocate (hb_90pT (imxb,jmxb))
      allocate (hb_sort (mmember))

      allocate (hb_tide (imxb,jmxb))
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
      
      read(11) mhour
      close(11)

      hb_tide=0.

      do n=1,47
        do m=1,mmember
           read(112+m) ((hb(m,i,j),i=1,imxb),j=1,jmxb)
        enddo
        if (tide=='Y') then
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
! Huiqing.Liu /MDL March 2020
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
               if (tide=='Y') then
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
        if (tide=='Y') then
           read(34) ((hb_tide(i,j),i=1,imxb),j=1,jmxb)
        endif
        do i=1,imxb
        do j=1,jmxb

! Initialize Arrays

           hb_max(i,j)=9999.
           hb_10p(i,j)=9999.
           hb_20p(i,j)=9999.
           hb_30p(i,j)=9999.
           hb_40p(i,j)=9999.
           hb_50p(i,j)=9999.
           hb_90p(i,j)=9999.
           hb_min(i,j)=9999.
           hb_mean(i,j)=9999.

           dry_num = 0

           do m=1,mmember
              hb_sort(m) = hb(m,i,j)+hb_tide(i,j)
              if (hb_sort(m)>60) dry_num = dry_num + 1
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

           if (hb_sort(mmember) < 60 ) then ! A) All memebers are wet

              hb_max(i,j)=hb_sort(mmember)
              hb_min(i,j)=hb_sort(1)
              hb_mean(i,j)=sum(hb_sort)/mmember
              if ( mmember == 21 ) then
                 hb_10p(i,j)=hb_sort(20)
                 hb_20p(i,j)=hb_sort(mmember-3)
                 hb_30p(i,j)=hb_sort(mmember-5)
                 hb_40p(i,j)=hb_sort(mmember-7)
                 hb_50p(i,j)=hb_sort(11)
                 hb_90p(i,j)=hb_sort(3)
              elseif ( mmember == 31 ) then
                 hb_10p(i,j)=hb_sort(29)
                 hb_20p(i,j)=hb_sort(26)
                 hb_30p(i,j)=hb_sort(23)
                 hb_40p(i,j)=hb_sort(20)
                 hb_50p(i,j)=hb_sort(16)
                 hb_90p(i,j)=hb_sort(4)
              elseif ( mmember == 42 ) then
                 hb_10p(i,j)=hb_sort(39)
                 hb_20p(i,j)=hb_sort(mmember-7)
                 hb_30p(i,j)=hb_sort(mmember-12)
                 hb_40p(i,j)=hb_sort(mmember-16)
                 hb_50p(i,j)=hb_sort(21)
                 hb_90p(i,j)=hb_sort(5)
              elseif (mmember == 52 ) then
                 hb_10p(i,j)=hb_sort(47)
                 hb_20p(i,j)=hb_sort(mmember-9)
                 hb_30p(i,j)=hb_sort(mmember-14)
                 hb_40p(i,j)=hb_sort(mmember-19)
                 hb_50p(i,j)=hb_sort(26)
                 hb_90p(i,j)=hb_sort(6)
              endif
           else                             ! B) Partial members are wet

              do m=1,mmember
                 if (hb_sort(m) > 60) exit ! Locate the first number of member is dry
              enddo

              hb_max(i,j)=hb_sort(mmember)
              hb_10p(i,j)=hb_sort(mmember)
              hb_20p(i,j)=hb_sort(mmember)
              hb_30p(i,j)=hb_sort(mmember)
              hb_40p(i,j)=hb_sort(mmember)
              hb_50p(i,j)=hb_sort(mmember)
              hb_90p(i,j)=hb_sort(mmember)
              hb_min(i,j)=hb_sort(mmember)

              if ((m-1) >= 1) then
                 if ((m-1) == 1) then ! --1 only one member is wet
                    hb_max(i,j)=hb_sort(1) ! Only ens max is wet, all others are dry
                 else                 ! --2 more than one member are wet
                    hb_max(i,j)=hb_sort(m-1)
                    if (mmember == 21) then
                       if ((m-2) >= 1)hb_10p(i,j) = hb_sort(m-2)
                       if ((m-4) >= 1)hb_20p(i,j) = hb_sort(m-4)
                       if ((m-6) >= 1)hb_30p(i,j) = hb_sort(m-6)
                       if ((m-8) >= 1)hb_40p(i,j) = hb_sort(m-8)
                       if ((m-11) >= 1)hb_50p(i,j) = hb_sort(m-11)
                       if ((m-19) >= 1)hb_90p(i,j) = hb_sort(m-19)
                    elseif (mmember == 31) then
                       if ((m-3) >= 1)hb_10p(i,j) = hb_sort(m-3)
                       if ((m-6) >= 1)hb_20p(i,j) = hb_sort(m-6)
                       if ((m-9) >= 1)hb_30p(i,j) = hb_sort(m-9)
                       if ((m-12) >= 1)hb_40p(i,j) = hb_sort(m-12)
                       if ((m-16) >= 1)hb_50p(i,j) = hb_sort(m-16)
                       if ((m-24) >= 1)hb_90p(i,j) = hb_sort(m-24)
                    elseif (mmember == 42) then
                       if ((m-4) >= 1)hb_10p(i,j) = hb_sort(m-4)
                       if ((m-8) >= 1)hb_20p(i,j) = hb_sort(m-8)
                       if ((m-13) >= 1)hb_30p(i,j) = hb_sort(m-13)
                       if ((m-17) >= 1)hb_40p(i,j) = hb_sort(m-17)
                       if ((m-22) >= 1)hb_50p(i,j) = hb_sort(m-22)
                       if ((m-39) >= 1)hb_90p(i,j) = hb_sort(m-39)
                    elseif (mmember == 52) then
                       if ((m-5) >= 1)hb_10p(i,j) = hb_sort(m-5)
                       if ((m-10) >= 1)hb_20p(i,j) = hb_sort(m-10)
                       if ((m-15) >= 1)hb_30p(i,j) = hb_sort(m-15)
                       if ((m-20) >= 1)hb_40p(i,j) = hb_sort(m-20)
                       if ((m-26) >= 1)hb_50p(i,j) = hb_sort(m-26)
                       if ((m-46) >= 1)hb_90p(i,j) = hb_sort(m-46)
                    endif
                 endif  
                 where (hb_sort > 60)
                    hb_sort = 0.
                 endwhere
                 hb_mean(i,j)=sum(hb_sort)/(m-1)
              endif
                                       
           endif
                                              ! C) All dry do nothing (vaules are all set to default.
           if (hb_max(i,j) > 60) hb_max(i,j) = 9999.
           if (hb_min(i,j) > 60) hb_min(i,j) = 9999.
           if (hb_mean(i,j) > 60) hb_mean(i,j) = 9999.
           if (hb_10p(i,j) > 60) hb_10p(i,j) = 9999.
           if (hb_20p(i,j) > 60) hb_20p(i,j) = 9999.
           if (hb_30p(i,j) > 60) hb_30p(i,j) = 9999.
           if (hb_40p(i,j) > 60) hb_40p(i,j) = 9999.
           if (hb_50p(i,j) > 60) hb_50p(i,j) = 9999.
           if (hb_90p(i,j) > 60) hb_90p(i,j) = 9999.

           if (abs(hb_mean(i,j)) <= 1.E-10) hb_mean(i,j) = 9999.
!----------------------------------------
! find the maximum value of 103 hours     
!---------------------------------------- 
           if ( n == 1) then
              hb_maxT(i,j)=hb_max(i,j)
              hb_minT(i,j)=hb_min(i,j)
              hb_meanT(i,j)=hb_mean(i,j)
              hb_10pT(i,j)=hb_10p(i,j)
              hb_20pT(i,j)=hb_20p(i,j)
              hb_30pT(i,j)=hb_30p(i,j)
              hb_40pT(i,j)=hb_40p(i,j)
              hb_50pT(i,j)=hb_50p(i,j)
              hb_90pT(i,j)=hb_90p(i,j)
           else
              if (hb_max(i,j) < 60 .and. hb_max(i,j) > hb_maxT(i,j).or. &
                  hb_maxT(i,j) > 60) hb_maxT(i,j)=hb_max(i,j)
              if (hb_min(i,j) < 60 .and. hb_min(i,j) > hb_minT(i,j).or. &
                  hb_minT(i,j) > 60) hb_minT(i,j)=hb_min(i,j)
              if (hb_mean(i,j) < 60 .and. hb_mean(i,j) > hb_meanT(i,j).or. &
                  hb_meanT(i,j) > 60) hb_meanT(i,j)=hb_mean(i,j)
              if (hb_10p(i,j) < 60 .and. hb_10p(i,j) > hb_10pT(i,j).or. &
                  hb_10pT(i,j) > 60) hb_10pT(i,j)=hb_10p(i,j)
              if (hb_20p(i,j) < 60 .and. hb_20p(i,j) > hb_20pT(i,j).or. &
                  hb_20pT(i,j) > 60) hb_20pT(i,j)=hb_20p(i,j)
              if (hb_30p(i,j) < 60 .and. hb_30p(i,j) > hb_30pT(i,j).or. &
                  hb_30pT(i,j) > 60) hb_30pT(i,j)=hb_30p(i,j)
              if (hb_40p(i,j) < 60 .and. hb_40p(i,j) > hb_40pT(i,j).or. &
                  hb_40pT(i,j) > 60) hb_40pT(i,j)=hb_40p(i,j)
              if (hb_50p(i,j) < 60 .and. hb_50p(i,j) > hb_50pT(i,j).or. &
                  hb_50pT(i,j) > 60) hb_50pT(i,j)=hb_50p(i,j)
              if (hb_90p(i,j) < 60 .and. hb_90p(i,j) > hb_90pT(i,j).or. &
                  hb_90pT(i,j) > 60) hb_90pT(i,j)=hb_90p(i,j)
                           
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
        endif

        write(51) ((hb_max(i,j),i=1,imxb),j=1,jmxb)

        write(52) ((hb_min(i,j),i=1,imxb),j=1,jmxb)

        write(53) ((hb_mean(i,j),i=1,imxb),j=1,jmxb)

        write(54) ((hb_10p(i,j),i=1,imxb),j=1,jmxb)

        write(55) ((hb_90p(i,j),i=1,imxb),j=1,jmxb)

        write(56) ((hb_20p(i,j),i=1,imxb),j=1,jmxb)
        write(57) ((hb_30p(i,j),i=1,imxb),j=1,jmxb)
        write(58) ((hb_40p(i,j),i=1,imxb),j=1,jmxb)
        write(59) ((hb_50p(i,j),i=1,imxb),j=1,jmxb)

      enddo
!-----------------------------------------------------------------------
! write the maximum value of 102 hour to the last record (103th) of ssgrid
!-----------------------------------------------------------------------
      write(51) ((hb_maxT(i,j),i=1,imxb),j=1,jmxb)

      write(52) ((hb_minT(i,j),i=1,imxb),j=1,jmxb)

      write(53) ((hb_meanT(i,j),i=1,imxb),j=1,jmxb)

      write(54) ((hb_10pT(i,j),i=1,imxb),j=1,jmxb)

      write(55) ((hb_90pT(i,j),i=1,imxb),j=1,jmxb)

      write(56) ((hb_20pT(i,j),i=1,imxb),j=1,jmxb)
      write(57) ((hb_30pT(i,j),i=1,imxb),j=1,jmxb)
      write(58) ((hb_40pT(i,j),i=1,imxb),j=1,jmxb)
      write(59) ((hb_50pT(i,j),i=1,imxb),j=1,jmxb)

      close(51)
      close(52)
      close(53)
      close(54)
      close(55)
      close(56)
      close(57)
      close(58)
      close(59)
      do m=1,mmember
         close(112+m)
      enddo
      if(tide=='Y')then
        close(34)
      endif
      deallocate (hb)
      deallocate (hb_sort)
      deallocate (hb_max)
      deallocate (hb_min)
      deallocate (hb_mean)
      deallocate (hb_10p)
      deallocate (hb_20p)
      deallocate (hb_30p)
      deallocate (hb_40p)
      deallocate (hb_50p)
      deallocate (hb_90p)
      deallocate (hb_maxT)
      deallocate (hb_minT)
      deallocate (hb_meanT)
      deallocate (hb_10pT)
      deallocate (hb_20pT)
      deallocate (hb_30pT)
      deallocate (hb_40pT)
      deallocate (hb_50pT)
      deallocate (hb_90pT)
      deallocate (hb_tide)
      deallocate (i_boun)

      end

   
