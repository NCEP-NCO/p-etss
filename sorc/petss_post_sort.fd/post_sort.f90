!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c
!c       MAIN PROGRAM TO POST PROCESSING OF THE P-ETSS OUTPUT STATION DATA
!c       AND GRIDDED DATA    
!c
!c    ARGUMENTS:
!c    
!c       READ POST PORCESSING TYPE FROM FILE.  stn -- CALL stn_sort
!c                                             exc -- CALL grid_exceedance
!c                                             pro -- CALL grid_probabl
!c
!c    INPUT FILES:
!c       FORT.49  - POST PORCESSING TYPE (STN - STATION
!c                                        EXC - GRID EXCEEDANCE
!c                                        PRO - GRID PROBABILITY)
!c
!c    OUTPUTFILES:
!c
!c
!c    VARIABLES:
!c
!c      INPUT
!c       prod     == stn/exc/pro
!c     OUTPUT
!c
!c    AUTHORS:
!c       Huiqing Liu /MDL Jan. 2017 
!c           
!c    HISTORY:
!c       01/2017--Huiqing Liu /MDL created routine
!c       02/2018--Huiqing Liu /MDL Updated to handle 42 NAEFS ens members
!c
!c    NOTES:
!c       INCLUDEING MODULE SUBSORT,WHICH WILL BE USED BY OTHER THREE SUBROUTINES
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

module subsort

contains
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between Start and End.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

   subroutine  FindMinimum(x, Start, End, LL)
      implicit none
      real, dimension(1:), intent(in)    :: x
      integer, intent(in)                :: Start, End
      integer, intent(out)               :: LL
      real                               :: Minimum
      integer                            :: Location
      integer                            :: i

      Minimum  = x(Start)               ! assume the first is the min
      Location = Start                  ! record its position
      do i = Start+1, End               ! start with next elements
         if (x(i) < Minimum) then       !   if x(i) less than the min?
            Minimum  = x(i)             !      Yes, a new minimum found
            Location = i                !      record its position
         endif
      enddo
      LL = Location            ! return the position
   end subroutine  FindMinimum

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

   subroutine  Swap(a, b)
      implicit none
      real, intent(inout)    :: a, b
      real                   :: Temp

      Temp = a
      a    = b
      b    = Temp
   end subroutine  Swap

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

   subroutine  Sort(x, Size)
      implicit none
      real, dimension(1:), intent(inout)    :: x
      integer, intent(in)                   :: Size
      integer                               :: i
      integer                               :: Location
!      INTEGER, EXTERNAL                     :: FindMinimum

      do i = 1, Size-1                  ! except for the last
         call FindMinimum(x, i, Size, Location)     ! find min from this to last
         call  Swap(x(i), x(Location))  ! swap this and the minimum
      enddo
   end subroutine  Sort

end module subsort

! quicksort.f -*-f90-*-
! Author: t-nissie
! License: GPLv3
! Gist: https://gist.github.com/t-nissie/479f0f16966925fa29ea
!!
recursive subroutine quicksort(a, first, last)
  implicit none
  real*8  a(*), x, t
  integer first, last
  integer i, j

  x = a( (first+last) / 2 )
  i = first
  j = last
  do
     do while (a(i) < x)
        i=i+1
     end do
     do while (x < a(j))
        j=j-1
     end do
     if (i >= j) exit
     t = a(i);  a(i) = a(j);  a(j) = t
     i=i+1
     j=j-1
  end do
  if (first < i-1) call quicksort(a, first, i-1)
  if (j+1 < last)  call quicksort(a, j+1, last)
end subroutine quicksort

program main

   implicit none

   character (len=255) :: fil_49
   character (len=10)  :: prod
   integer             :: cyc


   call getenv('FORT49',fil_49)
   open(49,file=fil_49)

   read(49,*) prod,cyc
   write (*,*) 'Prodcut is: ', prod
   close(49)

   if (prod == 'stn_gefs') then               !21 GEFS ens members for station
      call stn_sort_all (21)          
   elseif (prod == 'stn_naefs') then          !42 NAEFS ensemble members for station
      call stn_sort_all (42)
   elseif (prod == 'stn_gefsn') then          !31 NewGEFS ensemble members for station
      call stn_sort_all (31)
   elseif (prod == 'stn_naefsn') then         !52 NewNAEFS ensemble members for station
      call stn_sort_all (52)
   elseif (prod == 'exc_gefs') then           !21 GEFS ens members for exceendence
      call grid_exceedance_all (21,cyc)       
   elseif (prod == 'exc_naefs') then          !42 NAEFS ensemble members for exceendence
      call grid_exceedance_all (42,cyc)      
   elseif (prod == 'exc_gefsn') then          !31 NewGEFS ensemble members for exceendence
      call grid_exceedance_all (31,cyc)      
   elseif (prod == 'exc_naefsn') then         !52 NewNAEFS ensemble members for exceendence
      call grid_exceedance_all (52,cyc)      
   elseif (prod == 'pro_gefs') then           !21 GEFS ens members for probalistics
      call grid_probabl_all (21,cyc)         
   elseif (prod == 'pro_naefs') then          !42 NAEFS ensemble members for probalistics
      call grid_probabl_all (42,cyc)          
   elseif (prod == 'pro_gefsn') then          !31 NewGEFS ensemble members for probalistics
      call grid_probabl_all (31,cyc)
   elseif (prod == 'pro_naefsn') then         !52 NewNAEFS ensemble members for probalistics
      call grid_probabl_all (52,cyc)
   endif

end

