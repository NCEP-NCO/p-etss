      subroutine xFilter_new (version)
!=======================================================================
! Purpose:
!    Smooth surge heights to eliminate numerical noise (checkerboarding)
!    caused by grid-splitting due to the semi-staggered Arakawa B grid.
!    The filter is a weighted combination of '+' (plus) and 'x' (cross)
!    Laplacian diffusion operators.
!
! Input:
!    version       = 1 Use original version for backward compatibility,
!                  = 2 Use current correct version.
!
! Output:
!    hb( , )       = Filtered height (surge) field.
!
! Variables:
!    hb  ( , )     = Surge (height) field.
!    zb  ( , )     = Depth (bathymetry) field.
!    zbm ( , )    =  Max barrier heights at momentum points.
!    hsub( , )     = Scratch space.
!    ip(4) jp(4)   = Shift subscripts from a height (surge) point to 4
!                    momentum corner points.
!                    The (i,j) shifts to momentum points at corner
!                    points k = 1 to 4 are:
!                       i+0,j+1     i+1,j+1
!                        k = 4 ----- k = 3
!                        (u,v)       (u,v)
!                    ^     .           .
!                    |     .           .
!                    j     .     h     .
!                    |     .           .
!                    |     .           .
!                        (u,v)       (u,v)
!                        k = 1 ----- k = 2
!                       i+0,j+0     i+1,j+0
!                              --i-->
!    ibb(6, m)     = Ranges and increments for 3 do-loops of (i,j)
!                       m = 1 interior
!                       m = 2 horizontal boundary (bottom, top)
!                       m = 3 vertical   boundary (left, right)
!                       Ranges on three basin segments are:
!                          22222...22222
!                          31111...11113
!                       ^  31111...11113
!                       |      . . .
!                       j      . . .
!                       |  31111...11113
!                       |  31111...11113
!                          22222...22222
!                              --i-->
!    j2(j) jc      = Shift j-subscript to present time of surge field.
!    iih(4) jjh(4) = Shift subscripts from a height (surge) point to 4
!                    adjacent surge points.
!
! General Comments:
!    A 5-point smoothing operator is used, neighboring grids weighed
!    conditionally if they are dry or blocked by barriers.
!    Shifts of (i,j) to adjacent squares are set via iih(4) and jjh(4):
!    iih = /0,1,0,-1/, jjh = /-1,0,1,0/
!
!           i-1     i      i+1
!               .-----.             +hmx,hb,zb height pts
!         i+iih i i+0 i             .zbm barrier pts
!    j+1    +   i 3+  i
!         j+jjh i j+1 i             example 0.interior points
!         .-----.-----.-----.        i  1  i        i  0  i
!         i i-1 i     i i+1 i        i     i        i     i
!    j    i 4+  i  +  i 2+  i  (1/8) i1 4 1i + (1/8)ia 0 bi
!         i j+0 i(i,j)i j+0 i        i     i        i     i
!         .-----.-----.-----.        i  1  i        i  0  i
!               i i+0 i
!    j-1        i 1+  i
!               i j-1 i             a = gama1,b = gama
!               .-----.
!           i-1     i      i+1
!
!               .-----.
!               i     i
!    j+1        i 3+  i       example 1. suqare 2 excluded
!               i     i
!         .-----.-----.          i  1  i        i  0  i
!         i     i     i          i     i        i     i
!    j    i 4+  i  +  i    (1/8) i1 5 0i + (1/8)ia b 0i
!         i     i(i,j)i          i     i        i     i
!         .-----.-----.          i  1  i        i  0  i
!               i     i
!    j-1        i 1+  i
!               i     i
!               .-----.
!
!                              example 2. squares 2 and 3
!                                         are excluded
!         .-----.-----.          i  0  i        i  0  i
!         i     i     i          i     i        i     i
!    j    i 4+  i  +  i    (1/8) i1 6 0i + (1/8)ia b 0i
!         i     i(i,j)i          i     i        i     i
!         .-----.-----.          i  1  i        i  0  i
!               i     i
!    j-1        i 1+  i
!               i     i
!               .-----.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! Development History:
!    Date      Programmer              Description of change
!    ----      ----------              ---------------------
!    09/1980   Jye Chen  NOAA/TDL      Wrote the SLOSH filter code.
!    04/2011   A. Taylor NOAA/MDL,     Modified the SLOSH filter code
!              C. Forbes NOAA/NHC      to include the cross Laplacian
!                                      filter.
!    06/2011   C. Forbes NOAA/NHC      Incorporated masks according to
!                                      Killworth (1991) and Deleersnijder
!                                      (1995), implemented masks for
!                                      coastlines, barriers, boundaries,
!                                      and included averaging.
!                                      Recoded the do-loop and go-to
!                                      logic to make code more efficient.
!    08/2011   A. Taylor NOAA/MD       Allowed user to choose different
!                                      Hcrt values (0.5 or 0.1).
!    08/2013   C. Mattocks , UM/RSMAS  Modernized, optimized code.
!
! References:
!    Killworth, P.D., Stainforth, D., Webb, D.J., Paerson, S.M., 1991.
!       The development of a free-surface Bryan-Cox-Semtner ocean model.
!       Journal of Physical Oceanography, 21, 1333-1348.
!
!    Deleersnijder, E., Campin, J.M., 1995. On the computation of the
!       barotropic mode of a free-surface world ocean model.
!       Annales Geophysicae, 13, 675-688.
!=======================================================================
!      implicit none

      include 'parm.for'

      integer, intent(in) :: version

      !---------------
      ! Filter weights
      !---------------
      real, parameter :: alfa = 0.125  ! Weight of combination Laplacian filter
      real, parameter :: beta = 1.     ! Weight of 'x'         Laplacian filter

      !-------------------------
      ! Check mass conservation?
      !-------------------------
      logical, parameter :: checkMass = .false.

      integer, parameter :: np = 4

      integer :: imxb, jmxb, imxb1, jmxb1, imxb2, jmxb2
      common /dumb3/ imxb, jmxb, imxb1, jmxb1, imxb2, jmxb2

      integer, dimension(np) :: ip,jp, ih,jh
      common /dumb8/ ip,jp, ih,jh

      integer, dimension(np) :: iih,jjh, ihh,jhh
      common /dum88/ iih,jjh, ihh,jhh

      real                :: gama, gama1
      real, dimension(np) :: gamfp
      common /gamaf/ gama, gama1, gamfp

      character(len=2) :: dollar
      character(len=1) :: ebsn
      common /gprt1/ dollar, ebsn

      real :: Hter
      common /hterain/ Hter

      integer :: ismth
      common /smth/ ismth

      real :: Hcrt
      real :: hTemp, hst
      real :: ww,www, xx,xxx, yy,yyy, z,zz,zzz
      real :: xsm2, hsm2

      integer :: i,j, k,m                 ! Loop indices
      integer :: ii,jj, ii2,jj2, ii3,jj3
      integer :: ix,jx, ix2,jx2, ix3,jx3, il,jl, im,jm, incr,jncr
      integer :: ia,ja, ib,jb, ia1,ja1, ia2,ja2, iam1,jam1
      integer :: kk, kkm, km, km1, km1m

      integer :: massCnt
      real    :: massBef, massAft, massDif, massMin, massMax, massSum
      common /mass/ massCnt, massBef, massAft, massDif,
     &              massMin, massMax, massSum

      integer, dimension(np), parameter :: iix = (/ -1,  1, 1, -1 /)
      integer, dimension(np), parameter :: jjx = (/ -1, -1, 1,  1 /)
      integer, dimension(np), parameter :: ip1 = (/ -1,  1, 2,  0 /)
      integer, dimension(np), parameter :: jp1 = (/  0, -1, 1,  2 /)
      integer, dimension(np), parameter :: ip2 = (/  0,  2, 1, -1 /)
      integer, dimension(np), parameter :: jp2 = (/ -1,  0, 2,  1 /)

      integer                :: maskc , msumii, msumix
      integer, dimension(np) :: maskii, maskix
      integer, dimension(np) :: mbarrx, mbarri
      integer, dimension(np) :: mbouni, mbounx
!     real   , dimension(np) :: gamf  , gamfx
      real                   :: exclude
      real   , parameter     :: small = .00001

      integer, dimension(np+2, np-1) :: ibb

      data ibb(1,1), ibb(1,2), ibb(1,3) /2, 1, 2/,
     &     ibb(3,1),           ibb(3,3) /1,    1/,
     &     ibb(4,1), ibb(4,2), ibb(4,3) /2, 1, 1/,
     &     ibb(6,1), ibb(6,2)           /1, 1   /

!     data Hcrt /0.1/  ! Water surface height difference that determines
!     data Hcrt /0.5/  ! whether water spills over a barrier
!     data Hcrt /1.0/

      select case (ismth)
         case (2)
            Hcrt    = 0.5
            exclude = Hcrt
         case (3)
            Hcrt    = 0.1
            exclude = Hcrt
         case (4)
            Hcrt    = 0.2
            exclude = Hcrt
         case (5)
            Hcrt    = 0.3
            exclude = Hcrt
         case (6)
            Hcrt    = 0.4
            exclude = Hcrt
         case (7)
!           Hcrt    = 0.1
            Hcrt    = small
            exclude = small
         case (8)
            Hcrt    = 0.2
            exclude = small
         case (9)
            Hcrt    = 0.3
            exclude = small
         case (10)
            Hcrt    = 0.4
            exclude = small
         case (11)
            Hcrt    = 0.5
            exclude = small
         case default
           print *, "Error setting Hcrt in subroutine xFilter"
           stop
      end select

      ibb(2,1) = jmxb2
      ibb(5,1) = imxb2

      ibb(2,2) = jmxb1
      ibb(3,2) = jmxb2
      ibb(5,2) = imxb1

      ibb(2,3) = jmxb2
      ibb(5,3) = imxb1
      ibb(6,3) = imxb2

      do j = 1, jmxb1
         do i = 1, imxb1
            hsub(i,j) = hb(i,j)
         end do
      end do

      !---------------------------
      ! Check on mass conservation
      !---------------------------
      if (checkMass) then
         massBef = 0.
         do j = 1, jmxb1
            do i = 1, imxb1
               massBef = massBef + hb(i,j)
            end do
         end do
      end if

      !-----------------------------------------
      ! Start Main Loop for all (i,j) grid cells
      ! m = 1    interior cells
      ! m = 2,3  boundary cells  {AT??}
      !-----------------------------------------
      do 100 m = 1, 3
         jl   = ibb(1,m)
         jm   = ibb(2,m)
         jncr = ibb(3,m)
         il   = ibb(4,m)
         im   = ibb(5,m)
         incr = ibb(6,m)

         do 90 j = jl, jm, jncr
            do 80 i = il, im, incr
               !-------------------------------------------
               ! Skip computations for terrain > Hter feet.
               ! Changed from a hard-wired constant to Hter
               ! by NSM on 11/20/2010, accepted 4/4/2011
               !-------------------------------------------
               if (zb(i,j) <= -Hter) cycle

               !------------------------------------
               ! Test for land wetted, but < exclude
               !------------------------------------
               hst = hsub(i,j) + zb(i,j)
               if (hst < exclude) cycle

               !----------------------------
               ! Initialize filter variables
               !----------------------------
               hsm2  = 0.              ! Sum of all + cells for + filter
               xsm2  = 0.              ! Sum of all x cells for x filter
               maskc = 1               ! Mask for center cell
!              hdif  = 0.
!              xdif  = 0.
!              gam0  = elpdl2(i) + (elpcl2(i) - elpdl2(i)) * sinl2(j)

               !--------------------------------------------------------
               ! Start smoothing loop around the center cell, counter-
               ! clockwise from:
               ! south    , east     , north    , west      for + filter
               ! and from:
               ! southwest, southeast, northeast, northwest for x filter
               !--------------------------------------------------------
               do 30 k = 1, 4
                  !---------|---------|---------|
                  !    ix   |   ii    |    ix   |
                  !    jx   |   jj    |    jx   |
                  !  k = 4  | k = 3   |  k = 3  |
                  !---------|---------|---------|
                  !    ii   |    i    |    ii   |
                  !    jj   |    j    |    jj   |
                  !  k = 4  |         |  k = 2  |
                  !---------|---------|---------|
                  !    ix   |   ii    |    ix   |
                  !    jx   |   jj    |    jx   |
                  !  k = 1  | k = 1   |  k = 2  |
                  !---------|---------|---------|

                  !--------------------------
                  ! Initialize mask variables
                  !--------------------------
                  maskii(k) = 0 ! Mask for ii cells according to height
                                ! (+ filter) (0 no flow, 1 flow)

                  mbarri(k) = 0 ! Mask for ii cells according to barrier
                                ! (+ filter) (1 no flow, 0 flow)

                  mbouni(k) = 0 ! Mask for ii cells according to boundary
                                ! (+ filter) (1 no flow, 0 flow)

                  maskix(k) = 0 ! Mask for ix cells according to height
                                ! (x filter) (0 no flow, 1 flow)

                  mbarrx(k) = 0 ! Mask for ix cells according to barrier
                                ! (x filter) (1 no flow, 0 flow)

                  mbounx(k) = 0 ! Mask for ix cells according to boundary
                                ! (x filter) (1 no flow, 0 flow)

                  !---------------------------
                  ! Start '+' Laplacian filter
                  !---------------------------
                  ii = i + iih(k)
                  jj = j + jjh(k)
                  ix = i + iix(k)
                  jx = j + jjx(k)

                  !-----------------------------------------------
                  ! If any variable from filter hits the boundary,
                  ! do not do anything - cycle to end of loop.
                  !-----------------------------------------------
                  if (ii == 0 .or. jj == 0 .or.
     &                ix == 0 .or. jx == 0 .or.
     &                i  == 0 .or. j  == 0     ) cycle

!!                  if (m /= 1) then
!!                     if (ii == 0 .or. jj == 0) then
!!                        maskii(k) = 0
!!                        mbarri(k) = 0
!!                        mbouni(k) = 1
!!                        go to 10
!!                     end if
!!                  end if

                  !-------------------------------------------------
                  ! If the cell from + filter hits the imxb or jmxb
                  ! boundary or the height < critical height (Hcrt),
                  ! set masks values accordingly
                  !-------------------------------------------------
                  if (ii == imxb .or. jj == jmxb .or.
     &               hsub(ii,jj) + zb(ii,jj) < Hcrt) then
!!!                  maskii(k) = 0   ! already set to 0
!!!                  mbarri(k) = 0   ! already set to 0
                     mbouni(k) = 1
                  else
                     !---------------------------------------------
                     ! Water on either side must exceed the (lower)
                     ! barrier by at least Hcrt
                     !---------------------------------------------
                     kk  = mod(k,4) + 1
                     ia  = i + ip(k)
                     ib  = i + ip(kk)
                     ja  = j + jp(k)
                     jb  = j + jp(kk)
                     z   = zbm(ia,ja)
                     zz  = zbm(ib,jb)
                     zzz = min(z,zz) + Hcrt
!   ??               if (hsub(i,j) < zzz .or.  hsub(ii,jj) < zzz) then
                     if (hsub(i,j) < zzz .and. hsub(ii,jj) < zzz) then
!!!                     maskii(k) = 0   ! already set to 0
                        mbarri(k) = 1
                     else
                        maskii(k) = 1
!!!                     mbarri(k) = 0   ! already set to 0
                     end if
                  end if

 10               continue

                  !---------------------------
                  ! Start 'x' Laplacian filter
                  !---------------------------
                  kk   = mod(k,     4) + 1
                  km1  = mod(k + 2, 4) + 1

                  ia   = i + ip(k)
                  ib   = i + ip(kk)

                  ja   = j + jp(k)
                  jb   = j + jp(kk)

                  ia2  = i + ip2(k)
                  ja2  = j + jp2(k)

                  ia1  = i + ip1(k)
                  ja1  = j + jp1(k)

                  iam1 = i + ip(km1)
                  jam1 = j + jp(km1)

                  !---------------------------------------------
                  ! If any variable from the 'x' filter hits the
                  ! boundary, do not do anything
                  !---------------------------------------------
!!                if (m /= 1) then
!!                   if (ix == 0 .or. jx == 0) then
!!                      maskix(k) = 0
!!                      mbarrx(k) = 0
!!                      mbounx(k) = 1
!!                      go to 20
!!                   end if
!!                end if

                  !-------------------------------------------------
                  ! If the cell from x filter hits the imxb or jmxb
                  ! boundary or the height < critical height (Hcrt),
                  ! set masks values accordingly
                  !-------------------------------------------------
                  if (ix == imxb .or. jx == jmxb .or.
     &               hsub(ix,jx) + zb(ix,jx) < Hcrt) then
!!!                  maskix(k) = 0   ! already set to 0
!!!                  mbarrx(k) = 0   ! already set to 0
                     mbounx(k) = 1
                  else
                     !--------------------------------------------------
                     ! Water on either side must exceed the (lower)
                     ! barrier by at least Hcrt.
                     ! Water should be able to flow from ij to ii
                     ! and from ii to ix, clockwise or counter-clockwise
                     !--------------------------------------------------
                     z   = zbm(ia,ja)

                     zz  = zbm(ib,jb)
                     zzz = min(z,zz) + Hcrt

                     yy  = zbm(ia2,ja2)
                     yyy = min(z,yy) + Hcrt

                     xx  = zbm(ia1,ja1)
                     xxx = min(z,xx) + Hcrt

                     ww  = zbm(iam1,jam1)
                     www = min(z,ww) + Hcrt

                     if (((hsub(i ,j ) >= zzz .and.
     &                     hsub(ii,jj) >= yyy) .or.
     &                    (hsub(ix,jx) >= yyy .and.
     &                     hsub(ii,jj) >= zzz))
     &                    .or.
     &                   ((hsub(i  ,j  ) >= www .and.
     &                     hsub(ia1,ja1) >= xxx) .or.
     &                    (hsub(ix ,jx ) >= xxx .and.
     &                     hsub(ia1,ja1) >= www))    )
     &               then
!!!                     xsm = xsm + hsub(ix,jx)   ! un-used variable
!!!                     xzz = hsub(ix,jx)         ! un-used variable
                        maskix(k) = 1
!!!                     mbarrx(k) = 0             ! already set to 0
                     else
!!!                     xsm = xsm + hsub(i,j)     ! un-used variable
!!!                     xzz = hsub(i,j)           ! un-used variable
!!!                     maskix(k) = 0             ! already set to 0
                        mbarrx(k) = 1
                     end if
                  end if

  20              continue

  30           continue

               !-------------------------------------------------------
               ! Start combination of '+' and 'x' Laplacian filters:
               ! h = h + (alpha * (plusFilter - beta * crossFilter))*dt
               !-------------------------------------------------------
               do 40 km = 1, 4
                  kkm  = mod(km    , 4) + 1
                  km1m = mod(km + 2, 4) + 1

                  ix = i + iix(km)
                  jx = j + jjx(km)
                  ii = i + iih(km)
                  jj = j + jjh(km)

!!                  if (m == 1) go to 158
!!                  if (ii == 0 .or. jj == 0) then
!!                    gamf(k)=0.
!!                    go to 160
!!                  end if
!! 158              continue
!!                  if (ebsn=='$' .or. ebsn=='+') then
!!                     gamfk=elpdl2(ii)+(elpcl2(ii)-elpdl2(ii))*sinl2(jj)
!!                     gamfkx=elpdl2(ix)+(elpcl2(ix)-elpdl2(ix))*sinl2(jx)
!                      gamf(k)=.5*(1.+gamfk/gam0)-1.
!!                     z=gamfk/gam0
!!                     gamf(k)=2.*z/(1.+z)-1.
!!                     zx=gamfkx/gam0
!!                     gamfx(k)=2.*zx/(1.+zx)-1.
!!                  else
!!                     gamf(k)=gamfp(k)
!!                     gamfx(k)=gamfp(k)
!!                  end if

                  ix2 = i + iix(kkm)
                  jx2 = j + jjx(kkm)
                  ii2 = i + iih(kkm)
                  jj2 = j + jjh(kkm)

                  ix3 = i + iix(km1m)
                  jx3 = j + jjx(km1m)
                  ii3 = i + iih(km1m)
                  jj3 = j + jjh(km1m)

                  msumii = maskix(km ) + maskix(kkm)  + maskc
                  msumix = maskix(km ) + maskix(km1m) + maskc

                  !----------------------------------------------
                  ! If any cell of the filter is at the boundary,
                  ! do not do any combination or averaging
                  !----------------------------------------------
                  if (ii  == 0 .or. jj  == 0 .or.
     &                ix  == 0 .or. jx  == 0 .or.
     &                i   == 0 .or. j   == 0 .or.
     &                ix2 == 0 .or. jx2 == 0 .or.
     &                ix3 == 0 .or. jx3 == 0 .or.
     &                ii2 == 0 .or. jj2 == 0 .or.
     &                ii3 == 0 .or. jj3 == 0     ) cycle

                  !-----------------------------------------------------
                  ! If a cell in the '+' the filter is dry (e.g., ii is
                  ! coastline and ij, ix, and ix2 are water cells), then
                  ! average the two closest cells (ix and ix2) and
                  ! the center cell ij, multiplying by their masks and
                  ! weighting them with the addition of all masks,
                  ! so that if any of the 3 are dry, they do not get
                  ! averaged.
                  !
                  !    |---------|--------|---------|
                  !    |         |        |         |
                  !    |---------|--------|---------|
                  !    |         |   ij   |         |
                  !    |---------|--------|---------|
                  !    |    ix   |   ii   |  ix2    |
                  !    |    jx   |   jj   |  jx2    |
                  !    |---------|--------|---------|
                  !
                  !-----------------------------------------------------

!!                  if (maskii(km) == 0 .and. msumii     /= 0 .and.
!!     &                mbarri(km) == 0 .and. mbounx(km) == 0 .and.
!!     &                mbouni(km) == 0) then
!!               print *, "trouble: hsub is supposed to be read-only here"
!!               print *, "because it is needed for calc in other cells."
!!                     hsub(ii,jj) = (maskix(km )*hsub(ix  ,jx) +
!!     &                              maskix(kkm)*hsub(ix2,jx2) +
!!     &                              maskc      *hsub(i  ,j) ) /
!!     &                             (maskix(km ) + maskix(kkm) + maskc)
!!                  end if

                  !-----------------------------------------------------
                  ! If a cell in the 'x' the filter is dry (e.g., ix is
                  ! coastline and ij, ii, and ii3 are water cells), then
                  ! average the two closest cells (ii3 and ii) and
                  ! the center cell ij, multiplying by their masks and
                  ! weighting them with the addition of all masks,
                  ! so that if any of the 3 are dry, they do not get
                  ! averaged.
                  !
                  !    |---------|--------|---------|
                  !    |         |        |         |
                  !    |         |        |         |
                  !    |---------|--------|---------|
                  !    |   ii3   |    i   |         |
                  !    |   jj3   |    j   |         |
                  !    |---------|--------|---------|
                  !    |    ix   |   ii   |         |
                  !    |    jx   |   jj   |         |
                  !    |---------|--------|---------|
                  !
                  !-----------------------------------------------------

!!                  if (maskix(km) == 0 .and. msumix     /= 0 .and.
!!     &                mbarrx(km) == 0 .and. mbounx(km) == 0 .and.
!!     &                mbouni(km) == 0. )
!!     &            then
!!                     print *, "trouble: hsub is supposed to be read only here"
!!                     print *, "because it is needed for calc in other cells."
!!                     hsub(ix,jx) = (maskii(km  )*hsub(ii ,jj ) +
!!     &                              maskix(km1m)*hsub(ii3,jj3) +
!!     &                              maskc       *hsub(i  ,j  ) ) /
!!     &                             (maskix(km ) + maskix(km1m) + maskc)
!!                  end if

                  !------------------------------------------------------
                  ! Calculate the two terms of the combination '+' and 'x'
                  ! filter:
                  !    hsm2 = + filter: sum (ii,jj) from k = 1, 4
                  !    xsm2 = x filter: sum (ix,jx) from k = 1, 4
                  !
                  !    |---------|---------|---------|
                  !    |    ix   |   ii    |    ix   |
                  !    |    jx   |   jj    |    jx   |
                  !    |  k = 4  | k = 3   |  k = 3  |
                  !    |---------|---------|---------|
                  !    |    ii   |    i    |    ii   |
                  !    |    jj   |    j    |    jj   |
                  !    |  k = 4  |         |  k = 2  |
                  !    |---------|---------|---------|
                  !    |    ix   |   ii    |    ix   |
                  !    |    jx   |   jj    |    jx   |
                  !    |  k = 1  | k = 1   |  k = 2  |
                  !    |---------|---------|---------|
                  !
                  !------------------------------------------------------
                  ! ??? maskc is always 1.

                  hsm2 = hsm2 +
     &                   maskc * maskii(km) * hsub(ii,jj) -
     &                   maskc * maskii(km) * hsub( i, j)

!-----------------------------------------------------------------------
! A   B   C    Capital letters are interior, lower are exterior.
! a   D   e    Killsworth (91) recommended that:
! f   g   h    (1) If a cell 'a' is exterior for adjacent calculations to
!                  cell 'D' it be replaced with the average of any
!                  interior cells that it 'a' was adjacent to (ie 'D' and
!                  diagonals to 'D' that touch 'a' (i.e. 'A'))
!              (2) If a cell 'f' is exterior and diagonal to 'D' it is
!                  ignored.
!
! A   B   C    Capital letters are interior, lower are exterior.
! a   D   e    adj= ALFA * (HSM2 - BETA*(XSM2))  (assume Beta = 1)
! f   g   h    adj=ALFA ((B-D)+(a-D)+(g-D)+(e-D)
!                        -1/2[(f-D)+(h-D)+(C-D)+(A-D)]
!              Now (f-D), and (h-D) can be ignored since they are diagonals
!              (g-D) can be replaced with (D-D) = 0
!              => adj=ALFA ((B-D)+ (a-D) + (e-D) -1/2 [(C-D) + (A-D)]
!              (a-D) becomes ((A + D)/2 - D) = (A - D)/2
!              (e-D) becomes ((C + D)/2 - D) = (C - D)/2
!              => adj=ALFA ((B-D)+ (A-D)/2 + (C-D)/2 - (C-D)/2 - (A-D)/2
!              => adj=ALFA (B-D)
!
!              In looking at this, the A and C terms are dropped from the
!              XSM component.  However if 'e' had been 'E', we would have
!              kept the C term in the XSM.  So a diagonal cell is included
!              in XSM if it is interior (connected to point in question)
!              and the adjacent cells to it are interior.
!
!              One permutation that Killsworth didn't prepare for is:
! A   B   C    Capital letters are interior, lower are exterior.
! E   D   F    adj= ALFA * (HSM2 - BETA*(XSM2))  (assume Beta = 1)
! G   a   H    adj=ALFA ((B-D)+(E-D)+(F-D)+(a-D)
!                        -1/2[(A-D)+(C-D)+(G-D)+(H-D)]
!              (a-D) becomes ((G + D + H)/3 - D) = (G - D)/3 + (H - D)/3
!              => adj=ALFA ((B-D)+(E-D)+(F-D)+(G-D)/3+(H-D)/3
!                        -1/2[(A-D)+(C-D)+(G-D)+(H-D)]
!              In this case 0  /=  (G-D)/3 -(G-D)/2 +(H-D)/3 -(H-D)/2
!              I would argue that they should cancel, so XSM doesn't need
!              to take this into consideration.
!-----------------------------------------------------------------------
               if (version == 1) then
                  !------------------------------------------
                  ! Use original bad formulation for backward
                  ! compatibility with hch2 basin.
                  !------------------------------------------
                  xsm2 = xsm2 + .5 *
     &                  ((maskix(km) * maskii(km) * maskc * maskix(kkm))
     &                 * hsub(ix,jx) -
     &                   (maskix(km) * maskii(km) * maskc * maskix(kkm))
     &                 * hsub( i, j))
               else
                  !------------------------------
                  ! Use current, correct version.
                  !------------------------------
                  xsm2 = xsm2 + .5 *
     &                   maskc * maskix(km)*maskii(km)*maskii(km1m)*
     &                   (hsub(ix,jx) - hsub(i, j))
               end if

 40            continue

               !--------------------------------------------------------
               ! AT: Original plus filter was:
               !   hTemp =.5*hsub(i,j) + (hsm+hdif)/8.
               !         = hsub(i,j) + 1/8 *(hsm+hdif - 4*hsub(i,j))
               !         = hsub(i,j) + alfa*(hsm+hdif - 4*hsub(i,j))
               !
               ! Introducing cross Lapacian resulted in:
               !   hTemp = hsub(i,j) + alfa*((hsm+hdif - 4*hsub(i,j)) -
               !                       1/2 *( xsm+xdif - 4*hsub(i,j)))
               !
               ! Now hsm2 and xsm2 are calculated in such a way that
               !    hsm2 is the sum of hsm - 4*hsub
               !    xsm2 is the sum of .5 * (xsm - 4*hsub)
               ! So...

               !---------------------------------------------------------
               ! Calculate the combination '+' and 'x' Laplacian filter:
               ! h = h + (alpha * (plusFilter - beta * crossFilter)) * dt
               !---------------------------------------------------------
               hTemp = hsub(i,j) + alfa * (hsm2 - beta*xsm2)

               !----------------------------------------
               ! Assign the value to the height variable
               !----------------------------------------
               hb(i,j) = max(-zb(i,j), hTemp)

  80        continue
  90     continue
 100  continue

      !---------------------------
      ! Check on mass conservation
      !---------------------------
      if (checkMass) then
         massAft = 0.
         do j = 1, jmxb1
            do i = 1, imxb1
               massAft = massAft + hb(i,j)
            end do
         end do
         massDif = massBef - massAft
         massMax = max(massDif, massMax)
         massMin = min(massDif, massMin)
         massSum = massSum + abs(massDif)
         massCnt = massCnt + 1
         write (*,*) "mass?", massCnt, massMax, massMin, massSum,
     &                massBef, massAft, massDif, imxb1 * jmxb1
      end if

      return
      end subroutine xFilter_new
