      subroutine map_proj(MM,numm)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    PURPOSE:
!c       READ MERGING MASK FILE FOR CONUS AND ALASKA GRIDS
!c       PROJECTION THE CONUS/ALASKA NDFD GRID NUMBER TO MODEL BASIN GRID NUMBER
!c
!c    ARGUMENTS:
!c       READ DATA FROM SSGRID.CCB(00a for example)
!c       GENERATE MAXIMUM,MINNIMUM AND MEAN OF CONTROL AND ENSEMBLE RUNS
!c
!c    INPUT FILES:
!c       FORT.16  - MAP PROJECTION GRIDS alaska.bin (3km)
!c       FORT.49  - MAP PROJECTION GRIDS conus.bin (2.5km/625m)
!c
!c
!c    OUTPUTFILES:
!c
!c
!c    VARIABLES:
!c      INPUT
!c       MM    == BASIN NUMBERS
!c       numm  == NUMBER of MASK FILE (1 - 3km alaska; 2 - 2.5km conus and 
!c                                     3 - 625m conus)
!c     OUTPUT
!c       Mask  == mask variable (0 - no value; 1 - get value from basin grid)
!c       Ival  == I-index of merging basins grid
!c       Jval  == J-index of merging basins grid
!c
!c    AUTHORS:
!c       Modelers /MDL, Arthur, Taylor, Huiqing Liu /MDL
!c           
!c    HISTORY:
!c       03/1995--Modelers /MDL Created the routine
!c       10/2014--Huiqing Liu /MDL Updated the routine to read 2.5/3km mask 
!c                                 files
!c       01/2016--Huiqing Liu /MDL Updated the routine to read different res
!c                                 mask files
!c       01/2017--Huiqing Liu /MDL Put the routine to a independent fortran file
!c       02/2017--Huiqing Liu /MDL Added header block
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use Mask_Array

      implicit none

      character(len=7)    :: area
      character(len=5)    :: basin
      integer             :: nbasin,mm,k,l,numm,i,j,ii,m
      integer,allocatable :: i_lcal(:,:), j_lcal(:,:)
      integer,allocatable :: perc1(:),mask1(:),ival1(:,:),jval1(:,:)
      integer,allocatable :: i_lcal1(:), j_lcal1(:)

      if (numm == 1) then
         read(16) area
         write(*,*)'Reading Merging Mask File for ',area
         read(16) nx,ny

         allocate (Mask(Nx,Ny))
         allocate (Ival(Mm,Nx,Ny))
         allocate (Jval(Mm,Nx,Ny))
         allocate (i_lcal(NX,NY))
         allocate (j_lcal(NX,NY))

         read(16) ((mask(i,j),i=1,nx),j=1,ny)
         read(16) nbasin
         if(nbasin .ne. mm) write(*,*) 'WRONG NUMBER OF BASINS !'
         do m=1,mm
            read(16) basin
            read(16) ((i_lcal(i,j),i=1,nx),j=1,ny)
            read(16) ((j_lcal(i,j),i=1,nx),j=1,ny)
            ival(m,:,:)=i_lcal
            jval(m,:,:)=j_lcal
         enddo
         write(*,*)'Finished Reading Merging Mask file from Extra-Trop'
         deallocate (i_lcal)
         deallocate (j_lcal)

      else if (numm == 2) then
         read(49) area
         write(*,*)'Reading Merging Mask File for ',area
         read(49) nx,ny

         allocate (Mask(Nx,Ny))
         allocate (Perc(Nx,Ny))
         allocate (Ival(Mm,Nx,Ny))
         allocate (Jval(Mm,Nx,Ny))
         allocate (i_lcal(Nx,Ny))
         allocate (j_lcal(Nx,Ny))

         read(49) ((mask(i,j),i=1,nx),j=1,ny)
         read(49) ((Perc(I,J),i=1,nx),j=1,ny)
         read(49) nbasin
         if(nbasin .ne. mm) write(*,*) 'WRONG NUMBER OF BASINS !'
         do m=1,mm
            read(49) basin
            read(49) ((i_lcal(i,j),i=1,nx),j=1,ny)
            read(49) ((j_lcal(i,j),i=1,nx),j=1,ny)
            ival(m,:,:)=i_lcal
            jval(m,:,:)=j_lcal
         enddo
         write(*,*)'Finished Reading Merging Mask file'
         deallocate (i_lcal)
         deallocate (j_lcal)

      else
         read(49) area
         WRITE(*,*)'Reading Merging Mask File for ',AREA
         read(49) nx,ny
         read(49) Nrec1

         allocate (PID(Nrec1))
         allocate (perc1(Nrec1))
         allocate (mask1(Nrec1))
         allocate (ival1(MM,Nrec1))
         allocate (jval1(MM,Nrec1))
         allocate (i_lcal1(Nrec1))
         allocate (j_lcal1(Nrec1))

         allocate (MASK(NX,NY))
         allocate (Perc(NX,NY))
         allocate (IVAL(MM,NX,NY))
         allocate (JVAL(MM,NX,NY))

         pid=0
         perc1=0
         mask1=0
         ival1=0
         jval1=0
         mask=0
         perc=0
         ival=0
         jval=0

         read(49) (pid(i),i=1,Nrec1)
         read(49) (mask1(i),i=1,Nrec1)
         read(49) (Perc1(i),i=1,Nrec1)

         read(49) nbasin
         if(nbasin .ne. mm) write(*,*) 'WRONG NUMBER OF BASINS !'
         do m=1,mm
            read(49) basin
            read(49) (i_lcal1(i),i=1,Nrec1)
            read(49) (j_lcal1(i),i=1,Nrec1)
            ival1(M,:)=i_lcal1
            jval1(M,:)=j_lcal1
         enddo
         do ii=1,Nrec1
            i=pid(ii)/10000
            j=pid(ii)-i*10000
            mask(i,j)=mask1(ii)
            Perc(i,j)=Perc1(ii)
            do m=1,mm
               ival(m,i,j)=ival1(m,ii)
               jval(m,i,j)=jval1(m,ii)
            enddo
         enddo

         write(*,*)'Finished Reading Merging Mask file'
         deallocate (perc1)
         deallocate (mask1)
         deallocate (i_lcal1)
         deallocate (j_lcal1)

      endif

      return
      end





