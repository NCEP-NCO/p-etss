c defgridstations.f
c Author: Ryan Schuster (ryan.schuster@noaa.gov)
c History: Program created in 2014
c Abstract: 
c Allows allocatable arrays like GRID and STNS to be passed
c between subroutines (FORTRAN won't allow that behavior unless
c the allocatable arrays are defined in a module which is then
c included in each subroutine that passes and/or calls the arrays)
c
c No in or output parameters. It's just a module.

      MODULE DEFGRIDSTNS
        INTEGER, ALLOCATABLE :: GRID(:,:)
        CHARACTER(5), ALLOCATABLE :: STNS(:)
      END MODULE DEFGRIDSTNS
