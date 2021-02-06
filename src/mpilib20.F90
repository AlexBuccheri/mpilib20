!> \mainpage Fortran 2003/2008 wrappers for MPI and MPI2 
!>
!> All library routines should use the original MPI
!> subroutine names, with the mpi prefix replaced with mpilib20 
!
! TODO(Alex) Think about how to correctly handle errors

!> Expose library routines to callers via one module 
module mpilib20
  use mpilib20_init_finalise, only: mpi_env_type, init_mpi_env, finalise_mpi_env


  

  
end module mpilib20
