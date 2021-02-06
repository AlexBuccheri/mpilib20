!> \mainpage Fortran 2003/2008 wrappers for MPI and MPI2 
!>
!> All library routines should use the original MPI
!> subroutine names, with the mpi prefix replaced with mpilib20 

!> Exposure library types and subroutines 
module mpilib20
  use mpilib20_init_finalise, only: mpi_env_type, mpilib20_init, mpilib20_init_thread, mpilib20_finalize

  ! TODO Add preprocessing for serial overloads here
end module mpilib20
