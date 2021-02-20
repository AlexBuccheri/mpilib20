!> Driver program to test mpilib20_init_finalise.f90
!
!> Because mpi_init cannot be called after mpi_finalize,
!> we must test mpilib20_init and mpilib20_init_thread 
!> separately. 
program test_mpilib20_init
    use zofu_mpi
    use mpilib20_init_finalise_tests, only:  test_init_thread_finalize
    implicit none
  
    !> Test instance 
    type(unit_test_mpi_type) :: test
  
    call test%init()
    call test%run(test_init_thread_finalize, &
        'Test initialisation (with threads) and finalisation of the MPI env')
    call test%summary()
    if (test%failed) stop 1
  
  end program test_mpilib20_init