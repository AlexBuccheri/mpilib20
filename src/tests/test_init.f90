!> Driver program to run test routines defined about 
!
!> Because mpi_init cannot be called after mpi_finalize,
!> we must test mpilib20_init and mpilib20_init_thread 
!> separately. 
program test_mpilib20_init
    !> Test framework
    use zofu, only: unit_test_type
    use zofu_mpi, only: unit_test_mpi_type
    !> Module being tested
    use mpilib20_init_finalise, only: mpi_env_type 
    use mpi, only: mpi_comm_rank, MPI_COMM_WORLD
    implicit none
  
    ! Declarations 
    type(unit_test_type) :: test  
    type(mpi_env_type) :: mpi_env

    integer :: rank, ierr 

    ! Zofu's `unit_test_mpi_type` init method contains
    ! MPI calls, hence must initialise MPI first
    call mpi_env%init() 
    ! This works
    call mpi_comm_rank(mpi_env%comm%MPI_VAL, rank, ierr)
    ! This works 
    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

    ! THIS CAUSES THE CODE TO CRASH if unit_test_mpi_type is used
    call test%init()
    call test%run(test_init, 'Test initialisation and finalisation of the MPI env')
    
    call test%summary()
    if (test%failed) stop 1
    call mpi_env%finalize()

    contains
    
      subroutine test_init(test)
         class(unit_test_type), intent(inout) :: test  !! Test instance
    
          ! Does nothing
          call test%assert(.true., &
              name = "The library's world communicator should differ from MPI_COMM_WORLD due to duplication")

      end subroutine
      
end program 