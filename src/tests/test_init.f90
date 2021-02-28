
!> Test mpi_env%init 
!> MPI_INIT cannot be called after MPI_FINALIZE, hence 
!> test mpilib20_init and mpilib20_init_thread separately. 
module test_mpilib20_init
    use zofu, only: unit_test_type                     !! Test framework
    use mpilib20_init_finalise, only: mpi_env_type     !! Module being tested 

    implicit none
    private 
    public :: setup, teardown, test_init

    contains 

      !> Test Setup: Initialise MPI communicator
      !>  
      !> Cannot use MPILib20's init and finalize routines as 
      !> Zofu's setup doesn't take any arguments.
      !>
      !> It also uses the `mpi` bindings, rather than `mpi_f08`
      !> hence return MPI_COMM_WORLD via the use statement.
      subroutine setup
        use mpi, only: MPI_COMM_WORLD, MPI_INIT
        integer :: ierror
        call MPI_INIT(ierror)
      end subroutine

      !> Test Teardown: Finalise MPI 
      subroutine teardown
        use mpi, only: MPI_FINALIZE
        integer :: ierror
        call MPI_FINALIZE(ierror)
      end subroutine  

      subroutine test_init(test)
         !! Test initialisatin of the MPI environment object, following initialisation of MPI_COMM_WORLD bu MPI_INIT()
         use mpi, only: MPI_COMM_WORLD 
         class(unit_test_type), intent(inout) :: test  !! Test instance
         type(mpi_env_type) :: mpi_env                 !! mpi environment
    
         call mpi_env%init(bare_communicator = MPI_COMM_WORLD)
         call test%assert(mpi_env%get_comm() /= MPI_COMM_WORLD, &
              name = "MPILib20's global communicator should differ from MPI_COMM_WORLD due to duplication")
         call test%assert(mpi_env%n_processes == mpi_env%group_size, &
              name = "N processes (ranks) equals the group size for comm world.")
        
      end subroutine
      
end module  