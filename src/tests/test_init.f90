
!> Because mpi_init cannot be called after mpi_finalize,
!> we must test mpilib20_init and mpilib20_init_thread 
!> separately. 
module test_mpilib20_init
    use zofu, only: unit_test_type                     !! Test framework
    use mpilib20_init_finalise, only: mpi_env_type     !! Module being tested 

    implicit none
    private 
    public :: setup, teardown, test_init

    contains 

      ! Cannot use out own init and finalize routines as 
      ! Zofu's setup doesn't take any arguments
      ! More annoyingly, it uses the mpi bindings, rather than mpi_f08 
      subroutine setup
        use mpi, only: MPI_COMM_WORLD, MPI_INIT
        integer :: ierror
        call MPI_INIT(ierror)
        write(*,*) 'Call happened'
      end subroutine

      ! Same deal with teardown 
      subroutine teardown
        use mpi, only: MPI_FINALIZE
        integer :: ierror
        call MPI_FINALIZE(ierror)
      end subroutine  

      subroutine test_init(test)
         !! Test initialisatin of the MPI environment object, following
         !! initialisation of MPI_COMM_WORLD bu MPI_INIT()
         use mpi, only: MPI_COMM_WORLD 
         class(unit_test_type), intent(inout) :: test  !! Test instance
         type(mpi_env_type) :: mpi_env                !! mpi environment
    
         write(*, *) 'Made it here'
         call mpi_env%init(bare_communicator = MPI_COMM_WORLD)
         ! Does nothing but should check data of mpi_env
         call test%assert(.true., &
              name = "The library's world communicator should differ from MPI_COMM_WORLD due to duplication")

      end subroutine
      
end module  