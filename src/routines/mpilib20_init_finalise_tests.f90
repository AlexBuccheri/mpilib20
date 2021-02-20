!> Test initialisation and finalisation of the MPI environment
!  
!> Only test via the public OO API as these methods wrap the free functions
!
! When writing MPI tests with Zofu, note that:
! "the test module should still use zofu, and the test case subroutine interface 
!  is the same as for serial unit tests."

module mpilib20_init_finalise_tests
    !> Test framework
    use zofu,        only: unit_test_type
    !> Module being tested
    use mpilib20_init_finalise,  only: mpi_env_type

    implicit none
    private
    public :: test_init_finalize

contains

    subroutine test_init_finalize(test)
        use mpi_bindings, only: MPI_COMM_WORLD

        !> Test instance
        class(unit_test_type), intent(inout) :: test
        !> MPI environment 
        type(mpi_env_type) :: mpi_env

        call mpi_env%init() 
        test%assert(mpi_env%comm() /= MPI_COMM_WORLD, &
            "The library's world communicator should differ from MPI_COMM_WORLD due to duplication")
        ! Add more tests
        call mpilib20_finalize(mpi_env)

    end subroutine

    subroutine test_init_thread_finalize(test)
        use mpi_bindings, only: MPI_COMM_WORLD, MPI_THREAD_SINGLE

        !> Test instance
        class(unit_test_type), intent(inout) :: test
        !> MPI environment 
        type(mpi_env_type) :: mpi_env

        call mpi_env%init(MPI_THREAD_SINGLE) 
        test%assert(mpi_env%comm() /= MPI_COMM_WORLD, 
            "The library's world communicator should differ from MPI_COMM_WORLD due to duplication")
        ! Add more tests
        call mpilib20_finalize(mpi_env)

    end subroutine

end module