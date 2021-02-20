!> Test initialisation and finalisation of the MPI environment
!  
! When writing MPI tests with Zofu, note that:
! "the test module should still use zofu, and the test case subroutine interface 
!  is the same as for serial unit tests."

module mpilib20_init_finalise_tests
    ! Test framework
    use zofu,        only: unit_test_type
    ! Module being tested
    use mpilib20_init_finalise,  only: mpi_env_type,  &
                                       mpilib20_init, &
                                       mpilib20_init_thread, &
                                       mpilib20_finalize

    implicit none
    private

contains

    ! ------------------------
    ! Test free routine API
    ! ------------------------

    ! NOTE, no idea how this is going to work if
    ! the test framework has already spun up the MPI environment.... 
    ! Will have to do some simple tests before proceeding 

    subroutine, public :: test_mpilib20_init_finalize(test)
        ! Test initialisation and finalisation of the MPI env with free functions
        class(unit_test_type), intent(inout) :: test
        !> MPI environment 
        type(mpi_env_type) :: mpi_env

        call mpilib20_init(mpi_env)
        test%assert(mpi_env%get_comm() /= 0, "comm should be some non-zero number")


        call mpilib20_finalize(mpi_env)

    end subroutine

    ! ------------------------
    ! Test OO API - maybe a separate test module? 
    ! ------------------------


end module