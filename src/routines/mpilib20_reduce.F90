!> Wrapping of the MPI_REDUCE methods
!> TODOs
!> Up to rank 3 arrays
!> Remaining types (parametized derived types for precisions?)
!>  real(sp), real(dp), real(qp)
!>  complex
!> Overload for MPI_IN_PLACE - optional logical could work
!> Testing
!> Refactor when root_id moved to mpi_env_type
module mpilib20_reduce_m

    use mpilib20_init_finalise, only : mpi_env_type, root_id
    use mpi_bindings, only : MPI_REDUCE, MPI_Op, &
        MPI_INTEGER

    implicit none

    private

    interface mpilib20_reduce
        module procedure mpilib20_reduce_int_scalar
        module procedure mpilib20_reduce_int_vec
    end interface
    
    public :: mpilib20_reduce
    
contains

    
    !> Reduces values on all processes to a single value
    !> on process defined by process_id
    !> TODO usage examples
    subroutine mpilib20_reduce_int_scalar(sendbuf, recvbuf, &
                                          operation, mpi_env, &
                                          process_id)
        !> Variable containing set to be sent
        type(MPI_INTEGER),          intent(in)      :: sendbuf
        !> Variable to receive reduced set
        type(MPI_INTEGER),          intent(out)     :: recvbuf
        !> MPI Operation
        type(MPI_Op),               intent(in)      :: operation
        !> Instance of the MPI environment
        type(mpi_env_type),         intent(out)     :: mpi_env
        !> Rank of the process to receive the reduced set (override default)
        integer, optional,          intent(in)      :: process_id
        integer                                     :: process

        !> Overide root process if passed
        if (present(process_id)) then
            process = process_id
        else
            process = root_id
        end if

        call MPI_REDUCE(sendbuf, recvbuf, 1, MPI_INTEGER, operation, process, &
            mpi_env%comm, mpi_env%ierror)

    end subroutine mpilib20_reduce_int_scalar


    !> Reduces values on all processes to a single value
    !> on process defined by process_id
    !> TODO usage examples
    subroutine mpilib20_reduce_int_vec(sendbuf, recvbuf, &
                                       operation, mpi_env, & 
                                       process_id)
        !> Variable containing set to be sent
        type(MPI_INTEGER),         intent(in)      :: sendbuf(:)
        !> Variable to receive reduced set
        type(MPI_INTEGER),         intent(inout)   :: recvbuf(:)
        !> MPI Operation
        type(MPI_Op),               intent(in)      :: operation
        !> Instance of the MPI environment
        type(mpi_env_type),         intent(inout)   :: mpi_env
        !> Rank of the process to receive the reduced set (override default)
        integer, optional,          intent(in)      :: process_id
        integer                                     :: process

        !> Overide root process if passed
        if (present(process_id)) then
            process = process_id
        else
            process = root_id
        end if

        call assert(size(sendbuf) == size(recvbuf), "size(sendbuf) /= size(recvbuf)")

        call MPI_REDUCE(sendbuf, recvbuf, size(recvbuf), MPI_INTEGER, operation, process, &
        mpi_env%comm, mpi_env%ierror)

    end subroutine mpilib20_reduce_int_vec


end module mpilib20_reduce_m
