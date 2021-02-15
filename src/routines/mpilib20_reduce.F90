!> Wrapping of the MPI_REDUCE methods
!> TODOs
!> Unravel how MPI_Datatype operates
!> Overload for MPI_IN_PLACE?
!> Testing
module mpilib20_reduce_m

    use mpilib20_init_finalise, only : mpi_env_type

    implicit none

    private

    interface mpilib20_reduce
        module procedure mpilib20_reduce_int_scalar
        module procedure mpilib20_reduce_int_vec
    end interface
    
    public :: mpilib20_REDUCE
    
contains

    !> MPI_REDUCE WRAPPER: INTEGER
    subroutine mpilib20_reduce_int_scalar(sendbuf, recvbuf, operation, mpi_env, root)
        use mpi_bindings, only : MPI_REDUCE, MPI_Op, &
            MPI_Datatype, MPI_INTEGER
        use mpilib20_init_finalise, only : root_id
        !> Variable containing set to be sent
        type(MPI_Datatype),          intent(in)      :: sendbuf
        !> Variable to receive reduced set
        type(MPI_Datatype),          intent(inout)   :: recvbuf
        !> MPI Operation
        type(MPI_Op),                intent(in)      :: operation
        !> Instance of the MPI environment
        type(mpi_env_type),          intent(inout)   :: mpi_env
        !> Rank of the process to receive the reduced set (override default)
        integer, optional,           intent(in)      :: root
        integer                                      :: use_root

        !> Overide root process if passed
        if (present(root)) then
            use_root = root
        else
            use_root = root_id
        end if

        call MPI_REDUCE(sendbuf, recvbuf, 1, MPI_INTEGER, operation, use_root, mpi_env%comm, mpi_env%ierror)

    end subroutine mpilib20_reduce_int_scalar

    !> MPI_REDUCE WRAPPER: INTEGER(:)
    !> Rank 1 arrays don't require flattening
    subroutine mpilib20_reduce_int_vec(sendbuf, recvbuf, operation, mpi_env, root)
        use mpi_bindings, only : MPI_REDUCE, MPI_Op, &
            MPI_Datatype, MPI_INTEGER
        use mpilib20_init_finalise, only : root_id
        !> Variable containing set to be sent
        type(MPI_Datatype),         intent(in)      :: sendbuf(:)
        !> Variable to receive reduced set
        type(MPI_Datatype),         intent(inout)   :: recvbuf(:)
        !> MPI Operation
        type(MPI_Op),               intent(in)      :: operation
        !> Instance of the MPI environment
        type(mpi_env_type),         intent(inout)   :: mpi_env
        !> Rank of the process to receive the reduced set (override default)
        integer, optional,          intent(in)      :: root
        integer                                     :: count
        integer                                     :: use_root

        !> Overide root process if passed
        if (present(root)) then
            use_root = root
        else
            use_root = root_id
        end if
        !> Element count based-on receiving buffer size
        count = size(recvbuf)

        call MPI_REDUCE(sendbuf, recvbuf, count, MPI_INTEGER, operation, root, mpi_env%comm, mpi_env%ierror)

    end subroutine mpilib20_reduce_int_vec

end module mpilib20_reduce_m
