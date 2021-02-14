# Generic Subroutine Templates
Store templates for the most generic form of a subroutine to be overloaded. Allowing rapid replication.

## MPI_REDUCE
```fortran
!> MPI_REDUCE WRAPPER: INTEGER
    subroutine mpilib20_reduce_type(sendbuf, recvbuf, op, root, mpi_env)
        use mpi_bindings, only : MPI_REDUCE, MPI_Op, &
            MPI_TYPE
        !> Variable containing set to be sent
        type(MPI_TYPE),          intent(in)      :: sendbuf
        !> Variable to receive reduced set
        type(MPI_TYPE),          intent(inout)   :: recvbuf
        !> MPI Operation
        type(MPI_Op),       intent(in)      :: op
        !> Rank of the process to receive the reduced set
        integer,            intent(in)      :: root
        !> Instance of the MPI environment
        type(mpi_env_type), intent(inout)   :: mpi_env

        call MPI_REDUCE(sendbuf, recvbuf, 1, MPI_TYPE, op, root, mpi_env%comm, mpi_env%ierror)
    end subroutine mpilib20_reduce_type
````