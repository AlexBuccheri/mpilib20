!> Utility routines for internal types.
!> Takes an MPI type and returns its equivalent MPILib20 type, used internally.
!>
!> The return type is always consistent between all routines in an overload set,
!> and should be consistent with the types used by mpi_f08,
!> but preprocessing is required to correctly set the return value. 
!
module internal_utils
    use mpi_bindings, only: MPI_Comm, MPI_COMM_DUP
    implicit none
    private 

    ! TODO Consider renaming convert -> set
    interface convert  
        module procedure :: comm_convert_inputf08, comm_convert_inputf90
    end interface

    interface duplicate  
        module procedure :: comm_dup_inputf08, comm_dup_inputf90
    end interface

    public :: convert, duplicate 

contains

    ! TODO ALEX Could probably remove need for this routines if I had setters and getters
    ! for mpi_env_type? No, issue is returning data directly into a routine i.e.
    ! call MPI_COMM_DUP(input_comm%MPI_VAL, comm%set_comm(new_comm%MPI_VAL), ierror)


    !> Given an integer communicator, return a communicator of 
    !> type(MPI_Comm), which is used internally by MPILib20
    !> This would equivalent to `set` if it were a routine 
    !> and new_comm had intent(out)
    function comm_convert_inputf90(input_comm) result(new_comm)
        !> input communicator 
        integer, intent(in) :: input_comm
        !> return communicator      
        type(MPI_Comm) :: new_comm
        new_comm%MPI_VAL = input_comm
    end function

    !> Given a type(MPI_Comm) communicator, return a communicator 
    !> of type(MPI_Comm), which is used internally by MPILib20
    !> This would equivalent to `set` if it were a routine 
    !> and new_comm had intent(out)
    function comm_convert_inputf08(input_comm) result(new_comm)
        !> input communicator 
        type(MPI_Comm), intent(in) :: input_comm
        !> return communicator 
        type(MPI_Comm) :: new_comm
        new_comm = input_comm
    end function


    !> Given an integer communicator, duplicat it and return 
    !> a communicator of type(MPI_Comm), which is used internally 
    !> by MPILib20
    function comm_dup_inputf90(input_comm) result(new_comm)
        !> input communicator 
        integer, intent(in) :: input_comm
        !> return communicator      
        type(MPI_Comm) :: new_comm
        integer :: ierror
#ifdef MPI08
        call MPI_COMM_DUP(comm_convert_inputf90(input_comm), new_comm, ierror)
#else 
        call MPI_COMM_DUP(input_comm, new_comm%MPI_VAL , ierror)
#endif 
    end function


    !> Given a type(MPI_Comm) communicator, duplicate it and 
    !> return a communicator of type(MPI_Comm), which is used 
    !> internally by MPILib20
    function comm_dup_inputf08(input_comm) result(new_comm)
        !> input communicator 
        type(MPI_Comm), intent(in) :: input_comm
        !> return communicator 
        type(MPI_Comm) :: new_comm
        integer :: ierror
#ifdef MPI08 
       call MPI_COMM_DUP(input_comm, new_comm, ierror)
#else 
       call MPI_COMM_DUP(nput_comm%MPI_VAL, new_comm%MPI_VAL, ierror)
#endif 
    end function


end module