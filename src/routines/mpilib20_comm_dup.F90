module mod_mpilib20_comm_dup
    use mpi_bindings, only: MPI_Comm, mpi_comm_dup 
    use utils, only: get_comm
    implicit none
    private 

    interface mpilib20_comm_dup
        module procedure mpilib20_comm_dup_inputf90, mpilib20_comm_dup_inputf08
    end interface

contains
    
    ! Same implementation, except the input type differs
    ! Always returns type(MPI_Comm)

! NOTE, this is confusing because the behaviour is the opposite of the get_comm function
! Should think hard about the namings and conventions used

    function mpilib20_comm_dup_inputf90(input_comm) result(new_comm)
        integer, intent(in) :: input_comm
        type(MPI_Comm) :: new_comm
        integer :: ierror
#ifdef MPI08
        call MPI_COMM_DUP(get_comm(input_comm), new_comm, ierror)
#else 
        integer :: intermediate_comm
        call MPI_COMM_DUP(get_comm(input_comm), intermediate_comm, ierror)
        new_comm%MPI_VAL = intermediate_comm 
#endif 
    end function


    function mpilib20_comm_dup_inputf08(input_comm) result(new_comm)
        type(MPI_Comm), intent(in) :: input_comm
        type(MPI_Comm) :: new_comm
        integer :: ierror
#ifdef MPI08 
       ! Could use get_comm(input_comm) here, but no need
       call MPI_COMM_DUP(input_comm, new_comm, ierror)
#else 
       integer :: intermediate_comm
       call MPI_COMM_DUP(get_comm(input_comm), intermediate_comm, ierror)
       new_comm%MPI_VAL = intermediate_comm 
#endif 
    end function


end module
