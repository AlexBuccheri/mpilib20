module utils
    use mpi_bindings, only: MPI_Comm
    implicit none
    private

    !> Given a communicator, return the same communicator with 
    !> its type consistent with that required by MPI's API
    interface get_comm
        module procedure get_comm_inputf90, get_comm_inputf08
    end interface

    public :: get_comm

contains
 
    !> Given an integer input communicator (mpi f90 bindings) 
    !> return the communicator of internal type used by this library
    function get_comm_inputf90(input_comm) result(new_comm)
        integer, intent(in) :: input_comm
#ifdef MPI08
        ! integer -> MPI_Comm 
        type(MPI_Comm) :: new_comm
        new_comm%MPI_VAL = old_comm
#else 
        ! integer to integer (do nothing)
        integer :: new_comm
        new_comm = old_comm   
#endif 
    end function 


    !> Given an MPI_Comm input communicator (mpi f08 bindings) 
    !> return the communicator of internal type used by this library
    function get_comm_inputf08(input_comm) result(new_comm)
        type(MPI_Comm), intent(in) :: input_comm
#ifdef MPI08
        ! MPI_Comm -> MPI_Comm (do nothing)
        type(MPI_Comm) :: new_comm
        new_comm = old_comm
#else 
        ! TODO Probably want to through an error at this behaviour
        ! as the library should not be compiled with MPI08=off 
        ! in this instance  
        ! MPI_Comm -> integer 
        integer :: new_comm 
        new_comm = old_comm%MPI_VAL 
#endif 
    end function 

end module