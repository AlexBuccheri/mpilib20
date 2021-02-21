!>  Provides an MPI environment type and type-bound methods that
!>  wrap  MPI_initalise and MPI_finalise. 
!>  Also provides mpilib2020 API for MPI_initalise and MPI_finalise
!>  to be used as free subroutines.
module mpilib20_init_finalise
  use, intrinsic :: iso_fortran_env, only: error_unit
  use asserts, only: assert
  use mpi_bindings, only : MPI_Comm, MPI_Group, MPI_INIT, MPI_COMM_DUP, &
                           MPI_INIT_THREAD, MPI_COMM_RANK, MPI_ABORT, MPI_COMM_DUP, &
                           MPI_COMM_SIZE, MPI_GROUP_SIZE, MPI_FINALIZE, MPI_THREAD_SINGLE, &
                           MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED,  MPI_THREAD_MULTIPLE    
  implicit none
  private 

  !> MPI environment type
  ! TODO Consider making all data of the class private
  ! Currently sufficient to only implement for data with derived types
  !
  ! In general, it doesn't look like we require setters to set private data
  ! of a class, as long as we're accessing that data within the same module 
  ! that the class is defined. 
  type, public :: mpi_env_type
     private       
     integer         :: root_process !! Process designated as master/root 
     type(MPI_Comm)  :: comm         !! MPI communicator (integer in older bindings)
     integer, public :: process      !! Process id (rank)
     integer, public :: n_processes  !! Total number of processes
     type(MPI_Group) :: group        !! Group id (integer in older bindings)
     integer, public :: group_size   !! Number of processes in group
     integer, public :: ierror       !! Error code

   contains
    private
    ! Public routines 
    procedure, public :: init     => init_mpi_env         !! Initialise instance of MPI environment object
    procedure, public :: finalize => finalise_mpi_env     !! Terminate instance of MPI environment object
    procedure, public :: get_comm => get_communicator     !! Get the communicator 
    procedure, public :: get_group                        !! Get the group 
    procedure, public :: root_id                          !! Get root process id 

  end type mpi_env_type

  interface mpilib20_init
    module procedure :: mpilib20_init_f08, mpilib20_init_f90
  end interface

  interface mpilib20_init_thread
    module procedure :: mpilib20_init_thread_f08, mpilib20_init_thread_f90
  end interface

  !> Free subroutines 
  public :: mpilib20_init, mpilib20_init_thread, mpilib20_finalize
  
contains

  !> Get communicator from instance of mpi_env_type
  !> 
  !> If using pre-2008 mpi bindings, return an integer not
  !> MPI_Comm type. 
  function get_communicator(this) result(comm)
    class(mpi_env_type), intent(inout) :: this
#ifdef MPI08
    type(MPI_Comm) :: comm 
    comm = this%comm
#else 
    integer :: comm 
    comm = this%comm%MPI_VAL
#endif 
  end function

  !> Get group from instance of mpi_env_type
  !> 
  !> If using pre-2008 mpi bindings, return an integer not
  !> MPI_Group type. 
  function get_group(this) result(group)
    class(mpi_env_type), intent(inout) :: this
#ifdef MPI08
    type(MPI_Group) :: group 
    group = this%group
#else 
    integer :: group 
    group = this%group%MPI_VAL
#endif 
  end function

  !> Get root process associated with the communicator of an instance of mpi_env_type
  function root_id(this) result(root_process)
    class(mpi_env_type), intent(inout) :: this
    integer :: root_process
    root_process = this%root_process
  end function


  !--------------------------------------------------------
  ! Free subroutines 
  ! These are used by the type-bound procedures.
  ! Their API requires mpi_env_type, hence they must 
  ! be present in this file to avoid circular dependency.
  !--------------------------------------------------------

  ! Binding Permutations 
  !
  ! * Option 1. Required in case the mpi bindings on the system are somewhat out-of-date
  ! * Option 2. Don't implement as it makes no sense to use old bindings if f08 is available
  ! * Option 3. Required as we cannot control how external libraries are written 
  ! * Option 4. The ideal scenario that would have avoided the need for all this preprocessing
  ! 
  ! 1. MPILib20 mpif90   |  External lib mpi90
  ! temporary_communicator%MPI_VAL = communicator
  !
  ! 2. MPILib20 mpif90   |  External lib mpif08                      
  ! temporary_communicator%MPI_VAL = communicator%MPI_VAL
  !
  ! 3. MPILib20 mpif08   |  External lib mpi90
  ! temporary_communicator%MPI_VAL = communicator
  !  
  ! 4. MPILib20 mpif08   |  External lib mpif08
  ! temporary_communicator = communicator


  !TODO
  ! MAKES SENSE TO:
  ! Write free functions to copy_comm and copy_group
  !  - Can then abstract all this preprocessing junk 
  ! Two routines each (overloads) and the return type can be controlled by preprocessing
  ! Then the inputs to the init routines get immediately copied and one doesn't need
  ! to worry about the return object's type 
  ! Can also use them in type-bound methods if required
  ! Don't depend on mpi_env_type so put in different module 


  !> Initialise the MPI environment.
  !> 
  !> If the MPI has not been initialised, MPI_INIT is called.
  !> If it has been initialised, one can pass an existing communicator. 
  !> 
  !> While the CMake option sets a preprocessor argument to determine which
  !> mpi bindings to use, if we want to instantiate an MPI env instance
  !> with an existing communicator, this could be from either bindings. 
  !>
  !> As such, this routine provides two optional arguments for an existing 
  !> communicator, with the appropriate type. Only one should be supplied. 
  !> 
  !> @todo(Alex) This routine can be split up
  !
  subroutine mpilib20_init_f08(mpi_env, communicator)
    use mpi_bindings, only: MPI_COMM_WORLD

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Existing communicator of type consistent with mpi_f08 
    type(MPI_Comm), intent(in), optional :: communicator

    !> Temporarily hold the communicator in the scope of this routine
    type(MPI_Comm) :: temporary_communicator

#ifndef MPI08
    write(*,*) 'The supplied communicator is of type (mpi_env_type) but ' &
               'MPILib20 has been compiled with older MPI bindings (i.e. use mpi).'
    write(*,*) 'Please recompile MPILib20 with MPI08=On.' 
    stop 1
#endif 

    if (present(communicator)) then
      ! communicators have the same type 
      temporary_communicator = communicator
    else
      call MPI_INIT(mpi_env%ierror)
      temporary_communicator = MPI_COMM_WORLD
    endif   
    
    call MPI_COMM_DUP(temporary_communicator, mpi_env%comm, mpi_env%ierror)
    call MPI_COMM_RANK(mpi_env%comm,   mpi_env%process,     mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%comm,   mpi_env%n_processes, mpi_env%ierror)
    call MPI_COMM_GROUP(mpi_env%comm,  mpi_env%group,       mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%group, mpi_env%group_size,  mpi_env%ierror)
    ! Think about how to set this if this is a subcommunicator being initialised 
    ! Probably write a routine to get the lowest rank from this available w.r.t. comm 
    mpi_env%root_process = 0

  end subroutine mpilib20_init_f08 


  ! Name needs to be better
  subroutine mpilib20_init_f90(mpi_env, communicator)
    use mpi_bindings, only: MPI_COMM_WORLD

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Existing communicator of type consistent with old mpi
    integer, intent(in), optional :: communicator

    !> Temporarily hold the communicator in the scope of this routine
    type(MPI_Comm) :: temporary_communicator
  
    !TODO This is messy as fuck - needs refactoring now I can see the choices
    ! SHOULD JUST REFACTOR THIS INTO TWO ROUTINES AND OVERLOAD THE INTERFACE
    ! SUCH THAT ALL 4 MPI_INIT ROUTINES CALL THE SAME THING
    if (present(communicator)) then
#ifdef MPI08
      temporary_communicator%MPI_VAL = communicator
      call MPI_COMM_DUP(temporary_communicator, mpi_env%comm, mpi_env%ierror)
#else 
      call MPI_COMM_DUP(communicator, mpi_env%comm%MPI_VAL, mpi_env%ierror)
#endif 

    else
      call MPI_INIT(mpi_env%ierror)
#ifdef MPI08
      call MPI_COMM_DUP(MPI_COMM_WORLD, mpi_env%comm, mpi_env%ierror)
#else 
      call MPI_COMM_DUP(MPI_COMM_WORLD, mpi_env%comm%MPI_VAL, mpi_env%ierror)
#endif 
    endif   
    
    call MPI_COMM_RANK(mpi_env%get_comm(),   mpi_env%process,     mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%get_comm(),   mpi_env%n_processes, mpi_env%ierror)
    call MPI_COMM_GROUP(mpi_env%get_comm(),  mpi_env%group,       mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%get_comm(),  mpi_env%group_size,  mpi_env%ierror)
    ! Think about how to set this if this is a subcommunicator being initialised 
    mpi_env%root_process = 0

  end subroutine mpilib20_init_f90


  !> Initialises the MPI execution environment for use with hybrid MPI/threaded applications.
  !>
  !> MPI_COMM_WORLD is duplicated.
  !>
  !> Threading options 
  !> MPI_THREAD_SINGLE:     Only one thread will execute. Equivalent to calling MPI_init(ierror)
  !> MPI_THREAD_FUNNELED:   If the process is multithreaded, only the thread that called
  !>                        MPI_Init_thread will make MPI calls.
  !> MPI_THREAD_SERIALIZED: If the process is multithreaded, only one thread will make MPI library
  !>                        calls at one time.
  !> MPI_THREAD_MULTIPLE:   If the process is multithreaded, multiple threads may call MPI at once
  !>                        with no restrictions.
  !
  subroutine mpilib20_init_thread_f08(mpi_env, required, communicator)
    use mpi_bindings, only: MPI_COMM_WORLD

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer,            intent(in)    :: required
    !> Existing communicator 
    type(MPI_Comm), intent(in), optional :: communicator

    !> Error code returned to the MPI environment (arbitrary)
    integer, parameter :: errorcode = 0
    !> Threading options 
    integer, parameter, dimension(4) :: thread_options = [MPI_THREAD_SINGLE,     &
                                                          MPI_THREAD_FUNNELED,   &
                                                          MPI_THREAD_SERIALIZED, &
                                                          MPI_THREAD_MULTIPLE]
    !> Level of available (provided) threading support 
    integer :: available
    !> Communicator place-holder in the scope of this routine 
    type(MPI_Comm) :: temporary_communicator
    !> Process id
    integer :: process
    !> Error 
    integer :: ierror

    if(.not. any(thread_options == required)) then
       write(error_unit,'(1x,a,I1)') 'required thread is not a valid choice:', required
    endif

    if (present(communicator)) then
      temporary_communicator = communicator

    else
      call MPI_INIT_THREAD(required, available, ierror)
      temporary_communicator = MPI_COMM_WORLD
    endif  

    ! Think about how to set this if this is a subcommunicator being initialised 
    mpi_env%root_process = 0

    if(available < required) then 
      ! Is this reachable if not present(communicator)  and (available < required) 
      ! or will MPI_INIT_THREAD throw an opaque error first? 
      call MPI_COMM_RANK(MPI_COMM_WORLD, process, ierror)
      if(process == mpi_env%root_id())then
         write(error_unit,'(1x,a,I1,a,I1)') 'threading support available, ',&
              available, ', is less than is required, ', required
         call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierror)
      endif
   endif

    call MPI_COMM_DUP(temporary_communicator, mpi_env%comm,        mpi_env%ierror)
    call MPI_COMM_RANK(mpi_env%comm,          mpi_env%process ,    mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%comm,          mpi_env%n_processes, mpi_env%ierror)
    call MPI_COMM_GROUP(mpi_env%comm,         mpi_env%group,       mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%group,        mpi_env%group_size,  mpi_env%ierror)

  end subroutine mpilib20_init_thread_f08 


  !> Initialises the MPI execution environment for use with hybrid MPI/threaded applications.
  !>
  !> MPI_COMM_WORLD is duplicated.
  !>
  !> Threading options 
  !> MPI_THREAD_SINGLE:     Only one thread will execute. Equivalent to calling MPI_init(ierror)
  !> MPI_THREAD_FUNNELED:   If the process is multithreaded, only the thread that called
  !>                        MPI_Init_thread will make MPI calls.
  !> MPI_THREAD_SERIALIZED: If the process is multithreaded, only one thread will make MPI library
  !>                        calls at one time.
  !> MPI_THREAD_MULTIPLE:   If the process is multithreaded, multiple threads may call MPI at once
  !>                        with no restrictions.
  !
  subroutine mpilib20_init_thread_f90(mpi_env, required, communicator)
    use mpi_bindings, only: MPI_COMM_WORLD

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer,            intent(in)    :: required
    !> Existing communicator 
    integer, intent(in), optional :: communicator

    !> Error code returned to the MPI environment (arbitrary)
    integer, parameter :: errorcode = 0
    !> Threading options 
    integer, parameter, dimension(4) :: thread_options = [MPI_THREAD_SINGLE,     &
                                                          MPI_THREAD_FUNNELED,   &
                                                          MPI_THREAD_SERIALIZED, &
                                                          MPI_THREAD_MULTIPLE]
    !> Level of available (provided) threading support 
    integer :: available
    !> Communicator place-holder in the scope of this routine 
    type(MPI_Comm) :: temporary_communicator
    !> Process id
    integer :: process
    !> Error 
    integer :: ierror

    if(.not. any(thread_options == required)) then
       write(error_unit,'(1x,a,I1)') 'required thread is not a valid choice:', required
    endif

    if (present(communicator)) then
      temporary_communicator%MPI_VAL = communicator

    else
      call MPI_INIT_THREAD(required, available, ierror)
      temporary_communicator = MPI_COMM_WORLD
    endif  

    ! Think about how to set this if this is a subcommunicator being initialised 
    mpi_env%root_process = 0

    ! Not sure if this is ever reachable (i.e. will MPI_INIT_THREAD throw an error?)
    if(available < required) then 
      call MPI_COMM_RANK(MPI_COMM_WORLD, process, ierror)
      if(process == mpi_env%root_id())then
         write(error_unit,'(1x,a,I1,a,I1)') 'threading support available, ',&
              available, ', is less than is required, ', required
         call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierror)
      endif
   endif

    call MPI_COMM_DUP(temporary_communicator, mpi_env%comm,        mpi_env%ierror)
    call MPI_COMM_RANK(mpi_env%comm,          mpi_env%process ,    mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%comm,          mpi_env%n_processes, mpi_env%ierror)
    call MPI_COMM_GROUP(mpi_env%comm,         mpi_env%group,       mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%group,        mpi_env%group_size,  mpi_env%ierror)

  end subroutine mpilib20_init_thread_f90

  !> Terminates MPI execution environment.
  subroutine mpilib20_finalize(mpi_env)
    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    call MPI_FINALIZE(mpi_env%ierror)
  end subroutine mpilib20_finalize


  !---------------
  ! Methods
  !---------------

  !> Initialise an instance of the MPI environment.
  subroutine init_mpi_env_f08(self, required_threading, communicator)
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: self 
    !> Required level of threading support
    integer, optional,   intent(in)    :: required_threading
    !> Existing communicator of type consistent with mpi_f08 
    type(MPI_Comm), intent(in), optional :: communicator

    if(present(required_threading) .and. (.not. present(communicator))) then
       call mpilib20_init_thread_f08(self, required = required_threading)
    else if(present(communicator)) then 
       call mpilib20_init_f08(self, communicator = communicator)
    else if(present(bare_communicator)) then 
       call mpilib20_init(self, bare_communicator = bare_communicator)
    endif

  end subroutine init_mpi_env_f08


    !> Initialise an instance of the MPI environment.
  subroutine init_mpi_env(self, required_threading, communicator, bare_communicator)
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: self 
    !> Required level of threading support
    integer, optional,   intent(in)    :: required_threading
    !> Existing communicator of type consistent with mpi_f08 
    type(MPI_Comm), intent(in), optional :: communicator
    !> Existing communicator of type consistent with old mpi
    integer, intent(in), optional :: bare_communicator

    !Could simplify by putting bare_communicator into communicator here 

    if(present(required_threading)) then
       !TODO Extend to take  communicator or bare_communicator
       call mpilib20_init_thread(self, required = required_threading)
    else if(present(communicator)) then 
       call mpilib20_init(self, communicator = communicator)
    else if(present(bare_communicator)) then 
       call mpilib20_init(self, bare_communicator = bare_communicator)
    endif

  end subroutine init_mpi_env

  !> Terminates MPI execution environment. 
  subroutine finalise_mpi_env(self)
    !> Instance of the MPI environment
    class(mpi_env_type), intent(inout) :: self
    call mpilib20_finalize(self)    
  end subroutine finalise_mpi_env

end module mpilib20_init_finalise