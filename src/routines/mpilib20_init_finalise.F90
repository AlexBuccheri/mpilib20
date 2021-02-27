!> Provides an MPI environment type and type-bound methods that
!> 
!> Calls to initialise and finalise the mpi environment (not our instantiation)
!> are not provided as methods as the MPI environment should only be initialised
!> and finalised once.   
module mpilib20_init_finalise
  use, intrinsic :: iso_fortran_env, only: error_unit
  use asserts, only: assert
  use mpi_bindings, only : MPI_Comm, MPI_Group, MPI_COMM_DUP, MPI_COMM_RANK, ,
                           MPI_COMM_SIZE, MPI_GROUP_SIZE, , MPI_THREAD_SINGLE, &
                           MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED,  MPI_THREAD_MULTIPLE    
  use internal_utils, only: duplicate 
  implicit none
  private 

  !> Threading options for MPI initialisation in a hybrid MPI-OMP environment 
  integer, parameter, dimension(4) :: thread_options = [MPI_THREAD_SINGLE,     &
                                                        MPI_THREAD_FUNNELED,   &
                                                        MPI_THREAD_SERIALIZED, &
                                                        MPI_THREAD_MULTIPLE]


  ! TODO Consider making all data private
  ! Currently sufficient to only implement for data with derived types
  ! as these require preprocessed getters to interact with bare MPI calls
  !> MPI environment type
  type, public :: mpi_env_type
     private     

     ! Private data  
     integer         :: root_process      !! Process designated as master/root 
     integer         :: threading_support !! level of threading support   Needs method to view 
     type(MPI_Comm)  :: comm              !! MPI communicator (integer in older bindings)
     type(MPI_Group) :: group             !! Group id (integer in older bindings)

     ! Public data
     integer, public :: process         !! Process id (rank)
     integer, public :: n_processes     !! Total number of processes
     integer, public :: group_size      !! Number of processes in group
     integer, public :: ierror          !! Error code
     logical, public :: is_root_process !! is this%process the root_process? Should be private

   contains
    private

    ! Private routines
    procedure, private :: set_root_process                !! Set root process id
    procedure, private :: set_group                       !! Set group 
    procedure, private :: init_mpi_env_from_comm_world    !! Initialise mpi_env_type using MPI_COMM_WORLD
    procedure, private :: init_mpi_env_f08                !! Initialise mpi_env_type using a communicator declared with mpi f08 bindings
    procedure, private :: init_mpi_env_f90                !! Initialise mpi_env_type using a communicator declared with mpi f90 bindings
    procedure, private :: init_thread_mpi_env_from_comm_world  !! Initialise mpi_env_type using MPI_COMM_WORLD and OMP support 
    procedure, private :: init_thread_mpi_env_f08         !! Initialise mpi_env_type using a communicator declared with mpi f08 bindings, and OMP support 
    procedure, private :: init_thread_mpi_env_f90         !! Initialise mpi_env_type using a communicator declared with mpi f90 bindings, and OMP support 

    ! Public routines 
    procedure, public :: get_comm => get_communicator     !! Get the communicator 
    procedure, public :: get_group                        !! Get the group 
    procedure, public :: root_id                          !! Get root process id 
    final,     public :: finalize => finalise_mpi_env     !! Terminate instance of MPI environment object
    
    !> Public API to initialisation routines 
    generic, public :: init => init_mpi_env_from_comm_world, init_mpi_env_f08, init_mpi_env_f90, &
      init_thread_mpi_env_from_comm_world, init_thread_mpi_env_f08, init_thread_mpi_env_f90

  end type mpi_env_type

  !> Exposed, free subroutines 
  public :: mpilib20_init, mpilib20_init_thread, mpilib20_finalize, mpilib20_query_thread
  

contains

  ! ----------------------------------------------------
  ! Setter and getter methods
  ! ----------------------------------------------------

  !> Get communicator from instance of mpi_env_type
  !> 
  !> If using mpi f90 bindings, return an integer not
  !> type(MPI_Comm). 
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


  ! Currently no set comm method as in the current context we only require duplication of comm.
  ! comm can come from an external caller and hence be bare, therefore duplicate is
  ! a free routine for internal use only (i.e. no public API)

  
  !> Get group from instance of mpi_env_type
  !> 
  !> If using mpi f90 bindings, return an integer not
  !> type(MPI_Group) 
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


  !> Set group associated with the communicator of an instance of mpi_env_type
  subroutine set_group(this)
    class(mpi_env_type), intent(inout) :: this
#ifdef MPI08
    call MPI_COMM_GROUP(this%get_comm(), this%group, this%ierror)
#else 
    call MPI_COMM_GROUP(this%get_comm(), this%group%MPI_VAL, this%ierror)
#endif 
  end subroutine  


  !> Get root process associated with the communicator of an instance of mpi_env_type
  function root_id(this) result(root_process)
    class(mpi_env_type), intent(inout) :: this
    integer :: root_process
    root_process = this%root_process
  end function


  !> Set root process id for the communicator of an instance of mpi_env_type
  !
  ! TODO Want to change this so all processes associated with a comm are
  ! found, and the lowest is assigned as root.
  ! Therefore generalises to sub-communicators
  subroutine set_root_process(this)
    class(mpi_env_type), intent(inout) :: this
    this%root_process = 0
    this%is_root_process = this%process == this%root_process
  end subroutine 


  !----------------------------------------------------------------------------
  ! Free subroutines 
  ! These are used by the type-bound procedures.
  ! Their API requires mpi_env_type, hence they must 
  ! be present in this file to avoid circular dependency.
  !----------------------------------------------------------------------------
  !

  ! CLEAN THIS TEXT UP

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
  !
  !---------------------------------------------------------------------------- 

  !> While the CMake option sets a preprocessor argument to determine which
  !> mpi bindings to use, if we want to instantiate an MPI env instance
  !> with an existing communicator, this could be from either bindings. 
  !>
  !> As such, this routine provides two optional arguments for an existing 
  !> communicator, with the appropriate type. Only one should be supplied. 
  !> which could be integer, or type(MPI_Comm).

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


  
  !> Check that the provided threading support is enough to support the
  !> required threading support of the program. 
  !> 
  ! TODO Is this reachable or will MPI_INIT_THREAD throw an opaque error first? 
  ! In which case, passing mpi_env is pointless. 
  ! TODO Could extend to handle errors 
  subroutine mpilib20_query_thread(mpi_env, required, provided)
    use mpi_bindings, only: MPI_QUERY_THREAD, MPI_ABORT

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer,            intent(in)    :: required
    !> Level of threading support provided 
    integer, intent(out) :: provided

    !> Error code returned to the MPI environment (arbitrary)
    integer, parameter :: errorcode = 0

    call MPI_QUERY_THREAD(provided)

    if(provided < required) then 
      call MPI_COMM_RANK(mpi_env%get_comm(), mpi_env%process, mpi_env%ierror)
      if(process == mpi_env%root_id())then
         write(error_unit,'(1x,a,I1,a,I1)') 'threading support available, ',&
              provided, ', is less than is required, ', required
         call MPI_ABORT(mpi_env%get_comm(), errorcode, mpi_env%ierror)
      endif
   endif

  end subroutine mpilib20_query_thread
  
  !> Initialises the MPI environment.
  subroutine mpilib20_init(mpi_env)
    use mpi_bindings, only: MPI_INIT, MPI_COMM_WORLD
    !> Instance of the MPILIB20 environment
    type(mpi_env_type), intent(inout) :: mpi_env
    call MPI_INIT(mpi_env%ierror)
    call mpi_env%init(MPI_COMM_WORLD)
  end subroutine mpilib20_init

  !> Initialises the MPI environment, with threading support
  subroutine mpilib20_init_thread(mpi_env, required)
    use mpi_bindings, only: MPI_INIT_THREAD, MPI_COMM_WORLD
    !> Instance of the MPILIB20 environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer,            intent(in)    :: required
    call mpilib20_query_thread(mpi_env, required, mpi_env%threading_support)
    call MPI_INIT_THREAD(required, provided, mpi_env%ierror)
    call mpi_env%init(MPI_COMM_WORLD, required)
  end subroutine mpilib20_init_thread 

  !> Terminates the MPI environment.
  subroutine mpilib20_finalize(mpi_env)
    use mpi_bindings, only: MPI_FINALIZE
    !> Instance of the MPILIB20 environment
    type(mpi_env_type), intent(inout) :: mpi_env
    call MPI_FINALIZE(mpi_env%ierror)
  end subroutine mpilib20_finalize


  !--------------------------------------------------------
  ! Private routines
  !--------------------------------------------------------
  ! TODO REMOVE  mpilib20 prefix from each

  !> Initialise an instance of mpi_env_type using MPI_COMM_WORLD. 
  !> 
  !> This requires MPI_COMM_WORLD to have been initialised through a call 
  !> to MPI_INIT.
  !>
  !> MPI_COMM_WORLD is duplicated in the initialisation procedure.
  subroutine mpilib20_init_from_comm_world(mpi_env)
    use mpi_bindings, only: MPI_COMM_WORLD

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env 
    
    mpi_env%comm = duplicate(MPI_COMM_WORLD)
    call mpi_env%set_group()
    call mpi_env%set_root_process()
    call MPI_COMM_RANK(mpi_env%get_comm(),    mpi_env%process,     mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%get_comm(),    mpi_env%n_processes, mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%get_group(),  mpi_env%group_size,  mpi_env%ierror)
    
  end subroutine mpilib20_init_from_comm_world

  
  !> Initialise an instance of mpi_env_type given a communicator
  !> of type(MPI_Comm).
  !>
  !> communicator is duplicated in the initialisation procedure.
  subroutine mpilib20_init_from_commf08(mpi_env, communicator)

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Initialised input communicator of type consistent with mpi_f08 
    type(MPI_Comm), intent(in) :: communicator

#ifndef MPI08
    write(*,*) 'The supplied communicator is of type (mpi_env_type) but ' &
               'MPILib20 has been compiled with older MPI bindings (i.e. use mpi).'
    write(*,*) 'Please recompile MPILib20 with MPI08=On.' 
    stop 1
#endif 
    
    call mpi_env%set_root_process()
    ! No setters/getters used as types must be consistent between mpi and our bindings
    ! in this instance 
    call MPI_COMM_DUP(communicator,    mpi_env%comm,        mpi_env%ierror)
    call MPI_COMM_RANK(mpi_env%comm,   mpi_env%process,     mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%comm,   mpi_env%n_processes, mpi_env%ierror)
    call MPI_COMM_GROUP(mpi_env%comm,  mpi_env%group,       mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%group, mpi_env%group_size,  mpi_env%ierror)

  end subroutine mpilib20_init_from_commf08 


  !> Initialise an instance of mpi_env_type given an integer communicator. 
  !>
  !> communicator is duplicated in the initialisation procedure.
  subroutine mpilib20_init_from_commf90(mpi_env, communicator)

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Existing communicator of type consistent with old mpi
    integer, intent(in) :: communicator

    mpi_env%comm = duplicate(communicator)
    call mpi_env%set_group()
    call mpi_env%set_root_process()

    call MPI_COMM_RANK(mpi_env%get_comm(),   mpi_env%process,     mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%get_comm(),   mpi_env%n_processes, mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%get_comm(),  mpi_env%group_size,  mpi_env%ierror)

  end subroutine mpilib20_init_from_commf90

  !> Initialise an instance of mpi_env_type using MPI_COMM_WORLD, for
  !> a hybrid MPI-OMP calculation.
  !> 
  !> This requires MPI_COMM_WORLD to have been initialised through a call 
  !> to MPI_INIT_THREAD. 
  subroutine mpilib20_init_thread_from_comm_world(mpi_env, required)

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer, intent(in)    :: required

    if(.not. any(thread_options == required)) then
       write(error_unit,'(1x,a,I1)') 'required thread is not a valid choice:', required
    endif

    call mpilib20_query_thread(mpi_env, required, mpi_env%threading_support)
    call mpilib20_init_from_comm_world(mpi_env)

  end subroutine mpilib20_init_thread_from_comm_world 


  !> Initialise an instance of mpi_env_type given a communicator, for
  !> use with a hybrid MPI-OMP calculation.
  subroutine mpilib20_init_thread_f08(mpi_env, communicator, required)

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer, intent(in)    :: required
    !> Existing communicator 
    type(MPI_Comm), intent(in) :: communicator

    if(.not. any(thread_options == required)) then
       write(error_unit,'(1x,a,I1)') 'required thread is not a valid choice:', required
    endif

    call mpilib20_query_thread(mpi_env, required, mpi_env%threading_support)
    call mpilib20_init_from_commf08(mpi_env, communicator)

  end subroutine mpilib20_init_thread_f08 


  !> Initialise an instance of mpi_env_type given a communicator, for
  !> use with a hybrid MPI-OMP calculation.
  !>
  !> TODO required thread-level is an argument because the communicator may have been initialised
  !> using mpi_init, not mpi_init_thread. Need to check at what point this will crash (if any)
  subroutine mpilib20_init_thread_f90(mpi_env, communicator, required)

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer, intent(in)    :: required
    !> Existing communicator 
    integer, intent(in) :: communicator

    if(.not. any(thread_options == required)) then
       write(error_unit,'(1x,a,I1)') 'required thread is not a valid choice:', required
    endif

    call mpilib20_query_thread(mpi_env, required, mpi_env%threading_support)
    call mpilib20_init_from_commf90(mpi_env, communicator)

  end subroutine mpilib20_init_thread_f90


  !---------------------------------------------------------
  ! Methods for initialisation and finalising
  ! TODO 
  ! Can write overloads for the communicator type - see my references (somewhere)
  ! Could have written less code with runtime polymorphism
  ! but this will execute faster
  !---------------------------------------------------------

  !> Initialise an instance of the MPI environment.
  subroutine init_mpi_env_from_comm_world(this)
    use mpi_bindings, only: MPI_INITIALIZED
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: this 
    integer ::ierror 
    logical :: initialised

    call MPI_INITIALIZED(initialised, ierror)
    if (.not. initialised) then
      write(*,*) 'mpilib20_init must be called prior to calling this%init &
                  without passing an communicator' 
      stop 1
    endif 

    ! mpi_comm_world is passed via a use statement
    call mpilib20_init_from_comm_world(this)
  end subroutine init_mpi_env_from_comm_world

  !> Initialise an instance of the MPI environment.
  subroutine init_mpi_env_f08(this, communicator)
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: this 
    !> input communicator 
    type(MPI_Comm), intent(in) :: communicator
    call mpilib20_init_from_commf08(mpi_env, communicator)
  end subroutine init_mpi_env_f08

  !> Initialise an instance of the MPI environment.
  subroutine init_mpi_env_f90(this, communicator)
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: this 
    !> input communicator 
    integer, intent(in) :: communicator
    call mpilib20_init_from_commf90(mpi_env, communicator)
  end subroutine init_mpi_env_f90

  !> Initialise an instance of the MPI environment with MPI_COMM_WORLD
  !> and OMP support. 
  subroutine init_thread_mpi_env_from_comm_world(this, required)
    use mpi_bindings, only: MPI_INITIALIZED
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: this 
    !> Required level of threading support 
    integer, intent(in)    :: required
    integer ::ierror 
    logical :: initialised

    call MPI_INITIALIZED(initialised, ierror)
    if (.not. initialised) then
      write(*,*) 'mpilib20_init must be called prior to calling this%init &
                  without passing an communicator' 
      stop 1
    endif 

    ! mpi_comm_world is passed via a use statement
    call mpilib20_init_thread_from_comm_world(this, required)
  end subroutine init_thread_mpi_env_from_comm_world

  !> Initialise an instance of the MPI environment with OMP support. 
  subroutine init_thread_mpi_env_f08(this, communicator, required)
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: this 
    !> input communicator 
    integer intent(in) :: communicator
    !> Required level of threading support 
    integer, intent(in)    :: required
    call mpilib20_init_from_commf08(mpi_env, communicator, required)
  end subroutine init_thread_mpi_env_f08

  !> Initialise an instance of the MPI environment with OMP support. 
  subroutine init_thread_mpi_env_f90(this, communicator, required)
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: this 
    !> input communicator 
    integer, intent(in) :: communicator
    !> Required level of threading support 
    integer, intent(in) :: required
    call mpilib20_init_from_commf90(mpi_env, communicator, required)
  end subroutine init_thread_mpi_env_f90

  ! See this post: https://stackoverflow.com/questions/29038025/does-deallocating-a-fortran-derived-type-automatically-deallocate-member-arrays
  ! Implementation isn't clear though
  subroutine finalise_mpi_env(self)
    !> Instance of the MPI environment
    class(mpi_env_type), intent(inout) :: self
    deallocate(self) 
  end subroutine finalise_mpi_env

end module mpilib20_init_finalise