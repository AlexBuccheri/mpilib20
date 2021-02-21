# mpilib20

This library provides both free subroutine and object-orientated bindings to MPI fortran, whilst maintaining standard MPI naming conventions as far as 
possible and avoiding the use of non-standard preprocessing. 
The plan is to support both CMake and Meson build systems, and incorporate a unit testing framework. 

## Project Goals
* Produce a decent set of fortran wrappers for MPI that can be used in subsequent projects
* Test bed for writing better CMake and learning meson
* Explore both unit test and app test frameworks for MPI applications
* Provide a means of reusing Alex's standalone MPI tests 
* Provide a home for more exotic communicator splitting and topology wrappers
* Get Max into MPI 


## Building with CMake

The project Cmake has been written to include compiler warnings and flags for both GCC and Intel, for all supported build types.
Build types currently supported are: Release, Debug and RelWithDebInfo, with support for MinSizeRel to be added. 

To build in debug mode, one issues:

`cmake ../ -DCMAKE\_BUILD\_TYPE=Debug`

If no build type is specified, *MPILib20* will build with RelWithDebInfo.

Additionally, one can also build with user-specified flags:

`FFLAGS=-fopenmp cmake ../`

although at present, these will append **not** overwrite the defaults.

### Installing

To install *MPILib20*, the user can specify:

`cmake ../ -DCMAKE_INSTALL_PREFIX="/Users/PATH_TO_LIB/mpilib20_library"`

If no build directory is specified,  will build in:

`ROOT/BUILD_DIR/mpilib20_library`

After configuring CMake, *MPILib20* can be built and installed with:

`make install`


### Check Compilation

A simple test code is present in ROOT/test/init to check building and linking. To run, modify init/CMakeLists.txt ROOT to where the library has been installed, then in the terminal:

`mkdir build && cd build`    
`make`    
`mpirun -np 2 ./test.exe`

### Unit Test Failures

Problem:
MPI returns `MPI_COMM_RANK Invalid Communicator`

Solution
Ensure Zofu has been built with the same flavour of MPI as MPILib20. 

## Bugs/To Dos

General TODOS
 * Update CMake to reflect changes
 * Add ubnit testing framework to CMake 
 * Add make docs with FORD to cmake 
 * Document all existing routines with FORD
 * Write wrappers for routines listed below
 * Provide API for mpi_f08 and mpi bindings 
 * Allow OO and free-function API => the OO functions just wrap the free functions
 * Start on serial wrappers 
 * The mpi types we're writing should probably be protected, such thay the data can 
   be read but only modified by methods of the type:
   * Add get and set 
 * Start adding asserts into routines 
 
Future
 * Implement communicator splitting 
 * Implement cartesian comms splitting

TODOS(Alex)
 * Set up MPI unt test framework and write an example unit test
 * Take allgatherv and allscatterv routines that I wrote in the past. Reimplement here, but do some more cleanly 

TODOS(Max)
 * higher-level routines to perform send and receives 
 * Other things he'd like to take 
 

## Directory structure
errors_warnings
	asserts.F90
	errors_warnings.f90	
constants
	May not need
routines:
    each type and wrapped routine
mpi_bindings:
   Wrappers for when mpif08 is not available 

unit_tests
    Only test via the OO. That way we test the OO API and the underlying wrappers 	

##  Routines to Wrap
mpi_abort
mpi_barrier
mpi_wait
mpi_allreduce: Needs to be overloaded for integer, sp, dp, logical, character
               scalars and vectors
mpi_bcast               
mpi_send
mpi_recv
mpi_irecv
mpi_scatter and v
mpi_gather  and v
mpi_allgather and v
mpi_allscatter and v

comm splitting stuff
Probably more 


### CMake
* CMake still installs to .../include/modules rather than .../include
* `FFLAGS` option should replace the defaults, not append them 
* All Cmake options require documenting for the user
* Intel requires testing
* `WARNINGS_AS_ERRORS = True` fails for GCC
* Add doxygen support

### Meson
* meson needs fixing and extending 

