# mpilib20

This library provides both free subroutine and object-orientated bindings to MPI fortran, whilst maintaining standard MPI naming conventions as far as 
possible and avoiding the use of non-standard preprocessing. 
The plan is to support both CMake and Meson build systems, and incorporate a unit testing framework. 


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



## Bugs/To Dos

### CMake
* CMake still installs to .../include/modules rather than .../include
* `FFLAGS` option should replace the defaults, not append them 
* All Cmake options require documenting for the user
* Intel requires testing
* `WARNINGS_AS_ERRORS = True` fails for GCC
* Add doxygen support

### Meson
* meson needs fixing and extending 

