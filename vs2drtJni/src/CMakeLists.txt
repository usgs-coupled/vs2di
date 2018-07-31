cmake_minimum_required (VERSION 2.8)

# project
project(vs2drt C CXX Fortran)

# F90 source
SET(vs2drt_F90_SOURCES
Call_RM.F90
PhreeqcRM/src/RM_interface.F90
vs2dt3_3_reactive_withoutArgus1.F90
)

# source
SET(vs2drt_SOURCES
${vs2drt_F90_SOURCES}
mgmres.f90
modules.f90
)

source_group("Source Files" FILES ${vs2drt_F90_SOURCES})

# executable
add_executable(vs2drt ${vs2drt_SOURCES})

# Disable deprecation warnings for standard C and STL functions
# in VS2005 and later
IF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
ENDIF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)

#### check for zlib_root env
###if(NOT ZLIB_ROOT)
###  if(EXISTS $ENV{ZLIB_ROOT})
###    set(ZLIB_ROOT $ENV{ZLIB_ROOT})
###  endif()
###endif()
###
###if (WIN32)
###  # override PhreeqcRM zlib
###  set(PHREEQCRM_EXTERNALLY_CONFIGURED ON)
###  set(ZLIB_FOUND true)
###  if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
###    set(ZLIB_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/x64")
###    set(ZLIB_LIBRARIES "${CMAKE_CURRENT_SOURCE_DIR}/x64/zlib.lib")
###  else()
###    set(ZLIB_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/win32")
###    set(ZLIB_LIBRARIES "${CMAKE_CURRENT_SOURCE_DIR}/win32/zlib.lib")
###  endif()
###endif()

##{{
if(NOT ZLIB_ROOT)
  if (WIN32)
    if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
      set(ZLIB_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/x64")
    else()
      set(ZLIB_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/win32")
    endif()
  endif()
  find_package(ZLIB REQUIRED)
endif()
##}}

if (PHREEQCRM_USE_ZLIB AND ZLIB_FOUND)
  add_definitions(-DUSE_GZ)
  include_directories(${ZLIB_INCLUDE_DIRS})
endif()

# fortran name mangling
include(FortranCInterface)
FortranCInterface_HEADER(FC.h MACRO_NAMESPACE "FC_" SYMBOLS
)

# define CMAKE_FC
add_definitions(-DCMAKE_FC)

# source
SET(libvs2drt_SOURCES
${PROJECT_BINARY_DIR}/FC.h
FileHandler.cpp
FileHandler.h
)

# setup include directories
include_directories("PhreeqcRM/src")
include_directories("PhreeqcRM/src/IPhreeqcPhast")
include_directories("PhreeqcRM/src/IPhreeqcPhast/IPhreeqc")
include_directories("PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp")
include_directories("PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common")
include_directories("PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PhreeqcKeywords")
include_directories("${PROJECT_BINARY_DIR}")

# dll/shared objects
##OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
SET(LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  SET(LIB_TYPE SHARED)
endif()
add_library(libvs2drt ${LIB_TYPE} ${libvs2drt_SOURCES})
set_target_properties(libvs2drt PROPERTIES DEBUG_POSTFIX "d")

# library dependencies
if (PHREEQCRM_FORTRAN_TESTING)
  # need extra mpi fortran libs if using msmpi
  if (MPI_Fortran_LIBRARIES MATCHES "msmpi.lib$")
    STRING(REGEX REPLACE "msmpi.lib$" "msmpifec.lib" EXTRA_FORTRAN_LIB ${MPI_Fortran_LIBRARIES})
    SET(EXTRA_LIBS ${EXTRA_LIBS} ${EXTRA_FORTRAN_LIB})
  endif ()  
  if (PHREEQCRM_BUILD_MPI)
    SET(EXTRA_LIBS ${EXTRA_LIBS} libvs2drt PhreeqcRM ${MPI_CXX_LIBRARIES})
  else ()
    SET(EXTRA_LIBS ${EXTRA_LIBS} libvs2drt PhreeqcRM)
  endif ()
  # Override bug reported on:
  # http://www.cmake.org/pipermail/cmake/2009-July/030954.html
  if(WIN32 AND ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")  
    if (NOT CMAKE_Fortran_FLAGS_DEBUG MATCHES "/Od")
      set(CMAKE_Fortran_FLAGS_DEBUG "/Od ${CMAKE_Fortran_FLAGS_DEBUG}")
    endif()
  endif()
else ()
  if (PHREEQCRM_BUILD_MPI)
    SET(EXTRA_LIBS ${EXTRA_LIBS} libvs2drt PhreeqcRM ${MPI_CXX_LIBRARIES})
  else ()
    SET(EXTRA_LIBS ${EXTRA_LIBS} libvs2drt PhreeqcRM)
  endif ()
endif ()

# when building shared objects must include link libs
if (BUILD_SHARED_LIBS)
  target_link_libraries(libvs2drt PhreeqcRM)
  if (PHREEQCRM_USE_ZLIB AND ZLIB_FOUND)
    target_link_libraries(libvs2drt ${ZLIB_LIBRARIES})
  endif()
endif()

# check for openmp
find_package(OpenMP)

if (OPENMP_FOUND)
  ADD_DEFINITIONS(-DUSE_OPENMP)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   ${OpenMP_C_FLAGS}")
  set(VS2DRT_OPENMP_FOUND ON PARENT_SCOPE)
endif()

#
# set PhreeqcRM defaults for vs2drt
#

# PHREEQCRM_STATIC_RUNTIME
set(PHREEQCRM_STATIC_RUNTIME ON CACHE INTERNAL "doc" FORCE)
mark_as_advanced(PHREEQCRM_STATIC_RUNTIME)

# PHREEQCRM_USE_ZLIB (must be enabled)
set(PHREEQCRM_USE_ZLIB OFF CACHE INTERNAL "doc" FORCE)
mark_as_advanced(PHREEQCRM_USE_ZLIB)

# PHREEQCRM_BUILD_MPI (must be disabled)
set(PHREEQCRM_BUILD_MPI OFF CACHE INTERNAL "doc" FORCE)
mark_as_advanced(PHREEQCRM_BUILD_MPI)

# PHREEQCRM_FORTRAN_TESTING (just hide)
mark_as_advanced(PHREEQCRM_FORTRAN_TESTING)

# 
if (MSVC)
  set(CompilerFlags
    CMAKE_CXX_FLAGS
    CMAKE_CXX_FLAGS_DEBUG
    CMAKE_CXX_FLAGS_RELEASE
    CMAKE_CXX_FLAGS_MINSIZEREL
    CMAKE_CXX_FLAGS_RELWITHDEBINFO
    CMAKE_C_FLAGS
    CMAKE_C_FLAGS_DEBUG
    CMAKE_C_FLAGS_RELEASE
    CMAKE_C_FLAGS_MINSIZEREL
    CMAKE_C_FLAGS_RELWITHDEBINFO
  )

  # Set C/C++ Runtime Library to Multi-threaded (/MT) 
  foreach(CompilerFlag ${CompilerFlags})
    if(${CompilerFlag} MATCHES "/MD")
      string(REGEX REPLACE "/MD" "/MT" ${CompilerFlag} "${${CompilerFlag}}")
    endif()
  endforeach()
  
  set(FortranCompilerFlags
    CMAKE_Fortran_FLAGS
  )
  
  # Set Fortran Runtime Library to Multithreaded
  foreach(FortranCompilerFlags ${FortranCompilerFlags})
    if(${FortranCompilerFlags} MATCHES "/libs:dll")
      string(REGEX REPLACE "/libs:dll" "/libs:static" ${FortranCompilerFlags} "${${FortranCompilerFlags}}")
    endif()  
  endforeach()  
endif()

# subdirs
add_subdirectory(PhreeqcRM)

# link (zlib must be last on linux)
if (PHREEQCRM_USE_ZLIB AND ZLIB_FOUND)
  set(EXTRA_LIBS ${EXTRA_LIBS} ${ZLIB_LIBRARIES})
endif()
target_link_libraries(vs2drt ${EXTRA_LIBS})

install(TARGETS vs2drt DESTINATION ${CMAKE_INSTALL_BINDIR})
