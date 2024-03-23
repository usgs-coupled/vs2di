cmake_minimum_required(VERSION 2.8...3.20)

# project
project(vs2drt C CXX Fortran)

# F90 source
SET(vs2drt_F90_SOURCES
Call_RM.F90
parallel_solute_transport.F90
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
if (VS2DRT_BUILD_MPI AND VS2DRT_BUILD_OPENMP)
  add_executable(vs2drt${MPI_SUFFIX}${OPENMP_SUFFIX} ${vs2drt_SOURCES})
  if (UNIX AND NOT APPLE)
    set_target_properties(vs2drt${MPI_SUFFIX}${OPENMP_SUFFIX} PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
  endif()
elseif(VS2DRT_BUILD_MPI)
  add_executable(vs2drt${MPI_SUFFIX} ${vs2drt_SOURCES})
  if (UNIX AND NOT APPLE)
    set_target_properties(vs2drt${MPI_SUFFIX} PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
  endif()
elseif(VS2DRT_BUILD_OPENMP)
  add_executable(vs2drt${OPENMP_SUFFIX} ${vs2drt_SOURCES})
  if (UNIX AND NOT APPLE)
    set_target_properties(vs2drt${OPENMP_SUFFIX} PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
  endif()
else()
  add_executable(vs2drt ${vs2drt_SOURCES})
  if (UNIX AND NOT APPLE)
    set_target_properties(vs2drt PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
  endif()
endif()


# Disable deprecation warnings for standard C and STL functions
# in VS2005 and later
IF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
ENDIF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)

# define CMAKE_FC
add_definitions(-DCMAKE_FC)

# source
SET(libvs2drt_SOURCES
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
include_directories("${PROJECT_BINARY_DIR}/PhreeqcRM")

# dll/shared objects
if (WIN32)
  OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
else()
  OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" ON)
endif()
SET(LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  SET(LIB_TYPE SHARED)
endif()
add_library(libvs2drt ${LIB_TYPE} ${libvs2drt_SOURCES})
set_target_properties(libvs2drt PROPERTIES DEBUG_POSTFIX "d")
if (UNIX AND NOT APPLE)
  set_target_properties(libvs2drt PROPERTIES INSTALL_RPATH "\$ORIGIN")
endif()

# library dependencies
if (PHREEQCRM_FORTRAN_TESTING)
  # need extra mpi fortran libs if using msmpi
###   if (MPI_Fortran_LIBRARIES MATCHES "msmpi.lib$")
###     STRING(REGEX REPLACE "msmpi.lib$" "msmpifec.lib" EXTRA_FORTRAN_LIB ${MPI_Fortran_LIBRARIES})
###     SET(EXTRA_LIBS ${EXTRA_LIBS} ${EXTRA_FORTRAN_LIB})
###   endif ()
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
endif()

#
# set PhreeqcRM defaults for vs2drt
#

# PHREEQCRM_STATIC_RUNTIME
set(PHREEQCRM_STATIC_RUNTIME ON CACHE INTERNAL "doc" FORCE)

# PHREEQCRM_USE_ZLIB
set(PHREEQCRM_USE_ZLIB OFF CACHE INTERNAL "doc" FORCE)

# PHREEQCRM_FORTRAN_TESTING
set(PHREEQCRM_FORTRAN_TESTING OFF CACHE INTERNAL "doc" FORCE)

# BUILD_SHARED_LIBS
if (UNIX AND NOT APPLE)
  set(BUILD_SHARED_LIBS ON CACHE INTERNAL "doc" FORCE)
endif()

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
if (WIN32)
  set(CMAKE_INSTALL_LIBDIR ${CMAKE_INSTALL_BINDIR})
endif()
if (BUILD_SHARED_LIBS)
  set(PHREEQCRM_INSTALL_TARGETS ON)
endif()
add_subdirectory(PhreeqcRM)


if (VS2DRT_BUILD_MPI AND VS2DRT_BUILD_OPENMP)
  set(EXTRA_LIBS PUBLIC OpenMP::OpenMP_CXX PUBLIC OpenMP::OpenMP_Fortran MPI::MPI_CXX PUBLIC MPI::MPI_Fortran ${EXTRA_LIBS})
elseif (VS2DRT_BUILD_MPI)
  set(EXTRA_LIBS PUBLIC MPI::MPI_CXX PUBLIC MPI::MPI_Fortran ${EXTRA_LIBS})
elseif (VS2DRT_BUILD_OPENMP)
  set(EXTRA_LIBS PUBLIC OpenMP::OpenMP_CXX PUBLIC OpenMP::OpenMP_Fortran ${EXTRA_LIBS})
endif()

# executable
if (VS2DRT_BUILD_MPI AND VS2DRT_BUILD_OPENMP)
  target_link_libraries(vs2drt${MPI_SUFFIX}${OPENMP_SUFFIX} ${EXTRA_LIBS})
  install(TARGETS vs2drt${MPI_SUFFIX}${OPENMP_SUFFIX}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
  if (BUILD_SHARED_LIBS)
    install(TARGETS libvs2drt
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      )
  endif()
elseif (VS2DRT_BUILD_MPI)
  target_link_libraries(vs2drt${MPI_SUFFIX} ${EXTRA_LIBS})
  install(TARGETS vs2drt${MPI_SUFFIX}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
  if (BUILD_SHARED_LIBS)
    install(TARGETS libvs2drt
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      )
  endif()
elseif(VS2DRT_BUILD_OPENMP)
  target_link_libraries(vs2drt${OPENMP_SUFFIX} ${EXTRA_LIBS})
  install(TARGETS vs2drt${OPENMP_SUFFIX}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
  if (BUILD_SHARED_LIBS)
    install(TARGETS libvs2drt
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      )
  endif()
else()
  target_link_libraries(vs2drt ${EXTRA_LIBS})
  install(TARGETS vs2drt
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
  if (BUILD_SHARED_LIBS)
    install(TARGETS libvs2drt
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      )
  endif()
endif()
