cmake_minimum_required (VERSION 2.8)

# project
project(vs2drt C CXX Fortran)

# source
SET(vs2drt_SOURCES
include/c_arrays.inc
include/c_disch.inc
include/c_dumm.inc
include/c_dumm1.inc
include/c_dumm2.inc
include/c_equat.inc
include/c_equats.inc
include/c_hcon.inc
include/c_idumm.inc
include/c_ihdum.inc
include/c_isdum.inc
include/c_jtxx.inc
include/c_kcon.inc
include/c_kdum.inc
include/c_mprop.inc
include/c_obsp.inc
include/c_obst.inc
include/c_pit.inc
include/c_plott.inc
include/c_press.inc
include/c_ptet.inc
include/c_rprop.inc
include/c_rpropsh.inc
include/c_rspac.inc
include/c_scon.inc
include/c_sconItmax.inc
include/c_sip.inc
include/c_spfc.inc
include/c_temp.inc
include/c_trxv.inc
include/c_trxx.inc
include/c_trxxh.inc
include/c_trxy1.inc
include/c_trxy2.inc
include/d_arrays.inc
include/d_BF.inc
include/d_BFAlloc.inc
include/d_cleanup.inc
include/d_cleanup1.inc
include/d_compnam.inc
include/d_compname.inc
include/d_compnamm.inc
include/d_coordin.inc
include/d_disch.inc
include/d_dumm.inc
include/d_dumm1.inc
include/d_dumm1Alloc.inc
include/d_dumm1Dealloc.inc
include/d_dumm2.inc
include/d_dumm2Alloc.inc
include/d_dumm2Dealloc.inc
include/d_dumm3.inc
include/d_dumm3Alloc.inc
include/d_dumm3Dealloc.inc
include/d_equat.inc
include/d_equats.inc
include/d_hcon.inc
include/d_idumm.inc
include/d_idummAlloc.inc
include/d_ihdum.inc
include/d_ihdummAlloc.inc
include/d_ihdummDealloc.inc
include/d_isdum.inc
include/d_isdummAlloc.inc
include/d_isdummDealloc.inc
include/d_itemblo.inc
include/d_itembloAlloc.inc
include/d_itembloDealloc.inc
include/d_itemtxb.inc
include/d_itemtxbAlloc.inc
include/d_itemtxbDealloc.inc
include/d_jtxx.inc
include/d_kcon.inc
include/d_kdum.inc
include/d_kdumDealloc.inc
##include/d_modules.inc
include/d_mprop.inc
include/d_obsp.inc
include/d_obst.inc
include/d_phreecc.inc
include/d_pit.inc
include/d_plott.inc
include/d_press.inc
include/d_pricont.inc
include/d_ptet.inc
include/d_ptetAlloc.inc
include/d_react.inc
include/d_rpropsh.inc
include/d_rpropshAlloc.inc
include/d_rspac.inc
include/d_scon.inc
include/d_sconAlloc.inc
include/d_sip.inc
include/d_solindex.inc
include/d_solmassb.inc
include/d_spfc.inc
include/d_spfcAlloc.inc
include/d_temp.inc
include/d_tempcc.inc
include/d_trxv.inc
include/d_trxx.inc
include/d_trxxh.inc
include/d_trxy1.inc
include/d_trxy2.inc
Call_RM.F90
modules.f90
PhreeqcRM/src/RM_interface.F90
vs2dt3_3_reactive_withoutArgus1.f90
)

# executable
add_executable(vs2drt ${vs2drt_SOURCES})

# Disable deprecation warnings for standard C and STL functions in VS2005
# and later
IF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
ENDIF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)

# check for zlib
#set(ZLIB_ROOT "" CACHE PATH "Set to a zlib installation root")
if(NOT ZLIB_ROOT)
  if(EXISTS $ENV{ZLIB_ROOT})
    set(ZLIB_ROOT $ENV{ZLIB_ROOT})
  endif()
endif()
find_package(ZLIB QUIET)
if (ZLIB_FOUND)
  option(PHREEQCRM_USE_ZLIB "Use zlib compression for dump files" ON)
else()
  option(PHREEQCRM_USE_ZLIB "Use zlib compression for dump files" OFF)
endif()
if (PHREEQCRM_USE_ZLIB)
  find_package(ZLIB REQUIRED)
endif()
if (PHREEQCRM_USE_ZLIB AND ZLIB_FOUND)
  ADD_DEFINITIONS(-DUSE_GZ)
  include_directories(${ZLIB_INCLUDE_DIRS})
endif()

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

# dll/shared objects
OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
SET(LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  SET(LIB_TYPE SHARED)
endif()
add_library(libvs2drt ${LIB_TYPE} ${libvs2drt_SOURCES})
set_target_properties(libvs2drt PROPERTIES DEBUG_POSTFIX "d")

## project
#project(vs2drt Fortran)

#### source
###SET(vs2drt_SOURCES
###include/c_arrays.inc
###include/c_disch.inc
###include/c_dumm.inc
###include/c_dumm1.inc
###include/c_dumm2.inc
###include/c_equat.inc
###include/c_equats.inc
###include/c_hcon.inc
###include/c_idumm.inc
###include/c_ihdum.inc
###include/c_isdum.inc
###include/c_jtxx.inc
###include/c_kcon.inc
###include/c_kdum.inc
###include/c_mprop.inc
###include/c_obsp.inc
###include/c_obst.inc
###include/c_pit.inc
###include/c_plott.inc
###include/c_press.inc
###include/c_ptet.inc
###include/c_rprop.inc
###include/c_rpropsh.inc
###include/c_rspac.inc
###include/c_scon.inc
###include/c_sconItmax.inc
###include/c_sip.inc
###include/c_spfc.inc
###include/c_temp.inc
###include/c_trxv.inc
###include/c_trxx.inc
###include/c_trxxh.inc
###include/c_trxy1.inc
###include/c_trxy2.inc
###include/d_arrays.inc
###include/d_BF.inc
###include/d_BFAlloc.inc
###include/d_cleanup.inc
###include/d_cleanup1.inc
###include/d_compnam.inc
###include/d_compname.inc
###include/d_compnamm.inc
###include/d_coordin.inc
###include/d_disch.inc
###include/d_dumm.inc
###include/d_dumm1.inc
###include/d_dumm1Alloc.inc
###include/d_dumm1Dealloc.inc
###include/d_dumm2.inc
###include/d_dumm2Alloc.inc
###include/d_dumm2Dealloc.inc
###include/d_dumm3.inc
###include/d_dumm3Alloc.inc
###include/d_dumm3Dealloc.inc
###include/d_equat.inc
###include/d_equats.inc
###include/d_hcon.inc
###include/d_idumm.inc
###include/d_idummAlloc.inc
###include/d_ihdum.inc
###include/d_ihdummAlloc.inc
###include/d_ihdummDealloc.inc
###include/d_isdum.inc
###include/d_isdummAlloc.inc
###include/d_isdummDealloc.inc
###include/d_itemblo.inc
###include/d_itembloAlloc.inc
###include/d_itembloDealloc.inc
###include/d_itemtxb.inc
###include/d_itemtxbAlloc.inc
###include/d_itemtxbDealloc.inc
###include/d_jtxx.inc
###include/d_kcon.inc
###include/d_kdum.inc
###include/d_kdumDealloc.inc
#####include/d_modules.inc
###include/d_mprop.inc
###include/d_obsp.inc
###include/d_obst.inc
###include/d_phreecc.inc
###include/d_pit.inc
###include/d_plott.inc
###include/d_press.inc
###include/d_pricont.inc
###include/d_ptet.inc
###include/d_ptetAlloc.inc
###include/d_react.inc
###include/d_rpropsh.inc
###include/d_rpropshAlloc.inc
###include/d_rspac.inc
###include/d_scon.inc
###include/d_sconAlloc.inc
###include/d_sip.inc
###include/d_solindex.inc
###include/d_solmassb.inc
###include/d_spfc.inc
###include/d_spfcAlloc.inc
###include/d_temp.inc
###include/d_tempcc.inc
###include/d_trxv.inc
###include/d_trxx.inc
###include/d_trxxh.inc
###include/d_trxy1.inc
###include/d_trxy2.inc
###Call_RM.F90
###modules.f90
###PhreeqcRM/src/RM_interface.F90
###vs2dt3_3_reactive_withoutArgus1.f90
###)
###
#### executable
###add_executable(vs2drt ${vs2drt_SOURCES})

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


# subdirs
add_subdirectory(PhreeqcRM)

# link (zlib must be last on linux)
if (PHREEQCRM_USE_ZLIB AND ZLIB_FOUND)
  set(EXTRA_LIBS ${EXTRA_LIBS} ${ZLIB_LIBRARIES})
endif()
target_link_libraries(vs2drt ${EXTRA_LIBS})

