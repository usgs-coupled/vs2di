cmake_minimum_required (VERSION 2.8)
project(vs2dhJni C CXX Fortran)

# set standard directory locations
include(GNUInstallDirs)

# fortran name mangling
include(FortranCInterface)
FortranCInterface_HEADER(FC.h MACRO_NAMESPACE "FC_" SYMBOLS
  CLOSEIO
  DOHEAT
  DOSOLUTE
  DOTRANS
  GETCOMP
  GETCOMPCOUNT
  GETCONC
  GETDX
  GETDZ
  GETFLOWMBERR
  GETHEATTRANSMBERR
  GETKSAT
  GETMOIST
  GETNX
  GETNZ
  GETPHEAD
  GETSAT
  GETSOLTRANSMBERR
  GETSTEP
  GETSTIME
  GETTEMP
  GETTEX
  GETVX
  GETVZ
  RELEASEMEMORY
  SETUP
  STEP
)

# add definition for fortran name mangling
add_definitions(-DCMAKE_FC)

# Disable deprecation warnings for standard C and STL functions in VS2005
# and later
IF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
ENDIF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)

include_directories("${PROJECT_BINARY_DIR}")
include_directories("${PROJECT_SOURCE_DIR}/include")

##if (WIN32)
##  add_library(libvs2drt STATIC ${libvs2drt_SOURCES})
##else()
##  add_library(libvs2drt SHARED ${libvs2drt_SOURCES})
##endif()

SET(vs2dh_SOURCES
  vs2dh3_3.f
)

# place [*.F[90]] files into "Source Files"
###source_group("Source Files" FILES "src/Call_RM.F90")
###source_group("Source Files" FILES "${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/RM_interface.F90")
###source_group("Source Files" FILES "src/vs2dh3_3_reactive_withoutArgus1.F90")

if (WIN32)
  add_library(vs2dhf STATIC ${vs2dh_SOURCES})
else()
  add_library(vs2dhf SHARED ${vs2dh_SOURCES})
endif()

SET(vs2dhJni_SOURCES
  vs2dh.h
  vs2dhJni.c
  ${PROJECT_BINARY_DIR}/FC.h
)

add_library(vs2dhJni SHARED ${vs2dhJni_SOURCES})

# JNI
find_package(JNI REQUIRED)
if (JNI_FOUND)
  include_directories(${JNI_INCLUDE_DIRS})
  target_link_libraries(vs2dhJni vs2dhf ${JNI_LIBRARIES})
endif ()

# Java
find_package(Java)

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

if (WIN32)
  install(TARGETS vs2dhJni DESTINATION ${CMAKE_INSTALL_BINDIR})
else()
  install(TARGETS vs2dhJni DESTINATION ${CMAKE_INSTALL_BINDIR})
  ##install(TARGETS libvs2drt DESTINATION ${CMAKE_INSTALL_BINDIR})
  ##install(TARGETS vs2dhf    DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
