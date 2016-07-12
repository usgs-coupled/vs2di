cmake_minimum_required (VERSION 2.8)
project(vs2drtJni C CXX Fortran)

# fortran name mangling
include(FortranCInterface)
FortranCInterface_HEADER(FC.h MACRO_NAMESPACE "FC_" SYMBOLS
  FH_FinalizeFiles
  FH_ProcessRestartFiles
  FH_SetPointers
  FH_SetRestartName
  FH_WriteFiles
  FH_WriteBcRaw
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

# source files
SET(libvs2drt_SOURCES
  src/FileHandler.cpp
  src/FileHandler.h
  ${PROJECT_BINARY_DIR}/FC.h
)

include_directories("${PROJECT_BINARY_DIR}")

include_directories("${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src")
include_directories("${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/IPhreeqcPhast")
include_directories("${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc")
include_directories("${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp")
include_directories("${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common")
include_directories("${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PhreeqcKeywords")

add_library(libvs2drt STATIC ${libvs2drt_SOURCES})

# iphreeqc defs
ADD_DEFINITIONS(-DSWIG_SHARED_OBJ)
ADD_DEFINITIONS(-DUSE_PHRQ_ALLOC)

SET(PhreeqcRM_SOURCES
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/CSelectedOutput.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/CSelectedOutput.hxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/CVar.hxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/Debug.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/ErrorReporter.hxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/IPhreeqc.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/IPhreeqc.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/IPhreeqc.hpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/IPhreeqc_interface_F.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/IPhreeqc_interface_F.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/IPhreeqcCallbacks.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/IPhreeqcLib.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/advection.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/basicsubs.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ChartHandler.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ChartHandler.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ChartObject.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ChartObject.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cl1.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/Parser.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/Parser.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/PHRQ_base.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/PHRQ_base.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/PHRQ_io.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/PHRQ_io.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/phrqtype.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/Utils.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/common/Utils.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/CurveObject.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/CurveObject.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cvdense.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cvdense.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cvode.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cvode.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cxxKinetics.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cxxKinetics.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cxxMix.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/cxxMix.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/dense.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/dense.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Dictionary.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Dictionary.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/dumper.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/dumper.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Exchange.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Exchange.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ExchComp.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ExchComp.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/GasComp.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/GasComp.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/gases.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/GasPhase.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/GasPhase.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/global_structures.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/input.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/integrate.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/inverse.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ISolution.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ISolution.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ISolutionComp.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ISolutionComp.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/isotopes.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/kinetics.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/KineticsComp.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/KineticsComp.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/mainsubs.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/model.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/NA.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/NameDouble.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/NameDouble.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/NumKeyword.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/NumKeyword.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/nvector.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/nvector.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/nvector_serial.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/nvector_serial.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/parse.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PBasic.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PBasic.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/phqalloc.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/phqalloc.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Phreeqc.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Phreeqc.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PhreeqcKeywords/Keywords.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PhreeqcKeywords/Keywords.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PHRQ_io_output.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/pitzer.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/pitzer_structures.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PPassemblage.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PPassemblage.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PPassemblageComp.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/PPassemblageComp.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/prep.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Pressure.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Pressure.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/print.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Reaction.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Reaction.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/read.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/ReadClass.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/readtr.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/runner.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/runner.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SelectedOutput.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SelectedOutput.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Serializer.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Serializer.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/sit.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/smalldense.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/smalldense.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Solution.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Solution.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SolutionIsotope.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SolutionIsotope.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/spread.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SS.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SS.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SSassemblage.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SSassemblage.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SScomp.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SScomp.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/step.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/StorageBin.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/StorageBin.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/StorageBinList.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/StorageBinList.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/structures.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/sundialsmath.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/sundialsmath.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/sundialstypes.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Surface.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Surface.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SurfaceCharge.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SurfaceCharge.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SurfaceComp.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/SurfaceComp.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/System.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/System.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/tally.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Temperature.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Temperature.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/tidy.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/transport.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Use.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/Use.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/UserPunch.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/UserPunch.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/phreeqcpp/utilities.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/thread.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/Var.c
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/Var.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqc/Version.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqcPhast.cxx
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqcPhast.h
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqcPhastLib.cpp
  src/PhreeqcRM/src/IPhreeqcPhast/IPhreeqcPhastLib.h
  src/PhreeqcRM/src/IrmResult.h
  src/PhreeqcRM/src/PhreeqcRM.cpp
  src/PhreeqcRM/src/PhreeqcRM.h
  src/PhreeqcRM/src/RM_interface_C.cpp
  src/PhreeqcRM/src/RM_interface_C.h
  src/PhreeqcRM/src/RM_interface_F.cpp
  src/PhreeqcRM/src/RM_interface_F.h
)

add_library(PhreeqcRM STATIC ${PhreeqcRM_SOURCES})

SET(vs2drtf_SOURCES
  src/Call_RM.F90
  src/modules.f90
  src/vs2dt3_3_reactive_withoutArgus1.F90
  ${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/RM_interface.F90
)

# place [*.F[90]] files into "Source Files"
source_group("Source Files" FILES "src/Call_RM.F90")
source_group("Source Files" FILES "${PROJECT_SOURCE_DIR}/src/PhreeqcRM/src/RM_interface.F90")
source_group("Source Files" FILES "src/vs2dt3_3_reactive_withoutArgus1.F90")

add_library(vs2drtf STATIC ${vs2drtf_SOURCES})

SET(vs2drtJni_SOURCES
  vs2_vs2drt.h
  vs2dhJni.c
  ${PROJECT_BINARY_DIR}/FC.h
)

add_library(vs2drtJni SHARED ${vs2drtJni_SOURCES})

# JNI
find_package(JNI REQUIRED)
if (JNI_FOUND)
  include_directories(${JNI_INCLUDE_DIRS})
  target_link_libraries(vs2drtJni libvs2drt PhreeqcRM vs2drtf ${JNI_LIBRARIES})
endif ()

# Java
find_package(Java)
