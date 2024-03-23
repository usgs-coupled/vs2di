cmake_minimum_required(VERSION 2.8...3.20)
project(trimesh C)
add_library(trimesh SHARED triangle.c triangle.h trimesh.c trimesh.h)

# JNI
find_package(JNI REQUIRED)
if (JNI_FOUND)
  include_directories(${JNI_INCLUDE_DIRS})
  target_link_libraries(trimesh ${JNI_LIBRARIES})
endif ()

# Disable deprecation warnings for standard C and STL functions in VS2005
# and later
if(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
endif() 

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
endif()

install(TARGETS trimesh DESTINATION ${CMAKE_INSTALL_BINDIR})
