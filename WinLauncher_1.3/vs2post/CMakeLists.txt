cmake_minimum_required(VERSION 2.8)

#
# phasthdf
#
project(vs2post CXX)

include(GNUInstallDirs)

# defines

# include directories

# source
SET(vs2post_SOURCES
  phastexport.cpp
  phastexport.h
  phastexport.rc
  resource.h
  StdAfx.cpp
  StdAfx.h
  vs2post.ico
)


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

  # Set Runtime Library to Multi-threaded (/MT) 
  foreach(CompilerFlag ${CompilerFlags})
    if(${CompilerFlag} MATCHES "/MD")
      string(REGEX REPLACE "/MD" "/MT" ${CompilerFlag} "${${CompilerFlag}}")
    endif()
  endforeach()
endif()

add_executable(vs2post ${vs2post_SOURCES})

# link flags
if (MSVC)
  set_target_properties(vs2post PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
endif()

# install
install(TARGETS vs2post DESTINATION ${CMAKE_INSTALL_BINDIR})
