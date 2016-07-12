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
phasthdf_version.h
resource.h
StdAfx.cpp
StdAfx.h
vs2post.ico
)

add_executable(vs2post ${vs2post_SOURCES})

# link flags
set_target_properties(vs2post PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")

# install
install(TARGETS vs2post DESTINATION ${CMAKE_INSTALL_BINDIR})
