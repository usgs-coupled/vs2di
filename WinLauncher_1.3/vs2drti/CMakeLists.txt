cmake_minimum_required(VERSION 2.8)

#
# phasthdf
#
project(vs2drti CXX)

include(GNUInstallDirs)

# defines

# include directories

# source
SET(vs2drti_SOURCES
phastexport.cpp
phastexport.h
phastexport.rc
phasthdf_version.h
resource.h
StdAfx.cpp
StdAfx.h
vs2drti.ico
)

add_executable(vs2drti ${vs2drti_SOURCES})

# link flags
set_target_properties(vs2drti PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")

# install
install(TARGETS vs2drti DESTINATION ${CMAKE_INSTALL_BINDIR})
