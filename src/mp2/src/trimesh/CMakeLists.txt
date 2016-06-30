cmake_minimum_required (VERSION 2.8)
project(trimesh C)
add_library(trimesh SHARED triangle.c triangle.h trimesh.c trimesh.h)

# JNI
find_package(JNI REQUIRED)
if (JNI_FOUND)
  include_directories(${JNI_INCLUDE_DIRS})
  target_link_libraries(trimesh ${JNI_LIBRARIES})
endif ()