cmake_minimum_required (VERSION 2.8...3.20)

project(vs2drti C CXX Fortran)

# set standard directory locations
include(GNUInstallDirs)

set(CPACK_GENERATOR WIX)
set(CPACK_PACKAGE_NAME "VS2DRTI")
set(CPACK_PACKAGE_NAME_LC "vs2drti")
set(CPACK_PACKAGE_VENDOR "U.S. Geological Survey")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "4")
set(CPACK_PACKAGE_VERSION_PATCH "0")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_WIX_UPGRADE_GUID "94135F18-3C4E-4096-B87B-7E10F4AF8599")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "USGS/${CPACK_PACKAGE_NAME_LC}-${CPACK_PACKAGE_VERSION}")
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME_LC}-${CPACK_PACKAGE_VERSION}-x64")
set(CPACK_WIX_LICENSE_RTF "${CMAKE_CURRENT_SOURCE_DIR}/msi/License.rtf")
set(CPACK_WIX_PRODUCT_ICON "${CMAKE_CURRENT_SOURCE_DIR}/WinLauncher_1.3/vs2drti/vs2drti.ico")
set(CPACK_WIX_PROGRAM_MENU_FOLDER "VS2DRTI ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}")

##set(CPACK_WIX_PATCH_FILE "${CMAKE_CURRENT_BINARY_DIR}/patch.xml")
##set(POST_ICON_SRC "${CMAKE_CURRENT_SOURCE_DIR}/WinLauncher_1.3/vs2post/vs2post.ico")
##configure_file(${CMAKE_CURRENT_SOURCE_DIR}/msi/patch.xml.in patch.xml)

##set(CPACK_WIX_EXTRA_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/msi/vs2_assoc.wxs")
set(CPACK_WIX_PATCH_FILE "${CMAKE_CURRENT_SOURCE_DIR}/msi/patch.xml")

if (WIN32)
  set(CPACK_WIX_TEMPLATE "${CMAKE_CURRENT_SOURCE_DIR}/msi/WIX.MSVC11.template.in")
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/vs2drtJni/src/_vs2012_x64/Release/vs2drt.exe
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/NOTICE.TXT
    DESTINATION .
    )
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/README_LINUX_BATCH.TXT
    DESTINATION .
    )
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/README_WINDOWS_BATCH.TXT
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    )  
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/README_WINDOWS.TXT
    DESTINATION .
    )
endif()

if (WIN32)
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/vs2drti1.4_examples/
    DESTINATION examples
    )
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc/
    DESTINATION doc
    )
else()
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/vs2drti1.4_examples/
    DESTINATION ${CMAKE_INSTALL_DOCDIR}/examples
    )
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc/
    DESTINATION ${CMAKE_INSTALL_DOCDIR}
    )
endif()

INCLUDE(CPack)

add_subdirectory(vs2drtJni)
add_subdirectory(src/mp2/src/trimesh)
if (WIN32)
  add_subdirectory(WinLauncher_1.3/vs2drti)
  add_subdirectory(WinLauncher_1.3/vs2post)
endif()
add_subdirectory(src)
add_subdirectory(vs2di)
add_subdirectory(vs2drtJni/src/PhreeqcRM/database)
