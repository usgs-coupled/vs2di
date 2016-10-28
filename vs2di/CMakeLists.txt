cmake_minimum_required(VERSION 2.8)

project(jars)

set(NB_FILES
  "${CMAKE_CURRENT_BINARY_DIR}/nbproject/build-impl.xml"
  "${CMAKE_CURRENT_BINARY_DIR}/nbproject/genfiles.properties"
)

set(HELP_FILES
  "${CMAKE_CURRENT_BINARY_DIR}/help/vs2dhiHelp/build.xml"
  "${CMAKE_CURRENT_BINARY_DIR}/help/vs2dtiHelp/build.xml"
  "${CMAKE_CURRENT_BINARY_DIR}/help/vs2PostHelp/build.xml"
)

set(LIB_FILES
  "${CMAKE_CURRENT_BINARY_DIR}/lib/jh.jar"
)

set(SRC_FILES
  "${CMAKE_CURRENT_BINARY_DIR}/src/images"
  "${CMAKE_CURRENT_BINARY_DIR}/src/mp2"
  "${CMAKE_CURRENT_BINARY_DIR}/src/vs2"
)

add_custom_command(OUTPUT ${NB_FILES} ${HELP_FILES} ${LIB_FILES} ${SRC_FILES}
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/help"       "${CMAKE_CURRENT_BINARY_DIR}/help"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/nbproject"  "${CMAKE_CURRENT_BINARY_DIR}/nbproject"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/lib"        "${CMAKE_CURRENT_BINARY_DIR}/lib"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/src"        "${CMAKE_CURRENT_BINARY_DIR}/src"
  COMMAND ${CMAKE_COMMAND} -E copy           "${CMAKE_CURRENT_SOURCE_DIR}/build.xml"  "${CMAKE_CURRENT_BINARY_DIR}/build.xml"
)

set(JAR
  "${CMAKE_CURRENT_BINARY_DIR}/dist/vs2drti.jar"
)

set(LIB_JARS
  "${CMAKE_CURRENT_BINARY_DIR}/dist/lib/jh.jar"
  "${CMAKE_CURRENT_BINARY_DIR}/dist/lib/vs2dhiHelp.jar"
  "${CMAKE_CURRENT_BINARY_DIR}/dist/lib/vs2dtiHelp.jar"
  "${CMAKE_CURRENT_BINARY_DIR}/dist/lib/vs2PostHelp.jar"
)

set(CP
  "-Dlibs.CopyLibs.classpath=c:\jars\org-netbeans-modules-java-j2seproject-copylibstask.jar"
)

add_custom_command(OUTPUT ${JAR} ${LIB_JARS}
                   COMMAND ant ${CP} jar
                   COMMENT "Build jars"
)

add_custom_target(ANT_JAR DEPENDS ${NB_FILES} ${HELP_FILES} ${LIB_FILES} ${SRC_FILES} ${JARS} ${LIB_JARS})

install(FILES ${JAR}
        DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(FILES ${LIB_JARS}
        DESTINATION ${CMAKE_INSTALL_BINDIR}/lib
)
