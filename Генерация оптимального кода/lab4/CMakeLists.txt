find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(LLVM_LINK_COMPONENTS
  Core
  ExecutionEngine
  Interpreter
  MC
  MCJIT
  Support
  nativecodegen
  )

add_executable(lab4 lab4.cpp)

llvm_map_components_to_libnames(llvm_libs
  core
  executionengine
  interpreter
  mc
  mcjit
  support
  native
  nativecodegen
  target
)

target_link_libraries(lab4 ${llvm_libs})
