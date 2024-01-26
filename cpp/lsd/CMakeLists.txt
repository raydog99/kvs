cmake_minimum_required(VERSION 3.15)

# Find Cap'n Proto package
find_package(CapnProto REQUIRED)

# Set source files
set(SOURCE_FILES lsd.cpp)

# Generate C++ code from Cap'n Proto schema
capnp_generate_cpp(CAPNP_SOURCES CAPNP_HEADERS ChordProtocol.capnp)

# Add executable target
add_executable(lsd ${SOURCE_FILES} ${CAPNP_SOURCES})

# Include directories for Cap'n Proto generated files
target_include_directories(lsd PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

# Link against Cap'n Proto library
target_link_libraries(lsd PRIVATE capnp kj kj-http)

# Set C++ standard
set_target_properties(lsd PROPERTIES CXX_STANDARD 11)