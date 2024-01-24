cmake_minimum_required(VERSION 3.15)
project(test)

# Set C++ standard and compiler flags
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")

# Find the chord library
find_library(CHORD_LIB chord PATHS ${CMAKE_CURRENT_SOURCE_DIR}/../chord)

if(NOT CHORD_LIB)
    message(FATAL_ERROR "Chord library not found.")
endif()

# Define source files
set(SRC test_base_protocol.cpp)

# Find Cap'n Proto package
find_package(CapnProto REQUIRED)

# Create an executable target
add_executable(test_base_protocol ${SRC})

# Link the chord library to the test executable
target_link_libraries(test_base_protocol PUBLIC ${CHORD_LIB})

# Include the headers from the chord library
target_include_directories(test_base_protocol PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../chord)

# Link against Cap'n Proto library
target_link_libraries(test_base_protocol PRIVATE CapnProto::capnp)

# Optional: Define a custom target for running the executable
add_custom_target(run_test_base_protocol
    COMMAND ./test_base_protocol
    DEPENDS test_base_protocol
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)