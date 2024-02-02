cmake_minimum_required(VERSION 3.15)

set(CMAKE_CXX_STANDARD 14)

# Add the source files for the chord library
set(CHORD_SRC
    server.h
    server.cpp
    finger_table.h
    finger_table.cpp
)

# Find Cap'n Proto package
find_package(CapnProto REQUIRED)

# Generate C++ code from Cap'n Proto schema
capnp_generate_cpp(CAPNP_SOURCES CAPNP_HEADERS ChordProtocol.capnp)

# Create the chord library
add_library(chord ${CHORD_SRC} ${CAPNP_SOURCES} ${CAPNP_HEADERS})

# Link against Cap'n Proto library
target_link_libraries(chord PRIVATE CapnProto::capnp-rpc kj kj-http)

# Include the headers from the chord library
target_include_directories(chord PRIVATE ${CMAKE_CURRENT_BINARY_DIR})