cmake_minimum_required(VERSION 3.15)
project(chord)

set(CMAKE_CXX_STANDARD 14)

# Add the source files for the chord library
set(CHORD_SRC
    chord.h
    server.cpp
    server.h
)

# Create the chord library
add_library(chord ${CHORD_SRC})

# Include the headers from the chord library
target_include_directories(chord PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})