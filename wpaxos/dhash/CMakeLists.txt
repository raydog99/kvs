cmake_minimum_required(VERSION 3.15)

set(CMAKE_CXX_STANDARD 14)

# Add the source files for the dhash library
set(DHASH_SRC
    artNode.h
    artTree.h
    artTree.cpp
)

# Create the dhash library
add_library(dhash ${DHASH_SRC})