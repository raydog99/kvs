set(UTILS_SOURCES
    helper_functions.cpp
)

add_library(utils_library ${UTILS_SOURCES})

target_include_directories(utils_library PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})