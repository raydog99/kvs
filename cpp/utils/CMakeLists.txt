set(UTILS_SOURCES
	helperFunctions.h
    helperFunctions.cpp
)

add_library(utils_library ${UTILS_SOURCES})

target_include_directories(utils_library PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})