# Define source files
set(TEST_SOURCES
    test_base_protocol.cpp
)

# Create an executable from the sources
add_executable(test_base_protocol ${TEST_SOURCES})

# Link the executable with the chord library
target_link_libraries(test_base_protocol PRIVATE chord)

# Include the /chord directory as an include directory for headers
target_include_directories(test_base_protocol PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# Find Cap'n Proto package
find_package(CapnProto REQUIRED)

# Link against Cap'n Proto library
target_link_libraries(test_base_protocol PRIVATE CapnProto::capnp)