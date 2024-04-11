add_executable(test_base_node test_base_node.cpp)
add_executable(test_client_node test_client_node.cpp)

# Find Cap'n Proto package
find_package(CapnProto CONFIG REQUIRED)

# Link against Cap'n Proto library
target_link_libraries(test_base_node PRIVATE chord utils_library CapnProto::capnp-rpc)
target_link_libraries(test_client_node PRIVATE chord utils_library CapnProto::capnp-rpc)

# Include the /chord directory as an include directory for headers
target_include_directories(test_base_node PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ../chord PRIVATE $<TARGET_PROPERTY:chord,BINARY_DIR>)
target_include_directories(test_client_node PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ../chord PRIVATE $<TARGET_PROPERTY:chord,BINARY_DIR>)