include(FetchContent)

cmake_minimum_required(VERSION 3.14)

project(shell LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB_RECURSE SRC_CPP_FILES CONFIGURE_DEPENDS src/*.cpp)
file(GLOB_RECURSE TESTS_CPP_FILES CONFIGURE_DEPENDS tests/*.cpp)
file(GLOB_RECURSE HPP_FILES CONFIGURE_DEPENDS include/*.hpp)

add_compile_options(-g -Wall -Werror)

add_executable(${PROJECT_NAME} ${SRC_CPP_FILES})
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_SOURCE_DIR}/include)


add_custom_target(clang-format
  COMMAND
    clang-format
    -style=file
    -i
    ${SRC_CPP_FILES} ${TESTS_CPP_FILES} ${HPP_FILES}
  WORKING_DIRECTORY
    ${CMAKE_SOURCE_DIR}
  COMMENT
    "Running clang-format on sources..."
)

add_custom_target(clang-tidy
  COMMAND
    clang-tidy
    ${CPP_FILES} ${HPP_FILES}
    --
    -I ${CMAKE_SOURCE_DIR}/include
  WORKING_DIRECTORY
    ${CMAKE_SOURCE_DIR}
  COMMENT
    "Running clang-tidy on sources..."
)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        6910c9d9165801d8827d628cb72eb7ea9dd538c5 # release-1.16.0
)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

add_executable(tests ${SRC_CPP_FILES} ${TESTS_CPP_FILES})
target_include_directories(tests PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(tests PRIVATE gtest_main)
