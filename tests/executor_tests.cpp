#include "executor.hpp"

#include <gtest/gtest.h>

namespace {

TEST(ExecutorTests, DefaultTest) { ASSERT_EQ(default_function(), 42); }

} // namespace
