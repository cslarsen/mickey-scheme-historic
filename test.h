#include <string>

#define TEST_TRUE(expr) { test(expr, #expr); }
#define TEST_FALSE(expr) { test(expr == false, #expr); }
#define TEST_STREQ(expr, expected) { test_streq(#expr, expr, expected); }

void test_streq(const std::string& code, const std::string& actual, const std::string& expected);
void test(bool result, const char *descr);
void results();
