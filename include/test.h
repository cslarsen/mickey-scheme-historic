#include <stdexcept>
#include <string>

#define CATCH_ALL(expr) { try { expr; } catch (const std::exception& e) { fprintf(stderr, "  Exception: %s\n", e.what()); } }
#define TEST_TRUE(expr) { CATCH_ALL(test(expr, #expr);) }
#define TEST_FALSE(expr) { CATCH_ALL(test(expr == false, #expr);) }
#define TEST_STREQ(expr, expected) { CATCH_ALL(test_streq(#expr, expr, expected);) }

void test_streq(const std::string& code, const std::string& actual, const std::string& expected);
void test(bool result, const char *descr);
void results();
