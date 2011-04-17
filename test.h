#define TEST_TRUE(expr) { test(expr, #expr); }
#define TEST_FALSE(expr) { test(expr == false, #expr); }

void test(bool result, const char *descr);
void results();
