/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <stdexcept>
#include <string>

#ifndef NO_EXCEPTIONS
# define CATCH_ALL(expr) { try { expr; } catch (const std::exception& e) { printf("=> " #expr "\n"); fprintf(stderr, "   Exception: %s\n", e.what()); } }
# define TEST_TRUE(expr) { CATCH_ALL(test(expr, #expr);) }
# define TEST_FALSE(expr) { CATCH_ALL(test(expr == false, #expr);) }
# define TEST_STREQ(expr, expected) { CATCH_ALL(test_streq(#expr, expr, expected);) }
#else
# define CATCH_ALL(expr) { expr; } /***/
# define TEST_TRUE(expr) { test(expr, #expr); } /***/
# define TEST_FALSE(expr) { test(expr == false, #expr); } /***/
# define TEST_STREQ(expr, expected) { test_streq(#expr, expr, expected); } /***/
#endif

void test_streq(const std::string& code, const std::string& actual, const std::string& expected);
void test(bool result, const char *descr);
void results();
void reset_tests();
