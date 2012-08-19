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

#define CATCH_ALL(expr) { expr; } /***/
#define TEST_TRUE(expr) { test(expr, #expr); } /***/
#define TEST_FALSE(expr) { test(expr == false, #expr); } /***/
#define TEST_STREQ(expr, expected) { test_streq(#expr, expr, expected); } /***/

void test_streq(const std::string& code, const std::string& actual, const std::string& expected);
void test(bool result, const char *descr);
void results();
void reset_tests();
