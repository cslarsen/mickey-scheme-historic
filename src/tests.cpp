/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include "test.h"
#include "tests.h"
#include "print.h"
#include "parser.h"
#include "types.h"
#include "eval.h"
#include "module_base.h"
#include "module_math.h"
#include "module_assert.h"
#include "exceptions.h"
#include "options.h"

static environment_t globals;

static cons_t* symbol(const char* s)
{
  return symbol(s, &globals);
}

static program_t* parse(const char *program)
{
  return parse(program, &globals);
}

static std::string parse_eval_print(const char* expr)
{
  program_t *p = parse(expr);
  cons_t *r = eval(car(p->root), &globals);
  return print(r);
}

static std::string parse_eval_sprint(const std::string& expr)
{
  program_t *p = parse(expr.c_str());
  cons_t *r = eval(car(p->root), &globals);
  return sprint(r);
}

static std::string parse_sprint(const std::string& expr)
{
  return sprint(parse(expr.c_str())->root);
}

#define TEST_REPL(expr, expect) TEST_STREQ(parse_eval_sprint(expr), expect);
#define TEST_EVAL(expr, expect) TEST_STREQ(parse_eval_print(expr), expect);
#define TEST_PARSE(expr, expect) TEST_STREQ(parse_sprint(expr), expect);

void run_tests()
{
  reset_tests();

  import_defaults(&globals, global_opts.lib_path);
  import(&globals, exports_base);
  import(&globals, exports_math);
  import(&globals, exports_assert);

  { std::string format_abbc5d = format("a%sc%dd", "bb", 5); 
    TEST_STREQ(format_abbc5d, "abbc5d"); }

  TEST_STREQ(decode_literal_string("\"1 \\\"quo\\\" 2\""), "1 \"quo\" 2");

  TEST_TRUE(isatom("a"));
  TEST_TRUE(isatom("ab"));
  TEST_TRUE(isatom("ab12"));
  TEST_TRUE(isatom("a1"));
  TEST_FALSE(isatom("1"));
  TEST_FALSE(isatom("1a"));
  TEST_FALSE(isatom("12b"));
  TEST_FALSE(isatom("-"));
  TEST_FALSE(isatom("a-"));
  TEST_FALSE(isatom("a1-c"));
  TEST_FALSE(isatom("a1c-"));

  TEST_TRUE(isfloat("123.0"));
  TEST_FALSE(isfloat("123"));
  TEST_TRUE(isfloat("123f"));
  TEST_FALSE(isfloat("123ff"));
  TEST_TRUE(isfloat("1.0"));
  TEST_FALSE(isfloat("1.0."));
  TEST_FALSE(isfloat("1.."));
  TEST_FALSE(isfloat(".1.."));
  TEST_TRUE(isfloat(".1"));
  TEST_TRUE(isfloat(".1f"));
  TEST_TRUE(isfloat("1f"));
  TEST_TRUE(isfloat("0f"));
  TEST_TRUE(isfloat("1.0f"));
  TEST_TRUE(isfloat("+1.0f"));
  TEST_TRUE(isfloat("-1.0f"));
  TEST_TRUE(isfloat("-1.0"));
  TEST_TRUE(isfloat("-10f"));
  TEST_FALSE(isfloat("-10"));

  TEST_STREQ(sprint(integer(1)), "1");
  TEST_STREQ(sprint(cons(integer(1), NULL)), "(1)");
  TEST_STREQ(sprint(cons(integer(1), integer(1))), "(1 . 1)");

  TEST_STREQ(sprint(cons(symbol("one"), symbol("two"))), "(one . two)");
  TEST_STREQ(sprint(cons(integer(1), integer(2))), "(1 . 2)");
  TEST_STREQ(sprint(cons(integer(0), cons(integer(1), integer(2)))), "(0 1 . 2)");
  TEST_STREQ(sprint(cons(symbol("zero"), cons(symbol("one"), symbol("two")))), "(zero one . two)");

  /*
   * Common Lisp: (cons 1 (cons 2 nil))
   * Scheme:      (cons 1 (cons 2 '()))
   */
  TEST_STREQ(sprint(cons(integer(1), cons(integer(2), NULL))), "(1 2)");

  // (cons 1 (cons 2 (cons 3 nil)))
  TEST_STREQ(sprint(cons(integer(1), cons(integer(2), cons(integer(3), NULL)))), "(1 2 3)");

  // (cons 0 (cons (cons 1 (cons 2 nil)) nil))
  TEST_STREQ(sprint(cons(integer(0), cons(cons(integer(1), cons(integer(2), NULL)), NULL))), "(0 (1 2))");

  TEST_STREQ(sprint(list(integer(1))), "(1)");
  TEST_STREQ(sprint(list(integer(1), integer(2))), "(1 2)");
  TEST_STREQ(sprint(list(integer(1), list(integer(2), integer(3)))), "(1 (2 3))");
  TEST_STREQ(sprint(list(integer(1), list(integer(2), list(integer(3), integer(4))))), "(1 (2 (3 4)))");
  TEST_STREQ(sprint(list(list(integer(1), integer(2)), integer(3))), "((1 2) 3)");

  // (cons 1 (cons 2 (list 3 4)))
  TEST_STREQ(sprint(cons(integer(1), cons(integer(2), list(integer(3), integer(4))))), "(1 2 3 4)");

  // (cons (list 1 2) (list 3 4))
  TEST_STREQ(sprint(cons(list(integer(1), integer(2)), list(integer(3), integer(4)))), "((1 2) 3 4)");

  // (cons (cons 1 (cons 2 nil)) 3)
  TEST_STREQ(sprint(cons(cons(integer(1), cons(integer(2))), integer(3))), "((1 2) . 3)");

  // (cons (list 1 2) 3)
  TEST_STREQ(sprint(cons(list(integer(1), integer(2)), integer(3))), "((1 2) . 3)");

  // (append (list 1 2) (list (list 4 5)))
  TEST_STREQ(sprint(append(list(integer(1), integer(2)), list(list(integer(4), integer(5))))), "(1 2 (4 5))");

  // (append (list 1 2) (list 3 4))
  TEST_STREQ(sprint(append(list(integer(1), integer(2)), list(integer(3), integer(4)))), "(1 2 3 4)");

  // (cons 3 (cons 4 5)) 
  TEST_STREQ(sprint(cons(integer(3), cons(integer(4), integer(5)))), "(3 4 . 5)");

  // (append (list 1) 2)
  TEST_STREQ(sprint(append(list(integer(1)), integer(2))), "(1 . 2)");

  // clisp: (cons (cons nil nil) nil), yields: ((NIL)) ??
  TEST_STREQ(sprint(cons(cons(NULL, NULL), NULL)), "(())");
  TEST_STREQ(sprint(cons(cons(cons(NULL, NULL), NULL))), "((()))");

  // (append nil (list 1 2))
  TEST_STREQ(sprint(append(list(NULL), list(integer(1), integer(2)))), "(1 2)");

  // parser
  TEST_PARSE("(cOns 1 2)", "((cOns 1 2))");
  TEST_PARSE("(CONS 1 2)", "((CONS 1 2))");
  TEST_PARSE("(fx-+ (fx-* 1 2) 3)", "((fx-+ (fx-* 1 2) 3))");
  TEST_PARSE("(1)", "((1))");
  TEST_PARSE("((1))", "(((1)))");
  TEST_PARSE("((1 2))", "(((1 2)))");
  TEST_PARSE("((1 2) 3)", "(((1 2) 3))");
  TEST_PARSE("((a b) c)", "(((a b) c))");
  TEST_PARSE("(a (b c) d)", "((a (b c) d))");
  TEST_PARSE("(a (B c) D)", "((a (B c) D))");
  TEST_PARSE("a", "(a)");
  TEST_PARSE("A", "(A)");
  TEST_PARSE("(1 2 3) (4 5 6)", "((1 2 3) (4 5 6))");
  TEST_PARSE("(1 2 3)\r\n(4 5 6)", "((1 2 3) (4 5 6))");

  // string operations
  TEST_EVAL("(number->string 123)", "123");
  TEST_EVAL("(list 1 2 (list 3 4))", "(1 2 (3 4))");
  TEST_EVAL("(list 1 2 (list 3 (list 4 4 5) 4))", "(1 2 (3 (4 4 5) 4))");
  TEST_EVAL("(list 1 2 (list 3 4 (* 3 30)))", "(1 2 (3 4 90))");
  TEST_EVAL("(string-append \"one\" \"two\" \"three\")", "onetwothree");

  // side-effects / printing to console
  TEST_EVAL("(display 123)", "");
  TEST_EVAL("(display (+ 3 3 5))", "");
  TEST_EVAL("(display \"hello\")", "");

  // edge cases: car, cdr, etc
  TEST_EVAL("(cdar (list (list 1)))", "()");
  TEST_EVAL("(cddr (list 1 (list 2 3 4)))", "()");
  TEST_EVAL("(cddr (list 1 (list 2 3 4) (list 5 6)))", "((5 6))");

  // plus operator
  TEST_EVAL("(+ 0)", "0");
  TEST_EVAL("(+ 1)", "1");
  TEST_EVAL("(+ 0 1)", "1");
  TEST_EVAL("(+ 1 2)", "3");
  TEST_EVAL("(+ 1 2 3)", "6");
  TEST_EVAL("(+ -1)", "-1"); 
  TEST_EVAL("(+ 5 -2 3)", "6"); 
  TEST_EVAL("(+ -1 -3 -7)", "-11"); 
  TEST_EVAL("(+ (* 5 5) 4)", "29"); 
  TEST_EVAL("(+ (* 5 5) 4 12)", "41"); 

  // predicates
  TEST_EVAL("(symbol? 1)", "#f");
  TEST_EVAL("(symbol? 10)", "#f");
  TEST_EVAL("(symbol? (quote a))", "#t");
  TEST_EVAL("(symbol? (quote abba))", "#t");
  TEST_EVAL("(symbol? (list))", "#f");
  TEST_EVAL("(symbol? +)", "#f");
  TEST_EVAL("(symbol? list)", "#f");
  TEST_EVAL("(symbol? \"hey\")", "#f");

  TEST_EVAL("(integer?)", "#f");
  TEST_EVAL("(integer? 1)", "#t");
  TEST_EVAL("(integer? 0)", "#t");
  TEST_EVAL("(integer? (quote a))", "#f");
  TEST_EVAL("(integer? 1.0)", "#f");
  TEST_EVAL("(integer? 1.0f)", "#f");
  TEST_EVAL("(integer? 10f)", "#f");
  TEST_EVAL("(integer? 10)", "#t");
  TEST_EVAL("(integer? 100)", "#t");
  TEST_EVAL("(integer? 00)", "#t");
  TEST_EVAL("(integer? 1234)", "#t");
  TEST_EVAL("(integer? (list 1 2))", "#f");
  TEST_EVAL("(integer? (+))", "#t");
  TEST_EVAL("(integer? +)", "#f");
  TEST_EVAL("(integer? -1)", "#t");
  TEST_EVAL("(integer? -0)", "#t"); // works with mit-scheme, chicken scheme
  TEST_EVAL("(integer? -123)", "#t");
  TEST_EVAL("(integer? -123.0)", "#t");
  TEST_EVAL("(integer? ())", "#f");
  TEST_EVAL("(integer? \"hey\")", "#f");

  TEST_EVAL("(null? 0)", "#f");
  TEST_EVAL("(null? 1)", "#f");
  TEST_EVAL("(null? (quote a))", "#f");
  TEST_EVAL("(null? (list))", "#t");
  TEST_EVAL("(null? (car (list 1)))", "#f"); // -- car/cdr doesn't work yet (TODO)
  TEST_EVAL("(null? (cdr (list 1)))", "#t");

  TEST_EVAL("(procedure? #t)", "#f");
  TEST_EVAL("(procedure? 123)", "#f");
  TEST_EVAL("(procedure? (quote abba-rules))", "#f");
  TEST_EVAL("(procedure? procedure?)", "#t");
  TEST_EVAL("(procedure? +)", "#t");
  TEST_EVAL("(procedure? list)", "#t");
  TEST_EVAL("(procedure? (list 1 2))", "#f");
  TEST_EVAL("(procedure? (+ 1 2))", "#f");

  TEST_EVAL("(pair? (list 1 2 3))", "#t");
  TEST_EVAL("(pair? #t)", "#f");
  TEST_EVAL("(pair? 123)", "#f");
  TEST_EVAL("(pair? (quote abba))", "#f");
  TEST_EVAL("(pair? (+ 1 2))", "#f"); // cause "(+ 1 2)" evaluates to "3"
  TEST_EVAL("(pair? (list 1 2))", "#t");
  TEST_EVAL("(pair? (list 1))", "#t");
  TEST_EVAL("(pair? (+))", "#f");
  TEST_EVAL("(pair? +)", "#f");
  TEST_EVAL("(pair? (list))", "#f");

  TEST_EVAL("(list? 1)", "#f");
  TEST_EVAL("(list? (+ 1 2))", "#f");
  TEST_EVAL("(list? (list 1 2))", "#t");
  TEST_EVAL("(list? (list 1))", "#t");
  TEST_EVAL("(list? (list))", "#t");
  TEST_EVAL("(list? (quote a))", "#f");
  TEST_EVAL("(list? \"hey\")", "#f");

  TEST_EVAL("(length (list))", "0");
  TEST_EVAL("(length (list 1))", "1");
  TEST_EVAL("(length (list 1 2))", "2");
  TEST_EVAL("(length (list 1 2 3))", "3");
  TEST_EVAL("(length (list 1 2 (list 3 4)))", "3");

  // difference between PAIR and LIST
  TEST_EVAL("(list? (list))", "#t");
  TEST_EVAL("(pair? (list))", "#f");

  // car/cdr and friends
  TEST_EVAL("(car (list 1 2))", "1");
  TEST_EVAL("(car (list 1))", "1");
  TEST_EVAL("(car (list 2))", "2");
  TEST_EVAL("(car (list 1 2 3))", "1");

  TEST_EVAL("(cdr (list 1 2))", "(2)");
  TEST_EVAL("(cdr (list 1))", "()");
  TEST_EVAL("(cdr (list 2))", "()");
  TEST_EVAL("(cdr (list 1 2 3))", "(2 3)");
  TEST_EVAL("(cdr (list 1 2 3 4))", "(2 3 4)");
  TEST_EVAL("(null? (cdr (list 1)))", "#t");

  TEST_EVAL("(caar (list (list 1 2 3) 4))", "1");
  TEST_EVAL("(caar (list (list 9 8 7) (list 1 2 3) 4))", "9");
  TEST_EVAL("(cadr (list (list 2 3 4) (list 7 4 5)))", "(7 4 5)");
  TEST_EVAL("(cdar (list (list 2 3 4) (list 7 4 5)))", "(3 4)");
  TEST_REPL("(cddr (list (list 2 3 4) (list 7 4 5)))", "()");
  TEST_EVAL("(cddr (list 1 2 3 4))", "(3 4)");
  TEST_EVAL("(cdr (cddr (list 1 2 3 4)))", "(4)");
  TEST_EVAL("(car (cdr (cddr (list 1 2 3 4))))", "4");

  // define
  TEST_EVAL("(define a 123)", "");
  TEST_EVAL("a", "123");
  TEST_EVAL("(+ 1 a)", "124");
  TEST_EVAL("(+ a a)", "246");
  TEST_EVAL("(define b 100)", "");
  TEST_EVAL("(+ a b)", "223");
  TEST_EVAL("(define hello (quote (html (body (p (\"Hello, world!\"))))))", "");
  TEST_EVAL("hello", "(html (body (p (Hello, world!))))");
  TEST_REPL("hello", "(html (body (p (\"Hello, world!\"))))");

  // quote
  TEST_EVAL("(eval (quote (list 1 2 (quote 3) 3)))", "(1 2 3 3)");

  results();
}
