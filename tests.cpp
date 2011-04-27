#include "test.h"
#include "tests.h"
#include "print.h"
#include "parser.h"
#include "types.h"
#include "eval.h"
#include "primitives.h"

#define TEST_EVAL(expr, expect) TEST_STREQ(print(eval(parse(expr))), expect);
#define TEST_PARSE(expr, expect) TEST_STREQ(sprint(parse(expr)), expect);

static environment_t globals;

cons_t* symbol(const char* s)
{
  return symbol(s, &globals);
}

program_t* parse(const char *program)
{
  return parse(program, &globals);
}

void run_tests()
{
  load_default_defs(&globals);

  TEST_STREQ(format("a%sc%dd", "bb", 5), "abbc5d");
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

  TEST_STREQ(sprint(cons(integer(1), NULL)), "1");
  TEST_STREQ(sprint(cons(cons(integer(1)), NULL)), "(1)");
  TEST_STREQ(sprint(cons(cons(integer(1), integer(1)), NULL)), "(1 . 1)");

  TEST_STREQ(sprint(cons(cons(symbol("one"), symbol("two")))), "(one . two)");
  TEST_STREQ(sprint(cons(cons(integer(1), integer(2)))), "(1 . 2)");
  TEST_STREQ(sprint(cons(cons(integer(0), cons(integer(1), integer(2))))), "(0 1 . 2)");
  TEST_STREQ(sprint(cons(cons(symbol("zero"), cons(symbol("one"), symbol("two"))))), "(zero one . two)");

  /*
   * Common Lisp: (cons 1 (cons 2 nil))
   * Scheme:      (cons 1 (cons 2 '()))
   */
  TEST_STREQ(sprint(cons(cons(integer(1), cons(integer(2), NULL)))), "(1 2)");

  // (cons 1 (cons 2 (cons 3 nil)))
  TEST_STREQ(sprint(cons(cons(integer(1), cons(integer(2), cons(integer(3), NULL))))), "(1 2 3)");

  // (cons 0 (cons (cons 1 (cons 2 nil)) nil))
  TEST_STREQ(sprint(cons(cons(integer(0), cons(cons(integer(1), cons(integer(2), NULL)), NULL)), NULL)), "(0 (1 2))");

  TEST_STREQ(sprint(cons(list(integer(1)))), "(1)");
  TEST_STREQ(sprint(cons(list(integer(1), integer(2)))), "(1 2)");
  TEST_STREQ(sprint(cons(list(integer(1), list(integer(2), integer(3))))), "(1 (2 3))");
  TEST_STREQ(sprint(cons(list(integer(1), list(integer(2), list(integer(3), integer(4)))))), "(1 (2 (3 4)))");
  TEST_STREQ(sprint(cons(list(list(integer(1), integer(2)), integer(3)))), "((1 2) 3)");

  // (cons 1 (cons 2 (list 3 4)))
  TEST_STREQ(sprint(cons(cons(integer(1), cons(integer(2), list(integer(3), integer(4)))))), "(1 2 3 4)");

  // (cons (list 1 2) (list 3 4))
  TEST_STREQ(sprint(cons(cons(list(integer(1), integer(2)), list(integer(3), integer(4))))), "((1 2) 3 4)");

  // (cons (cons 1 (cons 2 nil)) 3)
  TEST_STREQ(sprint(cons(cons(cons(integer(1), cons(integer(2))), integer(3)))), "((1 2) . 3)");

  // (cons (list 1 2) 3)
  TEST_STREQ(sprint(cons(cons(list(integer(1), integer(2)), integer(3)))), "((1 2) . 3)");

  // (append (list 1 2) (list (list 4 5)))
  TEST_STREQ(sprint(cons(append(list(integer(1), integer(2)), list(list(integer(4), integer(5)))))), "(1 2 (4 5))");

  // (append (list 1 2) (list 3 4))
  TEST_STREQ(sprint(cons(append(list(integer(1), integer(2)), list(integer(3), integer(4))))), "(1 2 3 4)");

  // (cons 3 (cons 4 5)) 
  TEST_STREQ(sprint(cons(cons(integer(3), cons(integer(4), integer(5))))), "(3 4 . 5)");

  // (append (list 1) 2)
  TEST_STREQ(sprint(cons(append(list(integer(1)), integer(2)))), "(1 . 2)");

  // clisp: (cons (cons nil nil) nil), yields: ((NIL))
  TEST_STREQ(sprint(cons(cons(cons(NULL, NULL), NULL))), "(())");

  // (append nil (list 1 2))
  TEST_STREQ(sprint(cons(append(NULL, list(integer(1), integer(2))))), "(1 2)");

  // parser
  TEST_PARSE("(cons 1 2)", "(<closure> 1 2)");
  TEST_PARSE("(cOns 1 2)", "(cOns 1 2)");
  TEST_PARSE("(CONS 1 2)", "(CONS 1 2)");
  TEST_PARSE("(+ (* 1 2) 3)", "(<closure> (<closure> 1 2) 3)");
  TEST_PARSE("(fx-+ (fx-* 1 2) 3)", "(fx-+ (fx-* 1 2) 3)");
  TEST_PARSE("(1)", "(1)");
  TEST_PARSE("((1))", "((1))");
  TEST_PARSE("((1 2))", "((1 2))");
  TEST_PARSE("((1 2) 3)", "((1 2) 3)");
  TEST_PARSE("((a b) c)", "((a b) c)");
  TEST_PARSE("(a (b c) d)", "(a (b c) d)");
  TEST_PARSE("(a (B c) D)", "(a (B c) D)");
  TEST_PARSE("(display \"Hello\\nworld!\")))", "(<closure> \"Hello\\nworld!\")");
  TEST_PARSE("a", "a");
  TEST_PARSE("A", "A");
  TEST_PARSE("(1 2 3) (4 5 6)", "(1 2 3) (4 5 6)");
  TEST_PARSE("(1 2 3)\r\n(4 5 6)", "(1 2 3) (4 5 6)");
  TEST_PARSE("(display (string-append \"Hello\" \", \" \"world!\"))",
    "(<closure> (<closure> \"Hello\" \", \" \"world!\"))");

  // string operations
  TEST_EVAL("(->string 123)", "123");
  TEST_EVAL("(->string (list 1 2 (list 3 4)))", "1 2 (3 4)");
  TEST_EVAL("(->string (list 1 2 (list 3 (list 4 4 5) 4)))", "1 2 (3 (4 4 5) 4)");
  TEST_EVAL("(->string (list 1 2 (list 3 4 (* 3 30))))", "1 2 (3 4 90)");
  TEST_EVAL("(string-append \"one\" \"two\" \"three\")", "onetwothree");

  // side-effects / printing to console
  TEST_EVAL("(display 123)", "");
  TEST_EVAL("(display (+ 3 3 5))", "");
  TEST_EVAL("(display \"hello\")", "");

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
  TEST_EVAL("(integer?)", "#f");
  TEST_EVAL("(integer? 1)", "#t");
  TEST_EVAL("(integer? 0)", "#t");
  TEST_EVAL("(integer? a)", "#f");
  TEST_EVAL("(integer? 1.0)", "#f");
  TEST_EVAL("(integer? 1.0f)", "#f");
  TEST_EVAL("(integer? 10f)", "#f");
  TEST_EVAL("(integer? 10)", "#t");
  TEST_EVAL("(integer? 100)", "#t");
  TEST_EVAL("(integer? 00)", "#t");
  TEST_EVAL("(integer? 1234)", "#t");
  TEST_EVAL("(integer? (list 1 2))", "#f");
  TEST_EVAL("(integer? (+))", "#f");
  TEST_EVAL("(integer? +)", "#f");
  TEST_EVAL("(integer? -1)", "#t");
  TEST_EVAL("(integer? -0)", "#t"); // works with mit-scheme, chicken scheme
  TEST_EVAL("(integer? -123)", "#t");
  TEST_EVAL("(integer? -123.0)", "#f");
  TEST_EVAL("(integer? ())", "#f");

  TEST_EVAL("(procedure?)", "#f");
  TEST_EVAL("(procedure? 123)", "#f");
  TEST_EVAL("(procedure? abba-rules)", "#f");
  TEST_EVAL("(procedure? procedure?)", "#t");
  TEST_EVAL("(procedure? +)", "#t");
  TEST_EVAL("(procedure? list)", "#t");
  TEST_EVAL("(procedure? (list 1 2))", "#f");
  TEST_EVAL("(procedure? (+ 1 2))", "#f");

  results();
}
