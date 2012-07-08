Mickey Scheme
=============

Mickey Scheme is an incomplete, slow and buggy implementation of R7RS Scheme
small.

Current Features
----------------

  * Most core Scheme functions
  * Quotation and quasiquotation
  * Most `let`-forms, including named let
  * Rest arguments (aka variadic functions)
  * Macros (while it _does_ work, it's still incomplete)
  * Lazy evaluation (although _without_ memoization, currently)
  * Experimental LLVM JIT compilation (for _one_ function only, currently)
  * Tail call eliminiation (yeah, neither the JVM nor Python has that,
    so that's at least _something_ to be a little proud of!)

Some of these are demonstrated in the [example code section](#examples).

Current Shortcomings
--------------------

  * It's __slow__: The code is completely interpreted, without _any_
    optimizations.  I have plans to change this.

  * It's __incomplete__: Some key Scheme features are still missing, and
    quite some R7RS library functions.

  * It's __buggy__: There are inherent bugs in the engine as well as
    erronous implementations of library functions.

  * It does not collect __garbage__: Currently, there is no garbage
    collector.  Adding a simple mark-and-sweep GC is trivial, though, so
    I'll get to it once I think it's important enough.

Compiling
---------

Run `make -j check` to perform a simple check, then `./mickey` to play with a REPL.

The project is not really release-ready, so there is no use of autotools.
The Makefile assumes you have llvm-g++ installed.  If you want to compile
using gcc, do something like:

    CXX=g++ make -ej

to compile using `g++` and in parallel.

Feature flags
-------------

  * `-DBOEHM_GC` and `-lgc` for Bohem-Demers-Weiser garbage collector
     (doesn't currently work very well, though!)

  * `-DUSE_READLINE` and `-lreadline` for readline support (including TAB
     completion)

License
-------

Distributed under the modified BSD license.

Author
------

Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>

http://csl.sublevel3.org

## Examples

Here are a few example code snippets for Mickey Scheme.  

Besides demonstrating the basic capabilities of Mickey, it also serves as a
kind of soft introduction to Scheme.

First, let's start `mickey`.

    $ ./mickey 
                                                                      _
    Mickey Scheme (C) 2011 Christian Stigen Larsen                     \
    4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.9.00)      /\
    Readline 4.2                                                      /  \_
                                                                            
    Loaded 146 definitions
    Execute (exit [ code ]) to quit
    You can also (run-tests) and (list-globals)

    mickey> 

This is the REPL, or _read-evaluate-print loop_.  Now, let's type some code.

    mickey> (display "Hello, world!\n")
    Hello, world!

## Lambda

Let's play with lambdas.  First we'll create a lambda to square a
number and execute it on the fly.

    mickey> ((lambda (x) (* x x)) 12)
    144

We can it to a variable as well.  Let's bind it to the variable `square`.

    mickey> (define square (lambda (x) (* x x)))
    mickey> (square 12)
    144
    mickey> (square 3.1415)
    9.86902

Let's have some fun with lambdas.  Let's create
a function that creates other functions.

We'll create a general function `make-adder` that
creates a function that can add a static number to another
number.

    mickey> (define make-adder
              (lambda (frozen-number)
                (lambda (x)
                  (+ x frozen-number))))

As you can see, we give it a `frozen-number` and it returns
a lambda that takes a number `x` and adds the two together.

So to make a function that adds 5 to its argument, we simply
do

    mickey> (define add5 (make-adder 5))
    mickey> (add5 10)
    15

Likewise

    mickey> (define add17 (make-adder 17))
    mickey> (add17 10)
    27

Writing `(define (lambda (x) ...))` is tedious, so a
shorter variant is available:

    mickey> (define (cube x) (* x x x))
    mickey> (cube 101)
    1030301

Mickey uses GNU readline, so it offers both tab completion and history.  And
it actually works, too.  Here we write `(ca` and hit `TAB` two times to see
available definitions.

    mickey> (ca
    caaar  caadr  caar   cadr   car  

Here are `car` and `cdr`.  They extract the first and remaining items in a
list.

    mickey> (car '(1 2 3))
    1
    mickey> (cdr '(1 2 3)))
    (2 3)

I like the old school "car" and "could-er" forms because you can compose them, so that you can extract the `car`
of the `cdr` like so:

    mickey> (cadr '(1 2 3))
    2

They might not be very interesting for flat lists, but they shine for
accessing trees.

## Arithmetic

Here is some simple arithmetic.

    mickey> (+ 1 2 3 4 5 6 7 8 9 10)
    55

Summation of sequences can be calculated more quickly with

    mickey> (define (seq-sum n)
              (* n (/ (+ n 1) 2)))

which gives us

    mickey> (seq-sum 10)
    55
    mickey> (seq-sum 100)
    5050
    mickey> (seq-sum 127)
    8128

## Let-forms

Here is an example of the "let star" form.  It creates a local variable
scope, and evaluates them in the given order.

    mickey> (let* ((x 2)
                   (y 3)
                   (z (* x y)))
                   (display z))

This, of course, prints `6`.

There is also a `letrec` form that allows for mutually recursive
definitions.  Or in plain english, expressions that refer to each other.

The typical example of this is to implement `even?` and `odd?` in terms of
each other:

* A number is _even_ if the preceding number is odd, and
* A number is _odd_ if the preceding number is even

Since our definition is going to be mutually recursive, we need to handle
base cases of zero (negative values will make the code loop forever, though).

Let's write that out, along with a `check-number` function.

    (letrec
      ((even? (lambda (n)
                (if (zero? n) #t
                    (odd? (- n 1)))))

       (odd? (lambda (n)
               (if (zero? n) #f
                   (even? (- n 1)))))

       (check-number
         (lambda (n)
           (display `(The number ,n is ,(if (even? n) 'even 'odd)))
           (newline))))

       (check-number 2)
       (check-number 3)
       (check-number 88)
       (check-number 99))

If you run the above code in `mickey`, you'll get this output:

    (The number 2 is even)
    (The number 3 is odd)
    (The number 88 is even)
    (The number 99 is odd)

## Macros

Now, Scheme doesn't have a `when` function.  The `when` function checks
whether the first argument is true.  If it is, then it will evaluate --- or
execute --- the code given in the remaining arguments.

You can't do this with a simple function, because it would *always* evaluate
the code body.  As an example, let's say we have a boolean variable
`green-light`.  If it's `true`, we'll format the hard drive:

    (when green-light (format-drive))

If `when` was implemented as a function, it would always format the hard drive,
because function parameters must be evaluated _before_ entering the function
itself.

It is clear that we need a way to _control evaluation_.  Scheme's macros
will let ut do exactly that.

So let's implement `when` as a hygienic macro.

    mickey> (define-syntax when
              (syntax-rules ()
                ((when test expr ...)
                  (if test (begin expr ...)))))

That's it.  To demonstrate that we control the evaluation, let's create a
function with a side effect that prints to the console when it's evaluated.

    mickey> (define (say-hello) (display "Hello\n"))
    mickey> (say-hello)
    Hello

Calling

    mickey> (when #f (say-hello))

does not print anything, which is good.  In contrast,

    mickey> (when #t (say-hello))
    Hello

does indeed print to the console.

## Quotation

Now let's try some examples with quasi-quotation.  Since Scheme is a
symbolic language, we can easily create syntax trees for languages like SQL
by just using quotation.  But sometimes we'll want to embed actual
computations into them, so therefore we can use the specual `unquote` prefix
with a comma.

Here is an example of just that.

    mickey> (define (sql-get-user name)
              `(select * from user where name = ,name))

*Note*: _There is currently a bug in the mickey REPL, so that
quasiquotation requires an extra closing parenthesis to parse.  
This bug is not present if you run this example from a file, though._

Running the function should be self-explanatory.

    mickey> (sql-get-user "foo")
    (select * from user where name = "foo")

Furthermore, sometimes we want to splice two lists together when we quote.
We can do that by using unquote splice, or the `,@` prefix.

    mickey> (define date '(2012 05 17))
    mickey> date
    (2012 5 17)
    mickey> `(here is a date: ,@date)
    (here is a date: 2012 5 17)

## Lazy evaluation

Mickey Scheme also supports delayed -- or lazy -- evaluation.  That is,
computations that are not executed right away.

In fact, all languages that support evaluation control (for instance, via a
macro facility) and first class closures should be able to *implement* lazy
evaluation without any external library.

Let's create a list `queue` that contains some code we want to execute at a
later time.

    (define queue
      (list (delay (display "One! "))
            (delay (display "Three! "))
            (delay (display "Two! "))))

Now we want to execute the code in the list, but reordered so that it will
print "One! Two! Three!":

    (force (list-ref queue 0))
    (force (list-ref queue 2))
    (force (list-ref queue 1))

This outputs:

    One! Two! Three!

Note that Mickey Scheme's `force` does not currently memoize its computations,
as the standard requires (but that's easy to fix).

# Extensions

For debugging purposes, I've added some extension functions only available
to Mickey Scheme.

## (:syntax-expand _code_)

If you want to see how a macro is expanded, you can use
`(:syntax-expand ...)`.  Below is an example.

    mickey> (define-syntax my-when
      (syntax-rules ()
          ((my-when test expr ...)
                (if test (begin expr ...)))))
    mickey> (:syntax-expand '(my-when #t 123))
    (if #t (begin 123))
    mickey> (:syntax-expand '(my-when #f 123))
    (if #f (begin 123))

## (:debug ...)

Returns string with printable debug information.

    mickey> (display (:debug "foo"))
    adr=0x7fe501536560 type=pair    car=0x7fe501536340 cdr=0x7fe501536540
    adr=0x7fe501536340 type=string  value='foo'
    adr=0x7fe501536540 type=nil   

## (:type-of ...)

Prints what type Mickey determines the expression to be.

    mickey> (:type-of 123)
    integer
    mickey> (:type-of '())
    pair
    mickey> (:type-of "hey")
    string

## (:list->dot ...)

Convert the given expressions to Graphviz dot(1) format, which will display
a graph of the cons cells.

    mickey> (display (:list->dot '(root (left-child (left-grandchild) (right-child)))))
    digraph Scheme {
      "0x7fefeaceaca0":head -> "0x7fefeaceab40":head ["ol"="box"];
      "0x7fefeaceab40":head -> "0x7fefeace9cf0" ["ol"="box"];
      "0x7fefeace9cf0" [label="root", shape="none"];
      "0x7fefeaceab40":tail -> "0x7fefeaceab20":head ["ol"="box"];
      "0x7fefeaceab20":head -> "0x7fefeaceab00":head ["ol"="box"];
      "0x7fefeaceab00":head -> "0x7fefeace9d50" ["ol"="box"];
      "0x7fefeace9d50" [label="left-child", shape="none"];
      "0x7fefeaceab00":tail -> "0x7fefeaceaae0":head ["ol"="box"];
      "0x7fefeaceaae0":head -> "0x7fefeacea9c0":head ["ol"="box"];
      "0x7fefeacea9c0":head -> "0x7fefeacea020" ["ol"="box"];
      "0x7fefeacea020" [label="left-grandchild", shape="none"];
      "0x7fefeacea9c0" [label="<head>|<tail>", shape="record"];
      "0x7fefeaceaae0":tail -> "0x7fefeaceaac0":head ["ol"="box"];
      "0x7fefeaceaac0":head -> "0x7fefeaceaaa0":head ["ol"="box"];
      "0x7fefeaceaaa0":head -> "0x7fefeaceaa20" ["ol"="box"];
      "0x7fefeaceaa20" [label="right-child", shape="none"];
      "0x7fefeaceaaa0" [label="<head>|<tail>", shape="record"];
      "0x7fefeaceaac0" [label="<head>|<tail>", shape="record"];
      "0x7fefeaceaae0" [label="<head>|<tail>", shape="record"];
      "0x7fefeaceab00" [label="<head>|<tail>", shape="record"];
      "0x7fefeaceab20" [label="<head>|<tail>", shape="record"];
      "0x7fefeaceab40" [label="<head>|<tail>", shape="record"];
      "0x7fefeaceaca0" [label="<head>|<tail>", shape="record"];
    }

If you write the output to a file, you can render it with `dot(1)`:

    $ dot -Tpng -ofoo.png foo.dot

Output of `make check`
----------------------

Below is the output of the small test suite on my computer.  Since it's _my_
computer, it's considered _canonical_ at the moment. :-)

    echo "(run-tests)" | ./mickey
                                                                      _
    Mickey Scheme (C) 2011 Christian Stigen Larsen                     \
    4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.9.00)      /\
    Readline 4.2                                                      /  \_
                                                                            
    Loaded 146 definitions
    Execute (exit [ code ]) to quit
    You can also (run-tests) and (list-globals)
    
    1/1 OK:   format_abbc5d == "abbc5d"
    2/2 OK:   decode_literal_string("\"1 \\\"quo\\\" 2\"") == "1 "quo" 2"
    3/3 OK:   isatom("a")
    4/4 OK:   isatom("ab")
    5/5 OK:   isatom("ab12")
    6/6 OK:   isatom("a1")
    7/7 OK:   isatom("1")
    8/8 OK:   isatom("1a")
    9/9 OK:   isatom("12b")
    10/10 OK:   isatom("-")
    11/11 OK:   isatom("a-")
    12/12 OK:   isatom("a1-c")
    13/13 OK:   isatom("a1c-")
    14/14 OK:   isfloat("123.0")
    15/15 OK:   isfloat("123")
    16/16 OK:   isfloat("123f")
    17/17 OK:   isfloat("123ff")
    18/18 OK:   isfloat("1.0")
    19/19 OK:   isfloat("1.0.")
    20/20 OK:   isfloat("1..")
    21/21 OK:   isfloat(".1..")
    22/22 OK:   isfloat(".1")
    23/23 OK:   isfloat(".1f")
    24/24 OK:   isfloat("1f")
    25/25 OK:   isfloat("0f")
    26/26 OK:   isfloat("1.0f")
    27/27 OK:   isfloat("+1.0f")
    28/28 OK:   isfloat("-1.0f")
    29/29 OK:   isfloat("-1.0")
    30/30 OK:   isfloat("-10f")
    31/31 OK:   isfloat("-10")
    32/32 OK:   sprint(integer(1)) == "1"
    33/33 OK:   sprint(cons(integer(1), NULL)) == "(1)"
    34/34 OK:   sprint(cons(integer(1), integer(1))) == "(1 . 1)"
    35/35 OK:   sprint(cons(symbol("one"), symbol("two"))) == "(one . two)"
    36/36 OK:   sprint(cons(integer(1), integer(2))) == "(1 . 2)"
    37/37 OK:   sprint(cons(integer(0), cons(integer(1), integer(2)))) == "(0 1 . 2)"
    38/38 OK:   sprint(cons(symbol("zero"), cons(symbol("one"), symbol("two")))) == "(zero one . two)"
    39/39 OK:   sprint(cons(integer(1), cons(integer(2), NULL))) == "(1 2)"
    40/40 OK:   sprint(cons(integer(1), cons(integer(2), cons(integer(3), NULL)))) == "(1 2 3)"
    41/41 OK:   sprint(cons(integer(0), cons(cons(integer(1), cons(integer(2), NULL)), NULL))) == "(0 (1 2))"
    42/42 OK:   sprint(list(integer(1))) == "(1)"
    43/43 OK:   sprint(list(integer(1), integer(2))) == "(1 2)"
    44/44 OK:   sprint(list(integer(1), list(integer(2), integer(3)))) == "(1 (2 3))"
    45/45 OK:   sprint(list(integer(1), list(integer(2), list(integer(3), integer(4))))) == "(1 (2 (3 4)))"
    46/46 OK:   sprint(list(list(integer(1), integer(2)), integer(3))) == "((1 2) 3)"
    47/47 OK:   sprint(cons(integer(1), cons(integer(2), list(integer(3), integer(4))))) == "(1 2 3 4)"
    48/48 OK:   sprint(cons(list(integer(1), integer(2)), list(integer(3), integer(4)))) == "((1 2) 3 4)"
    49/49 OK:   sprint(cons(cons(integer(1), cons(integer(2))), integer(3))) == "((1 2) . 3)"
    50/50 OK:   sprint(cons(list(integer(1), integer(2)), integer(3))) == "((1 2) . 3)"
    51/51 OK:   sprint(append(list(integer(1), integer(2)), list(list(integer(4), integer(5))))) == "(1 2 (4 5))"
    52/52 OK:   sprint(append(list(integer(1), integer(2)), list(integer(3), integer(4)))) == "(1 2 3 4)"
    53/53 OK:   sprint(cons(integer(3), cons(integer(4), integer(5)))) == "(3 4 . 5)"
    54/54 OK:   sprint(append(list(integer(1)), integer(2))) == "(1 . 2)"
    55/55 OK:   sprint(cons(cons(NULL, NULL), NULL)) == "(())"
    56/56 OK:   sprint(cons(cons(cons(NULL, NULL), NULL))) == "((()))"
    57/57 OK:   sprint(append(list(NULL), list(integer(1), integer(2)))) == "(1 2)"
    58/58 OK:   parse_sprint("(cOns 1 2)") == "((cOns 1 2))"
    59/59 OK:   parse_sprint("(CONS 1 2)") == "((CONS 1 2))"
    60/60 OK:   parse_sprint("(fx-+ (fx-* 1 2) 3)") == "((fx-+ (fx-* 1 2) 3))"
    61/61 OK:   parse_sprint("(1)") == "((1))"
    62/62 OK:   parse_sprint("((1))") == "(((1)))"
    63/63 OK:   parse_sprint("((1 2))") == "(((1 2)))"
    64/64 OK:   parse_sprint("((1 2) 3)") == "(((1 2) 3))"
    65/65 OK:   parse_sprint("((a b) c)") == "(((a b) c))"
    66/66 OK:   parse_sprint("(a (b c) d)") == "((a (b c) d))"
    67/67 OK:   parse_sprint("(a (B c) D)") == "((a (B c) D))"
    68/68 OK:   parse_sprint("a") == "(a)"
    69/69 OK:   parse_sprint("A") == "(A)"
    70/70 OK:   parse_sprint("(1 2 3) (4 5 6)") == "((1 2 3) (4 5 6))"
    71/71 OK:   parse_sprint("(1 2 3)\r\n(4 5 6)") == "((1 2 3) (4 5 6))"
    72/72 OK:   parse_eval_print("(number->string 123)") == "123"
    73/73 OK:   parse_eval_print("(list 1 2 (list 3 4))") == "(1 2 (3 4))"
    74/74 OK:   parse_eval_print("(list 1 2 (list 3 (list 4 4 5) 4))") == "(1 2 (3 (4 4 5) 4))"
    75/75 OK:   parse_eval_print("(list 1 2 (list 3 4 (* 3 30)))") == "(1 2 (3 4 90))"
    76/76 OK:   parse_eval_print("(string-append \"one\" \"two\" \"three\")") == "onetwothree"
    12377/77 OK:   parse_eval_print("(display 123)") == ""
    1178/78 OK:   parse_eval_print("(display (+ 3 3 5))") == ""
    hello79/79 OK:   parse_eval_print("(display \"hello\")") == ""
    80/80 OK:   parse_eval_print("(cdar (list (list 1)))") == "()"
    81/81 OK:   parse_eval_print("(cddr (list 1 (list 2 3 4)))") == "()"
    82/82 OK:   parse_eval_print("(cddr (list 1 (list 2 3 4) (list 5 6)))") == "((5 6))"
    83/83 OK:   parse_eval_print("(+ 0)") == "0"
    84/84 OK:   parse_eval_print("(+ 1)") == "1"
    85/85 OK:   parse_eval_print("(+ 0 1)") == "1"
    86/86 OK:   parse_eval_print("(+ 1 2)") == "3"
    87/87 OK:   parse_eval_print("(+ 1 2 3)") == "6"
    88/88 OK:   parse_eval_print("(+ -1)") == "-1"
    89/89 OK:   parse_eval_print("(+ 5 -2 3)") == "6"
    90/90 OK:   parse_eval_print("(+ -1 -3 -7)") == "-11"
    91/91 OK:   parse_eval_print("(+ (* 5 5) 4)") == "29"
    92/92 OK:   parse_eval_print("(+ (* 5 5) 4 12)") == "41"
    93/93 OK:   parse_eval_print("(symbol? 1)") == "#f"
    94/94 OK:   parse_eval_print("(symbol? 10)") == "#f"
    95/95 OK:   parse_eval_print("(symbol? (quote a))") == "#t"
    96/96 OK:   parse_eval_print("(symbol? (quote abba))") == "#t"
    97/97 OK:   parse_eval_print("(symbol? (list))") == "#f"
    98/98 OK:   parse_eval_print("(symbol? +)") == "#f"
    99/99 OK:   parse_eval_print("(symbol? list)") == "#f"
    100/100 OK:   parse_eval_print("(symbol? \"hey\")") == "#f"
    101/101 OK:   parse_eval_print("(integer?)") == "#f"
    102/102 OK:   parse_eval_print("(integer? 1)") == "#t"
    103/103 OK:   parse_eval_print("(integer? 0)") == "#t"
    104/104 OK:   parse_eval_print("(integer? (quote a))") == "#f"
    105/105 OK:   parse_eval_print("(integer? 1.0)") == "#f"
    106/106 OK:   parse_eval_print("(integer? 1.0f)") == "#f"
    107/107 OK:   parse_eval_print("(integer? 10f)") == "#f"
    108/108 OK:   parse_eval_print("(integer? 10)") == "#t"
    109/109 OK:   parse_eval_print("(integer? 100)") == "#t"
    110/110 OK:   parse_eval_print("(integer? 00)") == "#t"
    111/111 OK:   parse_eval_print("(integer? 1234)") == "#t"
    112/112 OK:   parse_eval_print("(integer? (list 1 2))") == "#f"
    113/113 OK:   parse_eval_print("(integer? (+))") == "#t"
    114/114 OK:   parse_eval_print("(integer? +)") == "#f"
    115/115 OK:   parse_eval_print("(integer? -1)") == "#t"
    116/116 OK:   parse_eval_print("(integer? -0)") == "#t"
    117/117 OK:   parse_eval_print("(integer? -123)") == "#t"
    118/118 OK:   parse_eval_print("(integer? -123.0)") == "#f"
    119/119 OK:   parse_eval_print("(integer? ())") == "#f"
    120/120 OK:   parse_eval_print("(integer? \"hey\")") == "#f"
    121/121 OK:   parse_eval_print("(null? 0)") == "#f"
    122/122 OK:   parse_eval_print("(null? 1)") == "#f"
    123/123 OK:   parse_eval_print("(null? (quote a))") == "#f"
    124/124 OK:   parse_eval_print("(null? (list))") == "#t"
    125/125 OK:   parse_eval_print("(null? (car (list 1)))") == "#f"
    126/126 OK:   parse_eval_print("(null? (cdr (list 1)))") == "#t"
    127/127 OK:   parse_eval_print("(procedure? #t)") == "#f"
    128/128 OK:   parse_eval_print("(procedure? 123)") == "#f"
    129/129 OK:   parse_eval_print("(procedure? (quote abba-rules))") == "#f"
    130/130 OK:   parse_eval_print("(procedure? procedure?)") == "#t"
    131/131 OK:   parse_eval_print("(procedure? +)") == "#t"
    132/132 OK:   parse_eval_print("(procedure? list)") == "#t"
    133/133 OK:   parse_eval_print("(procedure? (list 1 2))") == "#f"
    134/134 OK:   parse_eval_print("(procedure? (+ 1 2))") == "#f"
    135/135 OK:   parse_eval_print("(pair? (list 1 2 3))") == "#t"
    136/136 OK:   parse_eval_print("(pair? #t)") == "#f"
    137/137 OK:   parse_eval_print("(pair? 123)") == "#f"
    138/138 OK:   parse_eval_print("(pair? (quote abba))") == "#f"
    139/139 OK:   parse_eval_print("(pair? (+ 1 2))") == "#f"
    140/140 OK:   parse_eval_print("(pair? (list 1 2))") == "#t"
    141/141 OK:   parse_eval_print("(pair? (list 1))") == "#t"
    142/142 OK:   parse_eval_print("(pair? (+))") == "#f"
    143/143 OK:   parse_eval_print("(pair? +)") == "#f"
    144/144 OK:   parse_eval_print("(pair? (list))") == "#f"
    145/145 OK:   parse_eval_print("(list? 1)") == "#f"
    146/146 OK:   parse_eval_print("(list? (+ 1 2))") == "#f"
    147/147 OK:   parse_eval_print("(list? (list 1 2))") == "#t"
    148/148 OK:   parse_eval_print("(list? (list 1))") == "#t"
    149/149 OK:   parse_eval_print("(list? (list))") == "#t"
    150/150 OK:   parse_eval_print("(list? (quote a))") == "#f"
    151/151 OK:   parse_eval_print("(list? \"hey\")") == "#f"
    152/152 OK:   parse_eval_print("(length (list))") == "0"
    153/153 OK:   parse_eval_print("(length (list 1))") == "1"
    154/154 OK:   parse_eval_print("(length (list 1 2))") == "2"
    155/155 OK:   parse_eval_print("(length (list 1 2 3))") == "3"
    156/156 OK:   parse_eval_print("(length (list 1 2 (list 3 4)))") == "3"
    157/157 OK:   parse_eval_print("(list? (list))") == "#t"
    158/158 OK:   parse_eval_print("(pair? (list))") == "#f"
    159/159 OK:   parse_eval_print("(car (list 1 2))") == "1"
    160/160 OK:   parse_eval_print("(car (list 1))") == "1"
    161/161 OK:   parse_eval_print("(car (list 2))") == "2"
    162/162 OK:   parse_eval_print("(car (list 1 2 3))") == "1"
    163/163 OK:   parse_eval_print("(cdr (list 1 2))") == "(2)"
    164/164 OK:   parse_eval_print("(cdr (list 1))") == "()"
    165/165 OK:   parse_eval_print("(cdr (list 2))") == "()"
    166/166 OK:   parse_eval_print("(cdr (list 1 2 3))") == "(2 3)"
    167/167 OK:   parse_eval_print("(cdr (list 1 2 3 4))") == "(2 3 4)"
    168/168 OK:   parse_eval_print("(null? (cdr (list 1)))") == "#t"
    169/169 OK:   parse_eval_print("(caar (list (list 1 2 3) 4))") == "1"
    170/170 OK:   parse_eval_print("(caar (list (list 9 8 7) (list 1 2 3) 4))") == "9"
    171/171 OK:   parse_eval_print("(cadr (list (list 2 3 4) (list 7 4 5)))") == "(7 4 5)"
    172/172 OK:   parse_eval_print("(cdar (list (list 2 3 4) (list 7 4 5)))") == "(3 4)"
    173/173 OK:   parse_eval_sprint("(cddr (list (list 2 3 4) (list 7 4 5)))") == "()"
    174/174 OK:   parse_eval_print("(cddr (list 1 2 3 4))") == "(3 4)"
    175/175 OK:   parse_eval_print("(cdr (cddr (list 1 2 3 4)))") == "(4)"
    176/176 OK:   parse_eval_print("(car (cdr (cddr (list 1 2 3 4))))") == "4"
    177/177 OK:   parse_eval_print("(define a 123)") == ""
    178/178 OK:   parse_eval_print("a") == "123"
    179/179 OK:   parse_eval_print("(+ 1 a)") == "124"
    180/180 OK:   parse_eval_print("(+ a a)") == "246"
    181/181 OK:   parse_eval_print("(define b 100)") == ""
    182/182 OK:   parse_eval_print("(+ a b)") == "223"
    183/183 OK:   parse_eval_print("(define hello (quote (html (body (p (\"Hello, world!\"))))))") == ""
    184/184 OK:   parse_eval_print("hello") == "(html (body (p (Hello, world!))))"
    185/185 OK:   parse_eval_sprint("hello") == "(html (body (p ("Hello, world!"))))"
    186/186 OK:   parse_eval_print("(eval (quote (list 1 2 (quote 3) 3)))") == "(1 2 3 3)"
    
    186/186 (100.00%) tests OK
    0/186 (0.00%) tests FAILED
    ./mickey -Itests tests/*.scm
    line 1
    line 2
    line 3, thank you!
    (1) Testing block comments
    (2) Testing nested comments
    (3) Should get 2 here: 2
    (4) End of comment-tests
    (bytevector? (vector 1 2 3)) => #f
    (bytevector? (make-bytevector 3)) => #t
    (bytevector? (make-bytevector 3 11)) => #t
    (make-bytevector 3) => #u8(0 0 0)
    (make-bytevector 3 11) => #u8(11 11 11)
    (define v1 (make-bytevector 3)) => 
    (define v2 (make-bytevector 20)) => 
    (bytevector-u8-set! v1 0 11) => 
    (bytevector-u8-set! v1 1 22) => 
    (bytevector-u8-set! v1 2 33) => 
    v1 => #u8(11 22 33)
    v2 => #u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (bytevector-copy! v1 v2) => 
    v2 => #u8(11 22 33)
    (bytevector-u8-ref v1 0) => 11
    (bytevector-u8-ref v1 1) => 22
    (bytevector-u8-ref v1 2) => 33
    (bytevector-u8-set! v2 1 55) => 
    v1 => #u8(11 22 33)
    v2 => #u8(11 55 33)
    (define v3 (bytevector-copy v2)) => 
    (bytevector-u8-set! v3 1 99) => 
    v1 => #u8(11 22 33)
    v2 => #u8(11 55 33)
    v3 => #u8(11 99 33)
    (bytevector-length v1) => 3
    (bytevector-u8-ref v3 0) => 11
    (bytevector-u8-ref v3 1) => 99
    (bytevector-u8-ref v3 2) => 33
    (bytevector-copy-partial v1 0 1) => #u8(11)
    (bytevector-copy-partial v1 0 2) => #u8(11 22)
    (bytevector-copy-partial v1 0 3) => #u8(11 22 33)
    (bytevector-copy-partial v1 1 3) => #u8(22 33)
    (bytevector-copy-partial v1 2 3) => #u8(33)
    (define v4 (make-bytevector 10)) => 
    v4 => #u8(0 0 0 0 0 0 0 0 0 0)
    (bytevector-copy-partial! v1 0 2 v4 0) => 
    v4 => #u8(11 22 0 0 0 0 0 0 0 0)
    (bytevector-copy-partial! v1 0 2 v4 3) => 
    v4 => #u8(11 22 0 11 22 0 0 0 0 0)
    (bytevector-copy-partial! v1 0 3 v4 6) => 
    v4 => #u8(11 22 0 11 22 0 11 22 33 0)
    Testing currying:
    1*3 = 3
    2*3 = 6
    3*3 = 9
    4*3 = 12
    5*3 = 15
    6*3 = 18
    0! = 1
    1! = 1
    2! = 2
    4! = 24
    8! = 40320
    16! = 2004189184
    32! = -2147483648
    64! = 0
    128! = 0
    256! = 0
    512! = 0
    1024! = 0
    2048! = 0
    4096! = 0
    8192! = 0
    32768! = 0
    0! = 1
    1! = 1
    2! = 2
    3! = 6
    4! = 24
    5! = 120
    6! = 720
    7! = 5040
    8! = 40320
    9! = 362880
    10! = 3628800
    11! = 39916800
    12! = 479001600
    13! = 1932053504
    20! = -2102132736
    40! = 0
    80! = 0
    100! = 0
    fib(0) = 0
    fib(1) = 1
    fib(2) = 1
    fib(3) = 2
    fib(4) = 3
    fib(5) = 5
    fib(6) = 8
    fib(7) = 13
    fib(8) = 21
    fib(9) = 34
    fib(10) = 55
    fib(20) = 6765
    fib(24) = 46368
    Hello, world!
    -- Mickey scheme
    
    i male(i) female(i)
    0   0         1
    1   0         1
    2   1         2
    3   2         2
    4   2         3
    5   3         3
    6   4         4
    7   4         5
    8   5         5
    12^2 = 144
    sqrt(5^2 + 4^2) = 6.40312
    Should be 30: 30
    Should be 20: 20
    Should be 30: 30
    Should be 18: 18
    Should be 77: 77
    Let's define `square`
    Let's use it!
    12*12 = 144
     6*6  =  36
     3*3  =  9
    Good bye!
    -1.0 <= 1.0 is correct
    399811
    Inner1: Hello, Inner1
    Inner2: Hello, Inner2
    Inner1: Hello, both!
    Inner2: Hello, both1
    Ok, inner1 works fine
    Printing numbers from 200 to 1
    200 199 198 197 196 195 194 193 192 191 190 189 188 187 186 185 184 183 182 181 180 179 178 177 176 175 174 173 172 171 170 169 168 167 166 165 164 163 162 161 160 159 158 157 156 155 154 153 152 151 150 149 148 147 146 145 144 143 142 141 140 139 138 137 136 135 134 133 132 131 130 129 128 127 126 125 124 123 122 121 120 119 118 117 116 115 114 113 112 111 110 109 108 107 106 105 104 103 102 101 100 99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 84 83 82 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 
    It works! Done
    (1/2) When: Works fine!
    (2/2) Unless: Works fine!
    Should get (2 1) now: (2 1)
                                                                                  
                                                     *                            
                                                   ****                           
                                                  ******                          
                                        * *   *    ****  *                        
                                          ** **************** ****                
                                         ************************                 
                                       ***************************                
                                      ******************************              
                         *********   *****************************                
                        ************ *****************************                
                        *****************************************                 
     **********************************************************                   
                        *****************************************                 
                        ************ *****************************                
                         *********   *****************************                
                                      ******************************              
                                       ***************************                
                                         ************************                 
                                          ** **************** ****                
                                        * *   *    ****  *                        
                                                  ******                          
                                                   ****                           
                                                     *                            
    (map cadr (quote ((a b) (d e) (g h)))) => (b e h)
    (map (lambda (n) (expt n n)) (quote (1 2 3 4 5))) => (1 4 27 256 3125)
    (let ((a 2)) (map (lambda (n) (expt n a)) (quote (1 2 3 4 5)))) => (1 4 9 16 25)
    (map + (quote (1 2 3)) (quote (4 5 6))) => (5 7 9)
    (let ((count 0)) (map (lambda (ignored) (set! count (+ count 1)) count) (quote (a b)))) => (1 2)
    3*4*5*6 = 360
    (1+2+3)*5 = 30
    10 9 8 7 6 5 4 3 2 1 
    Here is a list: (one)
    Here is a list: (one two)
    Here is a list: (one two three)
    bottles on the wall: 100
    bottles on the wall: 99
    bottles on the wall: *take one down, pass it around*
    bottles on the wall: *heavy drinking*
    bottles on the wall: 2
    bottles on the wall: 1
    bottles on the wall: 0
    Tests from
    Structure and Interpretation of Comptuter Programs (SICP)
    
    SICP SECTION 1.1.1
    1 OK: 486 ==> 486
    2 OK: (+ 137 349) ==> 486
    3 OK: (- 1000 334) ==> 666
    4 OK: (* 5 99) ==> 495
    5 OK: (/ 10 5) ==> 2
    6 OK: (+ 2.7 10) ==> 12.7
    7 OK: (+ 21 35 12 7) ==> 75
    8 OK: (* 25 4 12) ==> 1200
    9 OK: (+ (* 3 5) (- 10 6)) ==> 19
    10 OK: (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) ==> 57
    11 OK: (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) ==> 57
    
    SICP SECTION 1.1.2
    12 OK: size ==> 2
    13 OK: (* 5 size) ==> 10
    14 OK: (* pi (* radius radius)) ==> 314.159
    15 OK: circumference ==> 62.8318
    
    SICP SECTION 1.1.3
    16 OK: (* (+ 2 (* 4 6))) ==> 26
    17 OK: (+ 3 5 7) ==> 15
    
    SICP SECTION 1.1.4
    18 OK: (square 21) ==> 441
    19 OK: (square (+ 2 5)) ==> 49
    20 OK: (square (square 3)) ==> 81
    21 OK: (sum-of-squares 3 4) ==> 25
    22 OK: (f 5) ==> 136
    
    SICP SECTION 1.1.5
    23 OK: (f 5) ==> 136
    24 OK: (sum-of-squares (+ 5 1) (* 5 2)) ==> 136
    25 OK: (+ (square 6) (square 10)) ==> 136
    26 OK: (+ (* 6 6) (* 10 10)) ==> 136
    27 OK: (+ 36 100) ==> 136
    28 OK: (f 5) ==> 136
    29 OK: (sum-of-squares (+ 5 1) (* 5 2)) ==> 136
    30 OK: (+ (square (+ 5 1)) (square (* 5 2))) ==> 136
    31 OK: (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2))) ==> 136
    32 OK: (+ (* 6 6) (* 10 10)) ==> 136
    33 OK: (+ 36 100) ==> 136
    34 OK: 136 ==> 136
    34 / 34 tests OK, 0 failed
    
    SICP SECTION 1.1.6
    35 OK: (abs 1) ==> 1
    36 OK: (abs -1) ==> 1
    37 OK: (abs 0) ==> 0
    38 OK: (abs 1) ==> 1
    39 OK: (abs -1) ==> 1
    40 OK: (abs 0) ==> 0
    41 OK: (abs 1) ==> 1
    42 OK: (abs -1) ==> 1
    43 OK: (abs 0) ==> 0
    44 OK: ((lambda (x) (and (> x 5) (< x 10))) 4) ==> #f
    45 OK: ((lambda (x) (and (> x 5) (< x 10))) 5) ==> #f
    46 OK: ((lambda (x) (and (> x 5) (< x 10))) 6) ==> #t
    47 OK: ((lambda (x) (and (> x 5) (< x 10))) 9) ==> #t
    48 OK: ((lambda (x) (and (> x 5) (< x 10))) 10) ==> #f
    49 OK: ((lambda (x) (and (> x 5) (< x 10))) 11) ==> #f
    
    SICP SECTION 1.1.7
    50 FAIL: (number->string (sqrt 9)) != 3.000092
      Actual result: '3.00009'
    51 OK: (sqrt (+ 100 37)) ==> 11.7047
    52 FAIL: (number->string (sqrt (+ (sqrt 2) (sqrt 3)))) != 1.773928
      Actual result: '1.77393'
    53 OK: (square (sqrt 1000)) ==> 1000
    
    SICP SECTION 1.1.8
    
    SICP SECTION 1.2.1
    
    SICP SECTION 1.2.2
    54 OK: (count-change 100) ==> 292
    
    SICP SECTION 1.3.1
    
    SICP SECTION 1.3.2
    
    RESULTS
    52 / 54 tests OK, 2 failed
    Test 1: simple string
    Test 2: Using delimiters ( and )
    Test 3: Using "quoted" characters
    (1/4) Defining my-when macro
    (2/4) Trying our my-when macro
    (3/4) This is gonna be SOO cool!
    (4/4) END of program
    none
    meet me by the docks at midnight
    Syd is an aspiring musician trying to start his own new-wave band.
    Dave is Sandy's boyfriend. Organizer of the rescue effort.
    Razor is lead singer for the pnuk band 'Razor and the Scummettes'.
    Purple Tentacle is someone I don't know.
    3 and 2: greater
    2 and 3: less
    2 and 2: equal
    Numbers from 1 to 10: 1 2 3 4 5 6 7 8 9 10 
    1
    2
    3
    Result:
    a = 1
    b = 2
    Done!
    A circle with radius 0 has area 0
    A circle with radius 1 has area 3.14159
    A circle with radius 2 has area 12.5664
    A circle with radius 3 has area 28.2743
    A circle with radius 4 has area 50.2655
    A circle with radius 5 has area 78.5398
    Start
    123
    456
    Outside
    Number 0 even? yes. odd? no. 
    Number 1 even? no.  odd? yes.
    Number 2 even? yes. odd? no. 
    Number 3 even? no.  odd? yes.
    Number 4 even? yes. odd? no. 
    Number 5 even? no.  odd? yes.
    Value of `var` is 10
    my-sqrt(0) = 0.0078125 vs builtin sqrt(0) = 0
    my-sqrt(1) = 1 vs builtin sqrt(1) = 1
    my-sqrt(2) = 1.41422 vs builtin sqrt(2) = 1.41421
    my-sqrt(3) = 1.73205 vs builtin sqrt(3) = 1.73205
    my-sqrt(4) = 2 vs builtin sqrt(4) = 2
    my-sqrt(5) = 2.23607 vs builtin sqrt(5) = 2.23607
    my-sqrt(6) = 2.44949 vs builtin sqrt(6) = 2.44949
    Let's try recursing 30000 times
    Well, that worked out alright!
    Tests
    1 OK: (+ 1 2 3 4) ==> 10
    2 OK: (* 1 2 3 4) ==> 24
    3 OK: (- 1 2 3 4) ==> -8
    4 OK: (string-append a b b a) ==> abba
    5 OK: (number->string 12345) ==> 12345
    6 OK: (list 1 2 (list 3 4)) ==> (1 2 (3 4))
    7 OK: (apply + (list 1 2 3)) ==> 6
    8 OK: (apply + (quote (1 2 3))) ==> 6
    9 OK: (apply + (list 1 2 3 (* 5 6))) ==> 36
    10 OK: (apply + (list 1 2 3 (* 5 5))) ==> 31
    11 OK: (complex->string (*complex (make-complex 2 3) (make-complex 5 7))) ==> -11 + 29i
    12 OK: (- 0.5 -1.5) ==> 2
    13 OK: (- 0.5 -10.5) ==> 11
    14 OK: (- 1 2 3) ==> -4
    15 OK: (- 1) ==> -1
    16 OK: (- 2) ==> -2
    17 OK: (- -2) ==> 2
    18 OK: (if (> 4 2) 11 22) ==> 11
    19 OK: (if (> 3 2) 11 22) ==> 11
    20 OK: (if (> 2 2) 11 22) ==> 22
    21 OK: (if (> 1 2) 11 22) ==> 22
    22 OK: (if (> 3 2) 11) ==> 11
    23 OK: (if (< 1 2) 11) ==> 11
    24 OK: (reverse (list 1 2 3 4)) ==> (4 3 2 1)
    25 OK: (reverse (list 1 2 3)) ==> (3 2 1)
    26 OK: (reverse (list 1 2)) ==> (2 1)
    27 OK: (reverse (list 1)) ==> (1)
    28 OK: (abs -2) ==> 2
    29 OK: (abs -1) ==> 1
    30 OK: (abs 0) ==> 0
    31 OK: (abs 1) ==> 1
    32 OK: (abs 2) ==> 2
    33 OK: (abs -2.1) ==> 2.1
    34 OK: (abs -1.1) ==> 1.1
    35 OK: (abs 0.1) ==> 0.1
    36 OK: (abs -0.1) ==> 0.1
    37 OK: (abs 1.1) ==> 1.1
    38 OK: (abs 2.1) ==> 2.1
    39 OK: (assq (quote three) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3))) ==> (three 3)
    40 OK: (assq (quote two) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3))) ==> (two 2)
    41 OK: (assq (quote threee) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3))) ==> #f
    42 OK: (even? 0) ==> #t
    43 OK: (even? 1) ==> #f
    44 OK: (even? 2) ==> #t
    45 OK: (even? 3) ==> #f
    46 OK: (even? 4) ==> #t
    47 OK: (even? 9) ==> #f
    48 OK: (even? 100) ==> #t
    49 OK: (even? 1000) ==> #t
    50 OK: (odd? 0) ==> #f
    51 OK: (odd? 1) ==> #t
    52 OK: (odd? 2) ==> #f
    53 OK: (odd? 3) ==> #t
    54 OK: (odd? 4) ==> #f
    55 OK: (odd? 9) ==> #t
    56 OK: (odd? 100) ==> #f
    57 OK: (odd? 1000) ==> #f
    58 OK: (negative? 0) ==> #f
    59 OK: (negative? 1) ==> #f
    60 OK: (negative? 2) ==> #f
    61 OK: (negative? 3) ==> #f
    62 OK: (negative? -1) ==> #t
    63 OK: (negative? -2) ==> #t
    64 OK: (negative? -3) ==> #t
    65 OK: (positive? 0) ==> #f
    66 OK: (positive? 1) ==> #t
    67 OK: (positive? 2) ==> #t
    68 OK: (positive? 3) ==> #t
    69 OK: (positive? -1) ==> #f
    70 OK: (positive? -2) ==> #f
    71 OK: (positive? -3) ==> #f
    72 OK: (eqv? (quote a) (quote a)) ==> #t
    73 OK: (eqv? (quote a) (quote b)) ==> #f
    74 OK: (eqv? 2 2) ==> #t
    75 OK: (eqv? 2 1) ==> #f
    76 OK: (eqv? (list) (list)) ==> #t
    77 OK: (eqv? 100000000 100000000) ==> #t
    78 OK: (eqv? (cons 1 2) (cons 1 2)) ==> #f
    79 OK: (eqv? (lambda 1) (lambda 2)) ==> #f
    80 OK: (eqv? #f (quote nil)) ==> #f
    81 OK: (let ((p (lambda (x) x))) (eqv? p p)) ==> #t
    82 OK: (round 1) ==> 1
    83 OK: (round 2) ==> 2
    84 OK: (round 0.9) ==> 1
    85 OK: (round 1) ==> 1
    86 OK: (round 1.1) ==> 1
    87 OK: (round 1.2) ==> 1
    88 OK: (round 1.3) ==> 1
    89 OK: (round 1.4) ==> 1
    90 OK: (round 1.5) ==> 2
    91 OK: (round 1.6) ==> 2
    92 OK: (round 2.49) ==> 2
    93 OK: (round 2.5) ==> 3
    94 OK: (round 2.51) ==> 3
    95 OK: (truncate 1.1) ==> 1
    96 OK: (truncate 1.4) ==> 1
    97 OK: (truncate 1.5) ==> 1
    98 OK: (truncate 1.6) ==> 1
    99 OK: (truncate 1.9) ==> 1
    100 OK: (truncate 1) ==> 1
    101 OK: (truncate 2) ==> 2
    102 OK: (min 1 2 3) ==> 1
    103 OK: (min 4 2 3) ==> 2
    104 OK: (min 4.2 2 3) ==> 2
    105 OK: (min 4.2 2.3 3) ==> 2.3
    106 OK: (min 4.2) ==> 4.2
    107 OK: (max 1 2 3) ==> 3
    108 OK: (max 4 2 3) ==> 4
    109 OK: (max 4.2 2 3) ==> 4.2
    110 OK: (max 4.2 2.3 3) ==> 4.2
    111 OK: (max 4.2) ==> 4.2
    112 OK: (expt 2 0) ==> 1
    113 OK: (expt 2 1) ==> 2
    114 OK: (expt 2 2) ==> 4
    115 OK: (expt 2 3) ==> 8
    116 OK: (expt 3 3) ==> 27
    117 OK: (char-whitespace? a) ==> #f
    118 OK: (char-whitespace? b) ==> #f
    119 OK: (char-whitespace? c) ==> #f
    120 OK: (modulo 10 6) ==> 4
    121 OK: (modulo 10 5) ==> 0
    122 OK: (modulo 10 4) ==> 2
    123 OK: (modulo 10 3) ==> 1
    124 OK: (modulo 10 2) ==> 0
    125 OK: (modulo 10 1) ==> 0
    126 OK: (char->integer a) ==> 97
    127 OK: (char->integer b) ==> 98
    128 OK: (char->integer A) ==> 65
    129 OK: (char-alphabetic? a) ==> #t
    130 OK: (char-alphabetic? A) ==> #t
    131 OK: (char-alphabetic? 2) ==> #f
    132 OK: (char-alphabetic? 8) ==> #f
    133 OK: (char-lower-case? 8) ==> #f
    134 OK: (char-lower-case? Z) ==> #f
    135 OK: (char-lower-case? A) ==> #f
    136 OK: (char-lower-case? H) ==> #f
    137 OK: (char-lower-case? z) ==> #t
    138 OK: (char-lower-case? a) ==> #t
    139 OK: (char-lower-case? h) ==> #t
    140 OK: (char-upper-case? 8) ==> #f
    141 OK: (char-upper-case? Z) ==> #t
    142 OK: (char-upper-case? A) ==> #t
    143 OK: (char-upper-case? H) ==> #t
    144 OK: (char-upper-case? z) ==> #f
    145 OK: (char-upper-case? a) ==> #f
    146 OK: (char-upper-case? h) ==> #f
    147 OK: (char-numeric? 8) ==> #t
    148 OK: (char-numeric? Z) ==> #f
    149 OK: (char-numeric? A) ==> #f
    150 OK: (char-numeric? a) ==> #f
    151 OK: (char-numeric? 0) ==> #t
    152 OK: (char-numeric? 1) ==> #t
    153 OK: (char-numeric? 9) ==> #t
    154 OK: (char-<=? 9 0) ==> #f
    155 OK: (char-<=? 3 5) ==> #t
    156 OK: (char-<=? a z) ==> #t
    157 OK: (char-<=? a a) ==> #t
    158 OK: (char-<=? h e) ==> #f
    159 OK: (char-<=? r w) ==> #t
    160 OK: (char->=? 9 0) ==> #t
    161 OK: (char->=? 3 5) ==> #f
    162 OK: (char->=? a z) ==> #f
    163 OK: (char->=? a a) ==> #t
    164 OK: (char->=? h e) ==> #t
    165 OK: (char->=? r w) ==> #f
    166 OK: (char->? 9 0) ==> #t
    167 OK: (char->? 3 5) ==> #f
    168 OK: (char->? a a) ==> #f
    169 OK: (char->? h e) ==> #t
    170 OK: (char->? r w) ==> #f
    171 OK: (char-=? 9 0) ==> #f
    172 OK: (char-=? 3 5) ==> #f
    173 OK: (char-=? a a) ==> #t
    174 OK: (char-=? 4 4) ==> #t
    175 OK: (char-=? h e) ==> #f
    176 OK: (char-=? r w) ==> #f
    177 OK: (integer->char 65) ==> A
    178 OK: (integer->char 97) ==> a
    179 OK: (list->string (list a b c)) ==> abc
    180 OK: (list->string (list a b b A)) ==> abbA
    181 OK: (list-tail (list 1 2 3) 0) ==> (1 2 3)
    182 OK: (list-tail (list 1 2 3) 1) ==> (2 3)
    183 OK: (list-tail (list 1 2 3) 2) ==> (3)
    184 OK: (list-tail (list 1 2 3) 3) ==> ()
    185 OK: (member 10 (list 1 2 3)) ==> #f
    186 OK: (member 10 (list 10 20 30)) ==> (10 20 30)
    187 OK: (member 20 (list 10 20 30)) ==> (20 30)
    188 OK: (member 20 (list 10 20 30 (quote bee) (quote cee))) ==> (20 30 bee cee)
    189 OK: (member 30 (list 10 20 30)) ==> (30)
    190 OK: (member 40 (list 10 20 30)) ==> #f
    191 OK: (memv 10 (list 1 2 3)) ==> #f
    192 OK: (memv 10 (list 10 20 30)) ==> (10 20 30)
    193 OK: (memv 20 (list 10 20 30)) ==> (20 30)
    194 OK: (memv 20 (list 10 20 30 (quote bee) (quote cee))) ==> (20 30 bee cee)
    195 OK: (memv 30 (list 10 20 30)) ==> (30)
    196 OK: (memv 40 (list 10 20 30)) ==> #f
    197 OK: (memq 10 (list 1 2 3)) ==> #f
    198 OK: (memq 10 (list 10 20 30)) ==> (10 20 30)
    199 OK: (memq 20 (list 10 20 30)) ==> (20 30)
    200 OK: (memq 20 (list 10 20 30 (quote bee) (quote cee))) ==> (20 30 bee cee)
    201 OK: (memq 30 (list 10 20 30)) ==> (30)
    202 OK: (memq 40 (list 10 20 30)) ==> #f
    203 OK: (gcd) ==> 0
    204 OK: (gcd 10) ==> 10
    205 OK: (gcd -10) ==> 10
    206 OK: (gcd 10 2) ==> 2
    207 OK: (gcd 10 3) ==> 1
    208 OK: (gcd 10 5) ==> 5
    209 OK: (gcd 5 10) ==> 5
    210 OK: (gcd 32 -36) ==> 4
    211 OK: (gcd -32 36) ==> 4
    212 OK: (gcd -32 -36) ==> 4
    213 OK: (gcd 4 6 8 10) ==> 2
    214 OK: (gcd 4 6 8 12) ==> 2
    215 OK: (gcd 40 24) ==> 8
    216 OK: (gcd 1230 4560) ==> 30
    217 OK: (lcm) ==> 1
    218 OK: (lcm 10) ==> 10
    219 OK: (lcm -10) ==> 10
    220 OK: (lcm 10 10) ==> 10
    221 OK: (lcm 4 6) ==> 12
    222 OK: (lcm 6 4) ==> 12
    223 OK: (lcm 2 4 6) ==> 12
    224 OK: (lcm 2 4 6 8) ==> 24
    225 OK: (lcm 2 4 6 -8) ==> 24
    226 OK: (lcm 2 -4 6 -8) ==> 24
    227 OK: (lcm 201 202 203) ==> 8242206
    228 OK: (lcm 201 202 203 204) ==> 280235004
    229 OK: (list-ref (list 1 2 3) 2) ==> 3
    230 OK: (list-ref (list 1 2 3) 1) ==> 2
    231 OK: (list-ref (list 1 2 3) 0) ==> 1
    232 OK: (list-ref (list 1 2 3 4) 0) ==> 1
    233 OK: (list-ref (list 1 2 3 4) 3) ==> 4
    234 OK: (string->list hey) ==> (h e y)
    235 OK: (string->symbol hey) ==> hey
    236 OK: (string->number abba) ==> #f
    237 OK: (string->number 123) ==> 123
    238 OK: (string->number 456) ==> 456
    239 OK: (string->number 1.2) ==> 1.2
    240 OK: (string->number 1.5) ==> 1.5
    241 OK: (string-length abba) ==> 4
    242 OK: (string-length abb) ==> 3
    243 OK: (string-length ab) ==> 2
    244 OK: (string-length a) ==> 1
    245 OK: (string-length) ==> 0
    246 OK: (substring Hello! 0 0) ==> 
    247 OK: (substring Hello! 0 1) ==> H
    248 OK: (substring Hello! 0 3) ==> Hel
    249 OK: (substring Hello! 1 3) ==> ell
    250 OK: (substring Hello! 1 3) ==> ell
    251 OK: (substring Hello! 1 4) ==> ello
    252 OK: (substring Hello! 2 4) ==> llo!
    253 OK: (string=? hey hey) ==> #t
    254 OK: (string=? hey heya) ==> #f
    255 OK: (string<=? hey heya) ==> #t
    256 OK: (string-ref hey 0) ==> h
    257 OK: (string-ref hey 1) ==> e
    258 OK: (string-ref hey 2) ==> y
    259 OK: (append (list) 1) ==> 1
    260 OK: (append (list) (list 1 2)) ==> (1 2)
    261 OK: (append (list) (list 1 2)) ==> (1 2)
    262 OK: (append (list 1)) ==> (1)
    263 OK: (append (list 1)) ==> (1)
    264 OK: (append (list 1) 2) ==> (1 . 2)
    265 OK: (append (list 1) (list 3)) ==> (1 3)
    266 OK: (append (list 1) (list 3 4)) ==> (1 3 4)
    267 OK: (append (list 1) (list 3 4) 5) ==> (1 3 4 . 5)
    268 OK: (append (append (list 1 2 3) (list 4)) (list 5 6) 7) ==> (1 2 3 4 5 6 . 7)
    269 OK: a ==> (1)
    270 OK: (quote hey) ==> hey
    271 OK: (quote hey) ==> hey
    272 OK: (quote 'hey) ==> 'hey
    273 OK: (append (quote (1 2 3)) 4) ==> (1 2 3 . 4)
    274 OK: (apply + (quote (1 2 3))) ==> 6
    275 OK: (apply + (quote (1 2 3))) ==> 6
    276 OK: (quasiquote (1 2 (unquote (+ 3 4)))) ==> (1 2 7)
    277 OK: (quasiquote (1 2 (unquote (+ 3 4)) 3 y)) ==> (1 2 7 3 y)
    278 OK: (quasiquote (1 2 (unquote-splicing (list 3 4 5)) 6 7)) ==> (1 2 3 4 5 6 7)
    279 OK: (ceiling 3) ==> 3
    280 OK: (ceiling 3.1) ==> 4
    281 OK: (ceiling 3.4) ==> 4
    282 OK: (ceiling 3.5) ==> 4
    283 OK: (ceiling 3.6) ==> 4
    284 OK: (floor 3) ==> 3
    285 OK: (floor 3.1) ==> 3
    286 OK: (floor 3.4) ==> 3
    287 OK: (floor 3.5) ==> 3
    288 OK: (floor 3.6) ==> 3
    289 OK: (floor 3.9) ==> 3
    290 OK: (floor 3.999) ==> 3
    291 OK: (sqrt 3.999) ==> 1.99975
    292 OK: (sqrt 4) ==> 2
    293 OK: (sqrt -4) ==> nan
    294 OK: (exp 1) ==> 2.71828
    295 OK: (exp 2) ==> 7.38906
    296 OK: (atan 45) ==> 1.54858
    297 OK: (atan 12.3) ==> 1.48967
    298 OK: (acos 1) ==> 0
    299 OK: (acos 0.5) ==> 1.0472
    300 OK: (asin 1) ==> 1.5708
    301 OK: (asin 0.5) ==> 0.523599
    302 OK: (tan 0.5) ==> 0.546302
    303 OK: (cos 0.5) ==> 0.877583
    304 OK: (sin (* 0.5 3.14159)) ==> 1
    305 OK: (log 256) ==> 5.54518
    306 OK: (/ (log 256) (log 2)) ==> 8
    307 OK: (/ (log 1024) (log 2)) ==> 10
    308 OK: (llvm:gcd (* 11 123) (* 2 11)) ==> 11
    309 OK: (llvm:gcd (* 12 123) (* 2 12)) ==> 12
    310 OK: (llvm:gcd 444 555) ==> 111
    311 OK: (assert-type (quote string) foo) ==> #t
    312 OK: (assert-type (quote integer) 123) ==> #t
    313 OK: (assert-type (quote decimal) 123.45) ==> #t
    314 OK: (assert-type (quote pair) (list (quote a) (quote b))) ==> #t
    315 OK: (assert-type (quote symbol) (quote foo)) ==> #t
    316 OK: (assert-number 123) ==> #t
    317 OK: (assert-number 123.3) ==> #t
    318 OK: (assert-number 0) ==> #t
    319 OK: (assert-number 0) ==> #t
    320 OK: (assert-length 3 (list 1 2 3)) ==> #t
    321 OK: (assert-length 1 10 (list 1 2 3)) ==> #t
    322 OK: (assert-length-min 2 (list 1 2 3)) ==> #t
    323 OK: (assert-length-min 2 (list 1 2)) ==> #t
    324 OK: (string-length (make-string 0)) ==> 0
    325 OK: (string-length (make-string 1)) ==> 1
    326 OK: (string-length (make-string 2)) ==> 2
    327 OK: (string-length (make-string 42)) ==> 42
    328 OK: (string-length (make-string 42 a)) ==> 42
    329 OK: (string-length (make-string 42 0)) ==> 42
    330 OK: (make-string 1 () ==> (
    331 OK: (make-string 2 () ==> ((
    332 OK: (make-string 3 () ==> (((
    333 OK: (make-string 3 )) ==> )))
    334 OK: (make-string 5 a) ==> aaaaa
    335 OK: (make-string 5 A) ==> AAAAA
    336 OK: (make-string 7  ) ==>        
    
    The following test shows that sprint() doesn't escape chars
    
    337 OK: (string-length (make-string 9 
    )) ==> 9
    338 OK: (string-length (make-string 7 	)) ==> 7
    339 OK: (string-length (make-string 3 )) ==> 3
    
    The following two tests show a bug in the string implementation:
    
    340 FAIL: (string-length (make-string 3)) != 3
      Actual result: '0'
    341 FAIL: (string-length (make-string 3)) != 3
      Actual result: '0'
    342 OK: (+ 1 3) ==> 4
    343 OK: (+ 1 3) ==> 4
    344 OK: (quasiquote (1 2 (unquote (+ 3 4)))) ==> (1 2 7)
    345 OK: (list) ==> ()
    346 OK: (list) ==> ()
    347 OK: (list) ==> ()
    348 OK: (list) ==> ()
    
    Results
    346 / 348 tests OK, 2 failed
    
    no-args
    one-arg, with x='Uno!'
    one-arg-rest with x='Uno' and rest='dos!'
    one-arg-rest with x='Uno' and rest='dos tres!'
    one-arg-rest with x='Uno' and rest='dos tres cuatro!'
    two-arg-rest with x='Uno' y='dos!' and rest=''
    two-arg-rest with x='Uno' y='dos' and rest='tres!'
    two-arg-rest with x='Uno' y='dos' and rest='tres cuatro!'
    rest with rest='Uno!'
    rest with rest='Uno dos!'
    rest with rest='Uno dos tres!'
    rest with rest=''
    Fin!
    (vector 1 2 3 (list 4 5) 6) => #(1 2 3 (4 5) 6)
    (+ 1 2 3) => 6
    (vector? (vector (quote abba))) => #t
    (vector-length (vector 1 2 3 (list 4 5) 6)) => 5
    (make-vector 7) => #(#<nil> #<nil> #<nil> #<nil> #<nil> #<nil> #<nil>)
    (make-vector 7 (quote a)) => #(a a a a a a a)
    (vector-ref (vector 1 2 3 (list 4 5) 6) 0) => 1
    (vector-ref (vector 1 2 3 (list 4 5) 6) 1) => 2
    (vector-ref (vector 1 2 3 (list 4 5) 6) 2) => 3
    (vector-ref (vector 1 2 3 (list 4 5) 6) 3) => (4 5)
    (vector-ref (vector 1 2 3 (list 4 5) 6) 4) => 6
    (define v (vector 1 2 (list 3 4) 5 6)) => 
    v => #(1 2 (3 4) 5 6)
    (vector-set! v 0 10) => 
    v => #(10 2 (3 4) 5 6)
    (vector->list (vector 1 2 (vector 3 4) 5 6)) => (1 2 #(3 4) 5 6)
    (list->vector (quote (1 2 3))) => #(1 2 3)
    (list->vector (list 1 2 (list 3 4) 5 6)) => #(1 2 (3 4) 5 6)
    (vector->string (vector a b b a)) => abba
    (string->vector Hello!) => #(#\H #\e #\l #\l #\o #\!)
    (define v2 (vector-copy v)) => 
    v2 => #(10 2 (3 4) 5 6)
    (vector-set! v2 0 X) => 
    v2 => #(#\X 2 (3 4) 5 6)
    v => #(10 2 (3 4) 5 6)
    (vector-length v2) => 5
    (vector-fill! v2 x) => 
    v2 => #(#\x #\x #\x #\x #\x)
    (vector-length v2) => 5
    (Mickey Scheme (C) 2011 Christian Stigen Larsen
     Using Readline 4.2
     Compiler version: 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.9.00)
    )
    

