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

#include <stdlib.h>
#include <stdexcept>

/*
 * Use TRY { ... } and CATCH ( spec ) { ... } for
 * exception
 * handling.  
 *
 * This makes it possible to revert to longjumps
 * if C++ exception handling has been disabled.
 *
 * By the way, it's a dirty trick to use `extern´ to ignore
 * the catch specifier.
 *
 */
#ifdef NO_EXCEPTIONS
# define TRY                     \
    bool got_exception = false;  \
    if ( exception_raised() ) {  \
      got_exception = true;      \
      goto CATCH_POINT;        }

# define CATCH(args)                             \
  CATCH_POINT:                                   \
  args = std::runtime_error(__exception.what()); \
  if (got_exception)
#else
# define TRY try
# define CATCH(args) catch(args)
#endif

/*
 * Raise error.  If you have defined NO_EXCEPTIONS,
 * the error will be reported and the process aborted.
 *
 * If NO_EXCEPTIONS has not been defined, an exception
 * will be raised.
 */
void raise(const std::exception&);

#ifdef NO_EXCEPTIONS
# include <setjmp.h>
extern jmp_buf catch_point;
extern std::runtime_error __exception;
# define exception_raised() setjmp(catch_point)
#endif
