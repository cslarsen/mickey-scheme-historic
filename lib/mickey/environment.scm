#|

Library: (mickey uname)

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1

|#

(import (only (scheme base) define)
        (mickey library))

(open-library "lib/mickey/libmickey-environment.so" 'lazy)

(define bound?                  (bind-procedure "proc_boundp"))
(define environment-assign!     (bind-procedure "proc_env_assign"))
(define environment-assignable? (bind-procedure "proc_env_assignablep"))
(define environment-bindings    (bind-procedure "proc_env_bindings"))
(define environment-bound-names (bind-procedure "proc_env_bound_names"))
(define environment-bound?      (bind-procedure "proc_env_boundp"))
(define environment-eval        (bind-syntax    "proc_env_eval"))
(define environment-has-parent? (bind-procedure "proc_env_has_parentp"))
(define environment-lookup      (bind-procedure "proc_env_lookup"))
(define environment-parent      (bind-procedure "proc_env_parent"))
(define environment?            (bind-procedure "proc_envp"))
(define make-environment        (bind-syntax    "proc_make_environment"))
(define the-environment         (bind-procedure "proc_the_environment"))
