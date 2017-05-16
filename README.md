# lambdacalc
### Haskell library that helps understanding Lambda Calculus.
Currently, the implemented features are:
* Type definition for Lambda Calculus (Var => variable, Lam => abstraction, App => application)
* Study the environment of a lambda expression: see the bind and free variables of an expression
* Reduce an expression with the beta reduction (step-by-step or recursively)
* The function `substitute` avoid the capture of variables
* Print a given lambda expression in a readable format
* Definition of useful lambda expression (`succ`,  `zero`, `peano` that converts an integer to a lambda expression using `succ`, `mult`, `pow` and the Y combinator: `yComb`)

This project is in deployment. The documentation will be provide shortly.
