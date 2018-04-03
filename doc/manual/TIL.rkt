#lang scribble/manual

@title{The Inverse Language}

@section{Introduction}

The inverse language allows a user to construct and use the algebraic inverses of functions.

@racketblock[
 > (add1 5)
 6
 > ((invert add1) 5)
 4
 ]

@section{Core Language Features}

@defproc[(invert [func invertible?])
         invertible?]{
 Returns the inverse of an invertible function,
 or an error if the provided function is not invertible.
}


@defproc[(noinvert [invfunc invertible?]) procedure?]{
 Given an invertible function, return a regular non-invertible procedure.
}

@defproc[(invertible? [x any/c]) boolean?]{
 Determines if its input is an invertible function
}

@deftogether[(@defform[(lambda-create-invertible (argument) body invbody)]
               @defform[(λ-create-invertible (argument) body invbody)])]{
 Creates an invertible function with a specified body and inverse.
}

@deftogether[(@defform[(lambda-auto-invertible (argument) body)]
               @defform[(λ-auto-invertible (argument) body)])]{
 Automatically creates the inverse of a function provided that
 the body of the function meets specific constraints.

 The function must:
 @itemize{
  @item{take in only one argument}
  @item{be composed of invertible function.
   Note that you can apply functions here if they yield invertible functions}
 }
 
 If you apply a function that yields an invertible function, the argument
 to the autoinvertible function must not be used.

 For example, this is allowed:
 @racketblock[
 (λ-auto-invertible (x) ((invert (addn 7)) x))
 (λ-auto-invertible (x) ((dummy-func (λ (x) x)) x))
 ]

 But this is not allowed:
 @racketblock[
 (λ-auto-invertible (x) ((invert (addn x)) 7))
 (λ-auto-invertible (x) ((dummy-func (λ (y) x)) x))
 ]
}

@deftogether[(@defform[(declare-invertible id id)]
               @defform[(declare-invertible id procedure?)]
               @defform[(declare-invertible procedure? id)])]{
 Must be a top-level form
                                                              
 Sets two functions to be the inverses of each other.
 All ids used must be settable from the current context.
}

@subsection{Inverse Checking}
Note that the language will automatically check
to make sure that functions are really inverses of each other at runtime.

@racketblock[
 (define evil-func (λ-create-invertible (x)
                                        (+ x 1)
                                        (* x 0)))
 > (evil-func 0)
 1
 > ((invert evil-func) (evil-func 0))
 0
 > (evil-func 1)
 ERROR: Not a true invertible function for argument 1
 Applying the inverse to the result yields 0 instead
 ]
             

@section{Arithmetic}

@deftogether[(@defproc[(addn [n number?]) invertible?]
               @defproc[(subn [n number?]) invertible?])]{
 Creates a function that given a number, adds or subtracts n to that number

 @racketblock[
 > ((addn 5) 7)
 12
 > ((invert (addn 5)) 7)
 2
 ]
}

@deftogether[(@defproc[(add1 [x number?]) number?]
               @defproc[(sub1 [x number?]) number?])]{
 Adds or subtracts 1 to x. Equivalent to @racket[(addn 1)]
 or @racket[(subn 1)].
}

@deftogether[(@defproc[(muln [n number?]) invertible?]
               @defproc[(divn [n number?]) invertible?])]{
 Creates a function that given a number, multiplies or divides
 that number by n.
}

@section{Strings}

@defproc[(prepend-str [str string?]) [string? <-> string?]]{
 Creates a function that prepends a str onto a given string.
 The inverse function will remove str from the beginning of a given string, if str
 is the prefix of that string. If it is not, an error will be thrown.

 @racketblock[
 > ((prepend-str "Hello ") "World")
 "Hello World"
 >((invert (prepend-str "Hello ")) "Hello World")
 "World"
 >((invert (prepend-str "Hello ")) "Doesn't start with Hello")
 Error: Not a true invertible function...
 ]
}

@defproc[(append-str [str string?]) [string? <-> string?]]{
 Creates a function that appends a str onto a given string.
 The inverse function will remove str from the beginning of a given string, if str
 is the postfix of that string. If it is not, an error will be thrown.
}


@section{Utils}

@defproc[(invert-result [givesFunc [X -> [Y <-> Z]]]) [X -> [Z <-> Z=Y]]]{
 Given a function that produces an invertible function,
 produce a function that inverts the result of that.

 @racketblock[
 (define subn (invert-result addn))
 ]
}

@defform[(noinvert-out invertible? id id)]{
 Provide an invertible function out as two regular functions.

 @racketblock[
 (module m inverse
   (define x (addn 5))
   (provide (noinvert-out x add5 sub5)))

 (require 'm)
 (add5 7)
 (sub5 7)]
}

@section{Testing library}

This testing library relies on rackunit as its backend.

@defproc[(check-expect [actual any/c] [expected any/c]) void?]{
 Tests if two values are equal
 @racketblock[
 (check-expect 1 1) Passes
 (check-expect 1 2) Fails
 ]
}

@defform[(check-syntax-fail s-exp)]{
 The test passes if the s-exp contains a syntax error.
 Note that this test form does not work in the interactions window.

 @racketblock[
 (check-syntax-fail (λ () x)) Passes
 (check-syntax-fail (λ (x) x)) Fails]
}

@defform[(check-fail s-exp)]{
 The test passes if the body produces any runtime error.

 @racketblock[
 (check-fail (error "Hello")) Passes
 (check-fail "Hello") Fails]
}


