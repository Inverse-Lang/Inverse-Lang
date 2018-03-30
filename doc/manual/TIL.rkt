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

@section{Arithmetic}

@section{Strings}

@section{Utils}