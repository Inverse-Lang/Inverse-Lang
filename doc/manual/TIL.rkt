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

@deftogether[(@defform[(lambda-create-invertible (argument) body invbody)]
               @defform[(λ-create-invertible (argument) body invbody)])]{
 Creates an invertible function with a specified body and inverse.
}

@deftogether[(@defform[(lambda-auto-invert (argument) body)]
               @defform[(λ-auto-invert (argument) body)])]{
 Automatically creates the inverse of a function provided that
 the body of the function meets specific constraints.

 @itemize{
  @item{
   
  }
 }
}

@section{Arithmetic}

@section{Strings}

@section{Utils}