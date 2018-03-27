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