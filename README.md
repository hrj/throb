# throb

`throb` is a parser combinator library that can offer completions to the (possibly incomplete) input string.

It is suitable for parsing interactive input from a human user.

For a demo application see [nlq demo](http://nlq.lavadip.com/servlet/demo).

The combinator API is pretty similar to the standard parser combinator library that ships with scala.

The parser result, apart from success / failure, indicates the result of partial parsing and returns a list of possible
completions.

See the test cases for examples of how to use it.

Note that this was developed as part of a prototype. No attempt at a formal proof for parsing correctness has been made yet.
