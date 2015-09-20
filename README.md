# shell.struct

A collection of small programs for manipulating data structures at the command line.

## stack

Manipulate named stacks of strings from the command line.

Basic usage:

* `zstack push "hello"`: push the string "hello" (without quotes) onto the default stack.
* `zstack push`: push the contents of stdin onto the default stack.
* `zstack pop`: remove the top item of the default stack and write to stdout.
* `zstack peek`: write top item of default stack to stdout (do not remove).
* `zstack FOO push "hello"`: push the string "hello" (without quotes) onto the stack named FOO.
* `zstack FOO pop`: etc
* `zstack FOO peek`: etc
* `zstack show`: write list of named stacks to stdout
* `zstack help`: print usage

Stack data is stored at $HOME/.shellstruct/stack.
