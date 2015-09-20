# shell.struct

A collection of small programs for manipulating data structures at the command line. Why? Why not?

No attempt is made to be efficient because who cares.

## zstack

Manipulate named stacks of strings from the command line. A *stack* is a data structure consisting of zero or more items in a sequence. There are three basic operations we can perform on a stack: *push* places a new item at the beginning of the sequence, *pop* removes the first item from the sequence so we can see it, and *peek* lets us see the first item in the list without removing it (peek is equivalent to pop followed by push). We cannot directly view or manipulate any items in the sequence other than the first.

You can play with a default stack...

* `zstack push "hello"`: push the string "hello" (without quotes) onto the default stack. You can also give multiple string arguments or omit quotes like `zstack push "hello" there this "is an" example`.
* `zstack push`: push the contents of stdin onto the default stack.
* `zstack pop`: remove the top item of the default stack and write to stdout.
* `zstack peek`: write top item of default stack to stdout (do not remove).
* `zstack print`: write entire default stack to stdout.

...or with several different named stacks. (The default stack is really just a named stack named `stack`.) Pushing to a named stack creates it automagically.

* `zstack FOO push "hello"`: push the string "hello" (without quotes) onto the stack named FOO.
* `zstack FOO push`: etc
* `zstack FOO pop`: etc
* `zstack FOO peek`: etc
* `zstack FOO print`: etc

*stack*, *push*, *pop*, *peek*, *print*, *help*, and *show* are not good names for stacks since they will be interpreted as commands. I also recommend sticking to alphanumeric names with hyphens and without spaces, such as `kreb-123`, since the stack name will be used as a filename.

Other options:

* `zstack show`: write list of all named stacks to stdout
* `zstack help`: print usage

Stack data is stored at `$HOME/.shellstruct/stack`. Check it out sometime!
