[![Build Status](https://app.travis-ci.com/joergen7/lang-util.svg?branch=master)](https://app.travis-ci.com/joergen7/lang-util)

# lang-util
Basic utilities for language models in Common Lisp

The lang-util library provides common operations in handling language models like finding the first duplicate in a list or indenting/commenting the lines in a string.

## System Requirements

- A Common Lisp distribution; I tested the following distributions:
  * [SBCL](https://www.sbcl.org/)
  * [ABCL](https://abcl.org/)
- [ASDF](https://asdf.common-lisp.dev/) comes packaged with the aforementioned CL distributions
- [FiveAM](https://fiveam.common-lisp.dev/) and its dependencies

ASDF looks for system definitions in the `~/common-lisp/` folder. Create it and change to it.

``` bash
mkdir -p ~/common-lisp
cd ~/common-lisp
```

### Installing FiveAM

To run the test suite you must have the [FiveAM](https://fiveam.common-lisp.dev/) regression testing framework and its dependencies available. We use ASDF to manage systems and their dependencies. In the `~/common-lisp` folder run

``` bash
git clone http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git
git clone http://common-lisp.net/projects/alexandria/alexandria.git
git clone https://github.com/didierverna/asdf-flv.git
git clone https://github.com/lispci/fiveam.git
```


### Adding lang-util

The lang-util library also needs to be visible to ASDF. Assuming your clone of the lang-util repository resides in `~/git/lang-util/` this can be accomplished by creating a symbolic link.

``` bash
ln -s ~/git/lang-util ~/common-lisp/lang-util
```

Eventually, you should end up with a directory structure like this:

    ~/
	+- common-lisp/
	   +- alexandria/
	   +- asdf-flv/
	   +- fiveam/
	   +- lang-util/
	   +- trivial-backtrace/

## Loading lang-util

You can load the library using either ASDF or Quicklisp. Below, we give instructions for each method.

### Loading with ASDF

Most Common Lisp distributions come with ASDF packaged. Thus, you can simply require ASDF and load the lang-util system like so:

``` cl
(require :asdf)
(asdf:load-system :lang-util)
```

## Loading lang-util with Quicklisp

In addition to ASDF, you can load lang-util using [Quicklisp](https://www.quicklisp.org/beta/). Assuming, you have Quicklisp loaded you can run

``` cl
(ql:quickload :lang-util)
```

## Testing

### Running the Test Suite

With FiveAM and its dependencies in place you can now run:

``` cl
(require :asdf)
(asdf:test-system :lang-util)
```

### Coverage Info

Assuming, that you are running [SBCL](https://www.sbcl.org/) you can get coverage information using SBCL's  [sb-cover](http://www.sbcl.org/manual/#sb_002dcover) module.

``` cl
(require :asdf)
(require :sb-cover)
(asdf:test-system :lang-util)
(sb-cover:report "coverage/")
```

## Examples

Below, you find examples on how the library can be used.

### Finding Duplicates in Lists

The `find-duplicate` function allows to find duplicate entries in a list. What makes a duplicate can be defined by giving a test predicate that takes two arguments.

``` cl
(find-duplicate '(1 2 1))
1
```

``` cl
(find-duplicate '(1 2 3) :test #'=)
nil
```

``` cl
(find-duplicate '(a a) :test #'cl:eq)
'a
```

### Padding Lines in a String

``` cl
(line-pad "this is a comment" "// ")
"// this is a comment"
```

``` cl
(line-pad (format nil "indented~%lines") "  ")
(format nil "  indented~%  lines")
```

``` cl
(line-pad
  (format nil "#include <don't indent this>~%but indent everything else")
  "  "
  :unless-starts-with #\#)
(format nil "#include <don't indent this>~%  but indent everything else")
```

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)
