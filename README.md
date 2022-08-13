# lang-util
Basic utilities for language models in Common Lisp

## System Requirements

- A Common Lisp distribution; I tested the following distributions:
  * [SBCL](https://www.sbcl.org/)
  * [ABCL](https://abcl.org/)
- FiveAM and its dependencies

## Loading with ASDF

You can load lang-util using [ASDF](https://asdf.common-lisp.dev/). ASDF looks for system definitions in the `common-lisp` folder relative to the home directory. Thus, the first step is to create a link here to the `lang-util` folder. Assuming your clone of the lang-util repository resides in `~/git/lang-util` this can be accomplished by creating a symbolic link.

	mkdir -p ~/common-lisp
    ln -s ~/git/lang-util ~/common-lisp/lang-util

Most Common Lisp distributions come with ASDF packaged. Thus, you can simply require ASDF and load the lang-util system like so:

```lisp
(require :asdf)
(asdf:load-system :lang-util)
```

## Loading with Quicklisp

In addition to ASDF, you can load lang-util using [Quicklisp](https://www.quicklisp.org/beta/).

## Testing

### Installing FiveAM

To run the test suite you must have the [FiveAM](https://fiveam.common-lisp.dev/) regression testing framework and its dependencies available. In the `common-lisp` folder relative to your home directory run

    git clone http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git
	git clone http://common-lisp.net/projects/alexandria/alexandria.git
	git clone https://github.com/didierverna/asdf-flv.git
	git clone https://github.com/lispci/fiveam.git
	
### Running the Test Suite

With FiveAM and its dependencies in place you can now run:

```lisp
(require :asdf)
(asdf:test-system :lang-util)
```

## Coverage Info

Assuming, that you are running [SBCL](https://www.sbcl.org/) you can get coverage information using SBCL's  [sb-cover](http://www.sbcl.org/manual/#sb_002dcover) module.

```lisp
(require :asdf)
(require :sb-cover)
(asdf:test-system :lang-util)
(sb-cover:report "coverage/")
```

## Examples

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)
