# sptools

## Introduction
`sptools` is a frontend language library for the SourcePawn scripting language.


### Features

* Tokenizer
* Preprocessor
* Parser
* SourcePawn AST


## Contributing

To submit a patch, first file an issue and/or present a pull request.


### Code style
Code as you wish but use two slashes `//` or single star C++-style `/**/` for comments used as messages.

Commented out code should use three slashes `///` or double star C++-style.


### Known Bugs/Glitches

* Due to the Rust-style messaging system, syntax issues from macros can give inaccurate and/or confusing error messages.



## Help

If you need help or have any question, make an issue on the github repository.
Simply drop a message or your question and you'll be reached in no time!


## Installation

### Requirements

Golang version: Go 1.18


### Installation

Download library and import into your golang application.


### Using the Test Binaries

As of currently, the test binaries take any input file that contains SP code and will attempt to parse it.
On success (or error), you'll get as output a text file of all the generated tokens and the parse tree.
If you get a populated parse tree output file, search for "bad/err" as case insensitive to see what part of the code output to check for any silent parsing errors.


## Credits
* Kevin Yonan


## License

This project is licensed under MIT License.
