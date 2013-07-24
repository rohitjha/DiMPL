MPL
===

Mathematical Programming Language - A Domain-Specific Language for Discrete Mathematics.

Current Version: 0.2.0

This language (MPL) is implemented in Haskell, which lends itself easily to mathematical programming. The language has a syntax closer to the mathematical notation. This is achieved by applying the custom preprocessor, "preprocess.sh", while compiling with GHC (Glasgow Haskell Compiler). Additionally, the Domain-Specific Language (DSL) has a library which is installed by running the "Setup.hs" file. The installation instructions can be found in the next section.

This library covers the areas of:

1. Mathematical Logic
2. Set Theory
3. Graph Theory
4. Number Theory
5. Linear Algebra
6. Combinatorics

Apart from aiding mathematicians and physicists, the developed DSL is useful in studying and describing objects and problems in branches of computer science, such as algorithms, programming languages, cryptography, automated theorem proving, and software development. A technical paper for this DSL is published in the International Journal of Computer Applications (IJCA) and can be found here: http://www.ijcaonline.org/archives/volume70/number15/12036-7257

Installation
------------

Run these commands from the command line to install the DSL library:

$ cabal configure
$ cabal build
$ cabal install

The package requires the "random" package to be installed as a dependency. Though a remote possibility, while configuring, you may have to install other packages which may be displayed as dependencies.

The Preprocessor is simply the file "preprocess.sh", which uses sed scripts written in the file "script", so these can be added to PATH in order to be used for all programs. Both "preprocess.sh" and "script" can be found in the "Preprocessor" directory.

Usage
-----

$ ghc -F -pgmF preprocess.sh file.hs

This generates "file.o", "file.hi" and the executable "file". The executable can be run by typing:

$ ./file

The library can even be used in GHCi (without the preprocessor) just like any other package.

Help
----

E-mail: rohit305jha@gmail.com
