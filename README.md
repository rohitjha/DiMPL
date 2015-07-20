MPL
===

Mathematical Programming Language - A Domain-Specific Language for Discrete Mathematics.

Current Version: 0.3.0.0

This language (MPL) is implemented in Haskell, which lends itself easily to mathematical programming. The language has a syntax closer to the mathematical notation. This is achieved by applying the custom preprocessor, "preprocess.sh", while compiling with GHC (Glasgow Haskell Compiler).

The library covers functions from the areas of:

1. Mathematical Logic
2. Set Theory
3. Graph Theory
4. Number Theory
5. Linear Algebra
6. Combinatorics

Apart from aiding mathematicians and physicists, the developed DSL is useful in studying and describing objects and problems in branches of computer science, such as algorithms, programming languages, cryptography, and software development. A paper for this DSL, published in the International Journal of Computer Applications (IJCA), can be found here: http://www.ijcaonline.org/archives/volume70/number15/12036-7257

Installation
------------

Run these commands from the command line to install the DSL library:

$ cabal configure

$ cabal build

$ cabal install

$ cabal clean (optional)

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
