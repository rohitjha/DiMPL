DiMPL
=====

Discrete Mathematics Programming Language - A Domain-Specific Language for Discrete Mathematics.

Current Version: 0.1

This DSL is implemented in Haskell, which lends itself easily to mathematical programming. The language has a syntax closer to the mathematical notation used in discrete mathematics.

The library covers functions from the areas of:

1. Mathematical Logic
2. Set Theory
3. Graph Theory
4. Number Theory
5. Linear Algebra
6. Combinatorics

Apart from aiding mathematicians and physicists, DiMPL is useful in studying and describing objects and problems in branches of computer science, such as algorithms, programming languages, cryptography, and software development.

Installation
------------

Run these commands from the command line to install the DSL library:

$ cabal configure

$ cabal build

$ cabal install

$ cabal clean (optional)

The Preprocessor is simply the file "preprocessor.sh", which uses sed scripts written in the file "script", so these can be added to PATH in order to be used for all programs. Both "preprocessor.sh" and "script" can be found in the "preprocessor" directory.

Usage
-----

$ ghc -F -pgmF preprocess.sh file.hs

This generates "file.o", "file.hi" and the executable "file". The executable can be run by typing:

$ ./file

The library can even be used in GHCi (without the preprocessor) just like any other package.

Help
----

E-mail: rohit305jha@gmail.com
