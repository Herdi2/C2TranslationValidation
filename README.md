# C2TranslationValidation
This is the translation validation tool C2tv, used in my Master's thesis.
It applies translation validation to the intermediate representation
of the C2 compiler, which is the optimizing Just-in-Time compiler for 
the HotSpot Java Virtual Machine (JVM).

(NOTE: For my thesis work commit 42e8dfed04fd6b5befd0ea1dd8cc12d7ae543ae4 was used)

## Setup
This project is written in Haskell and uses the Stack build tool.
Furthermore, it requires a debug build of the JVM.
Make sure the following dependencies are installed:
1. `stack`, can be installed through [GHCup](https://www.haskell.org/ghcup/)
2. A debug build of the [JVM](https://github.com/Herdi2/jdk-thesis)
3. `z3`, which is the backend SMT solver found [here](https://github.com/z3prover/z3)

To run the comparison between C2tv and Wu's tool, you need to install it as well from [here](https://github.com/TerenceNg03/c2-translation-validation).

For more setup details, see the [nix folder](./nix/) in the source root for dependencies.

## Usage
To run C2tv, either use `stack run` or install the executable to `/usr/bin/` 
using `stack install`.
After installing, invoke `c2tv --help` to see all possible commands:

```
Usage: c2tv [-j|--java <FILE>] [--ReintroduceBugs] [-m|--MemoryBugs <INT>] 
            [-c|--ControlBugs <INT>] COMMAND

Available options:
  -j,--java <FILE>         Path to the Java binary to use (default: "java")
  --ReintroduceBugs        Choose to reintroduce the olds bugs used in Wu's
                           verification.
  -m,--MemoryBugs <INT>    Memory bug to introduce [20, 30]. Default is 0.
  -c,--ControlBugs <INT>   Control bug to introduce [10, 20, 21]. Default is 0.
  -h,--help                Show this help text

Available commands:
  verify                   Verify Java file(s)
  compare                  Compares if the two given graphs are semantically
                           equivalent
  fuzz                     Run the fuzzer
  campaign                 Run the campaign
  ast                      Print the internal AST
```


