# alijn

A program for extracting and aligning molecular pharmacophores.

## Usage

Included with the program is an example defintion of features (`data/example/features.smarts`) and a set of 11 known binders to `comt` (`data/example/comt_subset.sdf`).
The feature definitions are extraced from the [Daylight online manual](http://www.daylight.com/dayhtml_tutorials/languages/smarts/smarts_examples.html) and the `comt` binders are taken from the [Directory of Usefull Decoys](http://dud.docking.org/r2/).

To see a command line usage instruction use `lein run --help`.
To try the enclosed example type

    lein run align -s 1 -f data/example/features.smarts data/example/comt_subset.sdf

Be aware that the program is still quite buggy.

## Installation

Only requires [leiningen](http://github.com/technomancy/leiningen).
After having installed leiningen and cloned the project, issue the following:

    lein deps
    lein native-deps

The program is now ready to run.

## License

Not added yet.
