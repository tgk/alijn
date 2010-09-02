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

Requires [leiningen](http://github.com/technomancy/leiningen).
Requires CDK 1.3.5.git which I have not been able to find a maven repository with. 
To install in local repository [first fork cdk from github](http://github.com/egonw/cdk) and issue the commands

    ant dist-large
    mvn install:install-file -DgroupId=org.openscience -DartifactId=cdk -Dversion=1.3.5.git -Dpackaging=jar -Dfile=dist/jar/cdk-1.3.5.git.jar

Requires jama-1.0.1.jar (google for it) which can be installed by issuing

    mvn install:install-file -DgroupId=jama -DartifactId=jama -Dversion=1.0.1 -Dpackaging=jar -Dfile=jama-1.0.1.jar

After having installed leiningen and cloned the project, issue the following:

    lein deps
    lein native-deps

The program is now ready to run.

## License

Not added yet.
