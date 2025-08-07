# Lucian CI/CD

* CI (verification) works, but CD (modification) only works with a non-M2 or later (neural) processor on Apple. It may need backdating to when I bought an M2 MacBook Air on 1 May 2024.

![761B2A85-6DA4-4EA7-9F20-8CCB2DC60D28](https://user-images.githubusercontent.com/15845542/234572372-8446f119-6151-4ea8-844b-4df89f605143.jpeg)

* Image of different coloured pipelines

* Single-user continuous integration and continuous deployment.  Integrates (merges changed repositories from combinations of changes), builds repositories that depend on these repositories and tests predicates implicated, where tests are in comments before each predicate.

* Bottom-up Version: Lucian CI/CD can now check sets of repositories bottom-up, which means there are up to seven possible changes to a set of current predicates to find a working combination. Current predicates are each in depth-first, post-order, or sets of clauses or predicates involved in loops.

* Other programming languages apart from Prolog aren't fully supported yet (even though the <a href="https://github.com/luciangreen/luciancicd/tree/ba8cba7e419e036050cb0ff203e9289b91897169">non-bottom-up version</a> does).

# Getting Started

Please read the following instructions on installing the project on your computer for automatic testing.

# Prerequisites

* Use a search engine to find the Homebrew (or other) Terminal install command for your platform and install it, and search for the Terminal command to install swipl using Homebrew and install it or download and install SWI-Prolog for your machine at <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a>.

# Mac, Linux and Windows (with Linux commands installed): Prepare to run swipl

* In Terminal settings (Mac), make Bash the default shell:

```
/bin/bash
```

* In Terminal, edit the text file `~/.bashrc` using the text editor Nano:

```
nano ~/.bashrc
```

* Add the following to the file `~/.bashrc`:

```
export PATH="$PATH:/opt/homebrew/bin/"
```

* Link to swipl in Terminal:

```
sudo ln -s /opt/homebrew/bin/swipl /usr/local/bin/swipl
```

# 1. Install manually

* Download:
* <a href="https://github.com/luciangreen/luciancicd">this repository</a> and its dependencies
* <a href="https://github.com/luciangreen/List-Prolog-to-Prolog-Converter">List Prolog to Prolog Converter</a>

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>:

```
mkdir GitHub
cd GitHub/
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
['lppm'].
lppm_install("luciangreen","luciancicd").
../
halt.
```

# Running

* In Shell:
`cd luciancicd`
`swipl`

* To load the algorithm, enter:
```
['luciancicd.pl'].
```

# Instructions and troubleshooting

* To correct failures, it's essential to run Lucian CI/CD on algorithms all at once or one predicate at a time with dependencies, then make the necessary changes and rerun the algorithm.

* Lucian CI/CD is a necessary way to refactor, debug, and point to debugging. Lucian CI/CD builds the algorithm with only parts supported by the tests. This method requires checking that the code is as complete as possible, the tests, the code afterwards, and the algorithm's result.

* In the folder `GitHub2` (specified in `settings.pl`, not to be confused with `GitHub` or the folder's name housing your GitHub repositories), store your repositories, e.g. `b`. These folders contain Prolog (`*.pl`) and other files.

* Learn Prolog with <a href="https://lucianpedia.fandom.com/wiki/Prolog_Primer_with_family.pl">family.pl</a> and search for Prolog tutorials. Examine the examples in <a href="https://github.com/luciangreen/luciancicd/blob/main/luciancicdverify.pl">luciancicdverify.pl</a> and write examples to help test and debug algorithms. The following instructions will guide you in using Lucian CI/CD more easily.

* Change the LPPM user (your GitHub username), the repositories, any omitted folders, the output folder, `fail_if_greater_than_n_changes1` (used to find more combinations of lines to test) and the time limit for running algorithms in `settings.pl`.

* If necessary, modify tests in the Prolog files in the repositories, for example:

```
% remove_and_find_item_number([a,b,c],2,c,N2).
% N2 = 2.
% remove_and_find_item_number([a,b,b,c],2,c,N3).
% N3 = 3.
```

* Write a `main_file.txt` in the main folder of each repository, e.g.:

```
[
 ["c.pl",
  [[c,2],[d,3]]
 ],
 ["c1.pl",
  [[c1,2],[d1,3]]
 ]
] 
```

* contains the current main file in the repository and its possible main predicate names and arities (the number of arguments). Suppose a repository contains no main files to test; enter `[]`.

* Note: Dependencies are in `List-Prolog-Package-Manager/lppm_registry.txt` (LPPM) in the form `[[User,Repository,Description,Dependencies], etc]`, e.g.

```
["luciangreen","Daily-Regimen","Scripts for daily meditation, bot prep, uni and breasoning.",
[
	["luciangreen","listprologinterpreter"],
	["luciangreen","Languages"],
	["luciangreen","culturaltranslationtool"],
	["luciangreen","Algorithm-Writer-with-Lists"],
	["luciangreen","Text-to-Breasonings"],
	["luciangreen","mindreader"]
]]
```

* Lucian CI/CD only returns an overall successful result if all dependencies connected to a repository, their main files and predicates, and each level in the bottom-up order successfully pass all tests for each predicate.

* Lucian CI/CD works with:
    * Prolog files
    * Text data files

* Check if `main_file.txt` contains all main files (files with main predicates) and main predicates.

* Please ensure that each repository set loads the necessary files.

* `set_up_luciancicd.` - Records time modified of repositories in `repositories_paths1//1` (from `settings.pl`). I.e. it saves the files without testing them.

* `luciancicd.` - Tests repositories with changed modification dates. Run before committing changes.

* For more info, see a <a href="https://dev.to/luciangreen/an-open-source-cicd-for-prolog-29h2">Dev.to article about Lucian CI/CD</a>.

* Note: Once all tests are successful, the files in the temporary folder `../luciancicd-testing/` are moved into the repository. To undo this, enter `move_to_repository_or_back.`.

* You can commit the changes Once the files are in the repository.

* Note: A notification such as `"Cannot find "../luciancicd-cicd-tests/tests_a.txt"` means the repository `"a"` is connected with a changed file through dependencies, but Lucian CI/CD can't install it.

* <a href="https://www.youtube.com/watch?v=MS240K8TXtM">Lucian Academy - Lucian CI/CD Demo</a>

# Lucian CI/CD Web Service

* To list available logs and diff files, enter `luciancicd_server(8000).`, then go to `http://localhost:8000/luciancicd`.

# Caveats

* The container predicate used to run predicates in containers to save memory assumes that `luciancicd` is installed at `/Users/username/Dropbox/GitHub/luciancicd/`, where `username` is the username.

* Causes of the "1 or more tests failed." error include the first entered algorithm after deleting `Github_lc/tests_*x*.txt` not passing all tests. Programs with uninstantiated variables, etc, also cause the error.

* Writeln and commands that don't produce checkable output are not kept unless they are in `keep1/1` in the `keep.pl` register.

* The generated code currently loses formatting (extra newlines, etc.). Lucian CI/CD will print the code nicely later.
 
* Increase N to a higher number in `fail_if_greater_than_n_changes1(N).` in `settings.pl` (or the `fail_if_greater_than_n_changes1 overrider` value in `luciancicdverify.pl` to N in `[Increase Max to,N]` in the log in tests) to improve performance.

* Before testing, Lucian CI/CD backs up `GitHub2o` and `Github_lc` in `gh2_tmp2` and `GitHub2` in `gh2_tmp` (at the same level as `GitHub`, see `settings.pl`).

* If testing is problematic, remove the contents of `GitHub2`, `GitHub_lc`, and `GitHub2o`.

* Lucian CI/CD functions best with 40 GB of memory (free hard disk space).

# Compiling Lucian CI/CD for better performance

* Enter the command, e.g. `luciancicd` after `main:-` in `main.pl`.

* In bash, compile Lucian CI/CD with `swipl --goal=main --stand_alone=true -o luciancicd -c luciancicd.pl`.

* In bash, run with `./luciancicd`.

# Tests

* Running the tests
To run all tests, enter:
`lc_test(NTotal,Score).`

To run a specific test:
`lc_test1(TestNumber,Passed).`
where TestNumber is the test number from <a href="luciancicdverify.pl">luciancicdverify.pl</a>.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the <a href="LICENSE">LICENSE.md</a> file for details
