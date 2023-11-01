# Lucian CI/CD

![761B2A85-6DA4-4EA7-9F20-8CCB2DC60D28](https://user-images.githubusercontent.com/15845542/234572372-8446f119-6151-4ea8-844b-4df89f605143.jpeg)

* Image of different coloured pipelines

* Single-user continuous integration and continuous deployment.  Integrates (merges changed repositories from combinations of changes), builds repositories that depend on these repositories and tests predicates implicated, where tests are in comments before each predicate.

* Bottom-up Version: Lucian CI/CD can now check sets of repositories bottom-up, which means there are up to seven possible changes to a set of current predicates to find a working combination of. Current predicates are each predicate in depth first, post order, or sets of clauses or predicates involved in loops.

* Other programming languages apart from Prolog aren't fully supported yet (even though the <a href="https://github.com/luciangreen/luciancicd/tree/ba8cba7e419e036050cb0ff203e9289b91897169">non-bottom-up version</a> does).
# Getting Started

Please read the following instructions on how to install the project on your computer for automatic testing.

# Prerequisites

* Please download and install SWI-Prolog for your machine at `https://www.swi-prolog.org/build/`.

# 1. Install manually

* Download:
* <a href="https://github.com/luciangreen/luciancicd">this repository</a> and its dependencies
* <a href="https://github.com/luciangreen/List-Prolog-to-Prolog-Converter">List Prolog to Prolog Converter</a>

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download <a href="https://github.com/luciangreen/luciancicd">Lucian CI/CD</a>:

```
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
['lppm'].
lppm_install("luciangreen","luciancicd").
halt
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

* Change the LPPM user (your GitHub username) and the repositories and any omitted folders in `settings.pl`.

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

* which contains the current main file in the repository and its possible main predicate names and arities (the number of arguments). If a repository contains no main files to test, enter `[]` in it.

* Note: Dependencies are in `List-Prolog-Package-Manager/lppm_registry.txt`, (LPPM) in form `[[User,Repository,Description,Dependencies], etc]`, e.g.

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

* Lucian CI/CD only returns an overall successful result if all dependencies connected to a repository, their main files and predicates and each level in the bottom-up order successfully passes all tests for each predicate.

* Lucian CI/CD works with:
    * Prolog files
    * Text data files

* So far, Prolog files should test Prolog.
* Other text data files and some comments that aren't in the latest version may not necessarily be kept by Lucian CI/CD. <b>Important:</b> run `set_up_luciancicd.` to prevent tests and comments from being deleted before entering code to test changes in. Also check if `main_file.txt` contains all main files and main predicates.

* Please ensure that each test's file loads the necessary files.

* `set_up_luciancicd.` - Records time modified of repositories in `repositories_paths1//1` in `settings.pl`. I.e. it saves the files without testing them.

* `luciancicd.` - Tests repositories with changed modification dates. Run before committing changes.

* For more info, see a <a href="https://dev.to/luciangreen/an-open-source-cicd-for-prolog-29h2">Dev.to article about Lucian CI/CD</a>.

* Note: Once all tests are successful, the files in the temporary folder `../private2/luciancicd-testing/` are moved into the repository. To undo this, enter `move_to_repository_or_back.`.

* Once the files are in the repository, you can commit the changes.

* Run `set_up_luciancicd.` to save the files without running Lucian CI/CD.

* Note: A notification such as `"Cannot find "../private2/luciancicd-cicd-tests/tests_a.txt"` means the repository `"a"` is connected with a changed file through dependencies but Lucian CI/CD can't install it.

* <a href="https://www.youtube.com/watch?v=MS240K8TXtM">Lucian Academy - Lucian CI/CD Demo</a>

# Lucian CI/CD Web Service

* To list available logs and diff files, enter `luciancicd_server(8000).`, then go to `http://localhost:8000/luciancicd`.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the <a href="LICENSE">LICENSE.md</a> file for details

