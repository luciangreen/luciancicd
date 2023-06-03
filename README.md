# Lucian CI/CD

![761B2A85-6DA4-4EA7-9F20-8CCB2DC60D28](https://user-images.githubusercontent.com/15845542/234572372-8446f119-6151-4ea8-844b-4df89f605143.jpeg)

* Image of different coloured pipelines

* Single-user continuous integration and continuous deployment.  Integrates (merges changed repositories from combinations of changes), builds repositories that depend on these repositories and tests predicates implicated, where tests are in comments before each predicate.

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

* `find_tests_from_repos.` - Finds tests in comments in repositories (NB. this is done at the start of `luciancicd.`).

* If necessary, modify tests in the Prolog files in the repositories, for example:

```
% remove_and_find_item_number([a,b,c],2,c,N2).
% N2 = 2.
% remove_and_find_item_number([a,b,b,c],2,c,N2).
% N2 = 3.
```

* Lucian CI/CD works with non-Prolog files, where Prolog files should test the non-Prolog files. In addition, follow the instructions below to keep non-testable changes to comments and non-Prolog (data) files.

* Please ensure that each test's file loads the necessary files.

* `set_up_luciancicd.` - Records time modified of repositories in `repositories_paths1//1` in `luciancicd.pl`.

* `luciancicd.` - Tests repositories with changed modification dates. Run before committing changes.

* Note: Dependencies are in `List-Prolog-Package-Manager/lppm_registry.txt`, in form `[[User,Repository,Description,Dependencies], etc]`.

* For more info, see a <a href="https://dev.to/luciangreen/an-open-source-cicd-for-prolog-29h2">Dev.to article about Lucian CI/CD</a>.

* Note: Once all tests are successful, to move the files in the temporary folder `../private2/luciancicd-testing/` into the repository, enter `move_to_repository_or_back.`, then commit the changes. (NB. This is done in `luciancicd.` if tests are successful.) Re-enter `move_to_repository_or_back.` to swap the files back.

* Important: So far, Lucian CI/CD might not save comments entered without code or changes to data files. To revert to a previous version just run, run `move_to_repository_or_back.` and `set_up_luciancicd.` to save the files. I.e. to keep comments or data files when only comments  or data files are added, run `set_up_luciancicd.` instead of `luciancicd.`.

* Note: A notification such as `"Cannot find "../private2/luciancicd-cicd-tests/tests_a.txt"` means the repository `a` is connected with a changed file through dependencies but isn't installed.

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the <a href="LICENSE">LICENSE.md</a> file for details

