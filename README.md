# Lucian CI/CD

![761B2A85-6DA4-4EA7-9F20-8CCB2DC60D28](https://user-images.githubusercontent.com/15845542/234572372-8446f119-6151-4ea8-844b-4df89f605143.jpeg)

* Image of different coloured pipes

* Single-user continuous integration and continuous deployment

# Getting Started

Please read the following instructions on how to install the project on your computer for automatic testing.

# Prerequisites

* Please download and install SWI-Prolog for your machine at `https://www.swi-prolog.org/build/`.

# 1. Install manually

* Download:
* <a href="https://github.com/luciangreen/luciancicd">this repository</a> and its dependencies
* <a href="https://github.com/luciangreen/List-Prolog-to-Prolog-Converter">List Prolog to Prolog Converter</a>

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download the <a href="https://github.com/luciangreen/luciancicd">Lucian CI/CD</a>:

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

* `['find_tests_from_repos.pl'].` - Finds tests in comments in repositories.

* If necessary, modify tests in `../private2/luciancicd-cicd-tests/`.
Test files are named after each repository and are in the form `[["","a.pl",(a(B),B=1)]]`, i.e. `[[Subfolder,Filename,Test], Other tests]`. where `Subfolder` = `""` or `"subfolder"`.

* `set_up_luciancicd.` - Records time modified of repositories in `repositories_paths1//1` in `luciancicd.pl`.

* `luciancicd.` - Tests repositories with change modification dates. Run before committing changes.

* Note: Dependencies are in `List-Prolog-Package-Manager/lppm_registry.txt`, in form `[[User,Repository,Dependencies], etc]`. 

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the <a href="LICENSE">LICENSE.md</a> file for details

