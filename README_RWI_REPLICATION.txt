This readme file was generated on 15.05.2024 by Patrick Thiel

<help text in angle brackets should be deleted before finalizing your document>
<[text in square brackets should be changed for your specific dataset]>
<corresponding author is the RWI author that fills the folder/ is responsible within RWI>
<HINWEIS: Gross- und Kleinschreibung wie im Titel bzw. wie ueblich; Namen werden gross geschrieben; keine Umlaute, ß>


GENERAL INFORMATION

INFORMATION ABOUT THE PAPER
corresponding author: Patrick Thiel (patrick.thiel@rwi-essen.de; patrickthiel@outlook.de)
additional authors: Manuel Frondel (RWI) and Colin Vance (RWI)
title: Heterogeneous Pass-through over Space and Time: The Case of Germany’s Fuel Tax Discount
year: 2024
journal: USAEE Working Paper Series
volume: 
number: 24-619
pages: 39
DOI: 10.2139/ssrn.4785571

Projectnumber: -
RWI-Publication number: -
 
 
FOLDER STRUCTURE
|-- .git (hidden) --> Git repository with commit history
|-- _targets --> Specific files that are needed (in the background) for running the targets pipeline. NOTE: This folder will be automatically created when the pipeline is run the first time.
	|-- meta
	|-- objects
	|-- user
	|-- .gitignore --> Tells Git which files should not be tracked
|-- data
	|-- country_prices --> Bi-weekly fuel prices across European countries
	|-- country_shapes --> Shape files of countries neighboring Germany
		|-- Austria_shapefile
		|-- Belgium_shapefile
		|-- CzechRepublic_shapefile
		|-- Denmark_shapefile
		|-- France_shapefile
		|-- Germany_shapefile
		|-- Italy_shapefile
		|-- Luxembourg_shapefile
		|-- Netherlands_shapefile
		|-- Poland_shapefile
		|-- Switzerland_shapefile
	|-- french_fuel_data --> Station-level fuel price data for France
	|-- german_fuel_data --> Station-level fuel price data for Germany
	|-- german_stations --> Station information for German gas stations
	|-- google_trends --> Google trends data for various keywords
|-- output
	|-- archive --> Old output files that are not needed in the current version
	|-- descriptives --> Outputs that describe the data
	|-- estimation --> Regression outputs
	|-- graphs --> Graphical outputs
	|-- maps --> Maps
|-- renv --> Package management system for libraries
|-- src --> Code files
	|-- archive --> Old functions that are currently not needed
	|-- calculate
	|-- clean --> Functions to clean the data
	|-- estimate --> Functions for estimating the models
	|-- functions --> Support function for estimation
	|-- helpers --> Contains the config file with globals and paths (NOTE: This is not tracked by version control)
	|-- make --> Functions for studying the subsettings of purchasing power and station density
	|-- others --> Functions that could not be classified in other folders, e.g. testing robust parallel trends
	|-- plot --> Functions to plot outputs
	|-- python_french_prices --> Python files to extract and clean the raw French fuel prices
	|-- read --> Functions to read the raw inputs
|-- .gitattributes --> Background file for Git repository
|-- .gitignore --> Tells Git which files should not be tracked
|-- .Rprofile --> R-specific file
|-- _targets.R --> Orchestrates the functions in "src" (i.e. establishes the connections between the "src" files)
|-- renv.lock --> Stores the libraries and their versions/ dependencies

DATA & FILE OVERVIEW
- country_prices.dta[Dataset]_2023-09-21 --> Bi-weekly fuel prices across European countries
- station_prices.feather[Dataset]_2023-02-10 --> Station-level fuel price data for France
- fuel_prices_germany.fst[Dataset]_2024-03-08 --> Station-level fuel price data for Germany
- german_stations.fst[Dataset]_2024-04-11 --> Station information for German gas stations
- benzinpreis_germany_01042022_30092022.csv[Dataset]_2024-03-11 --> Google trends data for the keyword "Benzinpreis"
- dieselpreis_germany_01042022_30092022.csv[Dataset]_2024-03-11 --> Google trends data for the keyword "Dieselpreis"
- tankrabattpreis_germany_01042022_30092022.csv[Dataset]_2024-03-11 --> Google trends data for the keyword "Tankrabatt"


<Relationship between files, if important:>
The relationship between files is orchestrated by ../_targets.R.
The relationship between single files/ function can be visually assessed with the file ../network.html.





DATA-SPECIFIC INFORMATION FOR: [FILENAME]
<repeat this section for each dataset, folder or file, as appropriate>

<Specialized formats or other abbreviations used>



SOFTWARE-PACKAGES
NOTE: All packages can be found in ../renv/library/R-4.3/x86_64-w64-mingw32
The following paragraphs are the content of ../renv.lock which lists all used packages.

{
  "R": {
    "Version": "4.3.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://ftp.fau.de/cran"
      }
    ]
  },
  "Packages": {
    "BH": {
      "Package": "BH",
      "Version": "1.84.0-0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "a8235afbcd6316e6e91433ea47661013"
    },
    "CVXR": {
      "Package": "CVXR",
      "Version": "1.0-12",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "ECOSolveR",
        "Matrix",
        "R",
        "R6",
        "Rcpp",
        "RcppEigen",
        "Rmpfr",
        "bit64",
        "gmp",
        "methods",
        "osqp",
        "scs",
        "stats"
      ],
      "Hash": "d2616259133f8c280fca3b76ce203cd9"
    },
    "DBI": {
      "Package": "DBI",
      "Version": "1.2.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "methods"
      ],
      "Hash": "164809cd72e1d5160b4cb3aa57f510fe"
    },
    "ECOSolveR": {
      "Package": "ECOSolveR",
      "Version": "0.5.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "methods"
      ],
      "Hash": "1cd39406d7fde7d059c12e2940ca65cc"
    },
    "Formula": {
      "Package": "Formula",
      "Version": "1.2-5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "stats"
      ],
      "Hash": "7a29697b75e027767a53fde6c903eca7"
    },
    "HonestDiD": {
      "Package": "HonestDiD",
      "Version": "0.2.6",
      "Source": "GitHub",
      "RemoteType": "github",
      "RemoteHost": "api.github.com",
      "RemoteRepo": "HonestDiD",
      "RemoteUsername": "asheshrambachan",
      "RemoteRef": "HEAD",
      "RemoteSha": "99e05c7853631f012168a5f60a2778eb04215651",
      "Requirements": [
        "CVXR",
        "Matrix",
        "R",
        "Rglpk",
        "TruncatedNormal",
        "dplyr",
        "foreach",
        "ggplot2",
        "latex2exp",
        "lpSolveAPI",
        "matrixStats",
        "mvtnorm",
        "pracma",
        "purrr",
        "rlang",
        "stats",
        "tibble"
      ],
      "Hash": "70037065ee8de71f4d4bb0d96e4e6be6"
    },
    "KernSmooth": {
      "Package": "KernSmooth",
      "Version": "2.23-22",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "stats"
      ],
      "Hash": "2fecebc3047322fa5930f74fae5de70f"
    },
    "MASS": {
      "Package": "MASS",
      "Version": "7.3-60.0.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "graphics",
        "methods",
        "stats",
        "utils"
      ],
      "Hash": "b765b28387acc8ec9e9c1530713cb19c"
    },
    "Matrix": {
      "Package": "Matrix",
      "Version": "1.6-5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "graphics",
        "grid",
        "lattice",
        "methods",
        "stats",
        "utils"
      ],
      "Hash": "8c7115cd3a0e048bda2a7cd110549f7a"
    },
    "MetBrewer": {
      "Package": "MetBrewer",
      "Version": "0.2.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "ggplot2"
      ],
      "Hash": "9dda8af6359c4b3932426430c38fa1b1"
    },
    "R6": {
      "Package": "R6",
      "Version": "2.5.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "470851b6d5d0ac559e9d01bb352b4021"
    },
    "RApiSerialize": {
      "Package": "RApiSerialize",
      "Version": "0.1.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "d8a79c95f553670ceffbd190815bbfce"
    },
    "RColorBrewer": {
      "Package": "RColorBrewer",
      "Version": "1.1-3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "45f0398006e83a5b10b72a90663d8d8c"
    },
    "Rcpp": {
      "Package": "Rcpp",
      "Version": "1.0.12",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "methods",
        "utils"
      ],
      "Hash": "5ea2700d21e038ace58269ecdbeb9ec0"
    },
    "RcppArmadillo": {
      "Package": "RcppArmadillo",
      "Version": "0.12.8.2.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "methods",
        "stats",
        "utils"
      ],
      "Hash": "d5448fb24fb114c4da1275a37a571f37"
    },
    "RcppEigen": {
      "Package": "RcppEigen",
      "Version": "0.3.4.0.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "stats",
        "utils"
      ],
      "Hash": "df49e3306f232ec28f1604e36a202847"
    },
    "RcppParallel": {
      "Package": "RcppParallel",
      "Version": "5.1.7",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "a45594a00f5dbb073d5ec9f48592a08a"
    },
    "Rglpk": {
      "Package": "Rglpk",
      "Version": "0.6-5.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "slam"
      ],
      "Hash": "3214828c7e274287df2ce88aac1b359b"
    },
    "Rmpfr": {
      "Package": "Rmpfr",
      "Version": "0.9-5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "gmp",
        "methods",
        "stats",
        "utils"
      ],
      "Hash": "77c83b84e1dafd6709f5d6a4d786039f"
    },
    "TruncatedNormal": {
      "Package": "TruncatedNormal",
      "Version": "2.2.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "RcppArmadillo",
        "alabama",
        "nleqslv",
        "randtoolbox"
      ],
      "Hash": "26368f5395f9870d5c74a38cb27f0f56"
    },
    "alabama": {
      "Package": "alabama",
      "Version": "2023.1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "numDeriv"
      ],
      "Hash": "593db7eb170506e6b61ca0c803201924"
    },
    "arrow": {
      "Package": "arrow",
      "Version": "15.0.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "assertthat",
        "bit64",
        "cpp11",
        "glue",
        "methods",
        "purrr",
        "rlang",
        "stats",
        "tidyselect",
        "utils",
        "vctrs"
      ],
      "Hash": "117bf4b17bb420d115602b057b1e534a"
    },
    "assertthat": {
      "Package": "assertthat",
      "Version": "0.2.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "tools"
      ],
      "Hash": "50c838a310445e954bc13f26f26a6ecf"
    },
    "backports": {
      "Package": "backports",
      "Version": "1.4.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "c39fbec8a30d23e721980b8afb31984c"
    },
    "base64url": {
      "Package": "base64url",
      "Version": "1.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "backports"
      ],
      "Hash": "0c54cf3a08cc0e550fbd64ad33166143"
    },
    "bit": {
      "Package": "bit",
      "Version": "4.0.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "d242abec29412ce988848d0294b208fd"
    },
    "bit64": {
      "Package": "bit64",
      "Version": "4.0.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "bit",
        "methods",
        "stats",
        "utils"
      ],
      "Hash": "9fe98599ca456d6552421db0d6772d8f"
    },
    "callr": {
      "Package": "callr",
      "Version": "3.7.6",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "processx",
        "utils"
      ],
      "Hash": "d7e13f49c19103ece9e58ad2d83a7354"
    },
    "class": {
      "Package": "class",
      "Version": "7.3-22",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "MASS",
        "R",
        "stats",
        "utils"
      ],
      "Hash": "f91f6b29f38b8c280f2b9477787d4bb2"
    },
    "classInt": {
      "Package": "classInt",
      "Version": "0.4-10",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "KernSmooth",
        "R",
        "class",
        "e1071",
        "grDevices",
        "graphics",
        "stats"
      ],
      "Hash": "f5a40793b1ae463a7ffb3902a95bf864"
    },
    "cli": {
      "Package": "cli",
      "Version": "3.6.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "utils"
      ],
      "Hash": "1216ac65ac55ec0058a6f75d7ca0fd52"
    },
    "clipr": {
      "Package": "clipr",
      "Version": "0.8.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "utils"
      ],
      "Hash": "3f038e5ac7f41d4ac41ce658c85e3042"
    },
    "codetools": {
      "Package": "codetools",
      "Version": "0.2-19",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "c089a619a7fae175d149d89164f8c7d8"
    },
    "colorspace": {
      "Package": "colorspace",
      "Version": "2.1-0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "graphics",
        "methods",
        "stats"
      ],
      "Hash": "f20c47fd52fae58b4e377c37bb8c335b"
    },
    "cpp11": {
      "Package": "cpp11",
      "Version": "0.4.7",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "5a295d7d963cc5035284dcdbaf334f4e"
    },
    "crayon": {
      "Package": "crayon",
      "Version": "1.5.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "grDevices",
        "methods",
        "utils"
      ],
      "Hash": "e8a1e41acf02548751f45c718d55aa6a"
    },
    "data.table": {
      "Package": "data.table",
      "Version": "1.15.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "methods"
      ],
      "Hash": "8ee9ac56ef633d0c7cab8b2ca87d683e"
    },
    "digest": {
      "Package": "digest",
      "Version": "0.6.35",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "utils"
      ],
      "Hash": "698ece7ba5a4fa4559e3d537e7ec3d31"
    },
    "dplyr": {
      "Package": "dplyr",
      "Version": "1.1.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "cli",
        "generics",
        "glue",
        "lifecycle",
        "magrittr",
        "methods",
        "pillar",
        "rlang",
        "tibble",
        "tidyselect",
        "utils",
        "vctrs"
      ],
      "Hash": "fedd9d00c2944ff00a0e2696ccf048ec"
    },
    "dreamerr": {
      "Package": "dreamerr",
      "Version": "1.4.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "Formula",
        "stringmagic",
        "utils"
      ],
      "Hash": "8ff490ff7faf6952d558f046ab9a363a"
    },
    "e1071": {
      "Package": "e1071",
      "Version": "1.7-14",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "class",
        "grDevices",
        "graphics",
        "methods",
        "proxy",
        "stats",
        "utils"
      ],
      "Hash": "4ef372b716824753719a8a38b258442d"
    },
    "evaluate": {
      "Package": "evaluate",
      "Version": "0.23",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "methods"
      ],
      "Hash": "042db20f431158b8501cfe6a20b7ca02"
    },
    "fansi": {
      "Package": "fansi",
      "Version": "1.0.6",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "utils"
      ],
      "Hash": "962174cf2aeb5b9eea581522286a911f"
    },
    "farver": {
      "Package": "farver",
      "Version": "2.1.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "8106d78941f34855c440ddb946b8f7a5"
    },
    "fixest": {
      "Package": "fixest",
      "Version": "0.11.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "dreamerr",
        "grDevices",
        "graphics",
        "methods",
        "nlme",
        "numDeriv",
        "sandwich",
        "stats",
        "tools",
        "utils"
      ],
      "Hash": "3f663b18f5368c8485d9315354a0242b"
    },
    "forcats": {
      "Package": "forcats",
      "Version": "1.0.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "glue",
        "lifecycle",
        "magrittr",
        "rlang",
        "tibble"
      ],
      "Hash": "1a0a9a3d5083d0d573c4214576f1e690"
    },
    "foreach": {
      "Package": "foreach",
      "Version": "1.5.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "codetools",
        "iterators",
        "utils"
      ],
      "Hash": "618609b42c9406731ead03adf5379850"
    },
    "fs": {
      "Package": "fs",
      "Version": "1.6.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "methods"
      ],
      "Hash": "47b5f30c720c23999b913a1a635cf0bb"
    },
    "fst": {
      "Package": "fst",
      "Version": "0.9.8",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "fstcore"
      ],
      "Hash": "34637e000c63c3de8d8aafceb82fd082"
    },
    "fstcore": {
      "Package": "fstcore",
      "Version": "0.9.18",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp"
      ],
      "Hash": "4c33c7202fcfe741cdea205570305630"
    },
    "future": {
      "Package": "future",
      "Version": "1.33.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "digest",
        "globals",
        "listenv",
        "parallel",
        "parallelly",
        "utils"
      ],
      "Hash": "fd7b1d69d16d0d114e4fa82db68f184c"
    },
    "future.callr": {
      "Package": "future.callr",
      "Version": "0.8.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "callr",
        "future"
      ],
      "Hash": "0a5ad2f3fe4bf8a794bbf822d6c285bf"
    },
    "generics": {
      "Package": "generics",
      "Version": "0.1.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "methods"
      ],
      "Hash": "15e9634c0fcd294799e9b2e929ed1b86"
    },
    "ggplot2": {
      "Package": "ggplot2",
      "Version": "3.5.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "MASS",
        "R",
        "cli",
        "glue",
        "grDevices",
        "grid",
        "gtable",
        "isoband",
        "lifecycle",
        "mgcv",
        "rlang",
        "scales",
        "stats",
        "tibble",
        "vctrs",
        "withr"
      ],
      "Hash": "52ef83f93f74833007f193b2d4c159a2"
    },
    "globals": {
      "Package": "globals",
      "Version": "0.16.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "codetools"
      ],
      "Hash": "2580567908cafd4f187c1e5a91e98b7f"
    },
    "glue": {
      "Package": "glue",
      "Version": "1.7.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "methods"
      ],
      "Hash": "e0b3a53876554bd45879e596cdb10a52"
    },
    "gmp": {
      "Package": "gmp",
      "Version": "0.7-4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "methods"
      ],
      "Hash": "eef7063b82c79405f4eb2de9a051a312"
    },
    "gtable": {
      "Package": "gtable",
      "Version": "0.3.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "glue",
        "grid",
        "lifecycle",
        "rlang"
      ],
      "Hash": "b29cf3031f49b04ab9c852c912547eef"
    },
    "haven": {
      "Package": "haven",
      "Version": "2.5.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "cpp11",
        "forcats",
        "hms",
        "lifecycle",
        "methods",
        "readr",
        "rlang",
        "tibble",
        "tidyselect",
        "vctrs"
      ],
      "Hash": "9171f898db9d9c4c1b2c745adc2c1ef1"
    },
    "here": {
      "Package": "here",
      "Version": "1.0.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "rprojroot"
      ],
      "Hash": "24b224366f9c2e7534d2344d10d59211"
    },
    "highr": {
      "Package": "highr",
      "Version": "0.10",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "xfun"
      ],
      "Hash": "06230136b2d2b9ba5805e1963fa6e890"
    },
    "hms": {
      "Package": "hms",
      "Version": "1.1.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "lifecycle",
        "methods",
        "pkgconfig",
        "rlang",
        "vctrs"
      ],
      "Hash": "b59377caa7ed00fa41808342002138f9"
    },
    "igraph": {
      "Package": "igraph",
      "Version": "2.0.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "Matrix",
        "R",
        "cli",
        "cpp11",
        "grDevices",
        "graphics",
        "lifecycle",
        "magrittr",
        "methods",
        "pkgconfig",
        "rlang",
        "stats",
        "utils",
        "vctrs"
      ],
      "Hash": "c3b7d801d722e26e4cd888e042bf9af5"
    },
    "isoband": {
      "Package": "isoband",
      "Version": "0.2.7",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "grid",
        "utils"
      ],
      "Hash": "0080607b4a1a7b28979aecef976d8bc2"
    },
    "iterators": {
      "Package": "iterators",
      "Version": "1.0.14",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "utils"
      ],
      "Hash": "8954069286b4b2b0d023d1b288dce978"
    },
    "knitr": {
      "Package": "knitr",
      "Version": "1.46",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "evaluate",
        "highr",
        "methods",
        "tools",
        "xfun",
        "yaml"
      ],
      "Hash": "6e008ab1d696a5283c79765fa7b56b47"
    },
    "labeling": {
      "Package": "labeling",
      "Version": "0.4.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "graphics",
        "stats"
      ],
      "Hash": "b64ec208ac5bc1852b285f665d6368b3"
    },
    "latex2exp": {
      "Package": "latex2exp",
      "Version": "0.9.6",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "magrittr",
        "stringr"
      ],
      "Hash": "f0173e0120a278700d48998e5a93a000"
    },
    "lattice": {
      "Package": "lattice",
      "Version": "0.22-5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "graphics",
        "grid",
        "stats",
        "utils"
      ],
      "Hash": "7c5e89f04e72d6611c77451f6331a091"
    },
    "lifecycle": {
      "Package": "lifecycle",
      "Version": "1.0.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "glue",
        "rlang"
      ],
      "Hash": "b8552d117e1b808b09a832f589b79035"
    },
    "listenv": {
      "Package": "listenv",
      "Version": "0.9.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "e2fca3e12e4db979dccc6e519b10a7ee"
    },
    "lpSolveAPI": {
      "Package": "lpSolveAPI",
      "Version": "5.5.2.0-17.11",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "b062307fdfd15573643dbcb08948178c"
    },
    "lubridate": {
      "Package": "lubridate",
      "Version": "1.9.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "generics",
        "methods",
        "timechange"
      ],
      "Hash": "680ad542fbcf801442c83a6ac5a2126c"
    },
    "magrittr": {
      "Package": "magrittr",
      "Version": "2.0.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "7ce2733a9826b3aeb1775d56fd305472"
    },
    "matrixStats": {
      "Package": "matrixStats",
      "Version": "1.3.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "4b3ea27a19d669c0405b38134d89a9d1"
    },
    "mgcv": {
      "Package": "mgcv",
      "Version": "1.9-1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "Matrix",
        "R",
        "graphics",
        "methods",
        "nlme",
        "splines",
        "stats",
        "utils"
      ],
      "Hash": "110ee9d83b496279960e162ac97764ce"
    },
    "munsell": {
      "Package": "munsell",
      "Version": "0.5.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "colorspace",
        "methods"
      ],
      "Hash": "4fd8900853b746af55b81fda99da7695"
    },
    "mvtnorm": {
      "Package": "mvtnorm",
      "Version": "1.2-4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "stats"
      ],
      "Hash": "17e96668f44a28aef0981d9e17c49b59"
    },
    "nleqslv": {
      "Package": "nleqslv",
      "Version": "3.3.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "982736cfb7276c4d9a2afa4e6b8e95a4"
    },
    "nlme": {
      "Package": "nlme",
      "Version": "3.1-164",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "graphics",
        "lattice",
        "stats",
        "utils"
      ],
      "Hash": "a623a2239e642806158bc4dc3f51565d"
    },
    "numDeriv": {
      "Package": "numDeriv",
      "Version": "2016.8-1.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "df58958f293b166e4ab885ebcad90e02"
    },
    "openxlsx": {
      "Package": "openxlsx",
      "Version": "4.2.5.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "grDevices",
        "methods",
        "stats",
        "stringi",
        "utils",
        "zip"
      ],
      "Hash": "c03b4c18d42da881fb8e15a085c2b9d6"
    },
    "osqp": {
      "Package": "osqp",
      "Version": "0.6.3.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "Matrix",
        "R6",
        "Rcpp",
        "methods"
      ],
      "Hash": "e024cccc110a810e942c98a4a6de6795"
    },
    "parallelly": {
      "Package": "parallelly",
      "Version": "1.37.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "parallel",
        "tools",
        "utils"
      ],
      "Hash": "5410df8d22bd36e616f2a2343dbb328c"
    },
    "pillar": {
      "Package": "pillar",
      "Version": "1.9.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "cli",
        "fansi",
        "glue",
        "lifecycle",
        "rlang",
        "utf8",
        "utils",
        "vctrs"
      ],
      "Hash": "15da5a8412f317beeee6175fbc76f4bb"
    },
    "pkgconfig": {
      "Package": "pkgconfig",
      "Version": "2.0.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "utils"
      ],
      "Hash": "01f28d4278f15c76cddbea05899c5d6f"
    },
    "plyr": {
      "Package": "plyr",
      "Version": "1.8.9",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp"
      ],
      "Hash": "6b8177fd19982f0020743fadbfdbd933"
    },
    "pracma": {
      "Package": "pracma",
      "Version": "2.4.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "graphics",
        "stats",
        "utils"
      ],
      "Hash": "44bc172d47d1ea0a638d9f299e321203"
    },
    "prettyunits": {
      "Package": "prettyunits",
      "Version": "1.2.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "6b01fc98b1e86c4f705ce9dcfd2f57c7"
    },
    "processx": {
      "Package": "processx",
      "Version": "3.8.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "ps",
        "utils"
      ],
      "Hash": "0c90a7d71988856bad2a2a45dd871bb9"
    },
    "progress": {
      "Package": "progress",
      "Version": "1.2.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "crayon",
        "hms",
        "prettyunits"
      ],
      "Hash": "f4625e061cb2865f111b47ff163a5ca6"
    },
    "proxy": {
      "Package": "proxy",
      "Version": "0.4-27",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "stats",
        "utils"
      ],
      "Hash": "e0ef355c12942cf7a6b91a6cfaea8b3e"
    },
    "ps": {
      "Package": "ps",
      "Version": "1.7.6",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "utils"
      ],
      "Hash": "dd2b9319ee0656c8acf45c7f40c59de7"
    },
    "purrr": {
      "Package": "purrr",
      "Version": "1.0.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "lifecycle",
        "magrittr",
        "rlang",
        "vctrs"
      ],
      "Hash": "1cba04a4e9414bdefc9dcaa99649a8dc"
    },
    "qs": {
      "Package": "qs",
      "Version": "0.26.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "BH",
        "R",
        "RApiSerialize",
        "Rcpp",
        "stringfish"
      ],
      "Hash": "c0626a04f3021a24f05ab211280209ab"
    },
    "randtoolbox": {
      "Package": "randtoolbox",
      "Version": "2.0.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "rngWELL"
      ],
      "Hash": "3fbb3aa5688a4004b3d1c7efd80689f5"
    },
    "readr": {
      "Package": "readr",
      "Version": "2.1.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "cli",
        "clipr",
        "cpp11",
        "crayon",
        "hms",
        "lifecycle",
        "methods",
        "rlang",
        "tibble",
        "tzdb",
        "utils",
        "vroom"
      ],
      "Hash": "9de96463d2117f6ac49980577939dfb3"
    },
    "renv": {
      "Package": "renv",
      "Version": "1.0.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "utils"
      ],
      "Hash": "32c3f93e8360f667ca5863272ec8ba6a"
    },
    "rlang": {
      "Package": "rlang",
      "Version": "1.1.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "utils"
      ],
      "Hash": "42548638fae05fd9a9b5f3f437fbbbe2"
    },
    "rngWELL": {
      "Package": "rngWELL",
      "Version": "0.10-9",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "b5c163aebf9187ab8a91df92832ff88e"
    },
    "rprojroot": {
      "Package": "rprojroot",
      "Version": "2.0.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "4c8415e0ec1e29f3f4f6fc108bef0144"
    },
    "s2": {
      "Package": "s2",
      "Version": "1.1.6",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "wk"
      ],
      "Hash": "32f7b1a15bb01ae809022960abad5363"
    },
    "sandwich": {
      "Package": "sandwich",
      "Version": "3.1-0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "stats",
        "utils",
        "zoo"
      ],
      "Hash": "1cf6ae532f0179350862fefeb0987c9b"
    },
    "scales": {
      "Package": "scales",
      "Version": "1.3.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "RColorBrewer",
        "cli",
        "farver",
        "glue",
        "labeling",
        "lifecycle",
        "munsell",
        "rlang",
        "viridisLite"
      ],
      "Hash": "c19df082ba346b0ffa6f833e92de34d1"
    },
    "scs": {
      "Package": "scs",
      "Version": "3.2.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "4b6b744ee2710f3491f0c3df54cad92b"
    },
    "secretbase": {
      "Package": "secretbase",
      "Version": "0.4.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "e9591ffb6d5adcc9284213e496ed7fe9"
    },
    "sf": {
      "Package": "sf",
      "Version": "1.0-16",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "DBI",
        "R",
        "Rcpp",
        "classInt",
        "grDevices",
        "graphics",
        "grid",
        "magrittr",
        "methods",
        "s2",
        "stats",
        "tools",
        "units",
        "utils"
      ],
      "Hash": "ad57b543f7c3fca05213ba78ff63df9b"
    },
    "slam": {
      "Package": "slam",
      "Version": "0.1-50",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "stats"
      ],
      "Hash": "e25793551cbdb843154152e5ee88cbd6"
    },
    "stringfish": {
      "Package": "stringfish",
      "Version": "0.16.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp",
        "RcppParallel"
      ],
      "Hash": "b7eb79470319ae71d4b5ed9cd7bf7294"
    },
    "stringi": {
      "Package": "stringi",
      "Version": "1.8.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "stats",
        "tools",
        "utils"
      ],
      "Hash": "058aebddea264f4c99401515182e656a"
    },
    "stringmagic": {
      "Package": "stringmagic",
      "Version": "1.1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "Rcpp",
        "stats",
        "utils"
      ],
      "Hash": "793f281bfd6eddaf5cec8a7578ae54da"
    },
    "stringr": {
      "Package": "stringr",
      "Version": "1.5.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "glue",
        "lifecycle",
        "magrittr",
        "rlang",
        "stringi",
        "vctrs"
      ],
      "Hash": "960e2ae9e09656611e0b8214ad543207"
    },
    "tarchetypes": {
      "Package": "tarchetypes",
      "Version": "0.8.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "digest",
        "dplyr",
        "fs",
        "parallel",
        "rlang",
        "targets",
        "tibble",
        "tidyselect",
        "utils",
        "vctrs",
        "withr"
      ],
      "Hash": "38eddad863e58b95761d967887d7b59a"
    },
    "targets": {
      "Package": "targets",
      "Version": "1.6.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "R6",
        "base64url",
        "callr",
        "cli",
        "codetools",
        "data.table",
        "digest",
        "igraph",
        "knitr",
        "ps",
        "rlang",
        "secretbase",
        "stats",
        "tibble",
        "tidyselect",
        "tools",
        "utils",
        "vctrs",
        "yaml"
      ],
      "Hash": "35b83348e5cf547715687c93ce5c2126"
    },
    "tibble": {
      "Package": "tibble",
      "Version": "3.2.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "fansi",
        "lifecycle",
        "magrittr",
        "methods",
        "pillar",
        "pkgconfig",
        "rlang",
        "utils",
        "vctrs"
      ],
      "Hash": "a84e2cc86d07289b3b6f5069df7a004c"
    },
    "tidyr": {
      "Package": "tidyr",
      "Version": "1.3.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "cpp11",
        "dplyr",
        "glue",
        "lifecycle",
        "magrittr",
        "purrr",
        "rlang",
        "stringr",
        "tibble",
        "tidyselect",
        "utils",
        "vctrs"
      ],
      "Hash": "915fb7ce036c22a6a33b5a8adb712eb1"
    },
    "tidyselect": {
      "Package": "tidyselect",
      "Version": "1.2.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "glue",
        "lifecycle",
        "rlang",
        "vctrs",
        "withr"
      ],
      "Hash": "829f27b9c4919c16b593794a6344d6c0"
    },
    "timechange": {
      "Package": "timechange",
      "Version": "0.3.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cpp11"
      ],
      "Hash": "c5f3c201b931cd6474d17d8700ccb1c8"
    },
    "tzdb": {
      "Package": "tzdb",
      "Version": "0.4.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cpp11"
      ],
      "Hash": "f561504ec2897f4d46f0c7657e488ae1"
    },
    "units": {
      "Package": "units",
      "Version": "0.8-5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "Rcpp"
      ],
      "Hash": "119d19da480e873f72241ff6962ffd83"
    },
    "utf8": {
      "Package": "utf8",
      "Version": "1.2.4",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "62b65c52671e6665f803ff02954446e9"
    },
    "vctrs": {
      "Package": "vctrs",
      "Version": "0.6.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "cli",
        "glue",
        "lifecycle",
        "rlang"
      ],
      "Hash": "c03fa420630029418f7e6da3667aac4a"
    },
    "viridisLite": {
      "Package": "viridisLite",
      "Version": "0.4.2",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "c826c7c4241b6fc89ff55aaea3fa7491"
    },
    "vroom": {
      "Package": "vroom",
      "Version": "1.6.5",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "bit64",
        "cli",
        "cpp11",
        "crayon",
        "glue",
        "hms",
        "lifecycle",
        "methods",
        "progress",
        "rlang",
        "stats",
        "tibble",
        "tidyselect",
        "tzdb",
        "vctrs",
        "withr"
      ],
      "Hash": "390f9315bc0025be03012054103d227c"
    },
    "withr": {
      "Package": "withr",
      "Version": "3.0.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "graphics"
      ],
      "Hash": "d31b6c62c10dcf11ec530ca6b0dd5d35"
    },
    "wk": {
      "Package": "wk",
      "Version": "0.9.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R"
      ],
      "Hash": "5d4545e140e36476f35f20d0ca87963e"
    },
    "xfun": {
      "Package": "xfun",
      "Version": "0.43",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "grDevices",
        "stats",
        "tools"
      ],
      "Hash": "ab6371d8653ce5f2f9290f4ec7b42a8e"
    },
    "yaml": {
      "Package": "yaml",
      "Version": "2.3.8",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "29240487a071f535f5e5d5a323b7afbd"
    },
    "zip": {
      "Package": "zip",
      "Version": "2.3.1",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "fcc4bd8e6da2d2011eb64a5e5cc685ab"
    },
    "zoo": {
      "Package": "zoo",
      "Version": "1.8-12",
      "Source": "Repository",
      "Repository": "CRAN",
      "Requirements": [
        "R",
        "grDevices",
        "graphics",
        "lattice",
        "stats",
        "utils"
      ],
      "Hash": "5c715954112b45499fb1dadc6ee6ee3e"
    }
  }
}
