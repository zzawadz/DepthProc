# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

DepthProc is an R package (CRAN: https://CRAN.R-project.org/package=DepthProc) implementing statistical
data depth functions for multivariate analysis — depth-based descriptive statistics, robust regression,
multivariate quantile-quantile plots, scatter estimators, and Wilcoxon-type tests. Performance-critical
numerics are implemented in C++ (Rcpp/RcppArmadillo) and called from R wrappers, with OpenMP used for
multithreaded computation.

## Commands

All commands assume the working directory is the package root and are run from an R console
(`R` or `Rscript -e '...'`), typically via `devtools`/`pkgbuild` (both in `Suggests`).

- Install dependencies: `Rscript -e 'devtools::install_deps(dependencies = TRUE)'`
- Load package for interactive development (recompiles C++ as needed): `Rscript -e 'devtools::load_all()'`
- Compile only the C++ sources: `Rscript -e 'pkgbuild::compile_dll()'`
- Regenerate `NAMESPACE`/`man/*.Rd` from roxygen comments after editing any roxygen block: `Rscript -e 'devtools::document()'`
- Regenerate `src/RcppExports.cpp` / `R/RcppExports.R` after changing `// [[Rcpp::export]]` C++ signatures: `Rscript -e 'Rcpp::compileAttributes()'`
- Run the full test suite: `Rscript -e 'devtools::test()'`
- Run a single test file: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-DepthMBD.R")'`
- Run a single test by name (regex over `test_that` descriptions): `Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat", filter = "MBD")'`
- Lint: `Rscript -e 'lintr::lint_package()'` (rules live in `.lintr`; CI runs this too)
- Full R CMD check (build, test, docs, examples): `Rscript -e 'devtools::check()'`
- Test coverage: `Rscript -e 'covr::codecov()'` (as run in CI; `codecov.yml` configures reporting)
- Regenerate `README.md` from `README.Rmd`: `Rscript -e 'rmarkdown::render("README.Rmd")'`

CI is Travis (`.travis.yml`, tested against R oldrel/release/devel) and AppVeyor (`appveyor.yml`, Windows).
There is no GitHub Actions workflow.

## Architecture

### R wrapper / C++ kernel split

Each depth method has an R-level entry point in `R/depth.R` (`depthEuclid`, `depthMah`, `depthProjection`,
`depthTukey`, `depthLP`) that normalizes input (`data.frame`/vector → `matrix`, defaulting `X` to `u`) and
then, for anything non-trivial, delegates to a compiled routine such as `depthMahCPP`, `depthProjCPP`,
`depthTukeyCPP`, `depthLPCPP`. The generic dispatcher `depth(u, X, method = ...)` just `switch()`es over
`method` to one of these. `depthLocal` (`R/depthLocal.R`) is a meta-method: it recursively calls `depth()`
with a *different* method per "layer" (`depth_params1`, `depth_params2`), which is how `depthMedian()` in
the README chains `Local` over `LP`.

C++ sources live in `src/` and are wired to R through Rcpp attributes: `// [[Rcpp::export]]` in the `.cpp`
files generates `src/RcppExports.cpp` and `R/RcppExports.R` via `Rcpp::compileAttributes()` — **never hand-edit
either generated file**; change the C++ signature and regenerate instead. Key C++ files:
`Depth.cpp`/`Depth.h` (core depth kernels), `depthFunCPP.cpp`/`depthcpp.cpp` (R-facing wrappers),
`TukeyDepth.cpp`, `LocationScaleDepth*.cpp` (location-scale depth), `LocationEstimators.cpp`,
`CovFunCPP.cpp`/`RobCovLib.cpp` (robust covariance), `UtilsCPP.cpp`/`Utils.cpp`. Build flags in
`src/Makevars`/`src/Makevars.win` link BLAS/LAPACK, RcppArmadillo, and enable OpenMP
(`SHLIB_OPENMP_CXXFLAGS`) — most exported CPP functions accept a `threads` argument that maps directly to
OpenMP thread count (`-1` = use all cores).

### S4 class hierarchy for results

Depth results are not bare numeric vectors: they are S4 objects defined in `R/AllClasses.R` that *contain*
`numeric` (so they behave like vectors, e.g. `as.numeric(result)`) while also carrying `u`, `X`, and
`method` slots. The virtual class `Depth` is the base for `DepthEuclid`, `DepthMahalanobis`,
`DepthProjection`, `DepthTukey`, `DepthLP`, `DepthLocal`. Plot-oriented results build on the separate
virtual class `DepthCurve` (`ScaleCurve`, `AsymmetryCurve`), which supports a `%+%`-style combination via
`combineDepthCurves()` into a `DepthCurveList` (`ScaleCurveList`/`AsymmetryCurveList`) so multiple curves
can be rendered on one `ggplot2` plot through `getPlot()`/`plot()`. `DDPlot` wraps a pair of `Depth` objects
for depth-vs-depth plots. Functional-depth analogues live in `R/AllClassesFnc.R` (not read in detail here,
but follow the same "S4 wraps numeric + metadata" pattern) and are produced by `fncDepth()`
(`R/functional_depths.R`), which S3-dispatches on the input class (`fncDepth.matrix`, `fncDepth.zoo`).

### Roxygen-generated metadata

`NAMESPACE` and all `man/*.Rd` files are generated from roxygen2 comments (`RoxygenNote` in `DESCRIPTION`,
currently 7.3.3) — **edit the roxygen blocks in `R/*.R`, then run `devtools::document()`**; do not hand-edit
`NAMESPACE` or `man/`.

### Tests

`testthat` tests live in `tests/testthat/`, driven by `tests/testthat.R` (`test_check("DepthProc")`, legacy
`testthat` edition style — no `testthat::edition_set()`/`test_that()` 3e helpers in use). A recurring test
pattern (see `tests/testthat/test-depth-basic-api.R`) checks that the vector-input API and matrix-input API
of each depth function agree, and that omitting `X` defaults it to `u`, with `set.seed()` used around calls
whose depth methods are randomized (e.g. `Projection`, `Tukey` use random directions via `runifsphere()`).
