# Contributing

## Dependency floors

Two of the version requirements in `DESCRIPTION` are deliberate compatibility
floors, not pins and not the versions that deployment environments should
install. Moving either one is a decision; normal installations and the browser
Playground remain free to resolve the newest compatible versions available.

| Package | Floor | Why this floor |
|---|---|---|
| `ggformula` | 1.0.0 | Adapts the formula interface to ggplot2 4. |
| `ggplot2` | 4.0.2 | Provides the ggplot2 4 extension APIs, including the fix for `make_constructor()` capturing `rlang::list2()` at build time. |

`tests/testthat/test-docs.R` checks that this table and `DESCRIPTION` name the same
versions. CI derives its exact graphics pins from `DESCRIPTION`, verifies the
loaded versions, and runs that test against the checkout because `.github` is
excluded from the package tarball. Other CI jobs resolve newer compatible versions.

Source installation is a separate check: `layer_factory()` builds generated
functions into the lazy-load database during installation. A passing test suite
against a previously installed copy does not verify that step.

### Verifying a floor

Use a disposable source copy and a separate library. The commands below assume
the other package and test dependencies, including `vdiffr` and `svglite`, are
already installed.

1. Put the floor versions in a library of their own. Install `ggformula` first:
   its own dependency resolution can pull in a current `ggplot2`, and the pinned
   one has to be the copy that survives.

   ```sh
   export CK_GRAPHICS_LIB=$(mktemp -d)
   export R_LIBS_USER="$CK_GRAPHICS_LIB:$(Rscript -e 'cat(paste(.libPaths(), collapse = ":"))')"
   Rscript -e 'remotes::install_version("ggformula", "1.0.0", lib = Sys.getenv("CK_GRAPHICS_LIB"), upgrade = "never")'
   Rscript -e 'remotes::install_version("ggplot2", "4.0.2", lib = Sys.getenv("CK_GRAPHICS_LIB"), upgrade = "never")'
   ```

2. Confirm the library is the one you think it is, rather than trusting step 1:

   ```sh
   Rscript -e 'loadNamespace("ggformula"); stopifnot(as.character(getNamespaceVersion("ggformula")) == "1.0.0", as.character(getNamespaceVersion("ggplot2")) == "4.0.2")'
   ```

3. Install this package from source into the same temporary library:

   ```sh
   R CMD INSTALL -l "$CK_GRAPHICS_LIB" .
   Rscript -e 'loadNamespace("coursekata")'
   ```

4. Run the source tests and package check:

   ```sh
   NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'testthat::test_local(stop_on_failure = TRUE)'
   Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning")'
   ```

Review visual differences individually. R 4.1 skips visual comparisons because
its graphics engine renders text differently; behavior tests still run. Use a
disposable source copy for any run that skips visual tests: testthat can remove
unvisited snapshots. Keep snapshot updates separate from dependency installation.

### Verifying WebAssembly

From `jupyterlite/`, run `pixi run build` and serve `app/_output` locally. The recipe
is generated from `DESCRIPTION`; the browser solver may select any versions that
meet those minimums. The browser uses the `emscripten-forge-4x` channel, which
provides the compiled dependencies for ggplot2 4 and ggformula 1. The generated
recipe and browser lock are not committed.
For a fresh resolution, remove the generated `app/pixi.lock` and `app/.pixi`
environment first. Confirm that `.r-version.yaml` and the final browser lock name
the same R version; rebuild if dependency resolution changes it.

In a fresh xeus-r session, load `coursekata`, report the loaded ggplot2 and ggformula
versions, and render native layers and formula pipelines. Include faceted jitter
with residuals, squareplots, and distribution annotations. Building the package
uses `--no-test-load`, so a successful build does not replace this browser check.
No site deployment is needed.
