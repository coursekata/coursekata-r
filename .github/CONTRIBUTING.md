# Contributing

## Dependency floors

Two of the version requirements in `DESCRIPTION` are floors we measured a failure
below, not the versions that happened to be installed when the dependency was
added. Moving either one is a decision.

| Package | Floor | Below the floor |
|---|---|---|
| `ggformula` | 0.12.0 | `layer_factory()` resolves the `stat` and `geom` it is handed instead of capturing them unevaluated, so it looks up `coursekata::StatSdRuler` while `coursekata`'s own namespace is still being built. The package cannot be installed at all. |
| `ggplot2` | 3.5.2 | Nothing -- and that is the point. The floor sits deliberately below the current release because the browser-based Playground ships 3.5.2, so raising it drops those environments. |

`tests/testthat/test-docs.R` checks that this table and `DESCRIPTION` name the same
versions. It cannot check the floors themselves, and neither can any other test:
`layer_factory()` bakes the `Stat` and `Geom` it captured into the lazy-load
database while the package is being installed, so an installed copy of
`coursekata` either survived that or was never built. By the time a test session
starts, the question has already been answered.

### Verifying a floor

The only check that means anything is a source install into a library where the
floor versions are what resolve.

1. Put the floor versions in a library of their own. Install `ggformula` first:
   its own dependency resolution can pull in a current `ggplot2`, and the pinned
   one has to be the copy that survives.

   ```sh
   FLOOR=$(mktemp -d)
   Rscript -e "remotes::install_version('ggformula', '0.12.0', lib = '$FLOOR', upgrade = 'never')"
   Rscript -e "remotes::install_version('ggplot2', '3.5.2', lib = '$FLOOR', upgrade = 'never')"
   ```

2. Confirm the library is the one you think it is, rather than trusting step 1:

   ```sh
   export R_LIBS="$FLOOR:$(Rscript -e 'cat(paste(.libPaths(), collapse = ":"))')"
   Rscript -e 'cat(as.character(packageVersion("ggformula")), as.character(packageVersion("ggplot2")), "\n")'
   ```

   It must print `0.12.0 3.5.2`.

3. Install this package from source into a throwaway library:

   ```sh
   R CMD INSTALL -l "$(mktemp -d)" .
   ```

   The last line must be `* DONE (coursekata)`.

4. Confirm the check is capable of failing, or step 3 proved nothing. Put
   `ggformula` 0.10.2 in `$FLOOR` instead of 0.12.0, lower the floor in
   `DESCRIPTION` to match so the install gets past the declared requirement, and
   repeat step 3. It must stop immediately after
   `** byte-compile and prepare package for lazy loading` with

   ```
   Error : 'StatSdRuler' is not an exported object from 'namespace:coursekata'
   ERROR: lazy loading failed for package 'coursekata'
   ```

   Restore `DESCRIPTION` afterwards.

Running the test suite in the floor library is a separate and much weaker check.
It fails the `show_dgp` and `show_cutoffs` snapshots, because ggplot2 3.5.2 renders
them differently from the release the committed `.svg` files were generated under;
never accept a snapshot from a floor run. And never run the suite in a library
where `vdiffr` and `svglite` are missing altogether: the visual tests skip, and
`tests/testthat/_snaps` is then deleted outright, every committed file in it.
