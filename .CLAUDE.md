# tinyplot - AI Assistant Context

## Package Overview

**tinyplot** is a lightweight extension of base R graphics providing automatic legends, facets, themes, and other enhancements. Zero recursive dependencies — only base R.

- Main function: `tinyplot()` (alias: `plt()`)
- Add layers: `tinyplot_add()` / `plt_add()`
- Themes: `tinytheme()`
- Parameters: `tpar()`

## Quick Reference

When working on tinyplot interactively, always use `pkgload::load_all()` to load the development version — never `library(tinyplot)`. This ensures you're testing your local changes, not an installed copy.

```r
pkgload::load_all()

# Then test interactively, e.g.
plt(Sepal.Length ~ Petal.Length | Species, data = iris)
```

## Repository Structure

- `R/` — Package source. The main entry point is `tinyplot.R` (~57KB). Plot types live in `type_*.R` files. Other key files include `legend.R`, `facet.R`, `by_aesthetics.R`, `tinytheme.R`, `tpar.R`, and `environment.R`. Input validation helpers follow the `sanitize_*.R` naming convention. Utility functions are in `utils.R`.
- `inst/tinytest/` — Test suite (`tinytest` + `tinysnapshot`). Snapshot SVGs are in `_tinysnapshot/`.
- `man/` — roxygen2-generated `.Rd` files.
- `vignettes/` — Package vignettes (qmd format).
- `SCRATCH/` — Developer scratch files and experiments (not part of the package).

## Code Style & Conventions

### Zero Dependency Requirement
tinyplot has zero recursive dependencies — it imports only base R packages (`graphics`, `grDevices`, `stats`, `tools`, `utils`). All contributions must preserve this. Do not add new package dependencies under `Imports` or `Depends`.

### Assignment & Syntax
```r
# Use = not <-
x = 5

# Use function() not \() — package requires R >= 4.0.0 compatibility
fn = function(x) x^2

# Prefer [[ over $ for element access (no partial matching, works with variables)
legend_args[["title"]]
settings[["datapoints"]]
# NOT: legend_args$title, settings$datapoints
```

### No Pipes in Package Code
The package targets R >= 4.0.0, so the base pipe `|>` (introduced in 4.1) is not available. Use intermediate variables or nested calls instead.

### Line Length
Wrap at ~80 characters. Break long function calls across lines.

## Architecture & Key Patterns

### Execution Flow
The main pipeline in `tinyplot.default()` follows this sequence:
1. Save par state and the call (for `tinyplot_add()` replay)
2. Build a `settings` environment with all inputs
3. Sanitize inputs: type → axes → labels → facets → datapoints
4. Run the type's `data` function (`type_data(settings)`) to transform data
5. Handle flipping, bubble sizing, axis limits
6. Compute group aesthetics (colours, pch, lty, etc.)
7. Prepare legends
8. Draw the facet grid (if any)
9. **Nested drawing loop**: outer loop over facets, inner loop over `by` groups — each iteration calls the type's `draw` function with per-group data
10. Save end par state for layer recall

### Type System
Each plot type is a `tinyplot_type` S3 object created by a `type_*()` constructor:
```r
type_boxplot = function(range = 1.5, ...) {
  out = list(
    draw = draw_boxplot(range = range, ...),  # closure: does actual plotting per group
    data = data_boxplot(...),                  # closure: preprocesses data, injects into settings
    name = "boxplot"                           # string identifier
  )
  class(out) = "tinyplot_type"
  return(out)
}
```

- `draw` function signature: `function(iby, ix, iy, ipch, ilty, icol, ibg, ...)`
  - Called once per group (`iby` = group index)
  - Receives per-group subsetted data
- `data` function signature: `function(settings, ...)`
  - Receives the `settings` environment
  - Reads from settings via `env2env(settings, environment(), keys)`
  - Writes back via `env2env(environment(), settings, keys)`
  - Can modify `datapoints`, `xlabs`, `col`, `bg`, `by`, `facet`, `group_offsets`, legend args, etc.

### Settings Environment
Individual `tinyplot()` calls store plot state in a temporary `settings` environment. Type-specific `data` functions read/write to this environment using `env2env()`. This avoids copying large objects and allows types to customize behaviour.

### Package-Level State (.tinyplot_env)
Managed via `get_environment_variable()` / `set_environment_variable()` in `environment.R`:
- `.last_call` — last tinyplot call (used by `tinyplot_add()`)
- `.saved_par_before` / `.saved_par_after` / `.saved_par_first` — par state for layer restoration
- `.tpar_hooks` — theme hooks
- `.group_offsets` — dodge offsets for layering (used by jitter-on-boxplot etc.)

### recordGraphics() for Resize Handling
Coordinate-dependent calculations (especially legends) must be wrapped in `recordGraphics()` so they replay correctly on device resize:
```r
recordGraphics(
  tinylegend(legend_env),
  list = list(legend_env = legend_env),
  env = getNamespace("tinyplot")
)
```

### Theme System
Themes use `before.plot.new` hooks. Legend code must preserve/restore hooks:
```r
oldhook = getHook("before.plot.new")
setHook("before.plot.new", function() par(new = TRUE), action = "append")
plot.new()
setHook("before.plot.new", oldhook, action = "replace")
```

### Legend Positioning
- Inner positions: `"right"`, `"topleft"`, etc.
- Outer positions (with `!`): `"right!"`, `"bottom!"`, etc.
- Outer legends adjust plot margins via `par(oma=...)` and `par(mar=...)`

### Group Offsets / Dodge
When multiple groups share the same x-position (boxplot, violin, etc.), offsets are computed in the type's `data` function and stored as `group_offsets` + `offsets_axis`. These are saved to `.tinyplot_env` so that `tinyplot_add()` layers (e.g., jitter) can align correctly.

## Testing & CI

Tests use `tinytest` + `tinysnapshot`. Snapshot tests produce SVG output with Liberation fonts and must be run on Linux (`options("tinysnapshot_os" = "Linux")`). For non-Linux users, we provide a dedicated `.devcontainer` for running tests via VS Code or GitHub Codespaces:
1. Open repo in VS Code
2. Command Palette → "Dev Containers: Reopen in Container"
3. Dependencies install automatically

Non-snapshot tests (logical assertions, error checks, etc.) run fine on any platform. Even with the devcontainer, a small number of snapshot tests (~2-3) may produce false positive failures on macOS hosts due to imperceptible rendering differences. These show up in `inst/tinytest/_tinysnapshot_review/` but the visual differences are too small to detect by eye. Known false positives:

- `xaxl_yaxl`
- `palette_manual_continuous`

This is a known quirk — don't worry about these specific persistent failures. However, if you see more than ~3 snapshot failures, something real is likely broken and needs investigation.

### Running Tests
```bash
# Via Makefile
make testall                                          # Run all tests
make testone testfile="inst/tinytest/test-legend.R"   # Run single test file

# Via R
tinytest::run_test_dir("inst/tinytest")
tinytest::run_test_file("inst/tinytest/test-legend.R")
```

### Continuous Integration
All contributions should go through a pull request. PRs against `main` automatically trigger GitHub Actions CI, which runs `R CMD check` (including the full test suite) on Ubuntu with both R-release and R-devel. Snapshot tests are included in this CI run, so even if you can't run them locally, CI will catch any regressions.

### CRAN Submissions
The test suite (snapshot SVGs in particular) adds significant size to the installed package. Tests are only intended to run locally and on CI — not on CRAN.

**Pre-submission checklist:**
1. Create a new branch `cran_v<x.x.x>` from main
2. Verify version is updated and aligned in both `DESCRIPTION` and `NEWS.md`
3. Verify `Date` field in `DESCRIPTION` matches the submission date
4. Update `cran-comments.md` with new version and release type
5. Open a PR from the branch to main
6. Verify CI passes on the PR
7. Run reverse dependency checks: push a `revdep-v<x.x.x>` branch to trigger the revdep workflow
8. Uncomment the `inst/tinytest/` line in `.Rbuildignore` to exclude tests from tarball
9. Run `devtools::check_win_devel()` and wait for results
10. Run `R CMD check --as-cran` locally
11. Fix any issues / notes and update cran-comments.md as necessary
12. Submit to CRAN

**Post-acceptance:**
1. Re-comment the `inst/tinytest/` line in `.Rbuildignore` and push to PR so CI continues to pick up tests
2. Merge the PR to main
3. Create a new GitHub release tagged `v<x.x.x>`

### Manual Testing
Some features require manual testing, particularly:
- Window resize behaviour (legends, facets, layers should stay aligned)
- Positron IDE compatibility
- Interactive device behaviour

## Development Workflow

### Makefile Commands
```bash
make help        # Show all available commands
make document    # Generate documentation (devtools::document)
make check       # Full R CMD check
make install     # Install package locally
make website     # Build documentation website (altdoc)
```

### Adding a New Plot Type
Where possible, reuse existing `draw_*()` and `data_*()` functions rather than writing new ones from scratch. Many types are thin wrappers that combine existing building blocks with custom data transformations. For example:
- `type_lm`, `type_glm`, and `type_loess` all use `draw_ribbon()` — they only differ in their `data_*()` functions
- `type_barplot` and `type_histogram` both use `draw_rect()`
- `type_jitter` and `type_pointrange` both use `draw_points()`
- `type_spline` and `type_summary` both use `draw_lines()`
- `type_errorbar` reuses `data_pointrange()` entirely
- `type_area` has no draw function at all — its data function sets `type = "ribbon"` to delegate drawing

Steps:
1. Create `R/type_<name>.R` with the `type_<name>()` constructor, `draw_<name>()`, and optionally `data_<name>()`
2. Register the type in `R/sanitize_type.R`: add the string name to the `known_types` vector and a corresponding entry in the `switch` statement that maps it to the constructor
3. Add `@export` tag to the constructor
4. Run `make document` to update NAMESPACE
5. Add tests in `inst/tinytest/test-type_<name>.R`
6. Add snapshot SVGs by running tests on Linux (devcontainer)

### Modifying Legend Behaviour
Type-specific legend customizations should go in the type's `data` function by modifying `settings$legend_args`:
```r
settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
```

## Common Pitfalls

- **Legend misalignment on resize**: Ensure coordinate-dependent calculations are inside `recordGraphics()`. See PRs #438, #540, #541.
- **Layer alignment with grouped types**: When adding jitter/points on top of boxplot/violin, the layer needs access to `group_offsets` from `.tinyplot_env`. See PR #561.
- **Theme hook corruption**: Always save and restore `before.plot.new` hooks when calling `plot.new()` in legend code.
- **R 4.0.0 compat**: No `|>` pipe, no `\()` lambda.

## Links

- Docs: https://grantmcdermott.com/tinyplot/
- Issues: https://github.com/grantmcdermott/tinyplot/issues
- CRAN: https://CRAN.R-project.org/package=tinyplot
