FROM rocker/r2u:24.04

ENV NOT_CRAN=true

# DejaVu Sans is pinned by inst/tinytest/helpers.R as the plotmath symbol font,
# so make sure it is present (it is what CI's rocker image resolves to).
RUN apt-get update -qq \
 && apt-get install -y --no-install-recommends fonts-dejavu-core \
 && rm -rf /var/lib/apt/lists/*

# Test-suite R dependencies. On the rocker/r2u base, install.packages() is wired
# to apt via bspm, so these resolve to prebuilt r-cran-* binaries (no source
# compilation) -- the same mechanism as the devcontainer's postCreateCommand.
# tinysnapshot pulls in magick + diffobj automatically.
RUN R -e "install.packages(c('pkgload', 'tinytest', 'tinysnapshot', 'fontquiver', 'svglite', 'rsvg', 'png', 'knitr'))"

WORKDIR /work

CMD ["Rscript", "-e", "pkgload::load_all(quiet=TRUE);tinytest::run_test_dir('inst/tinytest')"]
