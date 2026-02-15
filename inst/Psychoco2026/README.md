## tinyplot: Lightweight Extension of the Base R Graphics System

_Grant McDermott, Vincent Arel-Bundock, Achim Zeileis_

**Presented at:** Psychoco 2026 - International Workshop on Psychometric Computing, Università di Padova, Italy, February 5-6.

**PDF slides:** <https://www.zeileis.org/papers/Psychoco-2026.pdf>

**Abstract:**

The base R graphics system provides a lot of powerful infrastructure for drawing
data visualizations. At the core is the `plot()` generic function with its
default and formula methods. The default method can handle many basic plotting
elements (points, lines, etc.) and the formula method flexibly handles various
`y ~ x` setups including scatterplots (numeric `y` vs. numeric `x`), boxplots
(numeric `y` vs. categorical `x`), and spineplots/spinograms (categorical `y`).
Moreover, there are many elements that can be added like legends, axes,
annotation, grids of displays, etc.

However, based on this powerful infrastructure base R provides only rather
limited convenience features such as those pioneered by newer (`grid`-based)
visualization packages like `ggplot2` and `lattice`, e.g., grouped plots with
automatic legends and/or facets, advanced visualization types, and easy
customization via ready-made themes.

The `tinyplot` package fills this gap by providing a lightweight extension of
the base R graphics system. It aims to preserve the strengths of the base R
infrastructure (including the formula-based interface) while adding the
convenience features above without requiring (strong) non-base dependencies.
The presentation provides an introduction to `tinyplot` using various
visualization examples, highlighting strengths and weaknesses compared to other
packages. The package is available from CRAN
(<https://doi.org/10.32614/CRAN.package.tinyplot>) and has many more galleries
and tutorials at <https://grantmcdermott.com/tinyplot/>.

**Notes:**

- The source file for the PDF slides is `slides-beamer.qmd`. This requires that
  the [UIBK beamer class](https://git.uibk.ac.at/uibklatex/beamer_letter) is installed.
- As an alternative a similar HTML version of the slides can be created from
  `slides-revealjs.qmd`.
