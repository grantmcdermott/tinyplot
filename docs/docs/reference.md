# Reference 

## Plot2-package

### Description

The goal of \*\*plot2\*\* is to extend the functionality of base R's
default (2D) 'plot()' function, particularly as it applies to scatter
and line plots with grouped data. For example, \*\*plot2\*\* makes it
easy to plot different categories of a dataset in a single function call
and highlight these categories (groups) using modern colour palettes.
Coincident with this grouping support, \*\*plot2\*\* also produces
automatic legends with scope for further customization. While the
package also offers several other minor enhancements, it tries as far as
possible to be a drop-in replacement for the equivalent base plot
function. Users should be able to swap a valid 'plot' call with 'plot2'
without any changes to the expected output.

### Details

`plot2-package`

### Author(s)

**Maintainer**: Grant McDermott <gmcd@amazon.com>

Authors:

-   Vincent Arel-Bundock <vincent.arel-bundock@umontreal.ca>
    ([ORCID](https://orcid.org/0000-0003-1995-6531)) (@vincentab)

-   Achim Zeileis <Achim.Zeileis@R-project.org>
    ([ORCID](https://orcid.org/0000-0003-0918-3766))

Other contributors:

-   Etienne Bacher <etienne.bacher@protonmail.com> \[contributor\]

### See Also

Useful links:

-   <https://grantmcdermott.com/plot2/>

-   <http://grantmcdermott.com/plot2/>

-   Report bugs at <https://github.com/grantmcdermott/plot2/issues>


---
## Plot2

### Description

Extends base R's default plotting function, particularly as it applies
to scatter and line plots with grouped data. For example, `plot2` makes
it easy to plot different categories of a dataset in a single function
call and highlight these categories (groups) using modern colour
palettes. Coincident with this grouping support, `plot2` also produces
automatic legends with scope for further customization. While the
package also offers several other minor enhancements, it tries as far as
possible to be a drop-in replacement for the equivalent base plot
function. Users should generally be able to swap a valid `plot` call
with `plot2` without any changes to the output.

### Usage

    plot2(x, ...)

    ## Default S3 method:
    plot2(
      x,
      y = NULL,
      by = NULL,
      data = NULL,
      type = "p",
      xlim = NULL,
      ylim = NULL,
      log = "",
      main = NULL,
      sub = NULL,
      xlab = NULL,
      ylab = NULL,
      ann = par("ann"),
      axes = TRUE,
      frame.plot = axes,
      asp = NA,
      grid = NULL,
      palette = NULL,
      legend = NULL,
      pch = NULL,
      lty = NULL,
      col = NULL,
      bg = NULL,
      cex = 1,
      par_restore = FALSE,
      ymin = NULL,
      ymax = NULL,
      ribbon_alpha = 0.2,
      add = FALSE,
      ...
    )

    ## S3 method for class 'formula'
    plot2(
      x = NULL,
      data = parent.frame(),
      type = "p",
      xlim = NULL,
      ylim = NULL,
      main = NULL,
      sub = NULL,
      xlab = NULL,
      ylab = NULL,
      ann = par("ann"),
      axes = TRUE,
      frame.plot = axes,
      asp = NA,
      grid = NULL,
      pch = NULL,
      col = NULL,
      lty = NULL,
      par_restore = FALSE,
      formula = NULL,
      subset = NULL,
      na.action = NULL,
      drop.unused.levels = TRUE,
      ...
    )

    ## S3 method for class 'density'
    plot2(
      x = NULL,
      by = NULL,
      type = c("l", "ribbon"),
      xlim = NULL,
      ylim = NULL,
      main = NULL,
      sub = NULL,
      xlab = NULL,
      ylab = NULL,
      ann = par("ann"),
      axes = TRUE,
      frame.plot = axes,
      asp = NA,
      grid = NULL,
      pch = NULL,
      col = NULL,
      lty = NULL,
      bg = NULL,
      par_restore = FALSE,
      ...
    )

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="plot2_:_x">x</code>, <code id="plot2_:_y">y</code></td>
<td><p>the x and y arguments provide the x and y coordinates for the
plot. Any reasonable way of defining the coordinates is acceptable. See
the function xy.coords for details. If supplied separately, they must be
of the same length.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_...">...</code></td>
<td><p>other <code>graphical</code> parameters (see <code>par</code> and
also the "Details" section of <code>plot</code>).</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_by">by</code></td>
<td><p>the grouping variable that you want to categorize (i.e., colour)
the plot by.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_data">data</code></td>
<td><p>a data.frame (or list) from which the variables in formula should
be taken. A matrix is converted to a data frame.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_type">type</code></td>
<td><p>character string giving the type of plot desired. Options
are:</p>
<ul>
<li><p>The same set of 1-character values supported by plot: "p" for
points, "l" for lines, "b" for both points and lines, "c" for empty
points joined by lines, "o" for overplotted points and lines, "s" and
"S" for stair steps and "h" for histogram-like vertical lines. "n" does
not produce any points or lines.</p></li>
<li><p>Additional plot2 types: "pointrange", "errorbar", and "ribbon"
for drawing these interval plot types. (Note that specifying "ribbon"
for objects of class <code>density</code> will yield a density plot with
a shaded interior.)</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code id="plot2_:_xlim">xlim</code></td>
<td><p>the x limits (x1, x2) of the plot. Note that x1 &gt; x2 is
allowed and leads to a ‘reversed axis’. The default value, NULL,
indicates that the range of the <code>finite</code> values to be plotted
should be used.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_ylim">ylim</code></td>
<td><p>the y limits of the plot.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_log">log</code></td>
<td><p>a character string which contains "x" if the x axis is to be
logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if
both axes are to be logarithmic.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_main">main</code></td>
<td><p>a main title for the plot, see also <code>title</code>.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_sub">sub</code></td>
<td><p>a subtitle for the plot.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_xlab">xlab</code></td>
<td><p>a label for the x axis, defaults to a description of x.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_ylab">ylab</code></td>
<td><p>a label for the y axis, defaults to a description of y.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_ann">ann</code></td>
<td><p>a logical value indicating whether the default annotation (title
and x and y axis labels) should appear on the plot.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_axes">axes</code></td>
<td><p>a logical value indicating whether both axes should be drawn on
the plot. Use <code style="white-space: pre;">⁠graphical parameter⁠</code>
"xaxt" or "yaxt" to suppress just one of the axes.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_frame.plot">frame.plot</code></td>
<td><p>a logical indicating whether a box should be drawn around the
plot. Can also use <code>frame</code> as an acceptable argument
alias.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_asp">asp</code></td>
<td><p>the y/xy/x aspect ratio, see <code>plot.window</code>.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_grid">grid</code></td>
<td><p>argument for plotting a background panel grid, one of either:</p>
<ul>
<li><p>a logical (i.e., <code>TRUE</code> to draw the grid), or</p></li>
<li><p>a panel grid plotting function like <code>grid()</code>. Note
that this argument replaces the <code>panel.first</code> and
<code>panel.last</code> arguments from base <code>plot()</code> and
tries to make the process more seemless with better default behaviour.
Default is not to draw a grid.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code id="plot2_:_palette">palette</code></td>
<td><p>one of the following options:</p>
<ul>
<li><p>NULL (default), in which case the palette will be determined by
the the user's default graphics palette, e.g. "R4". See
<code>?palette()</code>. Note that some internal checking is done to
make sure that resulting colours match the number of groups. For larger
group numbers, the "viridis" palette will be used instead.</p></li>
<li><p>A convenience string corresponding to one of the many palettes
listed by either <code>palette.pals()</code> or <code>hcl.pals()</code>.
Note that the string can be case-insensitive (e.g., "Okabe-Ito" and
"okabe-ito" are both valid).</p></li>
<li><p>A palette-generating function. This can be "bare" (e.g.,
<code>palette.colors</code>) or "closed" with a set of named arguments
(e.g., <code>palette.colors(palette = "Okabe-Ito", alpha = 0.5)</code>).
Note that any unnamed arguments will be ignored and the key
<code>n</code> argument, denoting the number of colours, will
automatically be spliced in as the number of groups.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_legend">legend</code></td>
<td><p>one of the following options:</p>
<ul>
<li><p>NULL (default), in which case the legend will be determined by
the grouping variable. If there is no group variable (i.e.,
<code>by</code> is NULL) then no legend is drawn. If a grouping variable
is detected, then an automatic legend is drawn to the <em>outer</em>
right of the plotting area. Note that the legend title and categories
will automatically be inferred from the <code>by</code> argument and
underlying data.</p></li>
<li><p>A convenience string indicating the legend position. The string
should correspond to one of the position keywords supported by the base
<code>legend</code> function, e.g. "right", "topleft", "bottom", etc. In
addition, <code>plot2</code> supports adding a trailing exclamation
point to these keywords, e.g. "right!", "topleft!", or "bottom!". This
will place the legend <em>outside</em> the plotting area and adjust the
margins of the plot accordingly. Finally, users can also turn off any
legend printing by specifying "none".</p></li>
<li><p>Logical value, where TRUE corresponds to the default case above
(same effect as specifying NULL) and FALSE turns the legend off (same
effect as specifying "none").</p></li>
<li><p>A list or, equivalently, a dedicated <code>legend()</code>
function with supported legend arguments, e.g. "bty", "horiz", and so
forth.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code id="plot2_:_pch">pch</code></td>
<td><p>plotting "character", i.e., symbol to use. Character, integer, or
vector of length equal to the number of categories in the
<code>by</code> variable. See <code>pch</code>. In addition, users can
supply a special <code>pch = "by"</code> convenience argument, in which
case the characters will automatically loop over the number groups. This
automatic looping will begin at the global character value (i.e.,
<code>par("pch")</code>) and recycle as necessary.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_lty">lty</code></td>
<td><p>line type. Character, integer, or vector of length equal to the
number of categories in the <code>by</code> variable. See
<code>lty</code>. In addition, users can supply a special
<code>lty = "by"</code> convenience argument, in which case the line
type will automatically loop over the number groups. This automatic
looping will begin at the global line type value (i.e.,
<code>par("lty")</code>) and recycle as necessary.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_col">col</code></td>
<td><p>plotting color. Character, integer, or vector of length equal to
the number of categories in the <code>by</code> variable. See
<code>col</code>. Note that the default behaviour in <code>plot2</code>
is to vary group colors along any variables declared in the
<code>by</code> argument. Thus, specifying colors manually should not be
necessary unless users wish to override the automatic colors produced by
this grouping process. Typically, this would only be done if grouping
features are deferred to some other graphical parameter (i.e., passing
the "by" keyword to one of <code>pch</code>, <code>lty</code>, or
<code>bg</code>; see below.)</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_bg">bg</code></td>
<td><p>background (fill) color for the open plot symbols 21:25: see
<code>points.default</code>. In addition, users can supply a special
<code>bg = "by"</code> convenience argument, in which case the
background color will inherit the automatic group coloring intended for
the <code>col</code> parameter.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_cex">cex</code></td>
<td><p>character expansion. A numerical vector (can be a single value)
giving the amount by which plotting characters and symbols should be
scaled relative to the default. Note that NULL is equivalent to 1.0,
while NA renders the characters invisible.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_par_restore">par_restore</code></td>
<td><p>a logical value indicating whether the <code>par</code> settings
prior to calling <code>plot2</code> should be restored on exit. Defaults
to FALSE, which makes it possible to add elements to the plot after it
has been drawn. However, note the the outer margins of the graphics
device may have been altered to make space for the <code>plot2</code>
legend. Users can opt out of this persistent behaviour by setting to
TRUE instead. (Another option would be calling <code>dev.off()</code> to
reset all <code>par</code> settings to their defaults.)</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_ymin">ymin</code>, <code
id="plot2_:_ymax">ymax</code></td>
<td><p>minimum and maximum coordinates of interval plot types. Only used
when the <code>type</code> argument is one of "pointrange", "errorbar",
or "ribbon".</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_ribbon_alpha">ribbon_alpha</code></td>
<td><p>numeric factor modifying the opacity alpha of any ribbon shading;
typically in <code style="white-space: pre;">⁠[0, 1]⁠</code>. Default
value is 0.2. Only used when <code>type = "ribbon"</code>.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_add">add</code></td>
<td><p>logical. If TRUE, then elements are added to the current plot
rather than drawing a new plot window. Note that the automatic legend
for the added elements will be turned off.</p></td>
</tr>
<tr class="odd">
<td><code id="plot2_:_formula">formula</code></td>
<td><p>a <code>formula</code> that may also include a grouping variable
after a "|", such as <code>y ~ x | z</code>. Note that the
<code>formula</code> and <code>x</code> arguments should not be
specified in the same call.</p></td>
</tr>
<tr class="even">
<td><code id="plot2_:_subset">subset</code>, <code
id="plot2_:_na.action">na.action</code>, <code
id="plot2_:_drop.unused.levels">drop.unused.levels</code></td>
<td><p>arguments passed to <code>model.frame</code> when extracting the
data from <code>formula</code> and <code>data</code>.</p></td>
</tr>
</tbody>
</table>

### Examples

```r
# plot2 should be a drop-in replacement for (most) regular plot calls. For
# example:

par(mfrow = c(1, 2))
plot(0:10, main = "plot")
plot2(0:10, main = "plot2")

par(mfrow = c(2, 2))
plot(airquality$Day, airquality$Temp, main = "plot")
plot(Temp ~ Day, data = airquality, main = "plot (formula)")
#' plot2(airquality$Day, airquality$Temp, main = "plot2")
plot2(Temp ~ Day, data = airquality, main = "plot2 (formula)")

dev.off() # reset to default layout

# Unlike vanilla plot, however, plot2 allows you to characterize groups 
# (using either the `by` argument or equivalent `|` formula syntax).
# Notice that we also get an automatic legend.

plot2(airquality$Day, airquality$Temp, by = airquality$Month)
plot2(Temp ~ Day | Month, airquality)

# Use standard base plotting arguments to adjust features of your plot.
# For example, change `pch` (plot character) to get filled points.

plot2(
  Temp ~ Day | Month,
  data = airquality,
  pch = 16
)

# Converting to a grouped line plot is a simple matter of adjusting the
# `type` argument.

plot2(
  Temp ~ Day | Month,
  data = airquality,
  type = "l"
)

# The (automatic) legend position and look can be customized using
# appropriate arguments. Note the trailing "!" in the `legend` position
# argument below. This tells `plot2` to place the legend _outside_ the plot
# area.

plot2(
  Temp ~ Day | Month,
  data = airquality,
  type = "l",
  legend = legend("bottom!", title = "Month of the year", bty = "o")
)

# Regular legend position keywords without the exclamation point (i.e., for
# inside the plot area) should still work. Grouped density plot example:

plot2(
  density(airquality$Temp),
  by = airquality$Month, 
  legend = legend("topright", bty="o", title = "Month")
)

# The default group colours are inherited from either the "R4" or "Viridis"
# palettes, depending on the number of groups. However, all palettes listed
# by `palette.pals()` and `hcl.pals()` are supported as convenience strings,
# or users can supply a valid palette-generating function for finer control
# over transparency etc.

plot2(
  Temp ~ Day | Month,
  data = airquality,
  type = "l",
  palette = "Tableau 10"
)

# It's possible to further customize the look of you plots using familiar
# arguments and base plotting theme settings (e.g., via `par`).

par(family = "HersheySans")
plot2(
  Temp ~ Day | Month,
  data = airquality,
  type = "b", pch = 16,
  palette = palette.colors(palette = "Tableau 10", alpha = 0.5),
  main = "Daily temperatures by month",
  frame = FALSE, grid = TRUE
)

# For nice out-of-the-box themes, we recommend the `basetheme` package.

par(family = "") # revert global font change from above

library(basetheme)
basetheme("royal") # or "clean", "dark", "ink", "brutal", etc.

plot2(
  Temp ~ Day | Month,
  data = airquality,
  type = "b", pch = 15:19,
  palette = "Tropic",
  main = "Daily temperatures by month"
)

basetheme(NULL)  # back to default theme
```


---
