
# Calculate placement of legend and and draw it

[**Source code**](https://github.com/grantmcdermott/plot2/tree/main/R/#L)

## Description

Internal function used to calculate the placement of (including outside
the plotting area) and drawing of legend.

## Usage

<pre><code class='language-R'>draw_legend(
  legend = NULL,
  legend.args = NULL,
  by_dep = NULL,
  lgnd_labs = NULL,
  type = NULL,
  pch = NULL,
  lty = NULL,
  col = NULL,
  bg = NULL,
  cex = NULL,
  new_plot = TRUE
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_legend">legend</code>
</td>
<td>
Legend placement keyword or list, passed down from <code>plot2</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_legend.args">legend.args</code>
</td>
<td>
Additional legend arguments to be passed to <code>legend()</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_by_dep">by_dep</code>
</td>
<td>
The (deparsed) "by" grouping variable name.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_lgnd_labs">lgnd_labs</code>
</td>
<td>
The labels passed to <code>legend(legend = …)</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_type">type</code>
</td>
<td>
Plotting type(s), passed down from <code>plot2</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_pch">pch</code>
</td>
<td>
Plotting character(s), passed down from <code>plot2</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_lty">lty</code>
</td>
<td>
Plotting linetype(s), passed down from <code>plot2</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_col">col</code>
</td>
<td>
Plotting colour(s), passed down from <code>plot2</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_bg">bg</code>
</td>
<td>
Plotting character background fill colour(s), passed down from
<code>plot2</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_cex">cex</code>
</td>
<td>
Plotting character expansion(s), passed down from <code>plot2</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="draw_legend_:_new_plot">new_plot</code>
</td>
<td>
Should we be calling plot.new internally?
</td>
</tr>
</table>
