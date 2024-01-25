

# Set or query plot2 parameters

[**Source code**](https://github.com/grantmcdermott/plot2/tree/main/R/#L)

## Description

<code>par2</code> can be used to set or query the additional set of
graphical parameters provided by <code>plot2</code> (i.e., beyond the
base set provided by <code>par</code>). Similar to its base counterpart,
parameters can be set by passing the appropriate tag-value argument
pairs to <code>par2</code>. Multiple parameters can be set or queried at
the same time, as a list.

## Usage

<pre><code class='language-R'>par2(...)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="par2_:_...">…</code>
</td>
<td>
arguments of the form <code>tag = value</code>. Supported
<code>plot2</code> parameters are described in the ‘Graphical
Parameters’ section below.
</td>
</tr>
</table>

## Graphical Parameters

<table>
<tr>
<td style="text-align: left;">
<code>facet.cex</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
Expansion factor for facet titles. Defaults to <code>1</code>.
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>facet.font</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
An integer corresponding to the desired font face for facet titles. For
most font families and graphics devices, one of four possible values:
<code>1</code> (regular), <code>2</code> (bold), <code>3</code>
(italic), or <code>4</code> (bold italic). Defaults to
<code>NULL</code>, which is equivalent to <code>1</code> (i.e.,
regular).
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>facet.col</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
Character or integer specifying the facet text colour. If an integer,
will correspond to the user’s default global colour palette (see
<code>palette</code>). Defaults to <code>NULL</code>, which is
equivalent to “black”.
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>facet.bg</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
Character or integer specifying the facet background colour. If an
integer, will correspond to the user’s default colour palette (see
<code>palette</code>). Passed <code>rect</code>. Defaults to
<code>NULL</code> (none).
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>facet.border</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
Character or integer specifying the facet border colour. If an integer,
will correspond to the users default colour palette (see
<code>palette</code>). Passed <code>rect</code>. Defaults to
<code>NA</code> (none).
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>fmar</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
A numeric vector of form <code>c(b,l,t,r)</code> for controlling the
(base) margin padding, in terms of lines, between the individual facets
in a faceted plot. Defaults to <code>c(1,1,1,1)</code>, i.e. a single
line of padding around each facet. If more that three facets are
detected, the <code>fmar</code> parameter is scaled by 0.75 (i.e.,
three-quarters) to reduce the excess whitespace that would otherwise
arise due to the absent axes lines and labels. (An exception is made for
2x2 plots to better match the <code>cex</code> expansion logic of the
base graphics system under this particular layout.) Similarly, note that
an extra 0.5 lines is subtracted from each side of the facet padding for
plots that aren’t framed, to reduce excess whitespace.
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>last_facet_par</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
Full list of graphical parameters used to constructed the most recent
faceted <code>plot2</code> plot during the current session. Unlike other
<code>par2</code> parameters, this parameter is intended for internal
use (specifically, to enable adding further elements on top of an
existing faceted plot) and should <em>not</em> be set by the user.
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>lmar</code>
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
A numeric vector of form <code>c(inner, outer)</code> that gives the
margin padding, in terms of lines, around the automatic
<code>plot2</code> legend. Defaults to <code>c(1.0, 0.1)</code>, where
the first number represents the “inner” margin between the legend and
the plot region, and the second number represents the “outer” margin
between the legend and edge of the graphics device. (Note that an
exception for the definition of the “outer” legend margin occurs when
the legend placement is <code>“top!”</code>, since the legend is placed
above the plot region but below the main title. In such cases, the outer
margin is relative to the existing gap between the title and the plot
region, which is itself determined by <code>par(“mar”)\[3\]</code>.)
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
</tr>
</table>
