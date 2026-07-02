source("helpers.R")

# Platform-independent checks for the lighter-opaque fill logic (#646, #614).
# These assert on the colours resolved by the internal by_bg() helper, so they
# run everywhere (no snapshot rendering required).

ac = grDevices::adjustcolor

bb = function(...) {
  tinyplot:::by_bg(
    palette = NULL, ribbon.alpha = 0.2, adjustcolor = ac, ...
  )
}

# Helper predicates on hex colours -------------------------------------------

# Fully opaque (alpha == FF), i.e. no transparency applied.
is_opaque = function(x) all(grepl("FF$", toupper(x)) | nchar(x) %in% c(7L) | !grepl("^#", x))
# Semi-transparent (alpha < FF), e.g. the legacy "33" ribbon.alpha fill.
is_semitransparent = function(x) all(grepl("^#.{6}(?!FF)..$", x, perl = TRUE))
# A "light" tint: high luminance in all channels.
is_light = function(x) {
  rgb = grDevices::col2rgb(x)
  all(colMeans(rgb) > 180)
}

# Activate a colour-forward theme so the qualitative palette starts with a
# chromatic colour (Tableau 10 -> blue), making the tint visible.
tinytheme("clean2")

# Grouped barplot: default lighten = TRUE -> light, opaque tints ---------------
bar_light = bb(
  bg = "by", fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 3,
  type = "barplot", by = factor(1:3), lighten = TRUE
)
expect_equal(length(bar_light), 3L)
expect_true(is_opaque(bar_light))
expect_true(is_light(bar_light))

# Grouped barplot: lighten = FALSE -> saturated, opaque palette colours --------
bar_dark = bb(
  bg = "by", fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 3,
  type = "barplot", by = factor(1:3), lighten = FALSE
)
expect_true(is_opaque(bar_dark))
expect_false(is_light(bar_dark))
# The lightened fill should differ from the saturated one.
expect_false(isTRUE(all.equal(bar_light, bar_dark)))

# Grouped boxplot: default lighten = TRUE -> light opaque (NOT semi-transparent)
box_light = bb(
  bg = "by", fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 3,
  type = "boxplot", by = factor(1:3), lighten = TRUE
)
expect_true(is_opaque(box_light))
expect_true(is_light(box_light))

# Grouped boxplot: lighten = FALSE -> legacy semi-transparent fill -------------
box_legacy = bb(
  bg = "by", fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 3,
  type = "boxplot", by = factor(1:3), lighten = FALSE
)
expect_true(is_semitransparent(box_legacy))

# Grouped violin mirrors barplot/boxplot ---------------------------------------
vio_light = bb(
  bg = "by", fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 3,
  type = "violin", by = factor(1:3), lighten = TRUE
)
expect_true(is_opaque(vio_light))
expect_true(is_light(vio_light))

# Single-group fill is lightened by default and matches grouped colour[1] ------
bar_single = bb(
  bg = NULL, fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 1,
  type = "barplot", by = NULL, lighten = TRUE
)
expect_equal(bar_single, bar_light[1])

# A numeric `fill` request layers transparency on top of the *lightened* base,
# so adding alpha lightens (never darkens) the interior (#646 follow-up). The
# base RGB should match the opaque light tint; only the alpha channel changes.
bar_fill07 = bb(
  bg = 0.7, fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 1,
  type = "barplot", by = NULL, lighten = TRUE
)
expect_equal(substr(bar_fill07, 1, 7), substr(bar_single, 1, 7))
expect_true(is_semitransparent(bar_fill07))

# An explicit fill colour is always honoured verbatim (never lightened) --------
box_bisque = bb(
  bg = "bisque", fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 3,
  type = "boxplot", by = factor(1:3), lighten = TRUE
)
expect_equal(box_bisque, rep("bisque", 3))

# Ordered groupings use a sequential palette and must NOT be lightened ---------
box_ordered = bb(
  bg = "by", fill = NULL, col = NULL, alpha = NULL,
  by_ordered = TRUE, by_continuous = FALSE, ngrps = 3,
  type = "boxplot", by = ordered(1:3), lighten = TRUE
)
expect_false(is_light(box_ordered))

tinytheme()

# No theme active: single-group area fills fall back to neutral "lightgray" ----
bar_none = bb(
  bg = NULL, fill = NULL, col = NULL, alpha = NULL,
  by_ordered = FALSE, by_continuous = FALSE, ngrps = 1,
  type = "barplot", by = NULL, lighten = TRUE
)
expect_equal(bar_none, "lightgray")

# Grouped spineplot (`y_by`) fills follow the same lighter-opaque tint as the
# other area types (#646). draw_spineplot() lightens the per-group bands; the
# legend swatch fill is resolved in prepare_legend() into `bg`. Here we check the
# lighten_fill() helper directly against the resolved group colours.
tinytheme("clean2")
spine_seed = tinyplot:::by_col(
  col = NULL, palette = NULL, alpha = NULL, by_ordered = FALSE,
  by_continuous = FALSE, ngrps = 3, adjustcolor = ac
)
spine_light = tinyplot:::lighten_fill(spine_seed)
expect_true(is_opaque(spine_light))
expect_true(is_light(spine_light))
expect_false(isTRUE(all.equal(spine_light, spine_seed)))

# Spineplot legend swatch: data_spineplot sets the swatch border width from the
# tile border width `lwd` (defaulting to 1), and build_legend_args() forces that
# border *black* whenever it is drawn (pt.lwd > 0), matching the tiles -- a
# group-coloured border would vanish against a pale fill. The fill arrives via
# `bg` (lightened in prepare_legend only when `lighten = TRUE`, which is opt-in
# for spineplots; saturated otherwise). Both fills are exercised below by
# passing `bg` explicitly.
build_swatch = function(bg, pt.lwd = 1) {
  le = new.env(parent = emptyenv())
  tinyplot:::build_legend_args(
    legend_env = le, legend = NULL,
    legend_args = list(pch = 22, pt.lwd = pt.lwd),
    by_dep = "g", lgnd_labs = c("a", "b", "c"),
    type = "spineplot", pch = 22, lty = 1, lwd = 1,
    col = spine_seed, bg = bg, cex = NULL,
    gradient = FALSE
  )
  le$args
}

sw_light = build_swatch(bg = spine_light)
expect_equal(sw_light[["col"]], par("fg"))      # black swatch border (matches tiles)
expect_true(is_light(sw_light[["pt.bg"]]))       # lightened fill (from bg)
expect_equal(sw_light[["pt.lwd"]], 1)            # border drawn

sw_dark = build_swatch(bg = spine_seed)
expect_equal(sw_dark[["col"]], par("fg"))        # black swatch border (matches tiles)
expect_equal(sw_dark[["pt.bg"]], spine_seed)     # saturated fill (from bg)
expect_equal(sw_dark[["pt.lwd"]], 1)             # border drawn

# A thicker tile border (lwd) carries through to the swatch border width.
sw_thick = build_swatch(bg = spine_seed, pt.lwd = 3)
expect_equal(sw_thick[["col"]], par("fg"))       # still black
expect_equal(sw_thick[["pt.lwd"]], 3)            # thick border

# `lwd = 0` (pt.lwd = 0) draws no swatch border, matching borderless tiles. The
# black-colour override is skipped, since no border is drawn.
sw_none = build_swatch(bg = spine_seed, pt.lwd = 0)
expect_equal(sw_none[["pt.lwd"]], 0)             # no border
expect_false(identical(sw_none[["col"]], par("fg")))  # colour not overridden to black

tinytheme()
