#' Translate a native composite annotation through ggformula's layer factory
#' @noRd
gf_b_layer_fun <- function(annotation) {
  force(annotation)
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL, ...) annotation
}

# Generate both public coefficient names from one recipe. They must remain two
# distinct closures: a forwarder would move `environment = parent.frame()` one
# frame inward, while sharing one closure would leave helper diagnostics with no
# stable literal name. `named_layer_factory()` preserves both properties.
gf_b_layer_factory <- function(function_name) {
  named_layer_factory(
    function_name = function_name,
    geom = ggplot2::GeomSegment, stat = "identity", position = "identity",
    aes_form = NULL,
    extras = alist(
      model = , color = "#b599ed", label_color = "black", label_size = 3.5,
      arrow_linewidth = 0.5, show_b0 = TRUE, run = NULL, run_x = NULL,
      b0_alpha = 0.3, b0_linewidth = 0.8, b0_size = 4,
      arrow_nudge = 0.18, label_nudge = 0.08
    ),
    .pre_bindings = alist(
      b_warn_unreachable = b_warn_unreachable,
      b_annotation = b_annotation,
      check_resid_plot = check_resid_plot,
      gf_b_layer_fun = gf_b_layer_fun
    ),
    note = "the model whose coefficients to annotate: a fit from lm() or aov()",
    pre = quote({
      if (!missing(gformula) && missing(model)) {
        model <- gformula
        gformula <- NULL
      }

      if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
        dots <- list(...)
        color <- dots$colour %||% color
        label_color <- dots$label_colour %||% label_color
        unreachable_dots <- dots[setdiff(names(dots), c("colour", "label_colour"))]
        args <- list(
          color = color, label_color = label_color, label_size = label_size,
          arrow_linewidth = arrow_linewidth, show_b0 = show_b0, run = run, run_x = run_x,
          b0_alpha = b0_alpha, b0_linewidth = b0_linewidth, b0_size = b0_size,
          arrow_nudge = arrow_nudge, label_nudge = label_nudge
        )
        b_warn_unreachable(
          unreachable_dots, show.legend, .coursekata_function_name
        )
        check_resid_plot(object, .coursekata_function_name)
        layer_fun <- gf_b_layer_fun(b_annotation(
          if (missing(model)) NULL else model, args,
          fn = .coursekata_function_name, call = environment()
        ))
      }
    })
  )
}

#' Annotate a model's coefficients on a plot
#'
#' Draws the intercept and slope (or group differences) of a fitted model as
#' arrows and labels directly on the plot they describe: a rise-over-run
#' triangle for a continuous predictor, an arrow to each non-reference group for
#' a categorical one. Where [gf_model()] draws the fit itself, `gf_b()` draws
#' the numbers that describe it.
#'
#' @details
#' # What is drawn
#'
#' **A continuous predictor**: a vertical rise arrow from `fit(run_x)` to
#' `fit(run_x + run)`, a horizontal run segment at its tip, a rise label
#' (plotmath b1 when `run` is 1, otherwise `run` times b1), a run-distance
#' label on the triangle's interior side of the run segment, and a hollow
#' dot at `(0, b0)` with a b0 label.
#'
#' **A categorical predictor**: one horizontal reference line at `b0` (the
#' reference level's mean), and for each level after it a segment from `b0` to
#' `b0` plus that level's coefficient, with an arrow head, labeled (plotmath)
#' b1, b2, … Level order is read off `coef(model)`, so a releveled factor still
#' labels the arrow that matches its coefficient.
#'
#' **No predictor** (the empty model): the `b0` line and its label, nothing
#' else.
#'
#' Layers have stable role tags such as `"b0"`, `"b1"`, `"run"`, and their
#' `"_label"` counterparts. Categorical annotations use numbered roles plus
#' shared `"bk_more"` and `"bk_more_label"` layers for additional groups.
#' Replacement data can add groups without changing the layer list. A role
#' can be empty when the model does not need it.
#'
#' # No model
#'
#' With no `model`, `gf_b()` reads the model the plot implies -- the same
#' decision [gf_model()] uses. The marks read that model's prediction grid,
#' after position scales transform values and remove rows outside hard limits.
#' If no model layer exists, the same model stat supplies the predictions.
#' Inference is refused on a faceted plot because its panels have different fits.
#' Position scales and `gf_lims()` can precede or follow the annotation. For an
#' inferred model on transformed scales, `run` and `run_x` use transformed units.
#' [geom_b()] provides the same annotation with ggplot2 syntax.
#'
#' # Placement
#'
#' Every mark is placed from the model's coefficients and from level indices,
#' never from a drawn point's position, so jitter never moves an arrow.
#'
#' `show_b0 = TRUE` (the default) expands the PREDICTOR's axis to include 0 on
#' a continuous model, because b0 is the prediction where the predictor is 0
#' and a picture of it that does not show that point is not a picture of b0.
#' Usually that is x; on a plot that puts the outcome on x it is y, and the
#' expansion follows the predictor rather than the letter. Calling `gf_lims()`
#' on that axis afterward overrides the expansion and can push the b0 dot off
#' the page.
#'
#' @param object A plot created with the `ggformula` package.
#' @param model The model to annotate: a fit from [`lm()`] or [`aov()`], with
#'   one predictor at most, and that predictor spelled the way the plot's own
#'   axis spells it -- `log(Height)` and `Height` are the same column but not
#'   the same axis, and b1 is a rise per unit of whichever one the model was
#'   fit on. It needs an intercept, because every mark here is measured from
#'   b0; a categorical predictor needs treatment coding, because every arrow is
#'   drawn as one group's difference from the reference group and no other
#'   coding's coefficients are that. Each of those is refused rather than
#'   drawn, because each would otherwise produce a picture that looks right. A
#'   formula is refused too -- `gf_b()`'s whole output is a set of labeled
#'   numbers, and there is no fit to read them from. May be given positionally
#'   or as `model =`. Omitted, the model the plot implies is fit and annotated
#'   instead.
#'
#'   Coefficient marks require Cartesian coordinates. Reversed position scales,
#'   `coord_cartesian(reverse = )`, and `coord_flip()` are supported; polar and
#'   other non-Cartesian coordinates are refused because rise, run, and label
#'   sides lack a linear coefficient interpretation there.
#' @param color,label_color The arrows/lines and the label text. `colour` and
#'   `label_colour` are accepted too. Each is a single value, not a mapping --
#'   every mark is one row computed from the coefficients, so there are no
#'   rows of data to map an aesthetic over; `color = ~variable` is refused.
#' @param label_size,arrow_linewidth,b0_linewidth,b0_size Sizes for the labels,
#'   the arrows, the b0 line and the b0 dot.
#' @param show_b0 Draw the `b0` line/dot and its label, and expand the
#'   PREDICTOR's axis to include 0 on a continuous model -- x on most plots, y
#'   on one that puts the outcome on x. `TRUE` by default; a later `gf_lims()`
#'   on that axis overrides the expansion and can push the b0 dot off the
#'   page.
#' @param run,run_x The run a continuous model's rise is measured over, and
#'   the x position the triangle starts at. Both chosen from the data when
#'   left `NULL`. Naming `run` on a categorical model is warned about and
#'   ignored -- its coefficients are group differences, not a rate.
#' @param b0_alpha The transparency of the categorical b0 reference line.
#' @param arrow_nudge A categorical arrow's x position in level units (1 = one
#'   group apart).
#' @param label_nudge The predictor-axis clearance for coefficient labels. It
#'   uses level units for a categorical predictor and adjusts the physical gap
#'   beside the b0 and rise marks for a continuous predictor. Not used on the
#'   empty model, whose one axis is a count rather than a predictor and whose b0
#'   label is placed at the panel's edge instead.
#' @param gformula Not used. `gf_b()` annotates a model, not an aesthetic
#'   formula; a model given positionally lands here and is moved to `model`.
#' @param data Not used. The marks are placed from the model's own
#'   coefficients and data.
#' @param ... Not used. Every mark states its own geom and params; set
#'   appearance with `color`, `label_color`, `label_size`, `arrow_linewidth`,
#'   `b0_linewidth`, `b0_size`, `b0_alpha`. Anything else here (`alpha`,
#'   `linetype`, ...) is warned about and dropped, because the marks are
#'   heterogeneous geoms with no single params bag to receive it.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom,stat,position Not set by the caller. Every mark states its own
#'   geom.
#' @param show.legend Not used. The marks are annotations and never
#'   contribute to a legend; a non-default value is warned about and dropped.
#' @param show.help Print the function's own help instead of drawing.
#' @param inherit Not set by the caller. Every mark states its own aesthetics.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with the model's coefficients annotated on it.
#'
#' @seealso [gf_model()] draws the fit itself.
#'
#' @export
#' @examples
#' # continuous: b1 as a rise-over-run triangle, b0 where the line meets x = 0
#' height_model <- lm(Thumb ~ Height, data = Fingers)
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>% gf_b(height_model)
#'
#' # the slope per one unit
#' gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(height_model, run = 1)
#'
#' # an explicit run labels the rise "10 x b1"
#' gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(height_model, run = 10)
#'
#' # categorical: b0 is the reference group's mean, each b_k is an arrow to group k
#' tip_model <- lm(Tip ~ Condition, data = TipExperiment)
#' gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1) %>% gf_b(tip_model)
#'
#' # no model: the model the plot implies, on the values the plot drew
#' set.seed(1)
#' gf_jitter(shuffle(Height) ~ Sex, data = Fingers, width = .1) %>%
#'   gf_model() %>%
#'   gf_b()
#'
#' # gf_coef() is the same function under the name coef() readers look for
#' flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_coef(flipper_model)
gf_b <- gf_b_layer_factory("gf_b")

#' @rdname gf_b
#' @description
#' `gf_coef()` is a fully supported alias of `gf_b()`. The package already
#' exports `b()`, `b0()`, `b1()` as its vocabulary for coefficients, and a
#' reader who knows [`stats::coef()`] will look for a plot-side counterpart
#' under that name.
#' @export
#
# Generated from the same recipe, not forwarded and not bound to `gf_b`'s
# closure. Its own name and caller frame therefore survive unchanged.
gf_coef <- gf_b_layer_factory("gf_coef")
