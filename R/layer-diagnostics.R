# Only line-width aesthetics use the retired size spelling. Point and text
# geoms, and marker-size arguments on annotations, retain their own size.
normalize_linewidth <- function(params, geom, fn, warn_size = TRUE) {
  if (is.null(params$size)) return(params)
  defaults <- tryCatch(ggplot2::get_geom_defaults(geom), error = function(cnd) NULL)
  if (!"linewidth" %in% names(defaults) || "size" %in% names(defaults)) return(params)

  if (warn_size) {
    warn(
      glue("`size` is now `linewidth` in `{fn}()`; use `linewidth` instead."),
      class = "coursekata_linewidth", call = call2(fn)
    )
  }
  params$linewidth <- params$linewidth %||% params$size
  params$size <- NULL
  params
}

public_layer_constructor <- function(layer, fn) {
  layer$constructor <- call2(fn)
  layer
}
