#' Share inherited observation ownership with a local annotation layer
#'
#' The visible source resolves its callback and mappings first. Its local
#' consumer gets the same raw rows and reuses matching positional evaluations;
#' genuinely different local mappings still evaluate against those rows. The
#' cache belongs to the build's cloned scale collection, so recursive builds
#' cannot replace an outer build's observations.
#' @noRd
b_share_source <- function(source, consumer) {
  key <- new.env(parent = emptyenv())
  source_parent <- source
  consumer_parent <- consumer
  source <- layer_with(source, compute_aesthetics = function(self, data, plot) {
    result <- ggplot2::ggproto_parent(source_parent, self)$compute_aesthetics(data, plot)
    frames <- attr(plot$scales, "coursekata_b_sources") %||% list()
    frames <- c(frames, list(list(key = key, data = data, mapping = self$computed_mapping,
                                  result = result)))
    attr(plot$scales, "coursekata_b_sources") <- frames
    result
  })
  consumer <- layer_with(consumer,
    data = data.frame(.b_placeholder = 1),
    compute_aesthetics = function(self, data, plot) {
      frames <- attr(plot$scales, "coursekata_b_sources") %||% list()
      hit <- which(vapply(frames, function(frame) identical(frame$key, key), logical(1)))
      frame <- frames[[utils::tail(hit, 1L)]]
      rows <- frame$data
      mapping <- self$computed_mapping
      shared <- intersect(c("x", "y", "weight"), intersect(names(mapping), names(frame$mapping)))
      for (axis in shared) {
        if (!identical(mapping[[axis]], frame$mapping[[axis]]) || !axis %in% names(frame$result)) next
        column <- utils::tail(make.unique(c(names(rows), paste0(".coursekata_b_shared_", axis))), 1L)
        rows[[column]] <- frame$result[[axis]]
        mapping[[axis]] <- new_quosure(sym(column), base_env())
      }
      view <- layer_with(self, computed_mapping = mapping)
      attr(view, "coursekata_b_source_mapping") <- self$computed_mapping
      ggplot2::ggproto_parent(consumer_parent, view)$compute_aesthetics(rows, plot)
    })
  list(source = source, consumer = consumer)
}
