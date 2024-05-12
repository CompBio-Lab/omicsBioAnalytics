#' @export
#' @rdname empty_plot
empty_plot = function(label){
  data.frame(x = 0.5, y = 0.5, label = label) %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = label)) +
    xlab("") +
    ylab("") +
    theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())
}

#' @export
#' @rdname zip_nPure
zip_nPure = function (.x, .fields = NULL, .simplify = FALSE) {
  if (length(.x) == 0)
    return(list())
  if (is.null(.fields)) {
    if (is.null(names(.x[[1]]))) {
      .fields <- seq_along(.x[[1]])
    }
    else {
      .fields <- stats::setNames(names(.x[[1]]), names(.x[[1]]))
    }
  }
  else {
    if (is.character(.fields) && is.null(names(.fields))) {
      names(.fields) <- .fields
    }
  }
  out <- lapply(.fields, function(i) lapply(.x, .subset2, i))
  if (.simplify)
    out <- lapply(out, simplify_if_possible)
  out
}
