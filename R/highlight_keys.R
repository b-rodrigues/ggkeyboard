#' Highlight keys
#'
#' Draw attention to keys in a \code{ggkeyboard}.
#'
#' @param ggkeyboard An input keyboard from \code{\link{ggkeyboard}}.
#' @param keys Keys to highlight. Key names are available from the \code{key} column of the data passed to \code{ggkeyboard}.
#' @param colour Highlight outline colour.
#' @param fill Highlight fill colour.
#' @param size Highlight outline signs.
#' @param ... Additional options, passed to \code{geom_rect}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggkeyboard(tkl) %>%
#'   highlight_keys(c("Alt Left", "Shift Left", "M"))
#' }
highlight_keys <- function(ggkeyboard, keys, colour = "yellow", fill = NA, size = 1, ...) {
  keyboard <- purrr::map(ggkeyboard$layers, "data") %>%
    purrr::keep(~ "key" %in% names(.x)) %>%
    dplyr::bind_rows()

  key_data <- keyboard %>%
    dplyr::filter(key %in% !!keys) %>%
    dplyr::distinct(key, x_start, x_end, y_start, y_end)

  ggkeyboard +
    ggplot2::geom_rect(
               data = key_data,
               ggplot2::aes(
                          xmin = x_start, xmax = x_end,
                          ymin = y_start, ymax = y_end
                        ),
               colour = colour,
               fill = fill,
               size = size,
               ...
             )
}

#' Keyboard Measurements
#'
#' Measurement options for \code{\link{ggkeyboard}}.
#'
#' There are the following options:
#' * key_height: Height of keys.
#' * key_width: Base width of keys.
#' * height_gap: Height gap between rows of keys.
#' * width_gap: Width gap between keys in the same row.
#' * segment_size: Size of segments used to draw arrows.
#' * arrow_size; Size of arrow head.
#'
#' @param name Measurement options name.
#'
#' @export
keyboard_measurements <- function(name = "default") {
  name <- match.arg(name)

  switch(name,
         default = c(
           key_height = 15 / 15.5,
           key_width = 1,
           height_gap = 2 / 15.5,
           width_gap = 2 / 15.5,
           segment_size = 0.25,
           arrow_size = 0.03
         )
         )
}
