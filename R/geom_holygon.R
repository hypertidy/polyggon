#' Holygon, a filled path that can include holes.
#'
#' The source for this was adapted from http://qiita.com/kohske/items/9272e29a75d32416ff5e
#' @seealso
#' \code{\link{geom_polygon}} for the implementation on \code{\link[grid]{polygonGrob}},
#' \code{\link{geom_map}} for a convenient way to tie the values and coordinates together,
#'  \code{\link{geom_path}} for an unfilled polygon,
#'  \code{\link{geom_ribbon}} for a polygon anchored on the x-axis
#' @export
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @importFrom grid pathGrob
#' @importFrom ggplot2 zeroGrob
#' @examples
#' # When using geom_holygon, you will typically need two data frames:
#' # one contains the coordinates of each polygon (positions),  and the
#' # other the values associated with each polygon (values).  An id
#' # variable links the two together.
#' # Normally this would not be created manually, but by using \code{\link{fortify}}
#' # to generate it from the Spatial classes in the `sp` package.
#'
#' positions = data.frame(x = c(0, 0, 46, 46, 0, 7, 13, 13, 7, 7, 18, 24,
#' 24, 18, 18, 31, 37, 37, 31, 31, 18.4, 18.4, 18.6, 18.8, 18.8,
#' 18.6, 18.4, 31, 31, 37, 37, 31, 0, 21, 31, 37, 46, 0, 18, 18,
#' 24, 24, 18, 18.4, 18.6, 18.8, 18.8, 18.6, 18.4, 18.4),
#' y = c(0, 19, 19, 0, 0, 6, 6, 13, 13, 6, 1, 1, 12, 12, 1, 4, 4, 11, 11,
#' 4, 6.89999999999999, 7.49999999999999, 7.69999999999999, 7.49999999999999,
#' 6.89999999999999, 6.69999999999999, 6.89999999999999, 27, 34,
#' 34, 24, 27, 19, 32, 27, 24, 19, 19, 1, 12, 12, 1, 1, 6.89999999999999,
#' 6.69999999999999, 6.89999999999999, 7.49999999999999, 7.69999999999999,
#'  7.49999999999999, 6.89999999999999),
#'  id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
#'  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
#'  2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
#'  group = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L,
#'  4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 7L,
#'  7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L, 9L, 9L))
#'
#' values <- data.frame(
#'   id = unique(positions$id),
#'   value = c(2, 5.4, 3)
#' )
#'
#' #  manually merge the two together
#' datapoly <- merge(values, positions, by = c("id"))
#'
#' # the entire house
#' (house <- ggplot(datapoly, aes(x = x, y = y)) + geom_holygon(aes(fill = value, group = group)))
#'
#' # just the front wall (and chimney), with its three parts, the first of which has three holes
#' (wall <- ggplot(datapoly[datapoly$id == 1, ], aes(x = x, y = y)) + geom_holygon(aes(fill = id, group = group)))
geom_holygon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, rule = "winding", ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHolygon,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm , rule = rule, ...))
}



#' @rdname geom_holygon
#' @format NULL
#' @usage NULL
#' @export
GeomHolygon <- ggproto(
  "GeomHolygon",
  GeomPolygon,
  extra_params = c("na.rm", "rule"),
  draw_panel = function(data, scales, coordinates, rule) {
    n <- nrow(data)
    if (n == 1)
      return(zeroGrob())

    munched <- coord_munch(coordinates, data, scales)
    munched <- munched[order(munched$group), ]
    ## function to be applied to get a pathGrob for each "region"
    object_munch <- function(xmunch) {
      first_idx <- !duplicated(xmunch$group)
      first_rows <- xmunch[first_idx, ]
      pathGrob(xmunch$x, xmunch$y, default.units = "native",
               id = xmunch$group, rule = rule,
               gp = gpar(col = first_rows$colour,
                         fill = alpha(first_rows$fill, first_rows$alpha),
                         lwd = first_rows$size * .pt,
                         lty = first_rows$linetype))
    }
    ggplot2:::ggname(
      "geom_holygon",
      do.call(grobTree, lapply(split(munched, munched$fill), object_munch))
    )
  }
)



