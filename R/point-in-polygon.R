

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine number of segment crossings from (x, y) to infinity
#'
#' @param x,y coordinates
#' @param poly data.frame with x and y coordinates of all vertices
#'
#' @return count of segment crossings
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
point_in_polygon <- function(x, y, poly) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract just the 'x', and 'y' values from the 'poly' data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  px <- poly$x
  py <- poly$y

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ensure polygon is closed
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  px <- c(px, px[1])
  py <- c(py, py[1])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is the setup for even/odd segment crossings
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n <- length(px)
  i <- 0
  j <- n
  crosses <- 0L

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Count crossings
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq(n)) {
    if ( ((py[i] >= y) != (py[j] >= y)) &&
         (x <= (px[i] + (px[j] - px[i]) * (y - py[i]) /
          (py[j] - py[i]))) ) {
      crosses <- crosses + 1L
    }
    j <- i
  }

  crosses
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine number of segment crossings from (x, y) within all polygons
#'
#' @param x,y coordinates of point to test
#' @param polygons_list list of polygon data.frames
#'
#' @return sum of all crossings over all polygons
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
point_in_polygons <- function(x, y, polygons_list) {
  stopifnot(is.list(polygons_list) && !is.data.frame(polygons_list))
  crosses <- vapply(polygons_list, function(poly) {
    point_in_polygon(x, y, poly)
  }, integer(1))

  sum(unlist(crosses))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine number of segment crossings from (x, y) within all polygons
#'
#' @param x,y coordinates of all points to test
#' @param polygons_list list of polygon data.frames
#'
#' @return sum of all crossings over all polygons for each point
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
points_in_polygons <- function(x, y, polygons_list) {
  vapply(seq_along(x), function(ci) {
    point_in_polygons(x[ci], y[ci], polygons_list)
  }, FUN.VALUE = integer(1))
}






if (FALSE) {
  library(ggplot2)

  poly <- data.frame(
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1)
  )




  N <- 1000
  xs <- runif(N, -0.5, 1.5)
  ys <- runif(N, -0.5, 1.5)

  cross <- vapply(seq_along(xs), function(i) {
    point_in_polygon(xs[i], ys[i], poly)
  }, integer(1))

  points <- data.frame(xs, ys, cross)

  ggplot(poly) +
    geom_polygon(aes(x, y), fill = NA, colour = 'black') +
    geom_point(data = points, mapping = aes(xs, ys, colour = as.factor(cross))) +
    theme_bw() +
    coord_fixed()


}



if (FALSE) {

  polys <- list(
    data.frame(
      x = c(0, 1, 1, 0),
      y = c(0, 0, 1, 1)
    ),
    data.frame(
      x = c(0.5, 0.75, 0.75, 0.5),
      y = c(0.5, 0.5, 0.75, 0.75)
    )
  )

  N <- 1000
  xs <- runif(N, -0.5, 1.5)
  ys <- runif(N, -0.5, 1.5)


  x <- -0.25
  y <- 0.55
  point_in_polygons(x, y, polys)



  cross <- vapply(seq_along(xs), function(i) {
    point_in_polygons(xs[i], ys[i], polys)
  }, integer(1))

  points <- data.frame(xs, ys, cross)

  ggplot() +
    geom_polygon(data = polys[[1]], aes(x, y), fill = NA, colour = 'black') +
    geom_polygon(data = polys[[2]], aes(x, y), fill = NA, colour = 'red') +
    # geom_point(data = points, mapping = aes(xs, ys, colour = as.factor(cross))) +
    geom_point(data = points, mapping = aes(xs, ys, colour = (cross %% 2 == 0))) +
    theme_bw() +
    coord_fixed()


}

















