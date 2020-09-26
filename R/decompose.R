



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create the S matrix for RTriangle::pslg()
#'
#' pslg = Planar Straight Line Graph object. A collection of vertices and segments.
#'
#' The 'S' matrix is: A 2-column matrix of segments in which each row is a
#' segment. Segments are edges whose endpoints are vertices in the PSLG, and
#' whose presence in any mesh generated from the PSLG is enforced. Each
#' segment refers to the indices in V of the endpoints of the segment. By
#' default the segments are not specified (NA), in which case the convex
#' hull of the vertices are taken to be the segments. Any vertices outside
#' the region enclosed by the segments are eaten away by the triangulation
#' algorithm. If the segments do not enclose a region the whole triangulation
#' may be eaten away.
#'
#' @param polygon_df data.frame containing a single polygon
#' @param offset an index counter to ensure different subgroup polygons will
#'        have different vertex indices.
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decompose_single <- function(polygon_df, offset) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the indices (v1, v2) of the start and end point of each segment.
  # Ensure that 'v2' wraps around to point at the first index.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  v1 <- seq_len(nrow(polygon_df))      + offset
  v2 <- c(seq(2, nrow(polygon_df)), 1) + offset


  S  <-  matrix(c(v1, v2), ncol = 2, nrow = nrow(polygon_df))

  S
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Decompose complex polygon into a set of simple polygons
#'
#' Preserve holes. Even-odd inside test
#'
#' @param polys_df polygon data.frame with 'subgroup' indicating primary/hole
#'        hierarchy
#'
#' @import RTriangle
#' @importFrom utils head
#' @importFrom stats aggregate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Polygon with a hole
#' polys_df <- df <- data.frame(
#'   x        = c(4, 8, 8, 4,   6, 7, 7, 6),
#'   y        = c(4, 4, 8, 8,   6, 6, 7, 7),
#'   group    = c(1, 1, 1, 1,   1, 1, 1, 1),
#'   subgroup = c(1, 1, 1, 1,   2, 2, 2, 2)
#' )
#' triangular::decompose(polys_df)
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decompose <- function(polys_df) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Polygons must be supplied in a data.frame with group/subgroup designations
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.data.frame(polys_df))
  stopifnot('group'    %in% names(polys_df))
  stopifnot('subgroup' %in% names(polys_df))
  stopifnot('x'        %in% names(polys_df))
  stopifnot('y'        %in% names(polys_df))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # We need to create an S matrix for each group/subgroup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygons_list <- split(polys_df, interaction(polys_df$subgroup, polys_df$group))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Work out the number of vertices within each group. This will be an
  # offset when calling 'decompose_single'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lens    <- vapply(polygons_list, nrow, integer(1))
  lens    <- c(0, head(lens, -1))
  offsets <- cumsum(lens)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an S matrix for each group/subgroup and combine into single matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  S <- lapply(seq_along(polygons_list), function(i) {
    decompose_single(polygons_list[[i]], offsets[i])
  })
  S <- do.call(rbind, S)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use RTriangle to triangulate the groups/subgroups
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ps <- RTriangle::pslg(P = as.matrix(polys_df[,c('x', 'y')]), S = S)
  tt <- RTriangle::triangulate(ps)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # T	  Triangulation specified as 3 column matrix in which each row
  #     contains indices in P of vertices.
  # E	  Set of edges in the triangulation.
  # EB	Boundary markers of edges. For each edge this is 1 if the point is
  #     on a boundary of the triangulation and 0 otherwise.
  # VP	The points of the Voronoi tessalation as a 2-column matrix
  # VE	Set of edges of the Voronoi tessalation. An index of -1 indicates an
  #     infinite ray.
  # VN	Directions of infinite rays of Voroni tessalation as a 2-column
  #     matrix with the same number of rows as VP.
  # VA	Matrix of attributes associated with the polygons of the Voronoi
  #     tessalation.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  plot_df <- as.data.frame(tt$P)    # the original points
  idx <- as.vector(t(tt$T))         # indices (in groups of 3) indicating vertices of tris
  colnames(plot_df) <- c('x', 'y')  # Set actual colnames
  plot_df <- plot_df[idx,]          # Replicate the vertices according to 'tt$T'
  N <- nrow(plot_df)/3              # Assign an index to each triangle
  plot_df$vert <- idx
  plot_df$idx  <- rep(seq_len(N), each = 3)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the centroid of each triangle
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  centroids <- aggregate(plot_df[,c('x', 'y')], by=list(plot_df$idx), FUN=mean)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Do a test to count how often the point crosses one of the original
  # polygon segments.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  crosses <- vapply(seq_len(nrow(centroids)), function(ci) {
    point_in_polygons(centroids$x[ci], centroids$y[ci], polygons_list)
  }, FUN.VALUE = integer(1))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign 'interior' as TRUE/FALSE depedning on whether the number of
  # segment crossing is odd or even
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inside <- data.frame(idx = seq_along(crosses), crosses)
  inside <- transform(inside, interior = crosses %% 2 != 0)
  plot_df <- merge(plot_df, inside)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  #  - the raw Rtriangle output
  #  - data.frame of triangles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list(
    tt      = tt,     # The full result of Rtriangle::triangulate()
    plot_df = plot_df # The polys_df
  )
}






