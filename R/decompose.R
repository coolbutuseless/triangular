



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
#' @param polygons_df polygon data.frame with 'subgroup' indicating primary/hole
#'        hierarchy
#'
#' @import RTriangle
#' @import polyclip
#' @importFrom utils head
#' @importFrom stats aggregate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Polygon with a hole
#' polygons_df <- df <- data.frame(
#'   x        = c(4, 8, 8, 4,   6, 7, 7, 6),
#'   y        = c(4, 4, 8, 8,   6, 6, 7, 7),
#'   group    = c(1, 1, 1, 1,   1, 1, 1, 1),
#'   subgroup = c(1, 1, 1, 1,   2, 2, 2, 2)
#' )
#' triangular::decompose(polygons_df)
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decompose <- function(polygons_df) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Polygons must be supplied in a data.frame with group/subgroup designations
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.data.frame(polygons_df))
  stopifnot('group'    %in% names(polygons_df))
  stopifnot('subgroup' %in% names(polygons_df))
  stopifnot('x'        %in% names(polygons_df))
  stopifnot('y'        %in% names(polygons_df))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # We need to create an S matrix for each group/subgroup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygons_list <- split(polygons_df, interaction(polygons_df$subgroup, polygons_df$group))

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
  ps <- RTriangle::pslg(P = as.matrix(polygons_df[,c('x', 'y')]), S = S)
  tt <- RTriangle::triangulate(ps)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Result from RTriangle::triangulate():
  #
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
  triangles_df <- as.data.frame(tt$P)    # the original points
  idx <- as.vector(t(tt$T))              # indices (in groups of 3) indicating vertices of tris
  colnames(triangles_df) <- c('x', 'y')  # Set actual colnames
  triangles_df <- triangles_df[idx,]     # Replicate the vertices according to 'tt$T'
  N <- nrow(triangles_df)/3              # Assign an index to each triangle
  triangles_df$vert <- idx
  triangles_df$idx  <- rep(seq_len(N), each = 3)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the centroid of each triangle
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  centroids <- aggregate(triangles_df[,c('x', 'y')], by=list(triangles_df$idx), FUN=mean)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use polyclip to do testing of point in polygon.
  # polyclip::pointinpolygon() only tests a set of points against a single
  # polygon, so have to calculations over all polygons and then accumulate
  # the result with a `Reduce()` call
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P        <- list(x = centroids$x, y = centroids$y)
  pip      <- lapply(polygons_list, function(A) polyclip::pointinpolygon(P, A))
  crosses  <- Reduce(`+`, pip)
  interior <- crosses %% 2 == 1
  inside   <- data.frame(idx = seq_along(crosses), crosses, interior)
  triangles_df <- merge(triangles_df, inside)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  #  - the raw Rtriangle output
  #  - data.frame of triangles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list(
    tt            = tt,            # Full result of Rtriangle::triangulate()
    centroids     = centroids,     # triangle centroids
    polygons_list = polygons_list, # list of original polygons. each element = 1 polygon
    triangles_df  = triangles_df   # Traingles data.frame
  )
}





