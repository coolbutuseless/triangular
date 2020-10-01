

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Assign unique vertex indices to all points
#'
#' This will be used later to de-dupe the input data for RTriangle::triangulate()
#'
#' @param polygons_df data.frame containing multiple polygons
#'        distinguished by group and subgroup
#'
#' @return inpput wit new 'vidx' and 'dupe' columns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assign_unique_vertex_indices <- function(polygons_df) {

  polygons_df$dupe <- duplicated(polygons_df[, c('x', 'y')])

  polygons_df$vidx <- as.integer(as.factor(with(polygons_df, interaction(x, y))))
  polygons_df$vidx <- match(polygons_df$vidx, unique(polygons_df$vidx))

  polygons_df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create the S matrix for RTriangle::pslg() for a single polygon
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
#' @param polygon_df data.frame containing a single polygon. Must have 'vidx' column
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_S <- function(polygon_df) {
  v1 <- polygon_df$vidx
  v2 <- c(polygon_df$vidx[-1], polygon_df$vidx[1])

  matrix(c(v1, v2), ncol = 2)
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
  # Deduplicate the vertices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygons_df_2 <- assign_unique_vertex_indices(polygons_df)
  polygons_list <- split(polygons_df_2, interaction(polygons_df_2$subgroup, polygons_df_2$group))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an S matrix for each group/subgroup and combine into single matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  S <- lapply(seq_along(polygons_list), function(i) {
    create_S(polygons_list[[i]])
  })
  S <- do.call(rbind, S)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only want the unique vertices passed to RTriangle
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  unique_verts <- as.matrix(polygons_df_2[!polygons_df_2$dupe, c('x', 'y')])


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use RTriangle to triangulate the groups/subgroups
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ps <- RTriangle::pslg(P = unique_verts, S = S)
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if centroid lies on the boundary pointinpolygon() return -1
  # and the only way a triangle centroid can lie on the boundary: the triangle
  # is so thin that numerical error lets it (a) exist as a triangle, (b) but
  # the cnetroid of the triangle is on the boundary. delete these
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  too_thin <- do.call(pmin, pip) == -1


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ncrosses <- Reduce(`+`, pip)
  interior <- ncrosses %% 2 == 1
  inside   <- data.frame(idx = seq_along(ncrosses), ncrosses, interior, too_thin, acceptable = interior & !too_thin)
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





