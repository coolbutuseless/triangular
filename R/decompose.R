

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Simplify a single polygon
#'
#' @param polygon_df data.frame containing a single polygon (x, y)
#'
#' @import polyclip
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
simplify_polygon <- function(polygon_df) {

  sp <- polyclip::polysimplify(polygon_df)
  sp <- lapply(seq_along(sp), function(ii) {
    res          <- as.data.frame(sp[[ii]])
    res$group    <- polygon_df$group[1]
    res$subgroup <- polygon_df$subgroup[1] + ii/10000
    res
  })
  sp <- do.call(rbind, sp)

  sp
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Simplify a polygon made up of groups and subgroups
#'
#' @param polygons_df data.frame containing multiple polygons
#'        distinguished by group and subgroup
#'
#' @import polyclip
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
simplify_polygons <- function(polygons_df) {

  polygons_list <- split(polygons_df, polygons_df$subgroup)

  sp <- lapply(seq_along(polygons_list), function(ii) {
    simplify_polygon(polygons_list[[ii]])
  })

  sp <- do.call(rbind, sp)

  sp
}



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
#' Inner function to decompose a single polygon 'group'
#'
#' @inheritParams decompose
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decompose_single <- function(polygons_df) {
  stopifnot(length(unique(polygons_df$group)) == 1)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If there are any duplicate vertices, then simplify the polygon first
  # with `polyclip`.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (anyDuplicated(polygons_df[,c('x', 'y')])) {
    polygons_df_1 <- simplify_polygons(polygons_df)
    polygons_df_2 <- assign_unique_vertex_indices(polygons_df_1)
  } else {
    polygons_df_2 <- assign_unique_vertex_indices(polygons_df)
  }
  polygons_list <- split(polygons_df_2, polygons_df_2$subgroup)


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
  # Properties to save/restore.
  # This assumes that the properties across a polygon 'group' are all
  # identical
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  property_cols <- setdiff(colnames(polygons_df), c('x', 'y', 'group', 'subgroup'))
  properties_df <- polygons_df[1, property_cols, drop = FALSE]


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
  P   <- list(x = centroids$x, y = centroids$y)
  pip <- lapply(polygons_list, function(A) polyclip::pointinpolygon(P, A))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if centroid lies on the boundary pointinpolygon() return -1
  # and the only way a triangle centroid can lie on the boundary: the triangle
  # is so thin that numerical error lets it (a) exist as a triangle, (b) but
  # the cnetroid of the triangle is on the boundary. delete these
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  too_thin <- do.call(pmin, pip) == -1


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prep dataframe for return. Filter out triangles which aren't acceptable
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ncrosses <- Reduce(`+`, pip)
  interior <- ncrosses %% 2 == 1

  inside   <- data.frame(idx = seq_along(interior), acceptable = interior & !too_thin)
  triangles_df <- merge(triangles_df, inside)
  triangles_df <- subset(triangles_df, triangles_df$acceptable)

  triangles_df$acceptable <- NULL
  triangles_df$vert       <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Join back in the properties of this polygon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  triangles_df <- cbind(triangles_df, properties_df)


  triangles_df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Decompose complex polygons into triangles
#'
#' This function wraps \code{RTriangle::triangulate()} to make it easier to
#' call for my use case, and returns results appropriate for plotting in
#' \code{ggplot2}.
#'
#' By combining \code{polyclip} and \code{RTriangle} packages, this package
#' will successfully decompose into triangles the following polygon types:
#'
#' \itemize{
#' \item{Simple polygons}
#' \item{Polyons with holes}
#' \item{Multiple polygons with holes}
#' \item{Polygons with self-intersection}
#' \item{Polygons with duplicated vertices}
#' }
#'
#' @param polygons_df polygon data.frame with \code{x,y} coordinates,  'group'
#'        column denoting coordinates which belong to the same group,
#'        and 'subgroup' indicating holes within that polygon.  It is assumed
#'        that any other column values are consistent across the entire group
#'        e.g. 'fill'
#'
#' @return data.frame with \code{x}, \code{y} coordinates of the vertices of the
#'         resulting triangles. Also includes \code{idx} numbering the triangles.
#'
#' @import RTriangle
#' @import polyclip
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
#' triangles_df <- triangular::decompose(polygons_df)
#' ggplot(triangles_df) +
#'     geom_polygon(aes(x, y, fill = as.factor(idx)))
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
  # polyclip::polysimplify() can be touchy on 'x', and 'y' being plain
  # numeric values.  So ensure conversion here.  'ggplot2' also sometimes
  # add some weird classes to numeric columns in data.frames for plotting,
  # and this will drop the classes which confuse polyclip
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygons_df$x <- as.numeric(polygons_df$x)
  polygons_df$y <- as.numeric(polygons_df$y)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each polygon group, decompose into triangles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygon_groups <- split(polygons_df, polygons_df$group)
  triangles_df_list <- lapply(
    polygon_groups,
    decompose_single
  )
  triangles_df <- do.call(rbind, triangles_df_list)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Final triangle numbering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  N <- nrow(triangles_df)/3              # Assign an index to each triangle
  triangles_df$idx  <- rep(seq_len(N), each = 3)

  triangles_df
}


