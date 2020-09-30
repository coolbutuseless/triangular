# triangular 0.1.5 2020-10-01

* Only use `polyclip::polysimplify()` if there are duplicated vertices, 
  otherwise, all test examples work with `RTriangle::triangulate()` without
  any other processing.

# triangular 0.1.4 2020-09-30

* new `accetpable` column in `triangles_df` returned by `decompose()`.
    * `acceptable = interior & !too_thin`
    * where `too_thin` indicates triangles in which the centroid lies on the 
      boundary, which makes them impossibly thin

# triangular 0.1.3 2020-09-28

* Combine `polyclip::polysimplify()` with `RTriangle::triangulate()` to now 
  cope with:
    * holes in polygons
    * self intersecting polygons
    * polygons with duplicated vertices

# triangular 0.1.2 2020-09-27

* Refactor: Now use `polyclip` package for point-in-polygon calculations. 
  Removed bespoke point-in-polygon R functions from this package.

# triangular 0.1.1 2020-09-27

* Refactor: abstracting 'point-in-polygons'

# triangular 0.1.0 2020-09-26

* Initial release
