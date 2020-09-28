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
