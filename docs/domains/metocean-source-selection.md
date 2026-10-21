# Metocean source selection

## Summary

`digitalmodel.gis.metocean_sources` ranks a caller-supplied catalogue by shortest
WGS 84 surface distance to a target. Only sources declaring all requested
variables qualify. Each result retains its source metadata and reference.
This is source discovery for issue #333; site-specific wave and current
extrapolation remains `NOT_EVALUATED`.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Added sourced distance ranking as the first step toward metocean extrapolation. |

## Use

```python
from digitalmodel.gis.core.geometry import GeoPoint
from digitalmodel.gis.metocean_sources import MetoceanSource, nearest_sources

# Synthetic equatorial positions for illustration, not measured metocean data.
catalogue = [
    MetoceanSource(
        source_id="example",
        longitude=2.0,
        latitude=0.0,
        variables=frozenset({"hs", "tp"}),
        reference="Synthetic example metadata",
    )
]
matches = nearest_sources(
    GeoPoint(x=0.0, y=0.0), catalogue, required_variables={"hs", "tp"}
)
candidate = matches[0]
assert candidate.extrapolation_status == "NOT_EVALUATED"
```

Catalogue identifiers must be unique, including entries excluded by the variable
filter. Variable identifiers use exact matching with no inferred aliases or
units. The source and result records are immutable. The target uses longitude
in `x` and latitude in `y`; elevation is ignored. Coordinates must be finite
and within the geographic bounds described in RFC 7946 (2016), sections 4 and
5.3. Projected coordinates require transformation before selection.

The optional positive integer `limit` defaults to one result. A request larger
than the qualifying catalogue returns all qualifying entries. Empty catalogues
and catalogues with no qualifying entries return an empty list. Exact distance
ties use source identifier order, independently of catalogue order.

## Distance basis

The inverse geodesic maps two longitude/latitude pairs to the shortest surface
distance `s12` in metres. The implementation calls `pyproj.Geod.inv` on the
PROJ WGS84 ellipsoid. The governing distance integral and inverse solution are
described in [Algorithms for geodesics (2013), sections 2 and 4, equation 7](https://arxiv.org/pdf/1109.4448)
(open preprint revised in 2012). [PROJ Geodesic calculations](https://proj.org/en/stable/geodesic.html)
documents the implemented algorithm. The [PROJ built-in ellipsoid table](https://proj.org/en/stable/usage/ellipsoids.html)
supplies WGS84 parameters; no Earth radius or flattening is guessed in this module.

Results are sorted by `(s12, source_id)` after filtering for set containment of
requested variables. These are catalogue-selection policies, not physical
equations. No distance cutoff, correction coefficient, or engineering acceptance
limit is assumed. Coordinate order, units and geographic bounds follow
[RFC 7946 (2016), sections 3.1.1, 4 and 5.3](https://www.rfc-editor.org/rfc/rfc7946).

## Test basis

The offline tests reproduce the short-line and nearly antipodal inverse examples
in the paper's Tables 3 and 4–6. Independent equatorial checks use
`s = a * delta_lambda`, with the WGS84 equatorial radius from the PROJ table and
longitude difference converted to radians. The test docstrings write out the
inputs, calculations and numerical comparison budget. The antimeridian case
uses the shorter equatorial arc. Additional tests cover poles, coincident
positions, ordering, exact ties, filters, empty results, metadata preservation,
duplicate identifiers and invalid input.

## Limits and later slices

Geographic proximity does not establish metocean representativeness. A catalogue
reference is retained, but source access, licensing, temporal coverage, data
quality, units and variable declarations are not verified. Variable filtering
does not compare observation records. No live catalogue or observations are
downloaded.

Bathymetry, terrain barriers, sheltering, fetch, tides and wave/current transfer
are `NOT_EVALUATED` because the required spatial and metocean inputs and a
qualified transfer method are absent. The status remains unevaluated even for
a colocated source. No wave height, period, direction or current value is copied
to the target.

Later slices can add public catalogue adapters, GIS assessment of bathymetry and
exposure, qualified transfer methods, paired-location validation, and mapped
source-to-target outputs. Each requires its own inputs, cited method and tests.
