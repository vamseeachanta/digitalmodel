# Layered slope-profile preflight example

This runnable example checks supplied SI geometry and soil properties with
`validate_profile`. Schema validation does not compute a factor of safety,
solve seepage, infer soil properties or establish design acceptance. All inputs
are synthetic and are supplied explicitly; no solver package is required.

## Revision history

| Revision | Date | Change and purpose |
| --- | --- | --- |
| 0 | 2026-10-26 | Add executable layered inputs, missing-datum rejection and coordinate-translation checks. |

## Input basis and units

The following synthetic input table is the source of every physical constant
in the snippets and their tests. Values are chosen for this example only and
are not measurements or design recommendations. Coordinates use `[x, elevation]`
in metres, with elevation positive upward; no unit conversion is performed.
The first layer starts at the ground surface. Each later layer starts at its
`top_boundary_m`; the last extends to the horizontal model bottom.

| Input | Supplied value and unit |
| --- | --- |
| Ground vertices | (0, 10), (10, 10), (20, 0), (40, 0) m |
| Lower layer top | (0, -2), (40, -2) m |
| Model bottom | -10 m |
| Piezometric line | (0, -1), (40, -1) m |
| Starting circle | Centre (10, 15) m; radius 20 m; schema input only |
| Water unit weight | 9.81 kN/m³, supplied illustrative value |
| Upper layer | Bulk 18 kN/m³; saturated 20 kN/m³; conductivity 1e-6 m/s |
| Upper layer strength | Undrained reference strength 5 kPa; gradient 1.5 kPa/m; reference elevation 10 m |
| Lower layer | Bulk 20 kN/m³; saturated 21 kN/m³; conductivity 1e-7 m/s |
| Lower layer strength | Drained cohesion 2 kPa; friction angle 30 degrees |
| Horizontal seismic coefficient | 0, dimensionless |
| Elevation translations | -100 m and +100 m; x coordinates and radii unchanged |

The upper layer uses the accepted overlapping fields `gamma_kn_m3` and
`Su_kpa`; validation returns their canonical names `unit_weight_kn_m3` and
`su_kpa`. The [schema guide](../domains/geotechnical/slope-stability.md#si-profile-schema)
describes the fields and the strength convention. In that convention the
relative depth is `max(0, reference_elevation_m - elevation_m)` and the
illustrative strength is reference strength plus gradient times relative depth.
These equations explain the datum; validation does not evaluate a strength
field. Independent subtraction and multiplication give the following checks:

| Elevation (m) | Depth below the 10 m datum (m) | Illustrative strength (kPa) |
| --- | --- | --- |
| 10 | 0 | 5 + 1.5 × 0 = 5 |
| 4 | 6 | 5 + 1.5 × 6 = 14 |
| -2 | 12 | 5 + 1.5 × 12 = 23 |

## Supplied profile

Run the Python blocks in order in one interpreter from the repository root,
with `PYTHONPATH=src`. The standard library and the source checkout suffice.
Each block contains executable assertions; the smoke tests execute these exact
blocks and refuse imports of optional solver packages.

```python
from copy import deepcopy

from digitalmodel.geotechnical.slope_profile import validate_profile

# Source of all physical constants: the synthetic input table above.
synthetic = {
    "units": "SI",
    "ground_surface_m": [[0, 10], [10, 10], [20, 0], [40, 0]],
    "bottom_elevation_m": -10,
    "water_table_m": [[0, -1], [40, -1]],
    "water_unit_weight_kn_m3": 9.81,
    "seismic_coefficient": 0,
    "circles": [{"x_m": 10, "y_m": 15, "radius_m": 20}],
    "layers": [
        {
            "gamma_kn_m3": 18,
            "saturated_unit_weight_kn_m3": 20,
            "Su_kpa": 5,
            "su_gradient_kpa_per_m": 1.5,
            "reference_elevation_m": 10,
            "permeability_m_s": 1e-6,
        },
        {
            "top_boundary_m": [[0, -2], [40, -2]],
            "unit_weight_kn_m3": 20,
            "saturated_unit_weight_kn_m3": 21,
            "c_kpa": 2,
            "phi_deg": 30,
            "permeability_m_s": 1e-7,
        },
    ],
}
before = deepcopy(synthetic)
checked = validate_profile(synthetic)
assert synthetic == before
assert checked is not synthetic

# Independent expected copy: canonical aliases and the implicit first-layer top.
expected = deepcopy(before)
upper = expected["layers"][0]
upper["unit_weight_kn_m3"] = upper.pop("gamma_kn_m3")
upper["su_kpa"] = upper.pop("Su_kpa")
upper["top_boundary_m"] = deepcopy(expected["ground_surface_m"])
assert checked == expected  # All supplied properties and coordinates survive.
```

## Rejected missing datum

A positive gradient needs an explicit finite `reference_elevation_m` in the
same elevation coordinate system as the geometry. An omitted datum cannot be
inferred from ground level or the model bottom.

```python
missing_datum = deepcopy(synthetic)
del missing_datum["layers"][0]["reference_elevation_m"]
missing_before = deepcopy(missing_datum)
try:
    validate_profile(missing_datum)
except ValueError as error:
    assert "positive strength gradient requires reference_elevation_m" in str(error)
else:
    raise AssertionError("missing positive-gradient datum was accepted")
assert missing_datum == missing_before
```

## Valid translated coordinates

Every geometric elevation, the piezometric elevation and the explicit strength
datum are shifted together by each offset in the input table. Layer properties,
x coordinates and circle radii retain their supplied values. For any elevation
`y` and offset `d`, `(reference + d) - (y + d) = reference - y`, so the
relative-depth inputs remain unchanged. No stability result is implied.

```python
def translate_elevations(data, offset):
    shifted = deepcopy(data)
    for key in ("ground_surface_m", "water_table_m"):
        shifted[key] = [[x, y + offset] for x, y in shifted[key]]
    shifted["bottom_elevation_m"] += offset
    for layer in shifted["layers"]:
        if "top_boundary_m" in layer:
            layer["top_boundary_m"] = [
                [x, y + offset] for x, y in layer["top_boundary_m"]
            ]
        if "reference_elevation_m" in layer:
            layer["reference_elevation_m"] += offset
    for circle in shifted["circles"]:
        circle["y_m"] += offset
    return shifted


translated_profiles = {}
for offset in (-100, 100):  # Source: synthetic input table.
    translated = translate_elevations(synthetic, offset)
    translated_before = deepcopy(translated)
    checked_translated = validate_profile(translated)
    assert translated == translated_before
    assert checked_translated == translate_elevations(checked, offset)
    translated_profiles[offset] = checked_translated
```

## Verification and remaining scope

The smoke tests are part of the complete profile-validation suite:

```sh
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/geotechnical/test_slope_profile.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/slope-profile-tests"
```

Set `TMPDIR` to a temporary directory outside the checkout before running.
The existing solver-dependent tests retain their dependency checks; the new
preflight tests require no solver and add no skips.

Issue #2504 remains open. Workbook-equivalent factor-of-safety reproduction,
solver and seepage benchmark coverage, and the wider dependency and adoption
evaluation remain outside this preflight example. Existing implementations and
benchmarks are not rerun or revalidated here. Soil-property sourcing and
engineering acceptance require separate evidence.
