# Field scene output contract

`validate_scene(scene)` checks the defined fields of the existing
`digitalmodel.field_layout_scene/2` export and returns structural counts.
The exporter and its geometry remain unchanged.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-11-16 | Define a read-only scene boundary for data consumers. |

## Consumer example

This synthetic scene has one asset and no connections:

```python
from digitalmodel.field_development.scene_contract import validate_scene

scene = {
    "schema": "digitalmodel.field_layout_scene/2",
    "field_name": "Synthetic example",
    "is_subsea": True,
    "surface": {"x_m": [0, 10], "y_m": [0, 10],
                "z_m": [[-5, -5], [-5, -5]], "source": "synthetic"},
    "assets": [{"id": "A", "name": "A", "kind": "well", "subtype": "",
                "x_m": 0, "y_m": 0, "z_m": -5, "on_surface": True}],
    "connections": [],
}
receipt = validate_scene(scene)
assert receipt == {"schema": scene["schema"],
                   "asset_count": 1, "connection_count": 0}
```

## Validation boundary

All defined exporter fields are required. Object mappings and list arrays are
checked without mutation. Extension keys are ignored. Asset and connection IDs
are nonblank strings, unique within their respective lists; kinds reuse the
layout model taxonomy. Every connection endpoint resolves to an asset ID.
Names, subtypes and surface source labels are strings; flags are booleans.

Surface axes are nonempty, finite and strictly increasing; elevation rows have
shape `(len(y_m), len(x_m))`. Asset coordinates are finite. Routes have at least
two finite XYZ points; waypoints are finite XY pairs and may be empty. Lengths
are finite and nonnegative. Booleans, numeric strings and numbers outside the
supported floating-point range are rejected in numeric fields. Invalid input
raises `ValueError` with a structural field path and no supplied names or values.
Version 1 is rejected. Byte parsing and input-size bounds remain caller duties.

## Qualification limits

Coordinates use metres in a local Cartesian frame with signed elevation; no
geographic CRS or vertical datum is established by validation. Source labels
do not establish provenance, rights or freshness. The receipt establishes only
the checked structure and counts: route lengths, endpoint coordinates, surface
placement, flags and engineering adequacy are not independently recalculated.
Extension contents are not validated. Renderer and downstream application
acceptance are not established; validation remains opt-in.
