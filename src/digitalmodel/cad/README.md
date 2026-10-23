# 1. Summary

The CAD reader package is a geometry ingestion interface. Analysis is performed
to determine whether coordinates have an explicit unit basis; key results are
summarized below. Computer-aided design is abbreviated as CAD.

Draft for client review

- Geometry ingestion is available for downstream analysis.
  - Straight Drawing Exchange Format (DXF) primitives and mesh faces are returned
    as indexed geometry in metres.
- Source units are required before coordinates are accepted.
  - Missing units are supplied by the caller; contradictory declarations are refused.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-19 | Initial interface and synthetic examples establish explicit units. |

# 2. Scope

Geometry is read from DXF modelspace, stereolithography (STL) meshes and Wavefront
object (OBJ) meshes. Drawing (DWG) input is accepted only through a configured
external converter producing DXF.

STEP solid and IGES surface adapters are deferred. ISO 10303 exchange files are
referred to as STEP; Initial Graphics Exchange Specification files are referred
to as IGES. No geometry kernel is imported or invoked by these readers.

# 3. Basis and criteria

Unit consistency is evaluated before geometry is returned. A known file
unit and a caller declaration are required to agree, because an override changes
physical dimensions without changing the stored coordinates.

The conversion is x_m = x_s × f, where x_s is a source coordinate, f is metres per
source unit and x_m is the coordinate in metres. The metre identity, SI milli and
centi prefixes, international inch and international foot definitions are used.
The supported source symbols are `m`, `mm`, `cm`, `in` and `ft`.

No size plausibility threshold, curve tolerance, surface closure criterion or
structural acceptance limit is assigned. Those checks are NOT_EVALUATED by this
package and require a separate analysis with stated inputs and criteria.

# 4. Geometry model

World coordinates are returned in the immutable `CadGeometry` record. Indices
in each `GeometryEntity` address `points`; vertex order and face winding are retained.

The entity kinds are point, line, polyline, triangle and polygon. Closed
polylines repeat their first index. DXF object-coordinate systems are transformed
to the world coordinate system before length conversion. DXF layer and handle
metadata are retained. Mesh appearance, normals, materials and groups are omitted.

The `UnitDecision` record identifies `source_units`, `metres_per_unit` and a
basis of `file`, `caller` or `file_and_caller`. Its scale is applied once; result
`units` is always `m`. Coincident vertices are permitted; source numbering is not
preserved by the STL decoder. No welding, triangulation or healing is performed.

# 5. Supported formats

The supported geometry is bounded explicitly. An unsupported DXF modelspace
entity causes the complete read to fail; no partial drawing is returned.

- DXF: POINT, LINE, straight LWPOLYLINE, straight two-dimensional or
  three-dimensional POLYLINE, and triangular or quadrilateral 3DFACE entities.
- DXF exclusions: bulge arcs, width, thickness, fitted polylines, polygon meshes,
  polyfaces, blocks, text, circles, splines and other unlisted modelspace entities.
  Paperspace is outside the read scope. Layout units are not modelspace units.
- STL: ASCII and binary triangle meshes, including binary headers starting with
  `solid`. Header text is not treated as a unit declaration.
- OBJ: polygon faces with positive or relative negative position indices. Inline
  comments are removed. Appearance indices are discarded. Line, point, freeform,
  continuation and other unlisted records are refused.
- DWG: converter call interface followed by the same DXF geometry and units rules.

# 6. Use

Optional dependencies are loaded only by their readers. The `drawings` extra
provides ezdxf for DXF; the `mesh` extra provides meshio for STL and OBJ. The
repository dependency constraint `numpy<2` also applies. Importing this package
requires neither reader dependency.

```python
from digitalmodel.cad import read_cad

# DXF units are read from the modelspace declaration.
drawing = read_cad("geometry.dxf")

# A mesh has no trusted length declaration in this interface.
mesh = read_cad("surface.stl", units="mm")
assert mesh.units == "m"
```

The DWG converter is supplied as an argument vector. The converter is required
to preserve coordinate values and their unit basis; unitless output is accepted
only with a caller declaration for those output coordinates. No shell expansion
or automatic executable discovery is performed.

```python
from pathlib import Path
from digitalmodel.cad import DwgConverter, read_cad

converter = DwgConverter(
    command=("dwg-to-dxf", "{input}", "{output}"),
    timeout_seconds=60,  # Caller-selected operational timeout, not a design limit.
    work_dir=Path("scratch"),  # An existing, writable directory.
)
geometry = read_cad("geometry.dwg", converter=converter)
```

A fresh subdirectory retains `converted.dxf` and `converter.log` for each attempt.
The converter runs in that subdirectory. Nonzero exit, timeout, missing output,
output symlinks and output directories are refused. The original input is passed
by absolute location; converter software is trusted to respect its input. The
package bundles no converter and has no DWG decoder.

# 7. Validation

Expected coordinates are established independently in synthetic test docstrings.
The metric prefix and customary-unit products are calculated by hand, and
position indices are checked against the fixture definitions.

Tests cover world-coordinate transforms, triangular and polygonal faces, missing
and conflicting units, non-finite coordinates, malformed inputs, optional
imports and explicit converter failures. Converter tests copy a synthetic DXF;
they establish call behavior, not DWG fidelity. No third-party drawing is used.

# 8. Failure handling

A failed ingestion is reported as `CadReadError`, with `UnitsError` identifying
unit failures. Missing optional packages raise an `ImportError` naming the extra.

An absent unit declaration is resolved by obtaining the coordinate unit from an
authoritative drawing basis and supplying it explicitly. A contradictory or
unsupported declaration is resolved in the source export. Unsupported geometry
is exported to a supported representation with a documented conversion basis.
Converter failures are investigated using retained diagnostics. Output from a
successful converter is still subject to independent fidelity review.

# 9. Appendix

The data basis and source locators are recorded here. No real drawing or inferred
physical dimension is included.

## 9.1 Data register and quality evidence

All fixtures are synthetic and are created in temporary directories by tests.
No missing dimension is filled by assumption. Geometry quality, geometric
fidelity, design suitability and source authenticity are NOT_EVALUATED.

The pre-flight checklist covers explicit unit basis, independent expected
coordinates, refusal of unsupported geometry, optional dependencies, retained
converter evidence and documented deferred formats. The synthetic triangle
schematic and comment controls are included in the accompanying HTML guide.

## 9.2 Sources and companion artifacts

- NIST SP 811 (2008), Appendix B.8, foot and inch entries:
  [conversion factors](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8).
- NIST SI Prefixes, table updated 2025-08-13, milli and centi rows:
  [prefix definitions](https://www.nist.gov/pml/owm/metric-si-prefixes).
- ezdxf 1.4.5, DXF Units, `$INSUNITS` table:
  [DXF units](https://ezdxf.readthedocs.io/en/stable/concepts/units.html).
- ezdxf 1.4.5, LWPOLYLINE, world-coordinate vertices and bulge semantics:
  [lightweight polyline](https://ezdxf.readthedocs.io/en/stable/dxfentities/lwpolyline.html).
- Library of Congress, OBJ format description, updated 2025-02-18,
  Description and Notes, Potential interoperability issues:
  [OBJ format](https://www.loc.gov/preservation/digital/formats/fdd/fdd000507.shtml).
- meshio 5.3.5, reader interface and published STL/OBJ reader implementations:
  [published distribution](https://pypi.org/project/meshio/5.3.5/).
- [Commentable HTML guide](../../../docs/cad-reader/guide.html) and
  [change panel data](../../../docs/cad-reader/changes.json).
- Synthetic validation: `tests/cad/`.

# 10. Further development

Kernel-backed STEP solids and IGES surfaces are required before those formats
are accepted. Separate adapters with synthetic kernel-generated reference shapes
and independently checked unit handling are the next scope.

Curved DXF entities, block expansion, assembly metadata and converter fidelity
checks remain separate extensions. Analysis-specific geometric criteria are
provided by downstream consumers.
