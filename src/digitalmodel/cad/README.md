# 1. Summary

The CAD reader package is a geometry ingestion interface. Analysis is performed
to determine whether coordinates have an explicit unit basis; key results are
summarized below. Computer-aided design is abbreviated as CAD.

Draft for client review

- Geometry ingestion is available for downstream analysis.
  - Straight Drawing Exchange Format (DXF) primitives and mesh faces are returned
    as indexed geometry in metres.
  - STEP and IGES shapes are returned as a display tessellation in metres, with
    the kernel's exact volume, area, bounding box, topology counts and validity.
- Source units are required before coordinates are accepted.
  - Missing units are supplied by the caller; contradictory declarations are refused.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-19 | Initial interface and synthetic examples establish explicit units. |
| 1 | 2026-10-25 | Disclose exact-coordinate STL deduplication and test shared connectivity in both encodings. |
| 2 | 2026-11-03 | Add STEP and IGES readers through the optional OCP kernel (#2903). |

# 2. Scope

Geometry is read from DXF modelspace, stereolithography (STL) meshes, Wavefront
object (OBJ) meshes, STEP files and IGES files. Drawing (DWG) input is accepted
only through a configured external converter producing DXF.

ISO 10303-21 exchange files are referred to as STEP; Initial Graphics Exchange
Specification files are referred to as IGES. STEP and IGES are read through the
OpenCASCADE Technology (OCCT) kernel, imported as `OCP` from cadquery-ocp. The
DXF, mesh and DWG readers import no geometry kernel.

Native SolidWorks (`.sldprt`, `.sldasm`) and Inventor (`.ipt`, `.iam`) files are
not read. They are refused with a request to export STEP or IGES from the
authoring system. No supported reader for these formats is available, and the
open-source native parsers are reverse-engineered. The evaluation is recorded in
[#3827](https://github.com/vamseeachanta/digitalmodel/issues/3827).

# 3. Basis and criteria

Unit consistency is evaluated before geometry is returned. A known file
unit and a caller declaration are required to agree, because an override changes
physical dimensions without changing the stored coordinates.

The conversion is x_m = x_s × f, where x_s is a source coordinate, f is metres per
source unit and x_m is the coordinate in metres. The metre identity, SI milli and
centi prefixes, international inch and international foot definitions are used.
The supported source symbols are `m`, `mm`, `cm`, `in` and `ft`. Kernel volumes
and areas are scaled by f³ and f².

STEP and IGES units are decoded from the file text by this package before the
kernel reads any geometry.

| Format | Declaration read | Accepted values | Treated as absent |
| --- | --- | --- | --- |
| STEP | The length unit of each `GLOBAL_UNIT_ASSIGNED_CONTEXT` | `SI_UNIT` metre with no prefix, `.MILLI.` or `.CENTI.`; `CONVERSION_BASED_UNIT` `INCH` or `FOOT` whose stated factor is 0.0254 m or 0.3048 m | No context assigns a length unit |
| IGES | Global section parameter 14 (units flag) and 15 (units name) | Flags 1 (in), 2 (mm), 4 (ft), 6 (m) and 10 (cm); flag 3 with the name `IN`, `INCH`, `MM`, `FT`, `M` or `CM` | Blank flag and blank name |

An absent declaration requires caller units. IGES 5.3 defaults a blank units flag
to inches; this package does not apply that default.

These cases are refused:

- STEP contexts with different length units.
- A STEP conversion-based unit whose name and factor disagree.
- A STEP unit that the kernel reports differently from the text decode.
- An IGES units flag and units name that disagree.
- Any other unit, and an IGES model space scale other than 1.

The kernel output unit (`xstep.cascade.unit`) is set to the resolved source unit.
The kernel conversion is therefore the identity, and f is applied once. For IGES
the resolved unit is also written into the kernel's copy of the Global section.

No size plausibility threshold, curve tolerance, surface closure criterion or
structural acceptance limit is assigned. Those checks are NOT_EVALUATED by this
package and require a separate analysis with stated inputs and criteria.

# 4. Geometry model

World coordinates are returned in the immutable `CadGeometry` record. Indices
in each `GeometryEntity` address `points`; vertex order and face winding are retained.

The entity kinds are point, line, polyline, triangle and polygon. Closed
polylines repeat their first index. STEP and IGES results contain triangles only.
DXF object-coordinate systems are transformed to the world coordinate system
before length conversion. DXF layer and handle metadata are retained. Mesh
appearance, normals, materials and groups are omitted.

The `UnitDecision` record identifies `source_units`, `metres_per_unit` and a
basis of `file`, `caller` or `file_and_caller`. Its scale is applied once; result
`units` is always `m`. Coincident vertices are permitted. The meshio STL decoder
deduplicates exact-coordinate corners into shared point indices before unit
conversion, so source corner numbering is not preserved. Face vertex order and
winding are retained. No tolerance-based welding, triangulation or healing is
performed by the DXF and mesh readers.

STEP and IGES results also carry `brep`, a `BrepSummary` record.

| Field | Meaning |
| --- | --- |
| `solids`, `shells`, `faces` | Counts of distinct entities in the kernel shape |
| `bounding_box` | Minimum and maximum corners in metres, from the exact geometry |
| `volume_m3` | Sum over the solids; None when the shape has no solid |
| `surface_area_m2` | Total over all faces, including faces outside solids |
| `is_valid` | Result of the OCCT `BRepCheck_Analyzer` topology and geometry check |
| `linear_deflection_m` | Chord deviation allowed for the display triangles |

The triangles are a display tessellation, not the exact surface. Their linear
deflection is 10⁻³ of the bounding-box diagonal; the angular deflection is
0.5 rad, the OCCT default. Each face is tessellated separately, and `source_id`
names the face (`face 1`, `face 2`, ...). Points on shared edges repeat once per
face. Triangles on reversed faces are reordered so that their winding follows the
face orientation. Volume, area and the bounding box are not computed from the
triangles.

# 5. Supported formats

The supported geometry is bounded explicitly. An unsupported DXF modelspace
entity causes the complete read to fail; no partial drawing is returned.

| Format | Suffixes | Extra | Units source |
| --- | --- | --- | --- |
| DXF | `.dxf` | `drawings` | `$INSUNITS`, or the caller |
| STL, OBJ | `.stl`, `.obj` | `mesh` | The caller only |
| STEP | `.step`, `.stp` | `cad-occ` | Representation context, or the caller |
| IGES | `.iges`, `.igs` | `cad-occ` | Global section, or the caller |
| DWG | `.dwg` | none; an external converter | Converted DXF, or the caller |

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
- STEP and IGES: every root the kernel transfers, merged into one shape. Faces
  are required; wireframe-only files are refused.
- STEP and IGES exclusions: assembly structure, names, colours, layers, product
  and manufacturing information and per-part units.
- DWG: converter call interface followed by the same DXF geometry and units rules.
- SolidWorks and Inventor native files: refused; see section 2.

# 6. Use

Optional dependencies are loaded only by their readers. The `drawings` extra
provides ezdxf for DXF; the `mesh` extra provides meshio for STL and OBJ; the
`cad-occ` extra provides cadquery-ocp for STEP and IGES. The repository dependency
constraint `numpy<2` also applies. Importing this package requires no reader
dependency.

```python
from digitalmodel.cad import read_cad

# DXF units are read from the modelspace declaration.
drawing = read_cad("geometry.dxf")

# A mesh has no trusted length declaration in this interface.
mesh = read_cad("surface.stl", units="mm")
assert mesh.units == "m"

# STEP units are read from the file; brep holds the exact kernel properties.
part = read_cad("part.step")
print(part.unit_decision.source_units, part.brep.volume_m3, part.brep.is_valid)
```

OCCT translator settings are process-wide. Reads in this package are serialised
by a lock and restore the settings they change. Other code in the same process
that changes OCCT translator settings at the same time is not coordinated.

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
imports and explicit converter failures. Two-facet ASCII and binary STL fixtures
check that six corners with one repeated position produce five points and one
shared index while retaining face vertex order. Converter tests copy a synthetic DXF;
they establish call behavior, not DWG fidelity. No third-party drawing is used.

STEP and IGES unit decoding is tested on hand-written file fragments, without the
kernel. Kernel round trips build two shapes with build123d, write them to STEP and
IGES in a temporary directory and read them back:

- A 100 × 200 × 300 mm box, written in millimetres: 0.006 m³ and 0.22 m².
- A cylinder of radius 50 mm and height 400 mm, written in inches: 0.001π m³
  and 0.045π m².

Volume and area are checked to a relative 10⁻⁶ and the bounding box to 10⁻⁶ m,
against hand values. A file with its length unit removed is refused without
caller units and reads correctly with them. These tests skip when OCP or
build123d is absent. They were run with cadquery-ocp 7.9.3 and build123d 0.7.0;
build123d 0.8 and later require numpy 2.

# 8. Failure handling

A failed ingestion is reported as `CadReadError`, with `UnitsError` identifying
unit failures. Missing optional packages raise an `ImportError` naming the extra.
A STEP or IGES file that the kernel cannot read or transfer, or a face that it
cannot tessellate, is a `CadReadError`. An invalid shape is returned with
`brep.is_valid` false; it is not repaired.

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
converter evidence and documented excluded formats. The synthetic triangle
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
- IGES/PDES Organization, Initial Graphics Exchange Specification 5.3 (1996),
  section 2.2.4, Global section parameters 13 to 15 (model space scale,
  units flag and units name).
- ISO 10303-41, the `si_unit`, `conversion_based_unit` and
  `global_unit_assigned_context` entities, as written in ISO 10303-21 files.
- OCCT 7.9 translator guides, `xstep.cascade.unit`:
  [STEP translator](https://dev.opencascade.org/doc/overview/html/occt_user_guides__step.html)
  and [IGES translator](https://dev.opencascade.org/doc/overview/html/occt_user_guides__iges.html).
- cadquery-ocp 7.9.3 and build123d 0.7.0 published distributions:
  [cadquery-ocp](https://pypi.org/project/cadquery-ocp/) and
  [build123d](https://pypi.org/project/build123d/0.7.0/).
- Native-format evaluation and the recommended adapter tiers:
  [#3827](https://github.com/vamseeachanta/digitalmodel/issues/3827).
- [Commentable HTML guide](../../../docs/cad-reader/guide.html) and
  [change panel data](../../../docs/cad-reader/changes.json).
- Synthetic validation: `tests/cad/`.

# 10. Further development

STEP assembly structure, per-part units, names and product and manufacturing
information are not returned. A caller-selected tessellation deflection and
BREP input are separate extensions. Native SolidWorks and Inventor geometry
depends on vendor export, tracked in #3827.

Curved DXF entities, block expansion, assembly metadata and converter fidelity
checks remain separate extensions. Analysis-specific geometric criteria are
provided by downstream consumers.
