# Abstracted hull coordinates, schema v1

The exporter requires ezdxf >=1.3,<2 and `dwgread` from
`scripts/tools/build-libredwg.sh` (LibreDWG 0.14). It reads all six source DWGs
through an anonymous stdout pipe; no raw DXF or decoder diagnostics are saved.
Output files use neutral ordinals from the existing probe (`hull-01` through
`hull-06`). The probe owns the source relationships and source SHA-256 digests.

Each JSON object contains:

| Field | Meaning |
|---|---|
| `schema_version` | Integer 1 |
| `source_sha256` | Exact source revision; filenames and CAD identifiers are excluded |
| `units` | `mm_declared`: matching source/probe and decoded INSUNITS=4; independent physical scale is unverified |
| `physical_scale`, `hull_axes` | `unverified`; these data are not qualified engineering input |
| `engineering_readiness` | `unqualified` |
| `audit` | Numeric unresolved-error and recovery-fix counts; audit messages are excluded |
| `bounds` | Minimum and maximum native coordinate triples |
| `curves` | Arrays of native `[x,y,z]` triples from modelspace spatial POLYLINE entities, polymesh grid rows/columns and closed polyface face boundaries |
| `planes` | `plane_x`, `plane_y`, `plane_z`; each has 21 equally spaced numeric plane coordinates and offset triples |

Table 1. Fields and qualification limits; drawing-unit declaration applies to all coordinates.

Plane offsets are intersections of source polyline segments with planes;
interpolation is linear, duplicate exact triples are removed, and coplanar
segments contribute endpoints. They are unordered section point sets, not
connected contours or a reconstructed surface. Grid positions are generated
sampling locations, not original station identifiers. Under a subsequently
verified x-longitudinal/y-transverse/z-vertical hull convention, these plane
families may support stations, buttocks and waterlines respectively. Until that
mapping and physical scale are established, semantic station assignments,
hydrostatics and physical hull dimensions cannot be calculated from this output.

The generic filter admits only LINE, POLYLINE, LWPOLYLINE, ARC and SPLINE.
LINE and spatial POLYLINE coordinates are retained directly; polymesh rows and columns preserve grid connectivity, and polyface boundaries exclude face-record pseudo-vertices. Mesh coordinates describe control geometry, not a qualified smooth hull surface; other allowed
entities are flattened at 0.01 drawing-unit chord tolerance. Published output is
more restrictive: only modelspace spatial polylines, polymesh grids and polyface boundaries, without block expansion,
are eligible. No title-block, text, attribute, layer, handle or XDATA fields are
serialized. Entity type alone does not establish hull identity; the verified-unit
source contains only modelspace polylines, and source fidelity remains unqualified.

All five sources without a matching declared unit return `units_unverified`
and write no geometry. `--allow-declared-units` is required to write the remaining
source; acceptance of that declaration for publication is an owner decision.
Existing outputs are never overwritten. Literal byte-substring comparison checks runtime ATTRIB and ATTDEF values from
all six drawings without exposing values in assertion output.
Any collision, decoder failure, nonfinite coordinate or unresolved audit error
blocks export. No data deletion or DWG modification is performed.

Publication is currently blocked. The header-declared source has unverified
physical scale and hull axes, and its numeric geometry bytes incidentally match
numeric title-block attribute strings from other drawings. The literal privacy
rule remains enforced; no geometry output is committed. Owner acceptance of a
declared-unit basis and an exception for incidental numeric matches would be
needed to change that disposition. Actual attribute values are never recorded.
