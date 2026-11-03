# Hull tessellation provenance

## Summary

`ingest_3dm` records the triangle source that executed. Enabling
`tessellate_breps` permits a fallback; it does not request a rebuild when usable
meshes are already present. This distinction prevents an unused deflection
parameter from being reported as evidence of tessellation convergence.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Record executed routes, selected-face coverage and effective rebuild deflection to replace the unconditional rebuild claim. |
| 2 | 2026-10-24 | Count only cached faces that contribute triangles, isolate nested evidence and retain available rebuild fidelity so the record reflects extraction and survives caller edits. |

## Reading the evidence

The returned `Rhino3dmGeometry.source` flag and its `to_provenance()` mapping
describe the reader's execution. `ingest_3dm` writes that mapping under
`provenance.tessellation` in the manifest. Separate appendage reads are recorded
by region name under `provenance.appendage_tessellation`; an appendage rebuild
does not imply that the hull was rebuilt.

| `source` | Triangles used | `linear_deflection` recorded? |
| --- | --- | --- |
| `cached-render-mesh` | Existing Brep or converted Extrusion face meshes | No |
| `mesh-object` | Standalone mesh objects | No |
| `cached-render-mesh+mesh-object` | Both existing sources | No |
| `occ-rebuild` | OpenCASCADE tessellation returned by `brep_tessellate` | Yes, the effective value returned by the tessellator |

`cached_mesh_face_coverage.meshed` counts selected Brep/Extrusion faces with
a cached mesh that contributes extracted triangles, while `total` counts all
selected Brep/Extrusion faces. A nonempty cache that contributes no triangles
adds zero to `meshed`, including when the reader falls back to a rebuild.
Layer exclusions contribute to neither count. Standalone meshes contribute no
Brep faces: zero out of zero means that no Brep faces were examined, rather
than complete coverage. Object counts and face counts are different quantities.

Deflection is in source-file units. Its value is copied from the tessellator's
result, including when the caller leaves it unspecified and the tessellator
chooses a default. No deflection is attributed to a cached or native mesh.
Direct `ingest_triangles` calls carry no CAD route unless evidence is supplied.

When the tessellator supplies `TrimFidelity`, the rebuild record also carries
`fidelity.within_reference_box`, `max_overshoot` and `max_trim_deviation`, copied
from that result. Distances and the retained `reference_bbox` and `trim_bbox`
are in source-file units. A missing box is recorded as `null`: the existing
`within_reference_box` flag alone does not establish that a reference was
available. Missing fidelity is omitted rather than invented. The existing
rejection of overshoot beyond the reference box remains in force.

Hull and appendage evidence are copied recursively both when ingested and when
converted to a manifest dictionary. Editing an input record or a returned
snapshot does not change the manifest's retained evidence.

## Worked example

The executable synthetic example is
[test_hull_ingest_provenance.py](../../../tests/naval_architecture/test_hull_ingest_provenance.py).
It supplies a tetrahedron with vertices `(0, 0, 0)`, `(6, 0, 0)`, `(0, 2, 0)`
and `(0, 0, 2)`, in metres, with forward direction explicitly set to `+x`.
These coordinates are chosen test inputs. The independent hand calculation is
`volume = 6 * 2 * 2 / 6 = 4.0 cubic metres`; its boundary has four triangular
faces. No engineering coefficient or acceptance limit is introduced.

The reader fixture provides a cached triangle on each face. Ingestion with
`tessellate_breps=True` and caller deflection `0.25` returns:

```json
{
  "source": "cached-render-mesh",
  "cached_mesh_face_coverage": {"meshed": 4, "total": 4}
}
```

The test checks the JSON written to disk, absence of the rebuild note, no kernel
call and displacement of `4.0 cubic metres`. It positively checks the
`read 4 triangles via cached-render-mesh` note and, when fallback is enabled,
the note stating that fallback was unused. Removing the cached meshes causes
the fallback to run. For this route test the kernel is a double returning the
same tetrahedron and an effective deflection of `0.125` file units:

```json
{
  "source": "occ-rebuild",
  "cached_mesh_face_coverage": {"meshed": 0, "total": 4},
  "linear_deflection": 0.125
}
```

The fallback test positively checks `read 4 triangles via occ-rebuild`.
The arbitrary deflection inputs test evidence propagation, not mesh accuracy.
The cached sweep test changes the requested deflection by a factor of two and
checks that the returned source still identifies a cache and omits deflection.
Identical cached triangles from that sweep establish no convergence result.

Run the example tests from the repository root, with temporary output outside
the repository:

```bash
uv run --no-project --with pytest --with pyyaml python -m pytest \
  tests/naval_architecture/test_hull_ingest_provenance.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/hull-route-example"
```

## Limits and way forward

Partial cached coverage is reported without changing selection behaviour:
existing meshes still take precedence even if some selected faces lack a mesh.
The usual closure gate remains in force. Coverage does not establish trim
fidelity, geometric completeness or mesh quality.

For a density study on a file with cached meshes, call
`brep_tessellate.tessellate_3dm` directly and pass its triangles to
`ingest_triangles`. Preserve the returned effective deflection and inspect trim
fidelity independently. The recorded fidelity summary preserves the existing
box checks; it does not establish CAD accuracy. This change neither forces a
rebuild nor validates OpenCASCADE accuracy. The route tests replace the decoder
and kernel; they exercise the production dispatch, triangle extraction and manifest writer.

## Sources

- [rhino3dm Python API, BrepFace.GetMesh](https://mcneel.github.io/rhino3dm/python/api/BrepFace.html#rhino3dm.BrepFace.GetMesh),
  accessed 2026-10-19: retrieves the specified mesh type from a Brep face.
- [Repository reader and manifest writer](../../../src/digitalmodel/naval_architecture/hull_ingest.py),
  baseline revision `6b243e6a` (2026): cached-mesh precedence, fallback opt-in,
  layer selection and existing closure checks.
- [Repository tessellator](../../../src/digitalmodel/naval_architecture/brep_tessellate.py),
  baseline revision `6b243e6a` (2026): `BrepTessellation.linear_deflection`
  contains the effective deflection returned by `tessellate_3dm`.
