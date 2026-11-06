# Prepare a diffraction spec from a mesh

This offline example creates an anonymous mesh in a caller-selected output
directory, then uses the existing `resolve` command to write a `DiffractionSpec`.
It covers input preparation for #607. It does not calculate hydrodynamic results.

## Synthetic input basis

All geometry and loading values below are synthetic demonstration inputs,
assumed, based on previous experience. They are declared independently of the
resolver rather than inferred from a mesh bounding box:

| Input | Declared value and basis |
| --- | --- |
| Length, beam, draft | 10 m, 4 m, 2 m; chosen rectangular box dimensions |
| Coordinates | Bow and stern at x = 0 and 10 m, sides at y = -2 and 2 m, bottom at z = -2 m, waterline at z = 0 m |
| Displacement | 82 tonnes; box volume = 10 × 4 × 2 = 80 cubic metres, multiplied by the assumed water density of 1025 kg per cubic metre, divided by 1000 kg per tonne |
| Water depth | 50 m; chosen finite-depth demonstration input |
| Centroid check | Uniform-box midpoint = (5, 0, -1) m; a geometric estimate, not measured mass properties |

The density and gravity basis is the existing `ResolverConfig` in
`src/digitalmodel/hydrodynamics/diffraction/resolver.py`: assumed seawater density
and standard gravity, respectively. The GDF header uses unit length 1 m and
gravity 9.80665 metres per second squared from that configuration. The mesh
contains the bottom and four wetted sides, with outward vertex ordering and no
waterplane lid. The zero symmetry flags request the full mesh. GDF header and
panel layout follow `src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py`.

## Run the example

Use an environment where the package and its Python dependencies are already
installed and `diffraction` is available. Set `OUTPUT_DIR` to a new directory of
the caller's choice; the commands write only there. Paths containing spaces are
supported. No download, solver discovery or native solver is needed.

```bash
OUTPUT_DIR="$PWD/mesh-to-spec-output"
python - "$OUTPUT_DIR" <<'PY'
from pathlib import Path
import sys

output_dir = Path(sys.argv[1]).resolve()
output_dir.mkdir(parents=True, exist_ok=True)
# Coordinates and face connectivity: Synthetic input basis above.
a, b, c, d = (0, -2, -2), (10, -2, -2), (10, 2, -2), (0, 2, -2)
e, f, g, h = (0, -2, 0), (10, -2, 0), (10, 2, 0), (0, 2, 0)
panels = [(a, d, c, b), (a, b, f, e), (b, c, g, f),
          (c, d, h, g), (d, a, e, h)]
lines = ["Synthetic box", "1.0 9.80665", "0 0", str(len(panels))]
lines.extend(" ".join(map(str, vertex)) for panel in panels for vertex in panel)
(output_dir / "box.gdf").write_text("\n".join(lines) + "\n", encoding="utf-8")
PY
diffraction resolve --outcome ship_raos \
  --mesh "$OUTPUT_DIR/box.gdf" \
  --loa 10 --beam 4 --draft 2 --displacement 82 \
  --water-depth 50 --inertia-mode free_floating \
  --out "$OUTPUT_DIR/spec.yml"
```

The command writes `spec.yml` and prints `Assumptions:`. The YAML can be loaded
with `DiffractionSpec.from_yaml`. Keep the printed assumption output alongside
the spec when reviewing the input basis; provenance is printed by this command,
not embedded as a separate ledger in the YAML.

## Physical inputs and assumptions

A mesh does not establish displacement, centre of gravity, inertia or water
depth. Supply dimensions together in metres (`--loa`, `--beam`, `--draft`),
displacement in tonnes (`--displacement`), and water depth in metres
(`--water-depth`) for this workflow. The CLI can estimate omitted dimensions and
mass, but those estimates do not replace independently established physical
inputs. This example supplies dimensions explicitly and does not use bounding
box inference.

The current CLI estimates centre of gravity and radii of gyration in
`free_floating` mode. It also records mass and vertical centre of gravity as
`estimated_from_data` with `medium` confidence, even when mass is obtained by
converting the supplied displacement. The centroid above is an independent
arithmetic check; the radii remain empirical estimates. Geometry conventions,
environment constants, and frequency and heading grids remain
`assumed_default` with `low` confidence. None of these
values should be interpreted as measured properties.

For an analysis, replace estimated inertia and default conditions with the
required physical data in the spec. The existing CLI does not accept measured
centre of gravity or inertia tensor components directly. This tiny mesh only
demonstrates schema-valid input preparation; it establishes no mesh convergence,
hydrostatic validation or solver readiness. No solve is performed.
