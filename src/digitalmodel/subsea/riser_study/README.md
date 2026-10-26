# Riser export post-processing

This package reads exported results. It produces SVG figures, exact numeric
CSV tables, manifests and value tokens without a native solver interface.
It does not compute a strength acceptance criterion or fatigue response.

Run from the repository root, with the repository dependencies available:

```bash
PYTHONPATH=src uv run python -m digitalmodel.subsea.riser_study.riser_strength_runs results figures --reference-pair R08-W1 --touchdown-window-m 50
PYTHONPATH=src uv run python -m digitalmodel.subsea.riser_study.riser_configuration_plot geometry.json figures --reference-pair R08-W1
```

The example touchdown window is **ASSUMED**, in metres on either side of each
exported touchdown. Select a window appropriate to the exported sampling.
The reference pair is explicit: both configurations must be present. No case is
silently selected as the reference. Every recursively discovered `strength.json`
is validated before any output is written. Duplicate cases and directory/case
mismatches fail validation. Each invocation represents one strength load case
per case; different load-case parameters remain in source provenance and are
not combined into an unrecorded envelope. An invalid batch leaves no new output.
Use a separate destination for each batch; existing artifacts are overwritten
when their token identifiers match.

## Output contract

| Token | Figure and CSV content |
| --- | --- |
| FIG-STR-01 | Effective tension against arc length from hang-off, m and kN |
| FIG-STR-02 | Bending moment against arc length from hang-off, m and kN.m |
| FIG-STR-03 | Maximum normalised strength utilisation per case, dimensionless |
| FIG-STR-04 | Samples in the touchdown window and the exported touchdown marker |
| FIG-STR-05 | Static top angle and dynamic minimum/maximum, degrees from vertical |
| FIG-STR-06 | Static and dynamic maximum hang-off effective tension, kN |
| SCH-01 | Nominal SCR/SLWR profiles, horizontal distance and depth in metres |

FIG-STR-05 and FIG-STR-06 are provisional identifiers for the additional figures.
Their CSV files serve as the corresponding tables. Every figure has matching
`<token>.svg` and `<token>.csv` files. `manifest.json` contains each strength
artifact's identifier, files and all plotted rows, including the normalised
limit line. The limit is unity by the definition stress divided by the supplied
allowable; it is not a numerical requirement taken from a standard. The
export's `method` explains its stress combination and allowable basis.

The renderer consumes the same rows that it writes to CSV and JSON. SVG
metadata embeds those rows, and each numeric mark records its row index and
unrounded coordinates. Presentation labels use one decimal place; numeric
artifacts retain the input precision. SCR uses solid lines and SLWR dashed
lines, with a common colour for each diameter/wall pair. Categorical results
use unconnected marks, with a vertical segment between dynamic angle extremes.

`tokens.json` is a list of `{id, value, unit, source_file}` records for
V-STR-GOV-CASE, V-STR-UMAX-GOV, V-STR-UMAX-SCR, V-STR-UMAX-SLWR,
V-STR-TDP-SCR-REF, V-STR-TDP-SLWR-REF and V-SW-VERSION. The touchdown
reference tokens contain utilisation, not touchdown position. Numeric tokens
are dimensionless. A software token lists every distinct recorded name/version
and all source files. Ties for governing strength resolve by sorted case id.
Source paths are relative labels rooted at `results/`; the manifest binds each
source's bytes with SHA-256 and preserves software, load case, method,
allowable and run record. Files are not modified during reading.

## Shared result exchange

`results_exchange.py` defines TypedDict contracts and `validate_result` /
`load_result`. `results_exchange.schema.json` is the shared JSON Schema
(Draft 2020-12), with strength and fatigue definitions. The schema ships as
package data. Both other post-processors can import this module without
importing geometry or solver packages.

The strength record is `results/<case_id>/strength.json`:

- `case_id`: diameter/wall/configuration, such as `R08-W1-SCR`.
- `software`: nonempty `name` and `version`, exactly as exported.
- `load_case`: `id`, named wave/current parameter objects and horizontal
  `offset` containing `x_m` and `y_m`. Parameter names retain their units.
- `arc_length_m`, `effective_tension_kN`, `bending_moment_kNm` and
  `von_mises_utilisation`: aligned arrays, arc increasing from hang-off zero.
- `method`: named exported utilisation method. `allowable`: positive `value`,
  `unit` of MPa and `source`. An assumed method or value is labelled ASSUMED.
- `max_utilisation`: `value`, `arc_length_m`, `location`; it must identify a
  maximum of the supplied utilisation samples.
- `touchdown`: `arc_length_m`, `utilisation`, `stress_range_MPa`. Its arc must
  lie in the exported range; a coincident utilisation sample must agree.
  An intermediate touchdown is retained as an independent exported marker;
  no interpolation of response is invented.
- `top_angle_deg`: acute inclination from vertical, `static`, `dynamic_min`,
  `dynamic_max`. Convert a solver's tangent convention before exporting.
- `hang_off_tension_kN`: `static` and `dynamic_max`, as effective tension.
- `run`: hexadecimal `input_sha256`, portable relative `result_file` and ISO
  calendar `date`. Absolute or parent-traversing paths are rejected.

The fatigue record is `results/<case_id>/fatigue-<sea_state_id>.json`:

- `case_id`, `sea_state` with `id`, `Hs_m`, `Tp_s`, `direction_deg`, `occurrence`
  as a fraction; `hot_spots` with unique `touchdown` or `hang_off` names.
- Each hot spot has `arc_length_m`, `cycles_per_year`, `scf` and exactly one
  of `stress_range_MPa` or `range_histogram`. Each histogram bin contains
  `stress_range_MPa` and `cycles_per_year`; annual cycles sum to the hot-spot
  total. These are annualised inputs, with occurrence weighting already
  reflected in the exported annual damage.
- `sn_curve`: `name`, `source` label, `m`, `log_a`, `thickness_correction`.
  These are supplied parameters, with no publication values supplied here.
- `damage_per_year`, `life_years` and the same `run` fields. Life is reciprocal
  annual damage; zero damage has null life rather than non-JSON infinity.

The validator rejects unknown keys, missing fields, non-finite numbers,
booleans in numeric fields, misaligned samples and inconsistent summaries.
The relative numerical consistency tolerance absorbs JSON round-off only;
it is not an engineering acceptance band. Diagnostics identify fields and
schema rules without reproducing rejected values.

## Geometry and verification basis

`geometry.json` contains numeric inputs accepted by
`digitalmodel.riser_configuration_screen.static`: `water_depth_m`,
`submerged_weight_n_m`, `grounded_length_m`, `hangoff_angle_deg`,
`sag_fraction`, `hog_fraction`, `buoyancy_factor`. The supplied reference
identifies the pair; geometry is not inferred from a nominal pipe size.

SCH-01 uses `scr_offsets` and `lazy_wave_offsets` at nominal zero offset,
converts elevation above seabed to depth below the water line, and uses the
same pixel scale in both directions. It labels hang-off, sag, hog, buoyancy,
touchdown, water line and seabed, with labels outside the profile panel and
leader lines. The buoyed material interval is highlighted. Its separate
`SCH-01.manifest.json` avoids replacing the strength manifest. Its rows also
record material arc length and the buoyed interval. The command-line input
record includes a digest of the geometry file.

Tests use ASSUMED synthetic exports, not design results. The uniaxial hand
check is stress divided by allowable. The plain-catenary fixture uses case C1
and the acceptance bands in
`docs/benchmarks/catenary_closed_form/benchmark.json`; its expected tension
and angle are read from that independent benchmark. The compliant elastic
case C2 is outside the inextensible screen's scope. No licensed document is
opened and no native solver is imported or queried.

The geometry screen's existing package imports need its plotting and tabular
dependencies even though this package renders SVG with the standard library.
The strength post-processor requires only the shared JSON validation dependency.
The fixture suite exercises the command line with native-interface imports
rejected. Native results, qualified methods, environmental inputs and
engineering review remain required before using the artifacts for a design.
