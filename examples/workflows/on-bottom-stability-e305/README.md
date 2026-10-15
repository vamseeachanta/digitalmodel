# Simplified on-bottom stability calculation interface

Qualification is incomplete. The OS-F101 equation 5.31 comparator is not
implemented because a verified equation transcription is unavailable. The
private workbook comparison has not been observed. A calculated demand ratio
alone does not establish standard compliance or overall qualification.

The basis is DNV RP E305 (1988), clause "Simplified Static Stability Analysis".
No licensed table values are distributed. The F109 interfaces are unchanged.
The implementation uses the requested weight-demand equation and the existing
repository's annular geometry and Morison force equations. Agreement of the
Morison expression with the edition-specific clause remains unverified.

## Units and geometry

Inputs are SI; gravity is required. Corrosion allowance is removed from the
inside steel wall, and contents occupy the resulting bore. The corrosion
coating covers the full length. Concrete and field-joint material occupy
complementary length fractions, with independently specified thicknesses.
Water absorption is added mass divided by dry concrete mass. Marine growth
covers each local outside diameter. Displaced volume is length-weighted; the
largest local outside diameter is used for hydrodynamic loading. Added mass
is converted to weight with the supplied gravity.

## The five comparison options

- Current is added once by default. `duplicate_current` explicitly reproduces
  a second addition for comparison; the result records the option.
- A phase scan evaluates simultaneous drag, inertia and lift, and maximizes
  their combined weight demand. `phase_rad` selects a single instant instead.
- Parity mode requires typed `R`, `Fw`, drag, inertia and lift coefficients.
  No F109 coefficient or soil default is inherited. Typed factors are inputs,
  not a claim of a licensed table lookup.
- Each case owns its wave period. The second case never inherits the first
  case's period. Both `extreme_wave` and `extreme_current` roles are required.
- Gravity is supplied explicitly. The private parity comparison uses 9.81.
  The existing spectral-wave helper has fixed gravity; an inconsistent gravity
  is rejected rather than silently replaced.

`reduction_application` is required: `wave_velocity` scales wave velocity and
acceleration; `wave_and_current` additionally scales current. Neither choice
is asserted to reproduce E305 without the private basis. K and M use the
unreduced wave amplitude and the diameter-averaged current, following the
algebra documented in the legacy repository tests. Zero wave gives K = 0
and undefined M; table lookup then requires a separately supported case.

The shifted logarithmic profile is integrated over the diameter and normalized
at the specified reference height. This follows the existing OpenFOAM current
profile's algebra; equivalence to Appendix A is not established. Roughness is
supplied from the external grain-size table, with no default roughness value.

## External tables

`E305_TABLES_FILE` or an explicit loader argument identifies a JSON file; no
location is assumed. A `schema_version` identifies the layout. Its `tables`
object contains exactly `roughness`, `R`, `Fw` and `clay_friction`. Each table
has `axes` (one or two named axes, each with `units` and ordered `values`),
output `units`, a rectangular `values` array, and a `citation` object matching
`digitalmodel.citations.schema.Citation`. Roughness output is in m; the other
outputs are dimensionless (`1`). Lookup uses declared coordinates, linear
interpolation and no extrapolation. The private data owner supplies the R
and clay axes; no standard-specific axes are inferred here.

Every citation identifies the edition, clause title and wiki page. Citation
resolution is fail-closed at lookup time. Missing configuration, a missing or
malformed table, invalid units, nonfinite numbers, mismatched citation metadata
or out-of-domain lookup raises an error. Values and `Citation` sidecars remain
separate. Synthetic test tables are constructed in temporary test directories;
they do not represent a standard.

## Concrete inversion

The inverse solve recomputes geometry and both cases within a caller-supplied
sign-changing thickness bracket. It reports the root, forward residual and
convergence status. It does not establish the smallest feasible thickness or
global monotonicity; diameter-dependent loading and tables can change both.
The missing OS-F101 comparator remains a limitation of the inverse result.

## Private workbook fixture

The converter takes a workbook path, a JSON cell map and an output path.
Workbook and JSON output remain outside the public repository. Only cached
cell values are read; formulas, macros and program interfaces are not run.
The cell map mirrors the output structure; cell leaves have `sheet` and `cell`
keys, while non-result configuration may use a `literal` leaf. Expected numeric
results must refer to independent cached workbook cells, never literal answers
or values computed by this implementation. Missing cached values are errors.

The fixture layout is `schema_version`, `g`, `segments`, `expected` and `source`.
Each segment has an `id`, a `pipe` object matching the `Pipe` dataclass and two
`cases` matching the `Case` dataclass. `expected` maps each segment identifier
to both case roles; each role supplies `required_weight_n_m`,
`available_weight_n_m` and `utilization`. The source records the workbook and
cell-map SHA-256 digests without their names or paths. The external fixture is
selected with `E305_GOBY_FIXTURE`; absent configuration or an absent file causes
an explicit skip. Present malformed data fails. The comparison tolerance is
0.1 % with g = 9.81. Without the private workbook, that reproduction is not
established.

## Direct batch entry

`PYTHONPATH=src uv run --no-sources python examples/workflows/on-bottom-stability-e305/run.py --input <input.yml>`
runs the segment table without the shared engine's eager licensed-interface
imports. JSON, CSV and a digest manifest are written to the configured output
directory. The supplied two-row example is synthetic and uses g = 10.0.
No engineering report is generated. The global registry and engine dispatch
are registered, but shared-engine execution has not been exercised here.

`PYTHONPATH=src uv run --no-sources python scripts/subsea/convert_e305_workbook.py <workbook.xlsx> --map <cell-map.json> --output <fixture.json>`
converts cached workbook cells. All three paths are external to the public
repository. Zero expected results in the parity comparison require exact zero.
The private source digests document conversion inputs; their presence alone
does not establish workbook qualification or freshness.
