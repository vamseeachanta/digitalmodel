# Uniform local-loss request study

This preparation assembles the four synthetic pipe geometries in issue 2287
into reproducible requests. Every cell remains `UNQUALIFIED`; both requested
levels remain `NOT_EVALUATED`. Allowable wall, criterion, margin and asset
acceptance remain null. The way forward is exact-edition source qualification,
independent examples and applicability review before assessment or inversion.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Add bounded request assembly, physical coordinates and separate FCA inputs so study preparation can be reproduced without implying acceptance. |

## Basis and scope

The [existing specification](../../plans/2026-10-06-issue-2287-uniform-local-loss-screening.md)
and [source audit](uniform-loss-source-audit-2026-10-06.md), Tables 1 and 3,
remain authoritative for the study scope and unresolved qualification evidence.
The latter withholds exact API 579-1/ASME FFS-1 2021 Part 5 qualification.
No licensed equations, coefficients, tables or acceptance values are added.

The fixed synthetic inventory is:

| OD (in) | Nominal and current sound wall (in) |
| --- | --- |
| 12.750 | 0.375 |
| 16.000 | 0.625 |
| 20.000 | 0.500 |
| 30.000 | 0.375 |

Issue 2287, Parametric basis (2026), supplies the geometry pairs, axial lengths
0.5, 1, 2, 4, 8, 16 and 32 in, and width fractions 0.02, 0.10, 0.25 and 0.50
of the current mean circumference. It also supplies the 16 by 8 physical grid,
20% nominal lower study bound, 0.0001 in requested resolution and baseline/FCA
sensitivities of 0.000, 0.020 and 0.050 in. These are study assumptions.

One positive pressure per geometry is required in the listed order. Pressure
is recorded without deriving design pressure or assigning a grade, allowable,
load suitability or acceptance limit. No X52 strength, design factor or RSFa
is inferred by this request builder. Those missing qualifications remain in the
existing per-level reason codes. Current loss is labelled external; internal
loss, nonpressure loading, interacting defects and other excluded geometries
have no assessment route here.

## Method and slices

1. `prepare_study` reuses `prepare_envelope_study` for current geometry and the
   separate Level 1 and Level 2 evidence requests. The default inventory has
   4 x 7 x 4 x 3 = 336 cells and 672 unevaluated level records.
2. Each cell carries physical axes: edges are zero and the requested extent;
   centers are `(j + 1/2) * extent / N` for `j = 0, ..., N-1`. Circumferential
   coordinates measure arc length at current mean diameter. Arrays describe
   geometry; no synthetic thickness measurement or solved wall is supplied.
3. The CLI writes deterministic strict JSON, complete configuration and exact
   source-byte SHA-256 digests. The inherited `config_sha256` covers ordered
   current-cell inputs only. `study_config_sha256` additionally covers FCA,
   grid dimensions and the fixed inventory. Neither digest authenticates a
   standard or grants engineering qualification. Case IDs identify rows within
   this configuration; they are not archive dataset identifiers.

Geometry uses `Di = OD - 2*t_sound`, `Dm = OD - t_sound`, arc `c = f*pi*Dm`,
angle `360*f` degrees and geometric length `s/sqrt(Di*t_sound)`. The diameter
relations follow the radial wall definition. Arc and angle follow
[OpenStax, Precalculus 2e (2021), section 5.1](https://openstax.org/books/precalculus-2e/pages/5-1-angles),
which gives `s = r*theta` and degree/radian conversion. The length coordinate
is defined by the study specification; it is not a qualified Part 5 lambda or
Tc. Physical axis spacing is independent of the assessment engine's inferred
flaw length.

FCA inputs are distinct `(sound, flaw)` pairs, with one to three distinct pairs
per run. Defaults request equal reductions at the three specified sensitivities;
unequal reductions can be requested explicitly. Reported wall subtractions are
ordinary arithmetic. They are labelled `NOT_EVALUATED`, do not change current
geometry, are not clipped into a future search interval, and are not asserted
to be normative Tc/tmm or a future-life prediction. A reduction that exhausts
sound wall or a lower requested study bound is rejected before output. This
validation is an input-consistency check, not a Part 5 applicability assessment.

## Worked example: inputs, method and answer

The committed [input file](../../../examples/asset_integrity/uniform_loss_requests.json)
supplies pressures 100, 200, 300 and 400 psi and one unequal FCA pair:
0.020 in sound reduction and 0.050 in flaw reduction. Pressures are synthetic
request values without a qualified material/load basis. This configuration
produces 112 requests and 224 unevaluated level records.

For the 20 in OD, 0.5 in current sound-wall cell at 8 in axial length and
quarter-circumference width, the independent calculation is:

- `Di = 20 - 2*0.5 = 19 in`; `Dm = 20 - 0.5 = 19.5 in`.
- `c = 19.5*pi/4 = 4.875*pi in`, displayed as 15.3 in; angle is 90.0 degrees.
- Geometric length is `8/sqrt(19*0.5)`, displayed as 2.6.
- Axial spacing is `8/16 = 0.5 in`; centers run from 0.25 to 7.75 in.
- Current lower study wall is `0.20*0.5 = 0.10 in`. Requested subtractions
  give sound wall `0.5-0.020 = 0.480 in`, lower wall `0.10-0.050 = 0.050 in`
  and upper wall `0.5-0.050 = 0.450 in`.

The answer is a geometric request at 300 psi with null allowable walls and
`NOT_EVALUATED` Level 1/2 checks. No passing or failing engineering verdict is
available. Exact arithmetic above explains inputs; displayed geometric results
are rounded, while JSON retains numerical precision.

From the repository root, with a writable output directory selected for retained
working data, run:

```sh
PYTHONPATH=src uv run --no-project python -m digitalmodel.asset_integrity.uniform_loss_study \
  --config examples/asset_integrity/uniform_loss_requests.json \
  --output "$TMPDIR/uniform-loss-requests.json"
```

The output is a working request inventory, not a numerical-results archive.
An identical existing output is reused without writing; differing existing
bytes are refused. Select a new output file when configuration or source bytes
change. No source or configuration timestamp, local path or host identifier is
embedded. Source digests cover the two preparation modules, specification and
source audit, not licensed standards or assessment-engine implementation.

## Validation and remaining work

The [tests](../../../tests/asset_integrity/test_uniform_loss_study.py) calculate
expected extents, centers, arcs and FCA subtractions independently in their
docstrings. They check inventory counts, null outputs, malformed inputs,
configuration/source digests, the committed example, strict JSON and byte-exact
resumption. Existing request and diagnostic tests exercise the unchanged
assessment behavior separately.

Exact 2021 edition/errata/access evidence, source-backed equations and bounds,
independent edition-matched examples, prerequisite checks and method/load
applicability remain unresolved. Qualified bisection, boundary and unit tests,
width/orientation response and FCA procedures depend on that evidence. Existing
pressure-method results retain their named preliminary scope. Level 3 remains
the gated proposal in the existing specification; no solver is selected or run.
Independent provider review remains pending and is not inferred from tests.
