# Riser utilization campaign inputs

## Data provenance

| Input | Source and section | Licence or terms | Date | Version | SHA-256 |
| --- | --- | --- | --- | --- | --- |
| field-development rows (NOT_USED) | No field-development rows used | Not applicable: no rows used. | 2026-10-25 | not applicable | not applicable: no rows used |
| FPSO particulars (ASSUMED) | [New-build FPSO hull technical description, Key Figures](https://www.modec.com/business/service/floater/fpso/hull/m350.html) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | Undated public page; retrieved edition | 9cee1d839086e49569be059477419423e18be02dd501430502accf4bc2257c76 |
| semi-submersible particulars (ASSUMED) | [Semisubmersible reference-system definition; report 60601, Section 3, Table 3-1](https://docs.nlr.gov/docs/fy14osti/60601.pdf) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | NREL/TP-5000-60601, September 2014 | ccb0f7cdc001cfc41f3a32abc858c5ab0fca2564152b0ed81a0f56781913a1a0 |
| spar particulars (ASSUMED) | [Spar reference-system definition; report 47535, Section 3, Table 3-1](https://docs.nlr.gov/docs/fy10osti/47535.pdf) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | NREL/TP-500-47535, May 2010 | ba6922aae6a9869a3b3e6ce440acb4e8529710a5bd62e8d41a0bf19b0ed45a5a |
| TLP particulars (ASSUMED) | [Tension-leg platform model development and loads analysis; report 45891, Section 3.2, Table 2](https://docs.nlr.gov/docs/fy10osti/45891.pdf) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | NREL/SR-500-45891, February 2010 | 09d07ddd20b92b2de185fb8befb6f1871efe8ecad7539bbc5c843e5fd0d59e53 |
| floater RAOs (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |
| metocean (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |
| riser properties (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |
| geometry and numerical settings (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |

Table 1. Source digests identify public response bytes or the authored basis; no source publications are included.

## Summary

This campaign prepares **48 ASSUMED screening cases** and **288 solver input
files**, without a licensed solver. It crosses four floater types, two size
classes, three water depths and two riser configurations. No stress utilization
curve, native equilibrium or operating limit is established by these inputs.
A distinct frozen tranche adds twelve pipe variants and 36 fatigue histories;
its native input templates remain unreleased pending readback and motion clearance.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-25 | Define an assumed comparison matrix and reproducible inputs for later licensed analysis. |
| 2 | 2026-10-25 | Place inputs under examples and bind each input group to explicit source terms, versions and digests. |
| 3 | 2026-10-26 | Add the frozen twelve-section tranche, depth-dependent pressure and three-sea expansion with explicit release gates. |
| 4 | 2026-10-28 | Grade frozen mesh transitions with explicit segment counts and retain a solver-free audit for native readback. |
| 5 | 2026-10-28 | Retain exact reviewed numerical geometry for portable input bytes and bind the producing runtime. |
| 6 | 2026-10-28 | Name normal and axial hydrodynamic coefficients explicitly and refresh source bindings after regeneration; preserve the counted mesh and physical basis. |
| 7 | 2026-10-28 | Apply named coefficients to the original comparison inputs with zero axial coefficients, regenerate worked examples and bind the proving regressions. |

## Matrix and existing code

| Axis | Selected values | Basis |
| --- | --- | --- |
| Floater | FPSO, semi-submersible, spar, TLP | Parametric concepts; public geometry context below |
| Size | Small, large | ASSUMED scaling; not an operating asset |
| Water depth (m) | 500, 1000, 1500 | ASSUMED comparison grid; not a population distribution |
| Configuration | SCR, lazy-wave | Existing configuration algorithms |
| Radial span change (% of water depth) | 0, -2, +2 | ASSUMED nominal, near and far offsets |
| Regular wave height (m), period (s) | (2, 8), (6, 12) | ASSUMED mild and severe states; no return period |

Table 2. The complete factorial matrix contains 48 geometry cases; each has six environmental inputs.

The generator reuses `riser_configuration_screen.static.scr_offsets` and
`lazy_wave_offsets`. These retain total installed length and fixed buoyancy
material coordinates across offsets. They use the existing general and lazy-wave
catenary kernels. The screening workflow's static stress proxy is deliberately
not reported as dynamic utilization. The existing
`digitalmodel.orcaflex.riser_config.calculate_weight_in_water` supplies submerged
weight, including contents once. Existing `GenericModelBuilder`,
`EnvironmentBuilder`, `PostGenerationValidator` and `OrcaFlexDumper` supply typed
model mapping, native references and YAML serialization. The modular campaign
writer and benchmark catenary writer are precedents; no new catenary solver or
YAML dialect is introduced.

## Assumed screening basis

All engineering inputs, including synthetic RAOs and inherited builder defaults,
are **ASSUMED**. [The authored basis](basis.md) defines every selected range,
quantity, unit, scale factor, RAO arithmetic method and geometry criterion.
Public dimension references provide context only. Field-development database
rows, measured RAOs and site metocean data are not used.

## Validate and generate

From the repository root, with the repository dependencies available:

```bash
PYTHONPATH=src:. uv run python -m scripts.riser_campaign.validate_manifest
PYTHONPATH=src:. uv run python -m scripts.riser_campaign.generate_inputs --output-dir <output-directory>
PYTHONPATH=src:. uv run python -m pytest tests/riser_campaign tests/riser_configuration_screen -q -rA -p no:cacheprovider
```

The output directory is mandatory. The generator validates the entire manifest
and every geometry before writing inputs. The printed table lists every case,
units, assumptions and input count, followed by the number of licensed runs
(always zero). Identical existing files are read and reused without rewriting;
a digest mismatch or symlink is rejected without replacing that file. An
interrupted generation can be resumed with the same manifest, code and output
directory. Changed inputs require a new output directory. No output is deleted.

The output contains exact manifest/schema/basis snapshots, `index.json`, a geometry
JSON file and six self-contained `.yml` models per case. The index records
relative paths, file sizes and SHA-256 digests for every input, geometry and
snapshot. Verify the index against a pinned repository revision and hash before
using it. Geometry JSON uses material arc length from hang-off, local horizontal
position from hang-off, and elevation above the touchdown centreline, in metres;
forces are newtons and angles are degrees. Geometry is an analytical seed, not
native solver output. Only one worked YAML input per configuration and their
hash index are retained here; bulk outputs belong outside the checkout.

## Frozen twelve-section tranche

The manifest declares the original comparison axes in `matrix_axes` and the
additional pipe variants in `frozen_tranche`. Coverage is checked separately:
the original tuples retain their existing meaning; the frozen rows require the
exact NPS 8/10/12 × wall W1/W2 × SCR/SLWR Cartesian set. A standalone original
matrix may declare a smaller subset of the supported axes. Its schema minimum
is one case; coverage still requires every declared combination. Frozen variants
are distinguished by validated case IDs and every supplied dimensional field.

`matrix-i0339-2.tsv` retains the supplied UTF-8/LF bytes. Its SHA-256 is
`f95a98735eb3882a354857fc56975a82d1b67095d6a57cb8cf565229e9ddf2c1`.
`criteria-i0339-2.json` records version `I0339-2-v1`, assumptions, units and
public-source lineage. Every TSV field is retained as text in the manifest and
compared against the frozen bytes, including the explicit `not_applicable`
values. The criteria digest is also pinned by the manifest. Public report bytes
were not independently acquired for this extension; environmental numbers are
the supplied frozen criteria. The public report catalogue identity remains a gap.

The same command writes the original inputs and this separate tranche. Outputs
include the scope/criteria snapshots, numerical section properties, fixed
geometry, load ledger and source-code snapshots with SHA-256 digests. The
tranche is indexed under `index.json:frozen_tranche`; it is not an extension of
the original synthetic-RAO example's qualification.

| Calculation | Written inputs | Phase A requirement | Pending governing selection |
| --- | ---: | ---: | ---: |
| Statics: 12 functional references and 72 offset/current equilibria | 84 | 88 | 4 |
| Regular dynamics, nominal offset, both headings | 24 | 30 | 6 |
| Fatigue: twelve sections and three seas | 36 | 40 | 4 |
| Total | 144 | 158 | 14 |

Table 4. Written inputs are not completed calculations. Four spatial QA statics
and ten QA dynamics depend on governing completed SCR/SLWR responses. They are
recorded as pending; no arbitrary section is substituted to reach the planned
88 statics and 70 dynamics. The full envelope additionally includes 48 offset
regular dynamics and two longer fatigue checks. No QA or Phase B input is counted
as written by this extension.

Nominal wall sets steel mass, EA and EI. The checked wall is nominal wall times
0.875 minus 3 mm, once only, and sets stress diameters. The coating contributes
mass and displacement once. Equivalent distributed modules conserve net uplift,
with pitch-weighted projected diameter for drag and volume-equivalent diameter
for inertia/displacement. These idealised modules are not depth-rated hardware.
The original module code-check screens are not rerun or represented as native
resistance qualification. Mesh targets are 3 m coarse and 0.5 m fine, with
20 m fine windows around hang-off, the analytical touchdown envelope, buoyancy
transitions and sag/hog material stations. The fine-window width is inherited
from the existing catenary template and remains an assumed starting mesh.
Frozen inputs now emit `LineType, Length, NumberOfSegments`. Initial counts
round upward so actual elements do not exceed their local targets. At each
abrupt join, the larger boundary cell is bisected until both directions of
every adjacent Length/NumberOfSegments ratio are at most 2.0. This also checks
material joins and short overlaps between fine windows. The remaining coarse
interiors are retained. Splitting preserves interval length and line type;
the installed length, physical properties and buoyancy boundaries do not move.

### Solver-free mesh audit and native handoff

The [mesh review packet](../../docs/verification/riser-mesh-review.json) records
the reviewed reference revision, source and test digests, generated index digest,
case order, all 144 frozen input digests and the twelve transition audits.
Ratios are evaluated on the serialized Length/NumberOfSegments values, with
both directions checked at every join. Display bounds round upward to one
decimal place; acceptance uses unrounded values and the unchanged 2.0 limit.
Geometry and section-property bytes and every model field outside the permitted
correction are compared with output generated from the reviewed reference source.
The frozen scope, criteria, load ledger, manifest and basis retain their
reference bytes. The original 288 comparison inputs replace only the positional
coefficient fields; geometry and all other model fields retain their reference
values. Both committed comparison examples and their hashes are regenerated.

The frozen handoff uses [reviewed numerical geometry](reviewed-geometry.json)
whose source revision and runtime are recorded with the exact input digests.
The snapshot was captured only after all 144 generated input digests matched
the existing packet. Its scope rows and raw criteria digest must match before
use, and the generator binds the snapshot's exact bytes. Frozen inputs are
rebuilt from these retained material coordinates, rather than repeating the
SLWR bounded least-squares solve on each platform. That solve feeds suspended
lengths and sampled turning points into mesh-window boundaries; small numerical
differences can change printed section lengths and integer grading decisions.
The general analytical solver remains available for independent checks.

The geometry retention repair preserved frozen physical values and input bytes.
The subsequent coefficient correction replaces positional `Cd` and `Ca` lists
with `Cdn: 1.2`, `Cdz: 0.008`, `Can: 1.0` and `Caz: 0` for every frozen line type.
The existing [line-type builder convention](../../src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py)
names `n` as normal and `z` as axial in `DEFAULT_HYDRO_COEFFS` and emits those
named fields in `_build_line_type_entry`. Normal applies equally to local x/y;
local z follows the line axis. `IncludeTorsion: No` is retained, consistent with
the [riser schema default](../../src/digitalmodel/solvers/orcaflex/modular_generator/schema/riser.py).
Equal normal coefficients require no separate material-axis orientation; enabling
torsion would introduce a different stiffness and orientation model. Native
readback must still confirm x/y/z drag of 1.2/1.2/0.008 and added mass of 1.0/1.0/0.
That frozen correction changed all 144 input digests, including both pipe and
buoyed line types, while counted sections and every other input field remained identical.
The audit's explicit coefficient mode accepts only the known old lists and the
four named replacements, checks unchanged counted sections and then compares
the complete remaining model. The default mesh-only audit remains available.

The original comparison matrix uses `Cdn: 1.2`, `Cdz: 0`, `Can: 1.0` and
`Caz: 0`, with torsion disabled. Its zero axial basis is retained independently
of the frozen tranche. The audit's `--original-coefficient-axes` option permits
only this replacement in all 288 original inputs and checks unchanged case
coverage, input metadata, geometry bytes, meshes and remaining model fields.
Parsed-axis regressions cover all 432 original line-type records. The current
repair preserves all 144 frozen input bytes relative to the preceding revision.

The catenary dependency's inextensible straight-line rejection is retained.
At chord length an inextensible weighted line has no finite sagging-catenary
solution; the guard rejects that singular case before root finding. Regression
tests cover rising, falling and horizontal chords. Retained geometry and section
bytes are compared exactly across the correction, so this dependency review
does not retune any campaign geometry.

Native serialization explicitly uses UTF-8, LF and finite binary64 float spelling.
The generated `runtime.json` records Python, operating system, architecture and
numerical/serialization library versions; its digest is part of the generated
index and review packet. Index metadata changes when source bytes or the
producing runtime change, even when all native input bytes remain identical.
The original comparison geometry continues to use the analytical solver and
is outside this frozen numerical snapshot's portability guarantee.

The audit is solver-free. It does not establish native MR0, G1 loading, statics,
baseline stress, perturbation stiffness or dynamic qualification. The following
steps remain required before dependent calculations:

1. Obtain independent review of the exact packet, source and input bytes. Any
   subsequent byte change invalidates that review and requires a fresh packet.
2. Obtain a current-revision release from the maintainer. Verify the packet,
   generated index, source snapshots and each input digest before loading.
   Retain the packet's case order and use the R10-W1 SCR/SLWR nominal pair for
   the first native readback.
3. Read back native Length and NumberOfSegments for every section and check
   positive integer counts and both adjacent ratios against 2.0. Compare total
   length, endpoints, buoyancy arcs, properties, offsets, headings, pressures
   and environments with the packet. Read back the normal x/y and axial z drag
   and added-mass coefficients for every line type and confirm torsion remains
   disabled. Check centreline/contact interpretation
   and retain all messages. Stop on a digest mismatch, renewed MR0 failure,
   load failure, changed physical input or undisposed warning.
4. Qualify matching native equilibria, whole-line baseline stress at both
   fibres and the 1 mm transverse, 15 s stiffness perturbation with linearity
   QA. Retain the existing 1.0 % allowable-stress uncertainty budget; a failure
   requires targeted refinement and disposition. Statics remain independent
   of RAO clearance; dynamic inputs remain blocked on qualified motions and
   matching converged equilibria.
5. Select the four governing QA statics and ten QA dynamics/fatigue inputs
   from completed native responses. Their pending status does not change.
   Separate hydrodynamic and robustness qualification remains outstanding.

To reproduce the audit, retain a reference output generated from the reviewed
revision and a candidate output from the current generator, then run:

```bash
PYTHONPATH=src:. uv run python -m scripts.riser_campaign.audit_mesh \
  --reference-dir <reference-output> --candidate-dir <candidate-output> \
  --reference-revision <reviewed-revision> --coefficient-axes --original-coefficient-axes \
  --output <new-audit-file>
```

The audit checks reference source snapshots against Git, verifies each indexed
file before comparing it and refuses unexpected physical changes or failing mesh
ratios. Candidate source snapshots must match the current working bytes. The audit
verifies the producing runtime record and binds its digest and versions; the
runtime used for the audit is recorded separately. It preserves existing outputs and rejects a mismatching existing audit file.
For the coefficient correction, the reference must contain the counted mesh and
the old positional lists. Omit `--coefficient-axes` only for the earlier audit
from target segment lengths to graded explicit counts.

The analytical reference uses the 1490 m hang-off separation and transforms
ASB elevation to global z by subtracting the physical 1500 m water depth. The
nominal sag/hog targets are 50/700 m ASB. Grounded length is derived from the
largest suspended-length increase across -90/0/+90 m plus 20 m, rounded upward
on the inherited 0.1 m grid. The anchor, line length and buoyancy material arcs
are then fixed. The full adverse axial-friction bound must retain at least 1 kN;
a failed bound raises an error rather than tuning geometry. The analytical
reference remains frictionless, separately from the production soil input of
100 kN/m³ normal/shear stiffness and 0.5 axial/lateral friction.

The reference centreline ends at the seabed elevation with an explicit zero
contact-radius correction. Native centreline/contact interpretation requires
readback before static release; this seed is not a contact-qualified equilibrium.
Any required radius correction must preserve the stated physical water depth
and be closure-tested in a subsequent reviewed change.

Positive span offset moves the floater in negative x while the anchor stays
fixed. Contents pressure is 20 MPa gauge at MSL with density 800 kg/m³; native
pressure is entered as 20000 kPa at reference z=0 and varies hydrostatically.
The external column uses 1025 kg/m³ water. All seven public current knots are
retained, including the explicitly assumed deep constant tail. Regular-wave
headings 0/180 degrees are paired with currents toward 15/195 degrees. The
full wave height is 1.86 Hs and the period is Tp, an assumed regular-wave proxy.
Nonlinear wave validity and native coefficient mappings require later QA.

Every frozen dynamic input is marked blocked in its header and index. Floater
motions are absent: no synthetic RAO, extrapolation or Froude scaling fills the
missing qualified motion packet. Exact-source hashes, complex phases, axes,
origin, heading and energy-bearing spectral-band clearance remain prerequisites.
Each dynamic row identifies its matching equilibrium dependency. The files are
native-format load templates for readback, not released dynamic models.

Fatigue inputs retain F03/F04/F05, their common paired seeds, nominal offset,
zero current, wave toward 180 degrees, gamma 1, 150 s build-up and 3600 s
stationary response at 0.05 s. The native wave period is explicitly Tp, not Tz.
Annual occurrences are 0.015, 0.010 and 0.013, without renormalisation. The
supplied SN80/SCF assumptions remain an air-equivalent benchmark. No rainflow,
damage, service life, native stress extraction or solver result is delivered:
**screening sensitivity: three sea states, not a full fatigue analysis.**

## Later licensed analysis and post-processing

The later run will consume the generated index and its exact manifest/schema/basis
snapshots. Each input will be hash-checked, loaded in the licensed solver and
qualified for native static equilibrium before dynamics. Missing native output,
load failures and failed equilibrium checks will remain explicit failed cases.
A zero exit status alone will not qualify a case. RAO origin, phase, direction,
units and draft will require independent review. Native load compatibility has
not been tested by this campaign preparation.

Before utilization can be calculated, the maintainer will specify a material
grade and yield strength, pressure/temperature conditions, corrosion allowance,
stress recovery method, governing design criterion and utilization limit. These
are not silently defaulted by this generator. Regular-wave sensitivity results
will not establish irregular-sea extreme or fatigue performance.

| Output required per case, offset and sea state | Units and retained evidence |
| --- | --- |
| Native input/run record | Case ID, offset ID, sea-state ID, RAO-set ID, input digest, manifest/schema/basis digests, pinned generator revision, criteria version, solver version, solver status, messages and result digest |
| Stress utilization versus material arc length | Arc length (m), component stresses (MPa), criterion/allowable and dimensionless utilization; separate missing strength basis from a true zero |
| Effective tension | Time (s), material arc length (m), effective tension (kN); minimum, maximum and time histories at hang-off, sag, hog and touchdown |
| Touchdown-point measures | Global position (m), arc coordinate (m), excursion range (m), curvature (1/m), bending moment (kN·m), contact force per length (kN/m), gap/penetration (m), and how touchdown was identified |
| Geometry and qualification | Node coordinates (m), unstretched arc lengths (m), equilibrium residuals with their units and limits, stage/time window (s), convergence and mesh evidence |

Table 3. Required later outputs; none is supplied as a licensed result by this preparation.

The later post-processor will join on case, offset and sea-state IDs, verify all
digests and plot utilization against material arc length by floater type, size,
RAO set and water depth. Geometry-screen results will remain distinct from
native stress/tension/contact results. No result archive, utilization curve,
newsletter figure or strength acceptance is delivered here.

## Baseline

None recorded at initial preparation; check results are reported with the change.

## Hooks that could not run

The hook framework is not part of the solver-free generation. File-level checks
are run directly during preparation; repository hooks run at commit time.

## Follow-up

Licensed loading, native equilibrium, dynamic response and utilization criteria
remain required before reporting utilization curves.
