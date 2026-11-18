# Lifting-lug module knowledge pack

| Header | Value |
| --- | --- |
| Package | `src/digitalmodel/lifting_lug` |
| Code pin | `1d9d0018aaf61db4cbba3d6c4d5864a34adbf4b0` |
| Status | DRAFT; software contracts documented, engineering qualification incomplete |
| Import | `digitalmodel.lifting_lug` |

Summary: this pack records the implemented screening interfaces and their
limits. Source locators establish software behavior; they do not establish
engineering acceptance. Synthetic examples check arithmetic only. Governing
criteria and applicability remain NOT_EVALUATED.

### Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-11-05 | Record the public inventory, synthetic example and qualification gaps so documentation drift is detectable. |

## 1. Purpose and scope

Three interfaces coexist: circular-padeye workflow screening, typed nominal
plate/direct-weld assessment, and an offline rectangular welded-plate demand
study. Their geometry, units and status conventions differ. No interface alone
establishes lifting qualification or code compliance.
[src/digitalmodel/lifting_lug/workflow.py:L4-L18]
[src/digitalmodel/lifting_lug/assessment.py:L4-L11]
[src/digitalmodel/lifting_lug/welded_study.py:L1-L6]

Inventory policy: section 2 includes public function/class definitions in
all package Python files and explicit public class members. Re-exports have
one defining-source row. Constants, private helpers, nested local functions and
generated dataclass methods have no separate rows. Field defaults and function
signatures below describe implementation choices, not qualified design values.
[scripts/docs/check_agent_pack_functions.py:L1-L10]

## 2. Inputs and outputs

The table records exact Python fields or signatures. Required fields have no
assignment; explicit assignments are software defaults. Dataclass fields are
also the result schema when a calculation returns that class. The method and
validation notes following the table explain units and constraints. Property
rows show getter signatures; access those members as attributes.

| Public symbol | Fields or call signature | Output contract | Evidence |
| --- | --- | --- | --- |
| `PlateGeometry` | `thickness_mm: float; pin_diameter_mm: float; hole_diameter_mm: float; width_mm: float; centre_to_loaded_edge_mm: float` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/assessment.py:L38-L56] |
| `PlateGeometry.demands` | `PlateGeometry.demands(self, load_kn: float)` | PlateDemands | [src/digitalmodel/lifting_lug/assessment.py:L51-L56] |
| `SlingLoad` | `static_load_kn: float; dynamic_amplification: float; skew_factor: float; basis: str` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/assessment.py:L60-L78] |
| `StressLimit` | `allowable_mpa: float; source: str` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/assessment.py:L82-L95] |
| `PadeyeAllowables` | `pin_bearing: StressLimit \| None = None; net_tension: StressLimit \| None = None; shear_tearout: StressLimit \| None = None; weld_direct_shear: StressLimit \| None = None` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/assessment.py:L99-L105] |
| `DirectWeld` | `effective_length_mm: float; effective_throat_mm: float` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/assessment.py:L109-L124] |
| `CheckResult` | `name: str; demand_mpa: float \| None; allowable_mpa: float \| None; utilization: float \| None; status: Status; method_source: str; allowable_source: str; reason: str` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/assessment.py:L128-L138] |
| `PadeyeAssessment` | `plate: PlateGeometry; load: SlingLoad; allowables: PadeyeAllowables; weld: DirectWeld \| None; design_load_kn: float; checks: tuple[CheckResult, ...]; qualification_checks: tuple[CheckResult, ...]; required_weld_throat_mm: float \| None; screening_status: Status; status: Status` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/assessment.py:L142-L154] |
| `assess_padeye` | `assess_padeye(plate: PlateGeometry, load: SlingLoad, allowables: PadeyeAllowables, weld: DirectWeld \| None=None)` | PadeyeAssessment | [src/digitalmodel/lifting_lug/assessment.py:L185-L247] |
| `PlateDemands` | `pin_bearing: float; net_tension: float; shear_tearout: float` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/mechanics.py:L22-L27] |
| `plate_demands_mpa` | `plate_demands_mpa(load_kn: float, thickness_mm: float, pin_diameter_mm: float, hole_diameter_mm: float, width_mm: float, centre_to_loaded_edge_mm: float)` | PlateDemands | [src/digitalmodel/lifting_lug/mechanics.py:L30-L73] |
| `render_report` | `render_report(result: PadeyeAssessment)` | HTML string; no file write. | [src/digitalmodel/lifting_lug/report.py:L18-L161] |
| `Load` | `gross_kip: float; factor: float; lug_count: int; source: str` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/welded_study.py:L98-L120] |
| `Load.fy` | `Load.fy(self)` | Factored vertical reaction per lug, kip. | [src/digitalmodel/lifting_lug/welded_study.py:L114-L120] |
| `Geometry` | `width: float; height: float; hole_x: float; hole_y: float; hole_radius: float; weld_leg: float; source: str` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/welded_study.py:L124-L166] |
| `Geometry.scaled` | `Geometry.scaled(self, scale)` | Scaled width, height and hole offsets, in. | [src/digitalmodel/lifting_lug/welded_study.py:L147-L155] |
| `Geometry.invalid_reason` | `Geometry.invalid_reason(self, scale)` | Reason string or None. | [src/digitalmodel/lifting_lug/welded_study.py:L157-L166] |
| `demands` | `demands(geometry, load, thickness, scale, weld_multiplier, angle, direction)` | Nine signed demands keyed by UNITS; kip, kip-in, kip/in and ksi. | [src/digitalmodel/lifting_lug/welded_study.py:L169-L214] |
| `build_study` | `build_study(geometry, load)` | Dictionary of grid rows, provenance, units, limits and content revision. | [src/digitalmodel/lifting_lug/welded_study.py:L297-L319] |
| `validate` | `validate(data)` | Same dictionary or ValueError. | [src/digitalmodel/lifting_lug/welded_study.py:L322-L328] |
| `lookup` | `lookup(data, revision, parameters, intended_use)` | Independent copy of exact grid row or error. | [src/digitalmodel/lifting_lug/welded_study.py:L331-L344] |
| `write_outputs` | `write_outputs(data, directory)` | None; writes versioned JSON, CSV and HTML. | [src/digitalmodel/lifting_lug/welded_study.py:L347-L351] |
| `render_report` | `render_report(data)` | HTML string; no file write. | [src/digitalmodel/lifting_lug/welded_study_output.py:L147-L150] |
| `write_outputs` | `write_outputs(data, directory)` | None; writes versioned JSON, CSV and HTML. | [src/digitalmodel/lifting_lug/welded_study_output.py:L190-L205] |
| `LugCheck` | `name: str; demand_mpa: float; allowable_mpa: float; utilization: float; passes: bool` | Dataclass record; fields shown at left. | [src/digitalmodel/lifting_lug/workflow.py:L39-L44] |
| `design_load_kn` | `design_load_kn(static_load_kn: float, dynamic_amplification: float, skew_factor: float)` | float | [src/digitalmodel/lifting_lug/workflow.py:L47-L57] |
| `lug_checks` | `lug_checks(design_load_kn: float, total_thickness_mm: float, pin_diameter_mm: float, hole_diameter_mm: float, outer_radius_mm: float, yield_strength_mpa: float)` | list[LugCheck] | [src/digitalmodel/lifting_lug/workflow.py:L60-L93] |
| `router` | `router(cfg: dict)` | dict | [src/digitalmodel/lifting_lug/workflow.py:L107-L164] |

Table 1. Public definitions, including methods and properties.

The SI mechanics and assessment interfaces use kN, mm and MPa. Load factors
and utilization are dimensionless. Plate geometry requires finite positive
dimensions, pin no larger than hole, width larger than hole and positive loaded
edge ligament. Mechanics accepts finite nonnegative load, including zero. The
typed load requires explicit factor basis; limits require positive finite MPa
and nonempty source text. Missing limits or effective weld geometry produce
NOT_EVALUATED checks. Source text is retained without authentication.
[src/digitalmodel/lifting_lug/mechanics.py:L30-L73]
[src/digitalmodel/lifting_lug/assessment.py:L27-L124]

The circular workflow uses kN, mm and MPa, requires positive load/factors and
compares to internal yield-based limits. Its router mutates cfg and writes JSON;
explicit skew input is required. Its defaults differ from the typed API.
[src/digitalmodel/lifting_lug/workflow.py:L47-L93]
[src/digitalmodel/lifting_lug/workflow.py:L107-L176]

The welded study uses inches, kip and degrees from upward vertical. Load takes
unfactored gross weight, a positive explicit factor, integer positive lug count
and source text. Geometry takes a rectangular profile, hole offsets/radius and
weld leg, all in inches, plus source text. Constructor finiteness checks do not
establish geometry validity: invalid_reason evaluates ligaments at a requested
scale. demands requires positive thickness, scale and weld multiplier, an angle
in the supported interval and direction exactly minus or plus one.
[src/digitalmodel/lifting_lug/welded_study.py:L98-L214]

## 3. Method and governing sources

Nominal plate demands divide force by projected bearing area, net tensile area
and two rectangular tear-out ligaments. Width is across the hole and loaded-edge
distance is measured from its centre. These idealizations exclude contact peaks,
pin bending, out-of-plane load, hoop tension and interacting failure modes.
[src/digitalmodel/lifting_lug/mechanics.py:L30-L73]

Typed assessment multiplies the supplied static tension by both supplied factors.
Direct weld shear uses total effective length times effective throat, with no
implicit doubling or leg conversion. Required throat rearranges the same
nominal demand equation using the supplied allowable. Screening compares demand
to allowable; overall status also includes unresolved qualification checks.
Thus passing nominal stresses does not make overall qualification PASS.
[src/digitalmodel/lifting_lug/assessment.py:L157-L247]

The circular workflow maps outline radius to plate width and edge distance and
combines main/cheek thickness. It uses internal yield multipliers and imported
leg-count skew minima. The source labels these with AISC and DNV references;
this pack neither reproduces licensed factors nor independently qualifies them.
The typed assessment accepts explicit limits instead and does not inherit those
workflow resistance policies.
[src/digitalmodel/lifting_lug/workflow.py:L32-L35]
[src/digitalmodel/lifting_lug/workflow.py:L60-L93]
[src/digitalmodel/lifting_lug/workflow.py:L107-L164]

The welded study resolves a vertical reaction and horizontal component, forms
the signed moment at the base, distributes direct and moment loads over two
co-projected equal straight weld lines, and calculates nominal rectangular plate
stresses. build_study evaluates its fixed parameter grid in both directions,
marks invalid geometry and duplicate zero-angle direction explicitly, and stores
absolute-response envelopes with signed values. Content revisions detect
accidental mutation; they are not approval signatures. lookup requires a matching
revision, exact grid node and screening use; interpolation is absent.
[src/digitalmodel/lifting_lug/welded_study.py:L169-L214]
[src/digitalmodel/lifting_lug/welded_study.py:L235-L344]

The source embeds references to elementary stress/strain, shear, equilibrium
and beam-stress texts and to lifting qualification requirements. These are code
provenance records, not independently reviewed publications in this pack.
[src/digitalmodel/lifting_lug/mechanics.py:L4-L13]
[src/digitalmodel/lifting_lug/assessment.py:L21-L23]
[src/digitalmodel/lifting_lug/welded_study.py:L43-L78]

| Topic | Status | Evidence needed before engineering use |
| --- | --- | --- |
| Workflow resistance and load-factor rules | NOT_EVALUATED | Governing editions, clauses and applicability review; internal constants alone are insufficient. |
| Supplied typed allowables and load basis | NOT_EVALUATED | Authenticated material, allowable and loading evidence for intended geometry and duty. |
| Weld detailing and load path | NOT_EVALUATED | Effective dimensions, centroid/load alignment, practical minima and any weld-group moments. |
| Lifting qualification | NOT_EVALUATED | Applicable testing, approval and complete failure-mode coverage. |
| Historical study reproduction | NOT_EVALUATED | Qualified source profile, original geometry and benchmark evidence. |
| Physical/solver comparison | NOT_EVALUATED | Independent benchmark or measured/solver evidence for the intended domain. |

Table 2. Open qualification items. No primary engineering publication or licensed
criterion value is qualified or reproduced by this pack.

## 4. Worked examples and regression examples

This synthetic section-arithmetic example supplies no allowable or design verdict.
Choose 10 kN, thickness 10 mm, pin/hole diameters 20 mm, width 60 mm and
centre-to-loaded-edge distance 30 mm. Independently, force is 10000 N and
areas are 200 mm2 for bearing, 400 mm2 for tension and 400 mm2 for two-plane
tear-out. Dividing gives 50, 25 and 25 MPa. Equal pin/hole diameters are a
software boundary case, not a fit recommendation.

```python
from digitalmodel.lifting_lug.mechanics import plate_demands_mpa

result = plate_demands_mpa(
    load_kn=10.0, thickness_mm=10.0, pin_diameter_mm=20.0,
    hole_diameter_mm=20.0, width_mm=60.0, centre_to_loaded_edge_mm=30.0,
)
print(result.pin_bearing, result.net_tension, result.shear_tearout)
```

Expected printed values: `50.0 25.0 25.0`. The documentation test executes this
exact block and compares the three demands with the independent area calculation.

## 5. QA gates and executable checks

Run with the declared test dependencies available and scratch under TMPDIR.
The inventory checker itself uses only the standard library and imports no solver.

```bash
uv run --no-project --with pytest python -m pytest \
  tests/lifting_lug tests/workflows/test_lifting_lug_design.py \
  tests/workflows/test_welded_padeye_study.py tests/docs/test_screening_agent_packs.py \
  tests/docs/test_agent_pack_functions.py tests/docs/test_digitalmodel_routing_contract.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/lifting-pack-tests"
uv run --no-project python scripts/docs/check_agent_pack_functions.py \
  --pack src/digitalmodel/lifting_lug/docs/agent-pack.md
```

Documentation tests check all three screening pack inventories, registration,
ten-section form, locator bounds, examples and CLI drift failures in temporary
copies of real packages. Existing mechanics, assessment, report and study tests
cover runtime behavior; none independently certifies engineering applicability.
`tests/docs/test_screening_agent_packs.py`

## 6. Known failure modes and pitfalls

- Do not mix SI mechanics with the inch/kip welded study. SlingLoad assumes
  tension already includes angle; study Load derives each equal vertical
  reaction from unfactored gross weight and applies its factor once.
  [src/digitalmodel/lifting_lug/assessment.py:L60-L79]
  [src/digitalmodel/lifting_lug/welded_study.py:L98-L120]
- The typed direct weld assumes concentric direct shear only. Its screening
  status is distinct from qualification. Missing limits are unevaluated, and
  overall status remains unevaluated unless a failed check makes it FAIL.
  [src/digitalmodel/lifting_lug/assessment.py:L109-L124]
  [src/digitalmodel/lifting_lug/assessment.py:L177-L247]
- Circular thickness aggregation and internal allowables do not resolve cheek
  load transfer or interaction. The workflow also fails a below-minimum entered
  skew factor even when its design calculation uses the governing minimum.
  [src/digitalmodel/lifting_lug/workflow.py:L107-L164]
- Study lookup returns a row with validity and qualification statuses, not
  necessarily calculated values. Digest validation detects content edits but
  does not authenticate provenance. Off-grid requests raise KeyError.
  [src/digitalmodel/lifting_lug/welded_study.py:L235-L282]
  [src/digitalmodel/lifting_lug/welded_study.py:L322-L344]
- Output functions have side effects. Circular JSON output may overwrite an
  existing summary; study output requires a new or empty directory. Two source
  modules define write_outputs and two define render_report: both definitions
  appear separately in section 2, distinguished by source path.
  [src/digitalmodel/lifting_lug/workflow.py:L167-L176]
  [src/digitalmodel/lifting_lug/welded_study_output.py:L190-L205]

## 7. Lessons log

A package inventory must include secondary assessment and study interfaces, not
only the engine router. Source-qualified duplicate names need separate rows.
Software status and engineering qualification must remain visible separately;
a nominal stress result alone cannot close a lifting approval requirement.

## 8. Maintenance and review

The maintainers own this pack. Changes to any package definition require an
inventory check; changes to stress models, factors, geometry or status policy
also require a method/limitation review. A reissue adds a revision-history row.
Independent engineering review must resolve section 3 before design acceptance.

## 9. Dependencies and other implementations

Mechanics and typed assessment use the standard library; reporting adds HTML
escaping. The circular router imports installation-analysis skew policy, bringing
that module's NumPy/Pydantic requirements and package initialization dependencies.
The welded study uses dataclasses, Decimal, hashing, JSON and parameter products;
its output module adds CSV and temporary staging. No licensed solver is run by
these interfaces. No independent program comparison is supplied by this pack.
[src/digitalmodel/lifting_lug/mechanics.py:L15-L18]
[src/digitalmodel/lifting_lug/assessment.py:L13-L19]
[src/digitalmodel/lifting_lug/workflow.py:L21-L30]
[src/digitalmodel/orcaflex/installation_analysis.py:L16-L21]
[src/digitalmodel/lifting_lug/welded_study.py:L8-L16]
[src/digitalmodel/lifting_lug/welded_study_output.py:L1-L11]

Existing study derivation: `docs/domains/lifting/welded-padeye.html`.
Routing: `docs/registry/module-routing.yaml`, module lifting_lug, agent_pack.

## 10. Coverage and open items

Focused tests cover plate mechanics and boundaries, explicit allowable handling,
report output, circular workflow and the offline study's signed demands, grid
lookup and outputs. New pack tests add definition identity, locator, registry,
example and drift contracts. Full-repository execution, physical validation,
code qualification, practical detailing and independent solver comparison remain
NOT_EVALUATED. No runtime algorithm, factor or acceptance policy is changed.
