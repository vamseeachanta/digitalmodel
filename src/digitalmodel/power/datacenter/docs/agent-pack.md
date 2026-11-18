# Data center power topology knowledge pack

| Header | Value |
| --- | --- |
| Module | power/datacenter |
| Repository | digitalmodel |
| Path | src/digitalmodel/power/datacenter |
| Pack revision | 3 |
| Code revision | 97dc074bad7e464d4b26ccd04bb83796ef4d0060 |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

Revision 0 code citations and offline results refer to the recorded code revision above.
Revision 1 updates the finite-input contract using symbol references; it does not
requalify the historical calculation examples.
Revision 2 names the local classifier, validates STS efficiency and states the
input-rating and equal-power-factor basis. The new hand calculations check the
local arithmetic; equipment sizing and certification remain unverified.
Revision 3 states the lower-bound conditions and downstream error comparator,
rejects boolean and non-real STS efficiencies, and pins the upper boundary.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-17 | Initial source-pinned knowledge pack and regression inventory. |
| 1 | 2026-10-19 | Reject nonfinite UPS/STS ratings and STS transfer times to prevent invalid capacities and timings; issue [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 2. |
| 2 | 2026-10-22 | Name the tier heuristic explicitly, reject invalid STS efficiency and document capacity assumptions with hand-calculated examples; issue [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), items 1, 3 and 4. |
| 3 | 2026-10-22 | State when the capacity approximation is a lower bound and quantify its downstream underestimate; reject boolean and non-real STS efficiencies and strengthen boundary coverage for #2749. |

## 1. Purpose and scope

The public API models components, ordered power paths, identity-based shared components and local integer tier classification. [src/digitalmodel/power/datacenter/__init__.py:L15-L31] [src/digitalmodel/power/datacenter/validator.py:L37-L107]
The containing power domain is beta; this is a domain maturity record rather than independent qualification of these calculations. [docs/registry/module-routing.yaml:L292-L297]
The scope is the complete datacenter package; the path model contains only an ordered component list and does not solve an electrical network. [src/digitalmodel/power/datacenter/topology.py:L16-L67]
UNVERIFIED: certification equivalence, design-load sufficiency and applicability limits have not been established; issue pending (3).

## 2. Inputs and outputs

| Input | Unit | Enforced range | Default | Source |
| --- | --- | --- | --- | --- |
| Generic rated_kva, efficiency | kVA, dimensionless | No runtime range check | 0.0, 1.0 | [src/digitalmodel/power/datacenter/models.py:L19-L41] |
| component_type, name | text | No enumerated restriction; empty name generated | Empty strings | [src/digitalmodel/power/datacenter/models.py:L24-L41] |
| UPS rated_kva, efficiency | kVA, dimensionless | Finite rated_kva > 0; finite 0 < efficiency <= 1 | Capacity inherited; efficiency 0.95 | `UPSModule.__post_init__` in `models.py` |
| battery_runtime_min, bypass_enabled | min, boolean | No runtime validation | 10.0, True | [src/digitalmodel/power/datacenter/models.py:L50-L75] |
| STS rated_kva, transfer_time_ms, efficiency | kVA, ms, dimensionless | Finite rated_kva > 0; finite transfer time >= 0; finite 0 < efficiency <= 1; boolean and non-real efficiency raises ValueError | Capacity inherited; transfer time 4.0; efficiency 1.0 (lossless) | `STSModule.__post_init__` in `models.py`; local contract, #2749, items 2 and 3 |
| preferred_source, alternate_source, retransfer_enabled | text, text, boolean | No runtime validation | A, B, True | [src/digitalmodel/power/datacenter/models.py:L94-L109] |
| path name, components | text, ordered objects | No topology constraints enforced | Required name; empty component list | [src/digitalmodel/power/datacenter/topology.py:L30-L35] |
| paths, redundancy_scheme | object list, enum | No minimum path count | Required paths; scheme None | [src/digitalmodel/power/datacenter/validator.py:L29-L35] |

| Output | Unit | Definition and limits |
| --- | --- | --- |
| UPS output_kva | kVA | Input-side rating multiplied by real-power efficiency, assuming equal, nonzero input/output power factors. `UPSModule.output_kva` in `models.py`. |
| cascaded_efficiency | dimensionless | Product of efficiencies; empty path returns 1.0. [src/digitalmodel/power/datacenter/topology.py:L37-L45] |
| bottleneck_kva, effective_capacity_kva | kVA | Minimum input-side rating, then minimum rating times full-chain efficiency; empty path bottleneck is 0.0. The approximation is a lower bound on propagated capacity only for finite nonnegative ratings and every efficiency in (0, 1]. `DataCenterPowerPath` properties in `topology.py`. |
| has_ups, has_sts | boolean | Instance-type predicates. [src/digitalmodel/power/datacenter/topology.py:L59-L67] |
| path_count, find_spofs, classify_local_tier, classify_tier | count, objects, integer | Path count; identity occurrence test; local levels 1 through 4. `classify_tier` remains a compatibility alias, with no certification equivalence. `TopologyValidator` in `validator.py`. |

The component and path dataclasses contain no coordinates, origin, phase, file reader or file writer; units are the attribute conventions above, with no conversion layer. [src/digitalmodel/power/datacenter/models.py:L19-L109] [src/digitalmodel/power/datacenter/topology.py:L16-L67]

## 3. Method and governing standards

| Method | Implementation | Basis and qualification |
| --- | --- | --- |
| UPS output | output_kva = rated_kva × efficiency | Local input-rating convention with equal, nonzero input/output power factors; derived from the real-power efficiency definition below. `UPSModule.output_kva`. |
| Chain efficiency | Product of every component efficiency | Cascading real-power output/input ratios under a common steady flow. `DataCenterPowerPath.cascaded_efficiency`. |
| Effective capacity | min(rated_kva) × product(efficiency) | Retained local approximation using input-side ratings; all losses apply regardless of bottleneck position. `DataCenterPowerPath.effective_capacity_kva`. |
| SPOF | Every component for one path; identity occurrences >= path count for multiple paths | Occurrences are counted, not distinct path membership. [src/digitalmodel/power/datacenter/validator.py:L49-L69] |
| Local tier heuristic | Ordered predicates: at least three paths, all STS, no detected SPOFs and 2n+1 gives 4; multiple paths with any STS and no detected SPOFs gives 3; any UPS gives 2; otherwise 1 | Historical local rules, not standards-derived thresholds; issue [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 1. `TopologyValidator.classify_local_tier`. |

The Uptime Institute compliance claim is removed. Its public presentation
*Mission Related - Critical Power Requirements* (2020), slide 11, distinguishes
basic capacity, redundant components, concurrent maintainability and fault
tolerance. The local model has no required-versus-installed component
counts, active-path states, maintenance switching or fault-isolation evaluation;
licensed Tier Standard clauses have not been checked. Certification is
NOT_EVALUATED. Three explicit divergences are retained and tested:

- A single nonredundant UPS returns local 2, without proving redundant capacity.
- Two paths without UPS, with STS on one, return local 3, without proving concurrent maintainability.
- Two independent STS paths declared 2N return local 3, since the local 4 predicate requires at least three paths and 2N+1; active-path status cannot be represented.

### Rating and power-factor convention

`rated_kva` denotes **input-side apparent power** for all component types in this
local calculation. It is not an equipment output nameplate rating. The
[ENERGY STAR UPS specification, version 2.0](https://www.energystar.gov/sites/default/files/ENERGY%20STAR%20Uninterruptible%20Power%20Supplies%20Final%20Version%202.0%20Specification_2.pdf),
Eligibility Criteria section 1 K, defines power factor as active/apparent power;
the included Test Method (December 2017), section 5 A, equation 2, defines
efficiency as real output/input power. Therefore, by algebra,
`S_out = efficiency × S_in × PF_in / PF_out`. Equal, nonzero power factors at
each input and output are assumed here, giving `S_out = efficiency × S_in`.
The module has no power-factor attribute and cannot check that assumption.

The chain product follows by cascading those ratios. Applying that product to
the minimum of all input ratings is a **local approximation**, not an ENERGY
STAR sizing procedure. It includes losses upstream of a downstream bottleneck
and is independent of component order. No propagation of nameplate limits,
power-factor conversion, load adequacy or certification is established. Supplied
output ratings require an independently established input-rating conversion;
the module does not perform it. No standards-derived equipment limit or
coefficient is introduced.

With finite nonnegative input ratings and every efficiency finite and in `(0, 1]`,
the approximation is a **lower bound** on the propagated deliverable capacity
`min_i(rated_kva_i × product(efficiency_j for j >= i))`. Each propagated
limit applies the component's own and downstream losses. The approximation
also applies upstream efficiencies, which cannot increase a nonnegative
limit within this domain. Generic `PowerComponent` inputs are unvalidated;
for example, efficiency above unity invalidates this lower-bound guarantee.
The underestimate depends on the ratings, efficiencies and component order;
the downstream example in section 4 quantifies one case.

## 4. Validated worked examples

Independent validated cases: 0; the six cases below are regression examples whose expected expressions or integer classifications are embedded in tests rather than an independent benchmark. [tests/power/datacenter/test_models.py:L55-L57] [tests/power/datacenter/test_topology.py:L37-L69] [tests/power/datacenter/test_examples.py:L27-L141]

| Regression case | Input and expected quantity | Tolerance | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| UPS output | 500.0 kVA × 0.95 = 475.0 kVA | pytest.approx default | `tests/power/datacenter/test_models.py::TestUPSModule::test_output_kva` | PASS at code pin, before and after; [tests/power/datacenter/test_models.py:L55-L57] |
| Chain efficiency | 0.98 × 0.95 × 0.99 dimensionless | pytest.approx default | `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_cascaded_efficiency_chain` | PASS at code pin, before and after; [tests/power/datacenter/test_topology.py:L37-L48] |
| Effective capacity | 500.0 × (0.98 × 0.95) kVA | pytest.approx default | `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_effective_capacity` | PASS at code pin, before and after; [tests/power/datacenter/test_topology.py:L62-L70] |
| Local tier II | One path with UPS; integer 2 | 0 tier levels | `tests/power/datacenter/test_examples.py::TestTierIIExample::test_tier_ii_topology` | PASS at code pin, before and after; [tests/power/datacenter/test_examples.py:L27-L61] |
| Local tier III | Two paths with STS on one; integer 3 | 0 tier levels | `tests/power/datacenter/test_examples.py::TestTierIIIExample::test_tier_iii_topology` | PASS at code pin, before and after; [tests/power/datacenter/test_examples.py:L64-L103] |
| Local tier IV | Three paths, all STS, declared 2n+1; integer 4 | 0 tier levels | `tests/power/datacenter/test_examples.py::TestTierIVExample::test_tier_iv_topology` | PASS at code pin, before and after; [tests/power/datacenter/test_examples.py:L106-L141] |

The input fixtures are inline Python constructions in these test nodes; no external dataset is consumed by these examples. [tests/power/datacenter/test_models.py:L55-L57] [tests/power/datacenter/test_topology.py:L37-L70] [tests/power/datacenter/test_examples.py:L27-L141]
The recorded offline command is `PYTHONPATH=src uv run --no-sources python -m pytest tests/power/datacenter -q -rA -p no:cacheprovider --basetemp=<scratch>`; both runs use code revision 97dc074bad7e464d4b26ccd04bb83796ef4d0060, with documentation changes only in the second run.
Run record: 34 run, 34 passed, 0 failed, 0 skipped, 0 NOT_RUN at BASE and after; no addopts override was used.

### Revision 2 worked example: upstream bottleneck

The following inputs are synthetic, selected for independent hand arithmetic;
they are not measured or assumed equipment performance. All ratings follow the
input-side convention, and every input/output power factor is assumed equal
and nonzero. The method and source equations are stated in section 3.

| Position | Component | Input rating (kVA) | Efficiency (dimensionless) |
| --- | --- | --- | --- |
| Upstream | Generic supply | 100.0 | 0.8 |
| Intermediate | UPS | 200.0 | 0.75 |
| Downstream | STS | 400.0 | 0.5 |

Table 1. Synthetic inputs for the local capacity example.

By hand, UPS output is `200 × 3/4 = 150.0 kVA`; chain efficiency is
`(4/5) × (3/4) × (1/2) = 3/10`; the bottleneck is `100.0 kVA`; effective
capacity is `100 × 3/10 = 30.0 kVA`. These are arithmetic expectations, not
independent equipment validation. The executable example is
`tests/power/datacenter/test_audit_regressions.py::test_upstream_bottleneck_uses_input_rating_and_full_chain_losses`.
Run it with `PYTHONPATH=src uv run python -m pytest tests/power/datacenter/test_audit_regressions.py`.
A second test places the minimum rating downstream and retains `30.0 kVA`,
explicitly checking the approximation against a propagated-capacity comparator.
Its ordered input ratings are `(400.0, 200.0, 100.0) kVA` and efficiencies
are `(0.5, 0.75, 0.8)`. The propagated limit is
`min(400 × 0.3, 200 × 0.6, 100 × 0.8) = 80.0 kVA`; the approximation is
`30.0 kVA`, or `50.0 kVA` (`62.5%`) below that limit. The test asserts both
the retained `30.0 kVA` result and `effective_capacity_kva <= 80.0`.
The upstream example gives `30.0 kVA` under both calculations. Neither
example establishes equipment adequacy or certification.

## 5. QA gates

The following are pack-specific checks under AQG r0, section 10, Python calculations, and section 9, QA appendix; a passing regression does not establish independent engineering qualification.

| Gate | Applicability | Check and numeric pass criterion | Executable evidence |
| --- | --- | --- | --- |
| G0 | Applicable: inputs, units, citations | C0a: UPS with efficiency 1.5 dimensionless and rating -100.0 kVA raises ValueError in each of 2 nodes; 0 unexpected exceptions. | `tests/power/datacenter/test_models.py::TestUPSModule::test_invalid_efficiency_raises`; `tests/power/datacenter/test_models.py::TestUPSModule::test_invalid_rated_kva_raises`. [tests/power/datacenter/test_models.py:L59-L65] |
| G0 | Applicable | C0b: default UPS runtime 10.0 min and efficiency 0.95 dimensionless, STS transfer 4.0 ms; 0 differences from explicit test constants. These check reproducibility, not adequacy. | `tests/power/datacenter/test_models.py::TestUPSModule::test_construction_defaults`; `tests/power/datacenter/test_models.py::TestSTSModule::test_construction_defaults`. [tests/power/datacenter/test_models.py:L34-L39] [tests/power/datacenter/test_models.py:L71-L77] |
| G1 | Applicable: Python import/test route | C1: API import node has 0 import errors and module suite has 34 passing nodes, 0 failures, 0 skips (unit: test count). | `tests/power/datacenter/test_examples.py::TestImportability::test_import_from_datacenter`; full module command in section 4. [tests/power/datacenter/test_examples.py:L13-L24] |
| G2 | Applicable: arithmetic hand check | C2: calculate 500.0 × 0.95 independently with decimal arithmetic; output_kva differs from 475.0 kVA by <= 0.001 kVA. This checks the formula implementation, not standards adequacy. | Execute `from decimal import Decimal; assert abs(Decimal("500")*Decimal("0.95")-Decimal("475")) <= Decimal("0.001")`, then the UPS output test. [tests/power/datacenter/test_models.py:L55-L57] [src/digitalmodel/power/datacenter/models.py:L72-L75] |
| G3 | NOT_APPLICABLE | No time-domain solver exists in this package. | [src/digitalmodel/power/datacenter/topology.py:L16-L67] [src/digitalmodel/power/datacenter/validator.py:L18-L107] |
| G4 | NOT_APPLICABLE | No diffraction mesh or hydrostatics result exists in this package. | [src/digitalmodel/power/datacenter/topology.py:L16-L67] [src/digitalmodel/power/datacenter/validator.py:L18-L107] |
| G5 | Applicable: evidence traceability | C5: all 6 section-4 nodes resolve and have results at the code pin, 0 missing results (unit: cases); all source citations resolve, 0 missing ranges (unit: citations). Independent signature remains required before clearance. | Section-4 node/result inventory and pinned citation readback; AQG r0, sections 9 and 11. |

G1 uses the ordinary Python package imports; the module defines no program-input export or licensed program load route. [src/digitalmodel/power/datacenter/__init__.py:L15-L31] [src/digitalmodel/power/datacenter/topology.py:L16-L67]

Revision 3 detection checks in `tests/power/datacenter/test_audit_regressions.py`:
`test_sts_rejects_invalid_efficiency` includes `math.nextafter(1.0, 2.0)`;
`test_sts_rejects_boolean_and_nonnumeric_efficiency` requires ValueError for
boolean, text, None and complex inputs without coercion;
`test_ups_rejects_nonfinite_efficiency` preserves rejection of NaN and both
infinities. The downstream case in section 4 checks the lower-bound comparator.

## 6. Known failure modes and pitfalls

sweep: 20 found

Sweep command: `rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|cache|0\.95|1\.0|10\.0|4\.0' src/digitalmodel/power/datacenter`; all four Python files were also read completely for defaults, units and error paths.
The sweep entries below distinguish normal defaults from missing rejection checks; they are not twenty confirmed defects.

| ID | Symptom or assumption | Detection check | Tracking and pinned location |
| --- | --- | --- | --- |
| F01 | NaN or infinity in UPS/STS ratings or STS transfer time can evade range-only comparisons. | `tests/power/datacenter/test_finite_inputs.py`: rejection and retained-boundary cases | [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 2; finite checks precede range checks in both constructors. |
| S01 | Generic rating defaults to 0.0 kVA with no rejection. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L34-L41] |
| S02 | Generic efficiency defaults to lossless 1.0 and has no bounds check. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L34-L41] |
| S03 | Empty generic type is accepted. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L34-L41] |
| S04 | Empty generic name is replaced by rounded capacity text; name is not identity. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L39-L41] |
| S05 | UPS uses assumed efficiency 0.95. | C0b records the default; no adequacy check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L56-L58] |
| S06 | UPS autonomy defaults to 10.0 min without range validation. | C0b records the default; no rejection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L56-L65] |
| S07 | UPS bypass defaults to True and does not enter the tier rules. | C0b records default; no functional detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L56-L58] [src/digitalmodel/power/datacenter/validator.py:L84-L107] |
| S08 | UPS substitutes type and generated name when empty. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L67-L70] |
| S09 | STS transfer defaults to 4.0 ms; negative and nonfinite times are rejected. | `test_reject_invalid_transfer_time`; default adequacy remains unverified | [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 2; `STSModule.__post_init__` in `models.py` |
| S10 | STS preferred/alternate labels default to A/B with no connectivity check. | C0b records labels; no connectivity detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L94-L109] |
| S11 | STS retransfer defaults to True; tier classification does not inspect it. | C0b records default; no functional detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L97-L109] [src/digitalmodel/power/datacenter/validator.py:L84-L107] |
| S12 | STS substitutes type and generated name when empty. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L106-L109] |
| S13 | New paths default to an empty component list. | no completeness detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/topology.py:L30-L35] |
| S14 | Empty path efficiency silently returns 1.0. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/topology.py:L37-L45] |
| S15 | Empty path bottleneck silently returns 0.0 kVA. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/topology.py:L47-L57] |
| S16 | Redundancy scheme defaults to None; only tier 4 checks its value. | no missing-scheme detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/validator.py:L29-L30] [src/digitalmodel/power/datacenter/validator.py:L90-L107] |
| S17 | No paths returns an empty SPOF list. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/validator.py:L49-L53] |
| S18 | No paths falls through to tier 1. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/validator.py:L84-L107] |
| S19 | Ratings assume kVA; no external-text or unit conversion is performed. | C2 checks one kVA result; no conversion detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L24-L41] [src/digitalmodel/power/datacenter/models.py:L72-L75] |
| S20 | Runtime and transfer time assume minutes and milliseconds. | C0b records defaults; no conversion detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L50-L58] [src/digitalmodel/power/datacenter/models.py:L84-L104] |
| F01 | SPOF documentation says multiple paths; implementation requires occurrence count >= total paths, permitting duplicate occurrences to distort membership. | no detection check exists for partial sharing or duplicate placement | issue pending (2); [src/digitalmodel/power/datacenter/validator.py:L40-L42] [src/digitalmodel/power/datacenter/validator.py:L59-L69] |
| F02 | Local tier rules are not an independent certification reference. | Divergence regressions in `test_audit_regressions.py` check local outcomes; no certification check exists | [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 1; `TopologyValidator.classify_local_tier`. |
| F03 | STS efficiency outside the finite interval (0, 1] previously allowed undefined, negative or inflated capacity. | Invalid values, smallest positive float and unity in `test_audit_regressions.py` | [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 3; `STSModule.__post_init__` now rejects invalid efficiency. |
| F04 | An output nameplate rating or unequal power factors invalidates the local input-rated interpretation. | Hand-derived UPS output and upstream/downstream bottleneck cases in `test_audit_regressions.py`; actual power-factor conformity is NOT_EVALUATED | [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 4; section 3 records the assumption and approximation. |

The multiplication accumulator 1.0 and empty Counter/dict initializers are normal algorithm identities, not error-result caching; no swallowed exception, skipped record, external numeric conversion, return None error path, 1970 fallback or cache write is present in the complete package bodies. [src/digitalmodel/power/datacenter/models.py:L1-L109] [src/digitalmodel/power/datacenter/topology.py:L1-L67] [src/digitalmodel/power/datacenter/validator.py:L1-L107] [src/digitalmodel/power/datacenter/__init__.py:L1-L31]
No incident issue is named in the scope; read-only issue searches for datacenter and SPOF returned no matching issue, so the confirmed limitations are tracked in the numbered drafts above.

Finite-input basis (revision 1): the [Python 3.11 Library Reference,
`math.isfinite`](https://docs.python.org/3.11/library/math.html#math.isfinite)
excludes NaN and both infinities while considering zero finite. Positive rating
and nonnegative transfer time remain local input-domain requirements from
[2749](https://github.com/vamseeachanta/digitalmodel/issues/2749), item 2, rather
than standards-derived equipment limits. The boundary tests derive expected
acceptance by hand in their docstrings. Generic component validation, mutable
post-construction attributes, tier definitions, STS efficiency and rating/power-factor
conventions were outside revision 1; revision 2 addresses the last three as
described below. No engineering qualification is established.

Revision 1 checks: all 57 datacenter test nodes passed, including 23 finite-input
cases. The historical 34-node results and inventory remain scoped to revision 0.

Revision 2 addresses [2749](https://github.com/vamseeachanta/digitalmodel/issues/2749),
items 1, 3 and 4: `classify_local_tier` names the local heuristic and
`classify_tier` delegates for compatibility; the three divergences in section 3
have regression cases. The declarations N/N+1 do not prove redundancy or change
those outcomes. STS efficiency now requires a finite value in (0, 1], a local
passive-component input domain rather than a certified equipment limit. The
rating and power-factor assumptions and both bottleneck positions are explicit.
Generic component validation, mutation after construction, partial sharing and
repeated-instance SPOF semantics remain limitations outside these audit fixes.
The F02 certification gap remains: the regression cases detect local behavior,
not compliance with a Tier Standard.
Revision 2 checks: all 73 datacenter nodes passed, including 16 new regression
cases. Before implementation, seven invalid STS-efficiency cases and four
local-classifier cases failed; the two hand-calculated capacity cases passed
under the existing arithmetic. The three positive-efficiency boundary cases
also preserve existing behavior.

Revision 3 retains the capacity formula and limits its lower-bound interpretation
to finite nonnegative ratings and passive efficiencies in `(0, 1]`. Boolean and
non-real STS efficiency inputs raise ValueError; an internal TypeError from
numeric validation is converted to that rejection rather than suppressed.
The revision-3 detection checks in section 5 cover this error path. Generic
components and post-construction mutation still do not enforce these bounds.
Revision 3 checks: 73 baseline nodes passed; the expanded regression file first
returned 4 failures and 21 passes for the intended constructor changes.
After implementation, all 82 datacenter nodes passed in both normal and UTF-8
modes, with 0 failures and 0 skips; package coverage is 95.6%.

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared log |
| --- | --- | --- | --- | --- | --- |
| 2026-10-17 | Initial pack revision 0 | Separate local tier regression from independent qualification. | [tests/power/datacenter/test_examples.py:L27-L141] [src/digitalmodel/power/datacenter/validator.py:L71-L107] | Sections 3–6 distinguish them. | issue pending (3) |

## 8. Ownership

Proposed roles: engineering calculations quality manager, independent engineering calculations deputy, and calculation documentation producer; clearance requires a different provider from the producer under AQG r0, section 11.
UNVERIFIED: named role assignments and deputy coverage are not established; issue pending (4).
Escalation: calculation criteria and reference disputes go to the family quality manager; unresolved scope and publication decisions go to the maintainer (AQG r0, section 11).

## 9. Dependencies and implementation inventory

The module imports standard-library dataclasses, enum, collections and math, plus its own sibling modules; tests use pytest. [src/digitalmodel/power/datacenter/models.py:L3-L6] [src/digitalmodel/power/datacenter/topology.py:L3-L12] [src/digitalmodel/power/datacenter/validator.py:L3-L14] [tests/power/datacenter/test_models.py:L6-L15]
The package exports six public symbols for downstream consumers; direct importability is exercised by its example test. [src/digitalmodel/power/datacenter/__init__.py:L24-L31] [tests/power/datacenter/test_examples.py:L13-L24]

Map pin: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `codex/implmap-work`; embedded code-data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`.
A8 inventory: 0 map families with implementation rows inside the package, 0 inventory rows, 0 counted families; counted-family grep: NOT_APPLICABLE because no family has an implementation row in the package.
The map's discovery-only TopologyValidator candidate is marked needs_review and excluded from implementation counts; it is not an implementation inventory row.
The map follow-up shall add UPS output, cascaded efficiency, bottleneck/effective capacity, identity-based SPOF and local tier classification from section 3; issue pending (4).
Map findings above are readback evidence at the separate map pin, not claims about a map at the code pin.

Whole-repository search: `git grep -n -E 'DataCenterPowerPath|classify_tier|find_spofs|cascaded_efficiency' <code-pin> -- '*.py'`; source implementations resolve to this package and tests exercise them, with no further production definition found by these exact symbols. [src/digitalmodel/power/datacenter/topology.py:L16-L67] [src/digitalmodel/power/datacenter/validator.py:L18-L107]
This symbol search includes scripts and is not a claim that differently named algorithms cannot exist; broader map enrollment remains issue pending (4).

## 10. Coverage and open items

Files read: the complete package `__init__.py`, `models.py`, `topology.py`, `validator.py`, and all five Python files under `tests/power/datacenter/`; the four test-bearing files contain 34 test functions. [src/digitalmodel/power/datacenter/__init__.py:L1-L31] [src/digitalmodel/power/datacenter/models.py:L1-L109] [src/digitalmodel/power/datacenter/topology.py:L1-L67] [src/digitalmodel/power/datacenter/validator.py:L1-L107] [tests/power/datacenter/test_models.py:L1-L108] [tests/power/datacenter/test_topology.py:L1-L90] [tests/power/datacenter/test_validator.py:L1-L144] [tests/power/datacenter/test_examples.py:L1-L141]
Discovery record: no package README or nested source subpackage exists at the pin; the docs text match for “data center” concerns weather rather than this module and is excluded from engineering evidence.
The routing registry power domain was read; the implementation map was read at its separate pin; the calculation-citation contract was readable and adds no new standards constant requirement for this documentation-only change.

| Open item | Evidence or limitation | Responsible role | Tracking |
| --- | --- | --- | --- |
| Input completeness and defaults | Section-6 S01–S20 require adequacy/rejection decisions, not just default regression. [src/digitalmodel/power/datacenter/models.py:L34-L109] [src/digitalmodel/power/datacenter/topology.py:L30-L57] | Calculation maintainer | issue pending (1) |
| SPOF semantics | Resolve partial sharing and repeated-instance criteria. [src/digitalmodel/power/datacenter/validator.py:L40-L69] | Calculation maintainer | issue pending (2) |
| Independent reference and validation | UNVERIFIED certification, applicability and standard attribution from sections 1 and 3; no independently sourced worked example. [src/digitalmodel/power/datacenter/validator.py:L71-L107] | Engineering calculations quality manager | issue pending (3) |
| QA evidence/signature | C5 traceability can be self-checked, but independent clearance is not established; AQG r0, sections 9 and 11. | Engineering calculations quality manager | issue pending (3) |
| Inventory/documentation and assignments | UNVERIFIED role assignments from section 8; enroll every section-3 method in the map and add module documentation. | Calculation maintainer | issue pending (4) |

Test inventory (all PASS in both offline runs; unit: test nodes):

- `tests/power/datacenter/test_examples.py::TestImportability::test_import_from_datacenter`. [tests/power/datacenter/test_examples.py:L15-L27]
- `tests/power/datacenter/test_examples.py::TestTierIIExample::test_tier_ii_topology`. [tests/power/datacenter/test_examples.py:L33-L62]
- `tests/power/datacenter/test_examples.py::TestTierIIIExample::test_tier_iii_topology`. [tests/power/datacenter/test_examples.py:L68-L98]
- `tests/power/datacenter/test_examples.py::TestTierIVExample::test_tier_iv_topology`. [tests/power/datacenter/test_examples.py:L104-L141]
- `tests/power/datacenter/test_models.py::TestRedundancyScheme::test_enum_values`. [tests/power/datacenter/test_models.py:L21-L25]
- `tests/power/datacenter/test_models.py::TestRedundancyScheme::test_enum_count`. [tests/power/datacenter/test_models.py:L27-L28]
- `tests/power/datacenter/test_models.py::TestUPSModule::test_construction_defaults`. [tests/power/datacenter/test_models.py:L34-L39]
- `tests/power/datacenter/test_models.py::TestUPSModule::test_construction_custom`. [tests/power/datacenter/test_models.py:L41-L53]
- `tests/power/datacenter/test_models.py::TestUPSModule::test_output_kva`. [tests/power/datacenter/test_models.py:L55-L57]
- `tests/power/datacenter/test_models.py::TestUPSModule::test_invalid_efficiency_raises`. [tests/power/datacenter/test_models.py:L59-L61]
- `tests/power/datacenter/test_models.py::TestUPSModule::test_invalid_rated_kva_raises`. [tests/power/datacenter/test_models.py:L63-L65]
- `tests/power/datacenter/test_models.py::TestSTSModule::test_construction_defaults`. [tests/power/datacenter/test_models.py:L71-L77]
- `tests/power/datacenter/test_models.py::TestSTSModule::test_construction_custom`. [tests/power/datacenter/test_models.py:L79-L90]
- `tests/power/datacenter/test_models.py::TestSTSModule::test_invalid_transfer_time_raises`. [tests/power/datacenter/test_models.py:L92-L94]
- `tests/power/datacenter/test_models.py::TestPowerComponent::test_construction`. [tests/power/datacenter/test_models.py:L100-L108]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_empty_path`. [tests/power/datacenter/test_topology.py:L22-L24]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_add_component`. [tests/power/datacenter/test_topology.py:L26-L30]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_cascaded_efficiency_single`. [tests/power/datacenter/test_topology.py:L32-L35]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_cascaded_efficiency_chain`. [tests/power/datacenter/test_topology.py:L37-L48]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_bottleneck_capacity`. [tests/power/datacenter/test_topology.py:L50-L60]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_effective_capacity`. [tests/power/datacenter/test_topology.py:L62-L70]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_component_order_preserved`. [tests/power/datacenter/test_topology.py:L72-L78]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_has_ups`. [tests/power/datacenter/test_topology.py:L80-L84]
- `tests/power/datacenter/test_topology.py::TestDataCenterPowerPath::test_has_sts`. [tests/power/datacenter/test_topology.py:L86-L90]
- `tests/power/datacenter/test_validator.py::TestSPOFDetection::test_single_path_has_spof`. [tests/power/datacenter/test_validator.py:L57-L60]
- `tests/power/datacenter/test_validator.py::TestSPOFDetection::test_dual_path_no_spof`. [tests/power/datacenter/test_validator.py:L62-L65]
- `tests/power/datacenter/test_validator.py::TestSPOFDetection::test_spof_identifies_component`. [tests/power/datacenter/test_validator.py:L67-L71]
- `tests/power/datacenter/test_validator.py::TestSPOFDetection::test_shared_utility_is_spof`. [tests/power/datacenter/test_validator.py:L73-L89]
- `tests/power/datacenter/test_validator.py::TestTierClassification::test_tier_i_no_redundancy`. [tests/power/datacenter/test_validator.py:L95-L99]
- `tests/power/datacenter/test_validator.py::TestTierClassification::test_tier_ii_single_path_with_ups`. [tests/power/datacenter/test_validator.py:L101-L105]
- `tests/power/datacenter/test_validator.py::TestTierClassification::test_tier_iii_dual_path_with_sts`. [tests/power/datacenter/test_validator.py:L107-L113]
- `tests/power/datacenter/test_validator.py::TestTierClassification::test_tier_iv_fully_fault_tolerant`. [tests/power/datacenter/test_validator.py:L115-L133]
- `tests/power/datacenter/test_validator.py::TestTierClassification::test_redundancy_scheme_validation`. [tests/power/datacenter/test_validator.py:L135-L140]
- `tests/power/datacenter/test_validator.py::TestTierClassification::test_path_count`. [tests/power/datacenter/test_validator.py:L142-L144]
