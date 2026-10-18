# Data center power topology knowledge pack

| Header | Value |
| --- | --- |
| Module | power/datacenter |
| Repository | digitalmodel |
| Path | src/digitalmodel/power/datacenter |
| Pack revision | 0 |
| Code revision | 97dc074bad7e464d4b26ccd04bb83796ef4d0060 |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

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
| UPS rated_kva, efficiency | kVA, dimensionless | rated_kva > 0; 0 < efficiency <= 1 | Capacity inherited; efficiency 0.95 | [src/digitalmodel/power/datacenter/models.py:L56-L75] |
| battery_runtime_min, bypass_enabled | min, boolean | No runtime validation | 10.0, True | [src/digitalmodel/power/datacenter/models.py:L50-L75] |
| STS rated_kva, transfer_time_ms | kVA, ms | rated_kva > 0; transfer time >= 0 | Capacity inherited; transfer time 4.0 | [src/digitalmodel/power/datacenter/models.py:L94-L109] |
| preferred_source, alternate_source, retransfer_enabled | text, text, boolean | No runtime validation | A, B, True | [src/digitalmodel/power/datacenter/models.py:L94-L109] |
| path name, components | text, ordered objects | No topology constraints enforced | Required name; empty component list | [src/digitalmodel/power/datacenter/topology.py:L30-L35] |
| paths, redundancy_scheme | object list, enum | No minimum path count | Required paths; scheme None | [src/digitalmodel/power/datacenter/validator.py:L29-L35] |

| Output | Unit | Definition and limits |
| --- | --- | --- |
| UPS output_kva | kVA | Rated capacity multiplied by efficiency. [src/digitalmodel/power/datacenter/models.py:L72-L75] |
| cascaded_efficiency | dimensionless | Product of efficiencies; empty path returns 1.0. [src/digitalmodel/power/datacenter/topology.py:L37-L45] |
| bottleneck_kva, effective_capacity_kva | kVA | Minimum rating, then minimum rating times full-chain efficiency; empty path bottleneck is 0.0. [src/digitalmodel/power/datacenter/topology.py:L47-L57] |
| has_ups, has_sts | boolean | Instance-type predicates. [src/digitalmodel/power/datacenter/topology.py:L59-L67] |
| path_count, find_spofs, classify_tier | count, objects, integer | Path count; identity occurrence test; tier values 1 through 4. [src/digitalmodel/power/datacenter/validator.py:L32-L107] |

The component and path dataclasses contain no coordinates, origin, phase, file reader or file writer; units are the attribute conventions above, with no conversion layer. [src/digitalmodel/power/datacenter/models.py:L19-L109] [src/digitalmodel/power/datacenter/topology.py:L16-L67]

## 3. Method and governing standards

| Method | Implementation | Basis and qualification |
| --- | --- | --- |
| UPS output | output_kva = rated_kva × efficiency | Local arithmetic convention. [src/digitalmodel/power/datacenter/models.py:L72-L75] |
| Chain efficiency | Product of every component efficiency | Local arithmetic convention. [src/digitalmodel/power/datacenter/topology.py:L37-L45] |
| Effective capacity | min(rated_kva) × product(efficiency) | Implemented approximation; component locations are not used to propagate loads. [src/digitalmodel/power/datacenter/topology.py:L47-L57] |
| SPOF | Every component for one path; identity occurrences >= path count for multiple paths | Occurrences are counted, not distinct path membership. [src/digitalmodel/power/datacenter/validator.py:L49-L69] |
| Tier | Ordered predicates: at least three paths, all STS, no SPOFs and 2n+1 gives 4; multiple paths with any STS and no SPOFs gives 3; any UPS gives 2; otherwise 1 | Local classification rules. [src/digitalmodel/power/datacenter/validator.py:L84-L107] |

UNVERIFIED: the Uptime Institute attribution has no publication title, edition or clause in the implementation; standards conformity and a governing licensed clause are not established, clause text not checked against the licensed copy; issue pending (3). [src/digitalmodel/power/datacenter/models.py:L9-L15] [src/digitalmodel/power/datacenter/validator.py:L71-L82]
The calculation bodies return scalars or lists, without a Citation sidecar; no standards-derived constant is introduced by this documentation change. [src/digitalmodel/power/datacenter/models.py:L72-L75] [src/digitalmodel/power/datacenter/topology.py:L37-L57] [src/digitalmodel/power/datacenter/validator.py:L49-L107]

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

## 6. Known failure modes and pitfalls

sweep: 20 found

Sweep command: `rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|cache|0\.95|1\.0|10\.0|4\.0' src/digitalmodel/power/datacenter`; all four Python files were also read completely for defaults, units and error paths.
The sweep entries below distinguish normal defaults from missing rejection checks; they are not twenty confirmed defects.

| ID | Symptom or assumption | Detection check | Tracking and pinned location |
| --- | --- | --- | --- |
| S01 | Generic rating defaults to 0.0 kVA with no rejection. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L34-L41] |
| S02 | Generic efficiency defaults to lossless 1.0 and has no bounds check. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L34-L41] |
| S03 | Empty generic type is accepted. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L34-L41] |
| S04 | Empty generic name is replaced by rounded capacity text; name is not identity. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L39-L41] |
| S05 | UPS uses assumed efficiency 0.95. | C0b records the default; no adequacy check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L56-L58] |
| S06 | UPS autonomy defaults to 10.0 min without range validation. | C0b records the default; no rejection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L56-L65] |
| S07 | UPS bypass defaults to True and does not enter the tier rules. | C0b records default; no functional detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L56-L58] [src/digitalmodel/power/datacenter/validator.py:L84-L107] |
| S08 | UPS substitutes type and generated name when empty. | no detection check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L67-L70] |
| S09 | STS transfer defaults to 4.0 ms; only negative times are rejected. | C0b records default; no adequacy check exists | issue pending (1); [src/digitalmodel/power/datacenter/models.py:L94-L105] |
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
| F02 | Local tier rules are not an independent certification reference. | no detection check exists | issue pending (3); [src/digitalmodel/power/datacenter/validator.py:L71-L107] |

The multiplication accumulator 1.0 and empty Counter/dict initializers are normal algorithm identities, not error-result caching; no swallowed exception, skipped record, external numeric conversion, return None error path, 1970 fallback or cache write is present in the complete package bodies. [src/digitalmodel/power/datacenter/models.py:L1-L109] [src/digitalmodel/power/datacenter/topology.py:L1-L67] [src/digitalmodel/power/datacenter/validator.py:L1-L107] [src/digitalmodel/power/datacenter/__init__.py:L1-L31]
No incident issue is named in the scope; read-only issue searches for datacenter and SPOF returned no matching issue, so the confirmed limitations are tracked in the numbered drafts above.

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
