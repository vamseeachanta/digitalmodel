# Materials knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | materials / digitalmodel |
| Package | `src/digitalmodel/materials` |
| Pack revision / date | 0 / 2026-10-17 |
| Code revision | `bd924737657b9932040fb6e8bdf0bbd7307584fa` |
| Analysis family | Engineering calculations (EC), AQG r0 Python calculations |
| Review role | Engineering calculations quality manager |
| Status | DRAFT; independent review pending |

## 1. Purpose and scope

The package provides a steel-grade registry and compatibility views, a line-pipe geometry catalogue, FFS material inputs and an indexed metadata projection; the routing registry records development maturity. [src/digitalmodel/materials/__init__.py:L15-L48] [src/digitalmodel/materials/ffs_material_library.py:L47-L52] [src/digitalmodel/materials/database.py:L125-L141] [docs/registry/module-routing.yaml:L577-L582]

The registry contains API 5L, IACS UR W11, EN 10025-2, ASTM A312 and ASTM A790 groups; its integrity test requires 40 grades rather than the older validation record's 33. [src/digitalmodel/materials/grades.py:L137-L210] [tests/materials/test_grade_matrix.py:L136-L146] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L66-L70]

The FFS routines estimate ferritic-steel toughness and create monotonic tensile curves; creep and fatigue curves are excluded, and recorded assessment temperature does not derate material properties. [src/digitalmodel/materials/ffs_material_library.py:L10-L45] [src/digitalmodel/materials/ffs_material_library.py:L51-L52] [src/digitalmodel/materials/ffs_material_library.py:L568-L572]

The line-pipe factory returns geometry and a reference string, with no product-standard or material-class check after grade lookup; its API 5L label therefore does not establish product qualification. [src/digitalmodel/materials/line_pipe.py:L218-L223] [src/digitalmodel/materials/line_pipe.py:L297-L321]

## 2. Inputs and outputs

### Inputs

| API / input | Unit / convention | Allowed input and default | Evidence |
| --- | --- | --- | --- |
| `get(name)`, `by_standard(standard)` | Text; no spatial frame | Canonical names, aliases and case normalization; standard query uses substring matching; neither has a default name | [src/digitalmodel/materials/grades.py:L224-L257] |
| `smts_mpa(name, psl=...)`, database `lookup` | MPa; PSL token | Default PSL2; a PSL1 request without a stored minimum falls back; other tokens select the canonical value | [src/digitalmodel/materials/grades.py:L265-L273] [src/digitalmodel/materials/database.py:L78-L89] |
| `line_pipe(grade, nps/od_in, schedule/wt_in)` | Geometry in inches | Exactly one diameter route and one wall route; schedule requires NPS; wall positive and less than half the OD; dimensions have no defaults | [src/digitalmodel/materials/line_pipe.py:L139-L160] [src/digitalmodel/materials/line_pipe.py:L273-L321] |
| `MaterialGrade` construction | MPa, kg/m³, dimensionless Poisson ratio | Elastic/density/class defaults in source; frozen record has no post-init value validation | [src/digitalmodel/materials/grades.py:L27-L88] |
| `lower_bound_toughness` | J, °C, mm, MPa; Pint quantities converted | Method defaults to auto, class to ferritic; reference routes require temperature and yield; method-specific reference thickness, fractile, cap and scope constants remain in source | [src/digitalmodel/materials/ffs_material_library.py:L84-L114] [src/digitalmodel/materials/ffs_material_library.py:L216-L230] [src/digitalmodel/materials/ffs_material_library.py:L267-L368] |
| `TrueStressStrainCurve` | MPa, °C; true/plastic strain dimensionless | Positive moduli/strengths; exponent strictly between 0 and 1; ultimate strain defaults to exponent; temperature default is recorded only | [src/digitalmodel/materials/ffs_material_library.py:L395-L448] |
| Curve evaluation / tables | Tensile strain positive; stress MPa | Negative strain rejected; total/plastic tables default to 20 rows; plateau strain defaults to twice ultimate; minimum row counts 4/3 respectively | [src/digitalmodel/materials/ffs_material_library.py:L469-L552] |
| `Strengths`, `MaterialRecord`, `RepositoryProvenance` | MPa and repository metadata | Finite positive strengths, tensile at least yield; nonempty identities; full revision/digest and ISO date; unestablished links remain null | [src/digitalmodel/materials/database_schema.py:L20-L37] [src/digitalmodel/materials/database_schema.py:L50-L73] [src/digitalmodel/materials/database_schema.py:L86-L113] |

### Outputs

| API / output | Unit / format | Meaning and limit | Evidence |
| --- | --- | --- | --- |
| Grade and legacy views | MPa, psi, Pa or kN/m² according to helper suffix | Canonical fields and historical snapshots are distinct; compatibility views deliberately preserve historical values | [src/digitalmodel/materials/grades.py:L260-L283] [src/digitalmodel/materials/grades.py:L296-L350] [src/digitalmodel/materials/grades.py:L396-L448] |
| `LinePipe` properties | in, mm, in², kg/m, lb/ft, m³/m, bbl/ft; D/t dimensionless | ID, area, weight and bore volume; weight uses a fixed density rather than the selected grade density | [src/digitalmodel/materials/line_pipe.py:L126-L151] [src/digitalmodel/materials/line_pipe.py:L163-L215] |
| `ToughnessEstimate` | MPa√m; method, class, °C, mm and notes | `float(estimate)` returns toughness; notes retain extrapolation and capping dispositions | [src/digitalmodel/materials/ffs_material_library.py:L168-L184] [src/digitalmodel/materials/ffs_material_library.py:L329-L368] |
| Curve and tables | `(true_strain, MPa)` or `(MPa, plastic_strain)` | Elastic, hardening and ultimate plateau; no deck file or solver object is written here | [src/digitalmodel/materials/ffs_material_library.py:L480-L552] |
| Database records | Immutable mapping; stable string key | Specification, edition or explicit unknown token, designation, product level or inapplicable token; source-digest flag is separate from individual provenance | [src/digitalmodel/materials/database_schema.py:L115-L121] [src/digitalmodel/materials/database.py:L55-L89] |

These scalar material and section quantities have no coordinate origin or phase convention; the tensile curve rejects negative strains, and inputs carry units by suffix or Pint conversion rather than a geometry file format. [src/digitalmodel/materials/grades.py:L43-L62] [src/digitalmodel/materials/line_pipe.py:L139-L151] [src/digitalmodel/materials/ffs_material_library.py:L84-L110] [src/digitalmodel/materials/ffs_material_library.py:L480-L488]

## 3. Method and governing standards

All licensed references below mean **clause text not checked against the licensed copy**; standard table values and coefficients are intentionally left at their cited code locations. [docs/domains/materials/database-gaps.html:L31-L49]

| Method / reference | Edition and clause evidence | Implementation and disposition |
| --- | --- | --- |
| API 5L / ISO 3183, Line Pipe | API 5L 46th edition; code names Tables 4/5 collectively; ISO edition unspecified in grade source, while the register groups both under 2018 | `_api5l`, registry and `get`; ISO/PSL selection is a code convention pending source qualification, not a claim of standard equivalence. [src/digitalmodel/materials/grades.py:L10-L16] [src/digitalmodel/materials/grades.py:L94-L103] [src/digitalmodel/materials/grades.py:L137-L157] [docs/domains/codes-register.md:L109-L114] |
| IACS UR W11, Normal and Higher Strength Hull Structural Steels | Revision unspecified; no numbered clause/table identified | `_hull`, normal/high-strength registry, `_charpy_test_temperature`; unknown classes may yield null. [src/digitalmodel/materials/grades.py:L95-L109] [src/digitalmodel/materials/grades.py:L159-L178] [src/digitalmodel/materials/ffs_material_library.py:L558-L561] [src/digitalmodel/materials/ffs_material_library.py:L617-L621] |
| ASME B36.10M, Welded and Seamless Wrought Steel Pipe | Edition and numbered dimensional table unspecified | `NPS_OD_IN`, `SCHEDULE_WT_IN`, `od_for_nps`, `wt_for_schedule`; section calculations use annular area, area × density and bore area. [src/digitalmodel/materials/line_pipe.py:L12-L17] [src/digitalmodel/materials/line_pipe.py:L34-L51] [src/digitalmodel/materials/line_pipe.py:L163-L215] [src/digitalmodel/materials/line_pipe.py:L234-L270] |
| BS 7910, flaw assessment | Annex J; edition absent from this package; related register names 2019 while the offering plan names 2013, so applicable edition is unresolved | Charpy route delegates to `asset_integrity.assessment.crack_fad.kmat_from_charpy`; it is not an independent implementation here. [src/digitalmodel/materials/ffs_material_library.py:L17-L20] [src/digitalmodel/materials/ffs_material_library.py:L299-L306] [docs/domains/codes-register.md:L19-L20] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L26-L28] |
| API 579-1/ASME FFS-1, Fitness-For-Service | 2007 Annex F F.4.4 / F.2.3; 2016 and later Annex 3D 3D.4.4 / 3D.2.3; register names 2021 | `lower_bound_toughness`, `TrueStressStrainCurve`; the curve is an input-driven Ludwik model, not the standard's tabulated material model. [src/digitalmodel/materials/ffs_material_library.py:L10-L45] [src/digitalmodel/materials/ffs_material_library.py:L374-L410] [docs/domains/codes-register.md:L19-L20] |
| ASTM E1921, Master Curve; Wallin, Master curve analysis of the Euro fracture toughness dataset, 2002 | Source comments identify the paper and scope paragraph; standard edition unspecified | `_master_curve_mpa_sqrt_m`, Charpy-transition route and thickness adjustment; source coefficients are cited rather than reproduced. [src/digitalmodel/materials/ffs_material_library.py:L123-L144] [src/digitalmodel/materials/ffs_material_library.py:L197-L207] [src/digitalmodel/materials/ffs_material_library.py:L314-L347] |
| ASME BPVC XI, Nonmandatory Appendix A, A-4200; source comments name EPRI NP-719-SR and NUREG/CR-6609 | Standard edition unspecified; primary-source correspondence not established here | `_asme_curve_ksi_sqrt_in` and Kic/Kir routes, degree conversion, class/yield bounds and capping. [src/digitalmodel/materials/ffs_material_library.py:L146-L162] [src/digitalmodel/materials/ffs_material_library.py:L210-L213] [src/digitalmodel/materials/ffs_material_library.py:L349-L368] |
| EN 10025-2, Hot rolled structural steel; ASTM A312/A312M and A790/A790M, stainless steel pipe | Grade source omits editions and numbered tensile tables; register names EN 2019 | `_en`, `_austenitic`, `_duplex` and their registry rows. [src/digitalmodel/materials/grades.py:L112-L134] [src/digitalmodel/materials/grades.py:L180-L204] [docs/domains/codes-register.md:L120-L121] |
| Monotonic tensile model | Input-driven elastic relation, plastic-strain power law and ultimate plateau; default ultimate strain follows the cited Considere construction | `stress_from_plastic_strain`, `stress` bisection, `plastic_strain`, `_plastic_grid`, `table`, `plastic_table`; convergence uses the relative bracket criterion in code. [src/digitalmodel/materials/ffs_material_library.py:L378-L410] [src/digitalmodel/materials/ffs_material_library.py:L469-L552] |
| Unit and compatibility views | Engineering conversions and explicitly preserved source snapshots | `MaterialGrade.smys_psi/smts_psi`; `smys_mpa/smts_mpa`, psi/Pa/kN/m² helpers and all seven `legacy_*` adapters; database is a projection, not a new strength dataset. [src/digitalmodel/materials/grades.py:L80-L88] [src/digitalmodel/materials/grades.py:L260-L350] [src/digitalmodel/materials/grades.py:L396-L448] [src/digitalmodel/materials/database.py:L98-L141] |

The package carries source strings and repository provenance, but no `Citation` sidecar is constructed by these APIs; sidecar adoption and edition qualification remain open under #1204 and issue pending (3). [src/digitalmodel/materials/grades.py:L58-L78] [src/digitalmodel/materials/database.py:L98-L116] [src/digitalmodel/materials/ffs_material_library.py:L299-L368]

## 4. Validated worked examples

The synthetic endpoint case below checks a prescribed true-stress plateau without using standard grade values; it establishes the boundary behavior only, not material calibration or the hardening branch. [tests/materials/test_ffs_material_library.py:L268-L273]

| ID / kind | Independent basis / input | Expected / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 validated closed-form boundary | Synthetic prescribed true ultimate, explicit ultimate strain, already-true input; no input file | 600 MPa at dimensionless strain 0.15 and 0.3; relative tolerances 1e-9 and 1e-12 are test numerical tolerances, not source uncertainty | `tests/materials/test_ffs_material_library.py::test_curve_explicit_ultimate_strain_and_true_ultimate_input` | BASE PASS; AFTER PASS [tests/materials/test_ffs_material_library.py:L268-L273] |
| R1 regression, historical worked workflow | Existing grade/schedule construction, no independent source opened | Weight equals code metal area × code density, relative tolerance 1e-12; weight conversion relative tolerance 1e-6 | `tests/materials/test_line_pipe.py::test_weight_is_area_times_density` | BASE PASS; AFTER PASS [tests/materials/test_line_pipe.py:L56-L61] |
| R2 regression | Canonical/ISO aliases | 0 designation mismatches, dimensionless count | `tests/materials/test_grade_matrix.py::test_iso_designation_alias_resolves` | BASE PASS; AFTER PASS [tests/materials/test_grade_matrix.py:L105-L108] |
| R3 regression | Shared Charpy routine | 0 implementation-identity mismatches; relative output difference at most 1e-12, MPa√m quantity | `tests/materials/test_ffs_material_library.py::test_charpy_fallback_is_the_crack_fad_implementation` | BASE PASS; AFTER PASS [tests/materials/test_ffs_material_library.py:L44-L52] |
| R4 regression | Database projected from grade records | 0 strength/citation mismatches, MPa and dimensionless mismatch counts | `tests/materials/test_database.py::test_values_and_citations_equal_the_source_records` | BASE PASS; AFTER PASS [tests/materials/test_database.py:L53-L63] |
| R5 regression | Typed solver-table rows | 25 total-strain rows and 10 plastic rows; 0 nonmonotonic pairs, dimensionless counts | `tests/materials/test_ffs_material_library.py::test_curve_table_for_solver_decks` | BASE PASS; AFTER PASS [tests/materials/test_ffs_material_library.py:L276-L295] |

The existing dimensional and grade validation records claim comparison with published standards; those original sources were not opened for this pack, so their source qualification remains unresolved (U1) and they are not promoted into independent validation here. [docs/domains/materials/line-pipe-validation-2026-06-28.md:L22-L43] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L34-L50]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, with G5 required by section 9; the checks below are the proposed module-specific acceptance record, not independent gate signatures. (AQG r0, sections 9–11.)

| Gate | Applicability | Numeric pass criterion / unit | Executable check / evidence |
| --- | --- | --- | --- |
| G0 | Applicable: inputs, units, citations | C01–C06 below | Input, provenance and method tests |
| G1 | Applicable: Python imports and tests | C07: 0 collection errors and 0 failures, count | Python pytest route; no solver file is emitted by the package |
| G2 | Applicable: hand or published check | C08: V1 error ≤ 1e-9 relative at ultimate and ≤ 1e-12 relative on plateau, dimensionless; stress MPa | V1; boundary-only coverage |
| G3 | NOT_APPLICABLE: no dynamics integration in these scalar/material APIs | Not a numeric result | [src/digitalmodel/materials/line_pipe.py:L163-L215] [src/digitalmodel/materials/ffs_material_library.py:L469-L552] |
| G4 | NOT_APPLICABLE: no diffraction body or mesh in these APIs | Not a numeric result | [src/digitalmodel/materials/__init__.py:L15-L48] [src/digitalmodel/materials/ffs_material_library.py:L69-L77] |
| G5 | Applicable: evidence traceability | C09–C10 below; 0 unresolved result/citation associations, count | Pack receipt, digest and traceability checks; downstream report appendix not established |

| Check | Gate | Module-specific check and numeric criterion | Execution / evidence |
| --- | --- | --- | --- |
| C01 | G0 | `Strengths` rejects invalid, nonpositive, inverted or wrong-unit strengths: 0 unexpected acceptances, count; strengths MPa | `tests/materials/test_database.py::test_invalid_strength_is_rejected` and adjacent nonpositive/unit tests. [tests/materials/test_database.py:L107-L126] |
| C02 | G0 | Database rejects duplicate keys, malformed provenance, missing strengths and unestablished foreign keys: 0 unexpected acceptances, count | `tests/materials/test_database.py::test_duplicate_keys_are_rejected`, `tests/materials/test_database.py::test_malformed_provenance_is_rejected`, `tests/materials/test_database.py::test_missing_strengths_are_rejected`, `tests/materials/test_database.py::test_unestablished_foreign_keys_are_rejected`. [tests/materials/test_database.py:L95-L104] [tests/materials/test_database.py:L129-L148] [tests/materials/test_database.py:L200-L204] |
| C03 | G0 | `line_pipe` rejects listed ambiguous diameter/wall and invalid catalogue inputs: 0 unexpected acceptances, count; geometry inches | `tests/materials/test_line_pipe.py::test_errors`; this does not test non-finite geometry or product-grade suitability. [tests/materials/test_line_pipe.py:L128-L140] |
| C04 | G0 | FFS wrong-unit, positional, class, missing-method-input and scope guards: 0 unexpected acceptances, count; J/°C/MPa | `tests/materials/test_ffs_material_library.py::test_toughness_wrong_unit_quantity_rejected`, `tests/materials/test_ffs_material_library.py::test_toughness_rejects_non_ferritic_material_class`, `tests/materials/test_ffs_material_library.py::test_toughness_requires_the_inputs_of_the_chosen_method`; scope and temperature tests in same range. [tests/materials/test_ffs_material_library.py:L148-L228] |
| C05 | G0 | `get` preserves known aliases and unknown-grade rejection; API A and hull Grade A remain distinct: 0 identity mismatches, count | `tests/materials/test_grade_matrix.py::test_iso_designation_alias_resolves`, `tests/materials/test_grade_matrix.py::test_api_a_and_hull_grade_a_do_not_collide`, `tests/materials/test_grade_matrix.py::test_get_unknown_raises`. [tests/materials/test_grade_matrix.py:L105-L125] |
| C06 | G0 | Stored null links and absent editions remain explicit: 0 invented associations/editions, count | `tests/materials/test_database.py::test_missing_associations_are_explicitly_null`, `tests/materials/test_database.py::test_edition_metadata_is_not_inferred`; these do not qualify original standards. [tests/materials/test_database.py:L78-L92] |
| C07 | G1 | Import and execute all collected materials nodes: 0 collection errors and 0 failed nodes, count | `python -m pytest tests/materials/ -q -rA -p no:cacheprovider`; imports are the actual Python load route. [tests/materials/test_database.py:L7-L10] [tests/materials/test_ffs_material_library.py:L26-L36] |
| C08 | G2 | Synthetic prescribed ultimate/plateau stress = 600 MPa; relative errors ≤ 1e-9/1e-12 respectively | V1 test node in section 4. [tests/materials/test_ffs_material_library.py:L268-L273] |
| C09 | G5 | `normalised_source_digest` equals pinned provenance and load flag is true: 0 digest mismatches, count | `tests/materials/test_database.py::test_provenance_pins_the_repository_source`, `tests/materials/test_database.py::test_source_drift_requires_refreshed_provenance`; line-ending and deliberate-drift tests exercise detection. [tests/materials/test_database.py:L66-L75] [tests/materials/test_database.py:L151-L182] |
| C10 | G5 | Resolve every pack citation at code pin; 0 missing files/out-of-range citations and 0 missing section-4 node results, count | `git show <code-pin>:<citation-path>` with inclusive line checks, then compare section-4 node IDs with pytest results; this is a pack acceptance procedure, not an implemented report generator. [tests/materials/test_database.py:L66-L75] |
| C11 | G1 | Total/plastic curve tables have 25/10 rows, 0 descending stress or strain pairs, count; stresses MPa | R5; curve negative-input/units checks supplement this. [tests/materials/test_ffs_material_library.py:L276-L329] |
| C12 | G0 | Auto method route and tagged-unit equivalence: 0 method mismatches; relative toughness difference ≤ 1e-6, dimensionless | `tests/materials/test_ffs_material_library.py::test_toughness_auto_method_selection`, `tests/materials/test_ffs_material_library.py::test_toughness_tagged_quantities_are_converted`. [tests/materials/test_ffs_material_library.py:L193-L228] |
| C13 | G1 | Fixed-density pipe weight and conversion consistency: relative differences ≤ 1e-12 and ≤ 1e-6, dimensionless; kg/m and lb/ft quantities | R1. [tests/materials/test_line_pipe.py:L56-L61] |
| C14 | G1 | Both legacy FFS dictionaries equal their live consumer views: 0 differing entries, count | `tests/materials/test_grade_matrix.py::test_legacy_smys_psi_matches_corroded_pipe`, `tests/materials/test_grade_matrix.py::test_legacy_smts_psi_matches_dnv_rp_f101`. [tests/materials/test_grade_matrix.py:L94-L103] |
| C15 | G1 | Database product-level selection equals existing reconciliation, including unknown token and absent PSL1: 0 differing values/levels, count; MPa | `tests/materials/test_database.py::test_psl_selection_matches_existing_reconciliation`. [tests/materials/test_database.py:L29-L42] |

Checks defined: G0 = 7, G1 = 5, G2 = 1, G5 = 2; total = 15, with G3/G4 inapplicable under the cited family mapping. (AQG r0, section 10.)

## 6. Known failure modes and pitfalls

sweep: 36 found

The sweep inventories 36 distinct default, nullable, unit, conversion or fallback sites below; guarded normal behavior is included so that a search match is not mistaken for a defect. [src/digitalmodel/materials/database_schema.py:L59-L113] [src/digitalmodel/materials/ffs_material_library.py:L84-L114] [src/digitalmodel/materials/line_pipe.py:L245-L270]

Exact text sweep, followed by reading all six package files and enclosing definitions:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|\b(float|int)\(|= 0([.]0)?\b|0[.]05|default|units|density|temperature|cache' src/digitalmodel/materials
rg -n 'except|continue|return|float\(|int\(|\.get\(|None|0\.0|0\.05|1970|write|cache' src/digitalmodel/materials --glob '*.py'
```

| Site | Symptom / disposition | Detection / tracking |
| --- | --- | --- |
| S01 `grades.get` | Last-character hull fallback precedes aliases; unrelated suffix text can select a grade and blank text raises indexing error. [src/digitalmodel/materials/grades.py:L232-L246] | Ordinary unknown-grade coverage C05 does not detect these inputs; no detection check exists; issue pending (1) |
| S02 `by_standard` | Empty substring returns all records; no-match substring returns empty list. [src/digitalmodel/materials/grades.py:L254-L257] | no detection check exists; issue pending (1) |
| S03 `smts_mpa` | Missing PSL1 minimum and unrecognized PSL text return canonical strength. [src/digitalmodel/materials/grades.py:L265-L273] | C15 characterizes fallback, but no qualification rejection check exists; #1204 [tests/materials/test_database.py:L29-L42] |
| S04 `MaterialGrade` elastic defaults | Per-family modulus assumptions feed constructors without temperature calibration. [src/digitalmodel/materials/grades.py:L27-L32] [src/digitalmodel/materials/grades.py:L99-L134] | Regression constants exist; original qualification remains under #1204; no detection check exists for source accuracy |
| S05 `MaterialGrade.nu` | Common Poisson default applies unless replaced. [src/digitalmodel/materials/grades.py:L33-L33] [src/digitalmodel/materials/grades.py:L69-L71] | no detection check exists; #1204 |
| S06 `MaterialGrade.rho_kg_m3` | Generic density default and stainless overrides are inherited assumptions. [src/digitalmodel/materials/grades.py:L34-L36] [src/digitalmodel/materials/grades.py:L69-L71] [src/digitalmodel/materials/grades.py:L122-L134] | no detection check exists for original density qualification; #1204 |
| S07 `MaterialGrade.material_class` | Default ferritic class; direct construction has no numeric/class validation. [src/digitalmodel/materials/grades.py:L39-L88] | C04 covers FFS rejection only; no detection check exists for invalid direct grade records; issue pending (1) |
| S08 grade unit views | MPa/psi conversion is approximate; Pa/kN/m² helpers assume canonical MPa; SMTS legacy view rounds before `float`. [src/digitalmodel/materials/grades.py:L23-L25] [src/digitalmodel/materials/grades.py:L80-L88] [src/digitalmodel/materials/grades.py:L281-L283] [src/digitalmodel/materials/grades.py:L304-L337] | C14 historical parity tests exist; no independent conversion-accuracy check exists; issue pending (3) [tests/materials/test_grade_matrix.py:L94-L103] |
| S09 legacy snapshots | Per-consumer strength differences persist by design; substituting canonical values changes the engineering basis. [src/digitalmodel/materials/grades.py:L340-L393] | C14 covers the two FFS dictionaries only; no detection check exists for independent qualification of all preserved snapshots; convergence #1147 [tests/materials/test_grade_matrix.py:L94-L103] |
| S10 line-pipe unit constants | Inch/SI, mass/length and bore-volume factors are fixed assumptions. [src/digitalmodel/materials/line_pipe.py:L27-L31] [src/digitalmodel/materials/line_pipe.py:L183-L215] | C13 covers weight conversion; bore test only checks positivity/consistency; no detection check exists for independent barrel conversion; issue pending (3) [tests/materials/test_line_pipe.py:L76-L81] |
| S11 line-pipe density | Weight uses fixed source density regardless of grade. [src/digitalmodel/materials/line_pipe.py:L126-L127] [src/digitalmodel/materials/line_pipe.py:L197-L200] | C13 characterizes fixed-density behavior; no detection check exists for product-density mismatch; issue pending (1) |
| S12 `available_schedules` | Unsupported NPS returns an empty list via empty-dict default. [src/digitalmodel/materials/line_pipe.py:L245-L247] | no detection check exists; issue pending (1) |
| S13 `wt_for_schedule` | Missing table and unknown schedule raise rather than silently default. [src/digitalmodel/materials/line_pipe.py:L256-L270] | C03; guarded behavior |
| S14 `LinePipe.__post_init__`, factory | Comparison-only checks admit NaN; any grade is accepted before an API 5L label is built. [src/digitalmodel/materials/line_pipe.py:L153-L160] [src/digitalmodel/materials/line_pipe.py:L218-L223] [src/digitalmodel/materials/line_pipe.py:L297-L321] | C03 covers ordinary errors; no detection check exists for non-finite/product inputs; issue pending (1) |
| S15 `_edition` | No edition regex match produces None. [src/digitalmodel/materials/database.py:L92-L95] | C06; intentional unknown metadata; #1204 |
| S16 database lookup | Unrecognized or unsupported PSL request selects a stored fallback level. [src/digitalmodel/materials/database.py:L78-L89] | C15 records fallback; not product qualification; #1204 [tests/materials/test_database.py:L29-L42] |
| S17 database digest flag | Direct indexes default to false; loader still returns records when digest drifts. [src/digitalmodel/materials/database.py:L37-L38] [src/digitalmodel/materials/database.py:L59-L67] [src/digitalmodel/materials/database.py:L125-L141] | C09; callers must inspect flag; #1204 |
| S18 `RepositoryProvenance` | Terms defaults describe repository reuse and leave original-source qualification unresolved; date parse exception re-raises. [src/digitalmodel/materials/database_schema.py:L50-L73] | C02/C09; no swallowed exception; #1204 |
| S19 `MaterialRecord.edition` | `continue` skips text checks only for intentional null edition. [src/digitalmodel/materials/database_schema.py:L97-L104] | C06; #1204 |
| S20 `sn_curve_ids` | Null default is absent grade-to-detail evidence, not an empty association list. [src/digitalmodel/materials/database_schema.py:L78-L94] | C02/C06; #1204 |
| S21 `standards_registry_id` | Null default is unresolved registry linkage. [src/digitalmodel/materials/database_schema.py:L78-L94] | C02/C06; #1204 |
| S22 `structural_shape_id` | Null default is unresolved shape linkage. [src/digitalmodel/materials/database_schema.py:L78-L94] | C02/C06; #1204 |
| S23 `Strengths.units` | MPa default with finite-positive and dimensional token validation. [src/digitalmodel/materials/database_schema.py:L20-L37] | C01; guarded behavior |
| S24 `_magnitude` | Bare reals assume suffix units; quantity conversion casts to float; string/bool/non-finite inputs rejected; dimensionality exception re-raises. [src/digitalmodel/materials/ffs_material_library.py:L84-L110] | C04/C12; no swallowed exception |
| S25 `_optional` | Missing input returns None; route-specific validation occurs later. [src/digitalmodel/materials/ffs_material_library.py:L113-L114] [src/digitalmodel/materials/ffs_material_library.py:L299-L324] | C04; guarded optional inputs |
| S26 automatic route | Multiple supplied reference inputs are resolved by fixed precedence, potentially leaving inputs unused. [src/digitalmodel/materials/ffs_material_library.py:L277-L287] | C12 covers ordinary route selection; no detection check exists for competing inputs; issue pending (2) |
| S27 fracture defaults | Fractile defaults to 0.05; reference thickness and class/yield/window limits are implicit source assumptions; source coefficients include a zero temperature shift. [src/digitalmodel/materials/ffs_material_library.py:L123-L165] [src/digitalmodel/materials/ffs_material_library.py:L216-L230] | C04 tests scope; no detection check exists for independent coefficient-source qualification; issue pending (3) |
| S28 extrapolation | Explicit extrapolation bypasses temperature-window rejection but appends a note. [src/digitalmodel/materials/ffs_material_library.py:L329-L337] | C04 temperature-window test; flagged behavior [tests/materials/test_ffs_material_library.py:L166-L173] |
| S29 toughness caps / ASME scope | Finite nonpositive caps are accepted; ASME yield only has upper-bound check and returned thickness lacks positivity check. [src/digitalmodel/materials/ffs_material_library.py:L344-L368] | no detection check exists for these inputs; issue pending (2) |
| S30 curve defaults | Assessment temperature is recorded; unspecified ultimate strain becomes exponent, with endpoint checks but no thermal derating. [src/digitalmodel/materials/ffs_material_library.py:L395-L448] | C04/C11 cover ordinary input guards; no detection check exists for source/material calibration; issue pending (3) |
| S31 curve strain conversion | `float()` on total/plastic strain has no finite or unit validation; plastic strain delegates through total stress. [src/digitalmodel/materials/ffs_material_library.py:L469-L505] | no detection check exists for NaN/infinity or textual strain; issue pending (2) |
| S32 total table | `float(strain_max)` lacks finite validation; absent maximum becomes twice ultimate; zero origin row is deliberate. [src/digitalmodel/materials/ffs_material_library.py:L513-L532] | C11 covers ordinary rows; no detection check exists for non-finite maxima; issue pending (2) |
| S33 plastic table | Same maximum default/conversion risk; zero initial plastic strain is deliberate. [src/digitalmodel/materials/ffs_material_library.py:L534-L552] | C11 covers ordinary rows; no detection check exists for non-finite maxima; issue pending (2) |
| S34 `_charpy_test_temperature` | Non-hull or missing class returns None; unknown table key also returns None. [src/digitalmodel/materials/ffs_material_library.py:L617-L621] | Existing hull lookup characterizes non-hull None; no detection check exists for malformed hull class; issue pending (2) [tests/materials/test_ffs_material_library.py:L368-L371] |
| S35 FFS input record / lookup | Default temperature records assessment conditions without derating; missing hardening exponent remains None and curve creation rejects it; `with_properties` copies without independent record validation. [src/digitalmodel/materials/ffs_material_library.py:L564-L608] [src/digitalmodel/materials/ffs_material_library.py:L624-L654] | C11 checks constructed curves; no detection check exists for invalid replacement records; issue pending (2) |
| S36 hull temperature tables | Unit-tagged table constants include zero entries; a test temperature is not an assessment-temperature toughness measurement. [src/digitalmodel/materials/ffs_material_library.py:L558-L561] [src/digitalmodel/materials/ffs_material_library.py:L617-L621] | Hull lookup test characterizes one entry only; no detection check exists for original table qualification; #1204 [tests/materials/test_ffs_material_library.py:L368-L371] |

No record-skipping loop, swallowed exception, integer parsing of external text, epoch-year fallback or cache write appears in the six-file package; the one `continue` and two exception handlers above implement nullable metadata or re-raise failures. [src/digitalmodel/materials/database_schema.py:L66-L73] [src/digitalmodel/materials/database_schema.py:L97-L104] [src/digitalmodel/materials/ffs_material_library.py:L92-L110]

Additional confirmed documentation defects are the historical grade-count mismatch and incomplete edition/provenance qualification; C06/C09 characterize the metadata boundary, while original-source and documentation freshness checks do not exist, issue pending (3). [tests/materials/test_grade_matrix.py:L136-L146] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L66-L70] [docs/domains/materials/database-gaps.html:L31-L49]

## 7. Lessons log

| Date | Entry | Lesson / pack change | Evidence | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-10-17 | Initial pack | Source qualification, regression parity and material applicability will be tracked separately; source snapshots will not be silently converged | [src/digitalmodel/materials/grades.py:L340-L350] [src/digitalmodel/materials/database.py:L125-L131] | Proposed record; issue pending (3) |

No historical clearance or client-derived lesson is asserted by this initial entry; future entries will retain the date, change reference and evidence without rewriting earlier rows.

## 8. Ownership

Proposed review assignment for this draft: engineering calculations quality manager; deputy: independent engineering calculations reviewer; producer responsibility: materials documentation and tests; unresolved engineering classifications will be referred to the family quality manager, with cross-family scope referred to the maintainer. These are review assignments for this pack, not claims of completed reviews. (AQG r0, section 11.)

## 9. Dependencies and implementation inventory

Upstream dependencies are Python standard-library utilities, the grade registry, Pint and `digitalmodel.units.Q_`, plus the delegated Charpy routine; database records depend on package source bytes, not an external service. [src/digitalmodel/materials/database.py:L10-L25] [src/digitalmodel/materials/database.py:L133-L141] [src/digitalmodel/materials/ffs_material_library.py:L56-L67] [src/digitalmodel/materials/line_pipe.py:L21-L25]

Every source mention of OrcaFlex, case-insensitive, is listed here: the unit-helper comment names linetype usage, the legacy table comment names `orcaflex.riser_config.PIPE_GRADES`, and the legacy adapter docstring repeats that dependency; these are text references and compatibility data, with no interface import, launch or version requirement. [src/digitalmodel/materials/grades.py:L315-L322] [src/digitalmodel/materials/grades.py:L377-L384] [src/digitalmodel/materials/grades.py:L425-L440]

Downstream grade-table consumers are documented in the migration matrix; legacy-preserved and canonical-derived routes remain distinct, with convergence tracked by #1147. [docs/domains/grade-table-migration-2026-06-28.md:L37-L60] [src/digitalmodel/materials/grades.py:L340-L350]

### Map basis and counted family

Map ref: `codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map's embedded source revision: `270c8759c374b67a7f032953e7fcdb14e0224461`; code search pin is the header code revision. The map bytes at that map ref equal the map bytes at the header pin, so the inventory citations below resolve at the code pin. [docs/registry/implementation-map.yaml:L1-L4]

Only `api-5ct-5l-grade-tables` has an implementation row inside this package: 1 in-package row and 39 rows overall, making it the sole counted family; no subpackage is excluded. [docs/registry/implementation-map.yaml:L24099-L24105] [docs/registry/implementation-map.yaml:L24252-L24261]

### Complete inventory for that family

The following list preserves every map implementation row, including entries outside this package; it records the map's classifications without asserting that every mapped row is an independently qualified calculation. [docs/registry/implementation-map.yaml:L24099-L24105]

| Row | Map path | Map definition / table | Map evidence |
| --- | --- | --- | --- |
| 1 | `docs/domains/orcaflex/templates/components/materials/steel.csv` | `(module-level)`; line 1 | [docs/registry/implementation-map.yaml:L24106-L24114] |
| 2 | `examples/demos/asset_integrity/ffs_lookup_build.py` | `(module-level) / PIPE_METHODS`; line 27 | [docs/registry/implementation-map.yaml:L24115-L24124] |
| 3 | `examples/demos/asset_integrity/ffs_lookup_build.py` | `build_records`; line 40 | [docs/registry/implementation-map.yaml:L24125-L24134] |
| 4 | `examples/demos/gtm/calc_values.py` | `(module-level) / PIPE_SIZES`; line 19 | [docs/registry/implementation-map.yaml:L24135-L24144] |
| 5 | `examples/demos/gtm/demo_02_wall_thickness_multicode.py` | `_load_engineering_modules`; line 58 | [docs/registry/implementation-map.yaml:L24145-L24154] |
| 6 | `examples/domains/structural/wall_thickness_example.py` | `run_single_design_check`; line 44 | [docs/registry/implementation-map.yaml:L24155-L24164] |
| 7 | `examples/domains/structural/wall_thickness_example.py` | `run_phase_analysis`; line 133 | [docs/registry/implementation-map.yaml:L24165-L24174] |
| 8 | `scripts/capabilities/build_casing_design_explorer.py` | `(module-level)`; line 58 | [docs/registry/implementation-map.yaml:L24175-L24183] |
| 9 | `src/digitalmodel/base_configs/modules/pipe_capacity/pipe_capacity.yml` | `(module-level)`; line 14 | [docs/registry/implementation-map.yaml:L24184-L24192] |
| 10 | `src/digitalmodel/base_configs/modules/vertical_riser/vertical_riser.yml` | `(module-level)`; line 31 | [docs/registry/implementation-map.yaml:L24193-L24201] |
| 11 | `src/digitalmodel/data_systems/data/steel_material.yml` | `(module-level)`; line 16 | [docs/registry/implementation-map.yaml:L24202-L24210] |
| 12 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient`; line 45 | [docs/registry/implementation-map.yaml:L24211-L24220] |
| 13 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient._initialize_pipe_database`; line 134 | [docs/registry/implementation-map.yaml:L24221-L24232] |
| 14 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient._generate_pipe_spec`; line 159 | [docs/registry/implementation-map.yaml:L24233-L24242] |
| 15 | `src/digitalmodel/infrastructure/templates/api_std_2rd_template.yml` | `(module-level)`; line 24 | [docs/registry/implementation-map.yaml:L24243-L24251] |
| 16 | `src/digitalmodel/materials/grades.py` | `(module-level) / _E_LINE_PIPE`; line 28 | [docs/registry/implementation-map.yaml:L24252-L24261] |
| 17 | `src/digitalmodel/orcaflex/mooring_design.py` | `(module-level) / MOORING_MATERIAL_LIBRARY`; line 174 | [docs/registry/implementation-map.yaml:L24262-L24271] |
| 18 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineSegment`; line 348 | [docs/registry/implementation-map.yaml:L24272-L24281] |
| 19 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.estimate_catenary`; line 390 | [docs/registry/implementation-map.yaml:L24282-L24291] |
| 20 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.check_mbl`; line 410 | [docs/registry/implementation-map.yaml:L24292-L24301] |
| 21 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.check_mbl_with_safety_factor`; line 527 | [docs/registry/implementation-map.yaml:L24302-L24311] |
| 22 | `src/digitalmodel/orcaflex/riser_config.py` | `(module-level) / PIPE_GRADES`; line 59 | [docs/registry/implementation-map.yaml:L24312-L24321] |
| 23 | `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.steel_mass_per_m`; line 110 | [docs/registry/implementation-map.yaml:L24322-L24331] |
| 24 | `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.bending_stiffness`; line 139 | [docs/registry/implementation-map.yaml:L24332-L24341] |
| 25 | `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.axial_stiffness`; line 146 | [docs/registry/implementation-map.yaml:L24342-L24351] |
| 26 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `(module-level) / MATERIAL_PROPERTIES`; line 20 | [docs/registry/implementation-map.yaml:L24352-L24361] |
| 27 | `src/digitalmodel/structural/analysis/__init__.py` | `(module-level)`; line 49 | [docs/registry/implementation-map.yaml:L24362-L24370] |
| 28 | `src/digitalmodel/structural/analysis/cli.py` | `check`; line 59 | [docs/registry/implementation-map.yaml:L24371-L24380] |
| 29 | `src/digitalmodel/structural/analysis/cli.py` | `sweep`; line 164 | [docs/registry/implementation-map.yaml:L24381-L24390] |
| 30 | `src/digitalmodel/structural/analysis/cli.py` | `phases`; line 252 | [docs/registry/implementation-map.yaml:L24391-L24400] |
| 31 | `src/digitalmodel/structural/analysis/cli.py` | `list_grades`; line 328 | [docs/registry/implementation-map.yaml:L24401-L24410] |
| 32 | `src/digitalmodel/structural/analysis/cli.py` | `lookup`; line 345 | [docs/registry/implementation-map.yaml:L24411-L24420] |
| 33 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `(module-level) / API_5L_GRADES`; line 40 | [docs/registry/implementation-map.yaml:L24421-L24430] |
| 34 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run`; line 66 | [docs/registry/implementation-map.yaml:L24431-L24440] |
| 35 | `src/digitalmodel/subsea/catenary_riser/models.py` | `(module-level) / STEEL_API_5L_X65`; line 57 | [docs/registry/implementation-map.yaml:L24441-L24450] |
| 36 | `src/digitalmodel/subsea/pipeline/pipeline_pressure.py` | `(module-level) / MATERIAL_LIBRARY`; line 137 | [docs/registry/implementation-map.yaml:L24451-L24460] |
| 37 | `src/digitalmodel/subsea/pipeline/pipeline_pressure_workflow.py` | `WallThicknessSizingWorkflow.run`; line 279 | [docs/registry/implementation-map.yaml:L24461-L24470] |
| 38 | `src/digitalmodel/well/tubulars/casing_design.py` | `(module-level)`; line 50 | [docs/registry/implementation-map.yaml:L24471-L24479] |
| 39 | `tests/fixtures/test_vectors/mooring_risers/riser_material_properties.yaml` | `(module-level)`; line 36 | [docs/registry/implementation-map.yaml:L24480-L24488] |

### Exact package grep and definition classification

Search semantics: Python `re.search`, case-sensitive, over the package's pinned `.py` text, with hits assigned to the smallest enclosing function/class and module-level statements grouped once per file; no signal matches the package-relative path `materials` under case-insensitive comparison, so none is excluded. The three patterns are preserved exactly, including the doubled backslashes in the final pattern; they are not repaired into a different regex. [docs/registry/implementation-map.yaml:L24101-L24104]

```python
signals = [
    r'(?:API.?5[CLT]|_api5l|API5L|API5CT|MATERIAL_LIBRARY|STEEL_GRADES|GRADE_REGISTRY|_GRADES|steel_material|smys|SMYS)',
    r'API.?5CT|API.?5L|steel.grade|casing.grade',
    r'\b(?:J55|K55|N80|L80|P110|Q125|X42|X52|X60|X65|X70|X80)\b',
]
```

| File / definition | Classification | Evidence |
| --- | --- | --- |
| `src/digitalmodel/materials/__init__.py` / `<module>` (1) | Imports, exports, references or docstrings; no further grade implementation | [src/digitalmodel/materials/__init__.py:L1-L82] |
| `src/digitalmodel/materials/database.py` / `lookup` (78) | Projection/delegation to listed grades; helper | [src/digitalmodel/materials/database.py:L78-L89] |
| `src/digitalmodel/materials/database.py` / `_record` (98) | Projection/delegation to listed grades; helper | [src/digitalmodel/materials/database.py:L98-L116] |
| `src/digitalmodel/materials/database.py` / `load_database` (125) | Projection/delegation to listed grades; helper | [src/digitalmodel/materials/database.py:L125-L141] |
| `src/digitalmodel/materials/database_schema.py` / `Strengths` (21) | Data record or validation; helper | [src/digitalmodel/materials/database_schema.py:L21-L37] |
| `src/digitalmodel/materials/database_schema.py` / `__post_init__` (28) | Data record or validation; helper | [src/digitalmodel/materials/database_schema.py:L28-L37] |
| `src/digitalmodel/materials/database_schema.py` / `__post_init__` (97) | Data record or validation; helper | [src/digitalmodel/materials/database_schema.py:L97-L113] |
| `src/digitalmodel/materials/ffs_material_library.py` / `<module>` (1) | Imports, exports, references or docstrings; no further grade implementation | [src/digitalmodel/materials/ffs_material_library.py:L1-L654] |
| `src/digitalmodel/materials/ffs_material_library.py` / `lookup_grade` (624) | Projection from listed grades; helper | [src/digitalmodel/materials/ffs_material_library.py:L624-L654] |
| `src/digitalmodel/materials/grades.py` / `<module>` (1) | Mapped module-level data; canonical and legacy registries listed in section 3 | [src/digitalmodel/materials/grades.py:L1-L448] |
| `src/digitalmodel/materials/grades.py` / `MaterialGrade` (40) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L40-L88] |
| `src/digitalmodel/materials/grades.py` / `smys_psi` (81) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L81-L83] |
| `src/digitalmodel/materials/grades.py` / `_api5l` (99) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L99-L103] |
| `src/digitalmodel/materials/grades.py` / `_hull` (106) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L106-L109] |
| `src/digitalmodel/materials/grades.py` / `_en` (112) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L112-L115] |
| `src/digitalmodel/materials/grades.py` / `_austenitic` (122) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L122-L127] |
| `src/digitalmodel/materials/grades.py` / `_duplex` (130) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L130-L134] |
| `src/digitalmodel/materials/grades.py` / `smys_mpa` (260) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L260-L262] |
| `src/digitalmodel/materials/grades.py` / `smts_mpa` (265) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L265-L273] |
| `src/digitalmodel/materials/grades.py` / `smys_psi` (276) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L276-L278] |
| `src/digitalmodel/materials/grades.py` / `legacy_smys_psi_dict` (296) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L296-L301] |
| `src/digitalmodel/materials/grades.py` / `legacy_smts_psi_dict` (304) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L304-L312] |
| `src/digitalmodel/materials/grades.py` / `smys_kn_m2` (320) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L320-L322] |
| `src/digitalmodel/materials/grades.py` / `smys_pa` (330) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L330-L332] |
| `src/digitalmodel/materials/grades.py` / `legacy_smys_table_mpa` (396) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L396-L401] |
| `src/digitalmodel/materials/grades.py` / `legacy_pipe_db_grades_mpa` (404) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L404-L409] |
| `src/digitalmodel/materials/grades.py` / `legacy_pipe_db_grades_pa` (412) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L412-L422] |
| `src/digitalmodel/materials/grades.py` / `legacy_riser_pipe_grades_mpa` (425) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L425-L440] |
| `src/digitalmodel/materials/grades.py` / `legacy_pipeline_material_library_mpa` (443) | Listed section-3 registry constructor, lookup/view or converter; no unlisted grade calculation | [src/digitalmodel/materials/grades.py:L443-L448] |
| `src/digitalmodel/materials/line_pipe.py` / `<module>` (1) | Imports, exports, references or docstrings; no further grade implementation | [src/digitalmodel/materials/line_pipe.py:L1-L321] |
| `src/digitalmodel/materials/line_pipe.py` / `LinePipe` (134) | Product assembly/reference or data holder; helper | [src/digitalmodel/materials/line_pipe.py:L134-L223] |
| `src/digitalmodel/materials/line_pipe.py` / `api_5l_reference` (218) | Product assembly/reference or data holder; helper | [src/digitalmodel/materials/line_pipe.py:L218-L223] |
| `src/digitalmodel/materials/line_pipe.py` / `line_pipe` (273) | Product assembly/reference or data holder; helper | [src/digitalmodel/materials/line_pipe.py:L273-L321] |

Search receipt: 33 enclosing definitions, 0 further unlisted in-package implementations; the summed miss count is 0, within the at-most-1 criterion. Definitions that hold data, expose unit views or delegate are distinguished from a further formula or grade criterion by the classifications above. [src/digitalmodel/materials/grades.py:L99-L134] [src/digitalmodel/materials/grades.py:L260-L350] [src/digitalmodel/materials/database.py:L78-L116]

### Repository-wide follow-up

Repository-wide searches included `src`, `scripts`, `examples` and `tests`; the grade-signal search found 266 Python files, and the definition-name search found the shared Charpy routine and the materials definitions, plus a test-only material class. These results are discovery receipts, not additional counted-package misses. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L203-L217] [tests/engineering_validation/pipe_sizing/test_industry_standards.py:L30-L44]

```bash
rg -l '(?:API.?5[CLT]|_api5l|API5L|API5CT|MATERIAL_LIBRARY|STEEL_GRADES|GRADE_REGISTRY|_GRADES|steel_material|smys|SMYS)' --glob '*.py' src scripts examples tests
rg -n 'def (kmat_from_charpy|lower_bound_toughness|_master_curve_mpa_sqrt_m|_asme_curve_ksi_sqrt_in)|class (TrueStressStrainCurve|MaterialGrade|LinePipe)' src scripts examples tests
```

The partial dimensional table in `PipeSpecificationClient` and annular-section properties in `subsea.pipeline.pipeline_pressure.PipeGeometry` are external method overlaps, not counted misses inside this package. [src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py:L45-L60] [src/digitalmodel/subsea/pipeline/pipeline_pressure.py:L78-L122]

Map follow-up: pipe dimensions and section properties, IACS/EN/ASTM registry families, toughness/Charpy-transition/Master Curve/ASME routes and the tensile constitutive model need explicit family coverage or association; metadata projection and the delegated Charpy route will remain classified as helpers where applicable, issue pending (4). [src/digitalmodel/materials/line_pipe.py:L34-L215] [src/digitalmodel/materials/grades.py:L159-L204] [src/digitalmodel/materials/ffs_material_library.py:L197-L213] [src/digitalmodel/materials/ffs_material_library.py:L299-L368] [src/digitalmodel/materials/ffs_material_library.py:L469-L552]

## 10. Coverage and open items

### Read coverage

All six Python files in `src/digitalmodel/materials` were read: `__init__.py`, `grades.py`, `line_pipe.py`, `ffs_material_library.py`, `database_schema.py` and `database.py`; the package has no nested Python subpackage at the pin. [src/digitalmodel/materials/__init__.py:L15-L48] [src/digitalmodel/materials/grades.py:L1-L17] [src/digitalmodel/materials/line_pipe.py:L1-L18] [src/digitalmodel/materials/ffs_material_library.py:L1-L53] [src/digitalmodel/materials/database_schema.py:L1-L5] [src/digitalmodel/materials/database.py:L1-L7]

Documentation found by searching for `digitalmodel.materials`, `src/digitalmodel/materials`, `materials/grades` and `material.grade matrix` in repository Markdown/HTML, with routing/map discovery added: the operator map, module-routing registry, implementation map and its Markdown index, codes register, grade-table migration record, the two materials validation records, materials database gaps page and the FFS offering plan. No package README was found in the package-file enumeration. [docs/maps/digitalmodel-operator-map.md:L79-L79] [docs/registry/module-routing.yaml:L577-L582] [docs/registry/implementation-map.md:L26-L26] [docs/domains/codes-register.md:L109-L121] [docs/domains/grade-table-migration-2026-06-28.md:L1-L21] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L1-L18] [docs/domains/materials/line-pipe-validation-2026-06-28.md:L1-L20] [docs/domains/materials/database-gaps.html:L6-L20] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L168-L180]

The four materials test files were read and selected in full, including all parametrized cases; historical documentation counts are not used as current collection counts. [tests/materials/test_grade_matrix.py:L20-L48] [tests/materials/test_line_pipe.py:L18-L53] [tests/materials/test_database.py:L29-L42] [tests/materials/test_ffs_material_library.py:L1-L19]

Offline receipt at the header pin: `PYTHONPATH=src uv run --no-sources python -m pytest tests/materials/ -q -rA -p no:cacheprovider --basetemp=<scratch>` was used before and after documentation edits, with the same test configuration and no addopts override. Each completed run collected 179 nodes: 179 passed, 0 failed, 0 skipped, 0 NOT_RUN; the passing node sets match exactly. By file: database 78, FFS 33, grades 29 and line pipe 39. The initial environment installation completed, but that first invocation exited 141 before collection; it is not counted as a test run. [tests/materials/test_database.py:L18-L26] [tests/materials/test_ffs_material_library.py:L1-L19] [tests/materials/test_grade_matrix.py:L20-L48] [tests/materials/test_line_pipe.py:L18-L53]

### Open items

| Item | Scope / evidence needed | Tracking / responsible role |
| --- | --- | --- |
| U1 | UNVERIFIED: original qualification of grade values, dimensions and physical defaults; publisher sources and editions have not been checked here; existing values stay referenced rather than reproduced. [docs/domains/materials/database-gaps.html:L31-L49] | #1204 and issue pending (3); materials maintainer |
| U2 | UNVERIFIED: correspondence of toughness coefficients and published-anchor tests to the named original publications; only the repository statements were inspected. [src/digitalmodel/materials/ffs_material_library.py:L123-L165] [tests/materials/test_ffs_material_library.py:L5-L18] | issue pending (3); engineering calculations quality manager |
| U3 | UNVERIFIED: calibrated, independent end-to-end FFS material assessment example; V1 covers a synthetic boundary, and the remaining examples are regressions. [tests/materials/test_ffs_material_library.py:L268-L295] | issue pending (3); engineering calculations quality manager |
| Input guards | S01–S14 gaps concerning grade identity, product labels, finite geometry and catalogue fallback; no module code changed. [src/digitalmodel/materials/grades.py:L224-L257] [src/digitalmodel/materials/line_pipe.py:L153-L160] [src/digitalmodel/materials/line_pipe.py:L297-L321] | issue pending (1); materials maintainer |
| FFS guards | S26/S29/S31–S35 gaps concerning unused reference inputs, cap/yield/thickness scope and finite evaluator/table inputs. [src/digitalmodel/materials/ffs_material_library.py:L277-L287] [src/digitalmodel/materials/ffs_material_library.py:L344-L368] [src/digitalmodel/materials/ffs_material_library.py:L469-L552] | issue pending (2); materials maintainer |
| Metadata and links | Null editions, grade-to-S–N associations, standards-registry and structural-shape links, original rights and dimensional-source readiness. [docs/domains/materials/database-gaps.html:L31-L66] | #1204; materials maintainer |
| Historical convergence | Consumer-specific snapshots require an explicit engineering-basis decision before convergence. [src/digitalmodel/materials/grades.py:L340-L350] | #1147; materials maintainer |
| Documentation and citations | Stale grade count, edition ambiguity, missing calculation sidecars and downstream report evidence integration. [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L66-L70] [src/digitalmodel/materials/database.py:L98-L116] [src/digitalmodel/materials/ffs_material_library.py:L299-L368] | issue pending (3); materials maintainer |
| Map follow-up | Section-9 uncounted methods need map rows before entering counted-family coverage. [src/digitalmodel/materials/line_pipe.py:L34-L215] [src/digitalmodel/materials/ffs_material_library.py:L469-L552] | issue pending (4); materials maintainer |
| Review disposition | This DRAFT will require independent A1–A8 review, actual issue numbers replacing draft references, and a gate disposition before clearance | issue pending (3); engineering calculations quality manager; AQG r0 section 11 |
