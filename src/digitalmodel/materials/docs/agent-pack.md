# Materials knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | materials / digitalmodel |
| Package | `src/digitalmodel/materials` |
| Pack revision / date | 4 / 2026-10-21 |
| Code revision | `b4c10284fa7d28398e6fa02c262c219c0fec8fef`; reachable from main |
| Normalized grades.py SHA-256 | `4be7b32cf536e519d9dd716a124b5ef5f222f5c32d90bd20604793602206df90` |
| Analysis family | Engineering calculations (EC), AQG r0 Python calculations |
| Review role | Engineering calculations quality manager |
| Status | DRAFT; independent review pending |

The assigned revision identifies the inspected grade source on main. The digest
above hashes UTF-8 text after replacing CRLF and lone CR with LF. All bracketed
line citations below are one-based, inclusive locators at that revision,
including citations to tests, documentation and generated map metadata. They
can be read with `git show <code-revision>:<path>`; they do not refer to the
working tree. Citation ranges were re-anchored to that revision. Missing history
fails the provenance tests rather than skipping them. This establishes repository
traceability; original-standard qualification remains pending.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-17 | Initial pack recorded package scope, checks and unresolved qualification. |
| 1 | 2026-10-18 | Grade and curve corrections separated regression evidence from source qualification. |
| 2 | 2026-10-18 | Normalized digest recorded the inspected source while a reachable revision was pending. |
| 3 | 2026-10-18 | Assigned reachable source history, re-anchored locators and removed stale inventory claims. |
| 4 | 2026-10-21 | Linked the executable PSL1 lookup example and expanded the product-level regression coverage; the inspected grade source is unchanged. |

## 1. Purpose and scope

The package provides a steel-grade registry and compatibility views, a line-pipe geometry catalogue, FFS material inputs and an indexed metadata projection; the routing registry records development maturity. [src/digitalmodel/materials/__init__.py:L15-L48] [src/digitalmodel/materials/ffs_material_library.py:L47-L52] [src/digitalmodel/materials/database.py:L123-L143] [docs/registry/module-routing.yaml:L729-L738]

The registry contains API 5L, IACS UR W11, EN 10025-2, ASTM A312 and ASTM A790 groups; its integrity test requires 40 grades rather than the older validation record's 33. [src/digitalmodel/materials/grades.py:L149-L223] [tests/materials/test_grade_matrix.py:L140-L150] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L80-L84]

The FFS routines estimate ferritic-steel toughness and create monotonic tensile curves; creep and fatigue curves are excluded, and recorded assessment temperature does not derate material properties. [src/digitalmodel/materials/ffs_material_library.py:L10-L45] [src/digitalmodel/materials/ffs_material_library.py:L51-L52] [src/digitalmodel/materials/ffs_material_library.py:L572-L576]

The line-pipe factory returns geometry and a reference string, with no product-standard or material-class check after grade lookup; its API 5L label therefore does not establish product qualification. [src/digitalmodel/materials/line_pipe.py:L218-L223] [src/digitalmodel/materials/line_pipe.py:L297-L321]

## 2. Inputs and outputs

### Inputs

| API / input | Unit / convention | Allowed input and default | Evidence |
| --- | --- | --- | --- |
| `get(name)`, `by_standard(standard)` | Text; no spatial frame | Canonical names, aliases and case normalization; standard query uses substring matching; neither has a default name | [src/digitalmodel/materials/grades.py:L237-L274] |
| `smts_mpa(name, psl=...)`, database `lookup` | MPa; PSL token | Default PSL2; API requests for absent PSL1 or unknown PSL tokens raise ValueError; non-API grades keep their canonical value | [src/digitalmodel/materials/grades.py:L282-L299] [src/digitalmodel/materials/database.py:L79-L86] |
| `line_pipe(grade, nps/od_in, schedule/wt_in)` | Geometry in inches | Exactly one diameter route and one wall route; schedule requires NPS; wall positive and less than half the OD; dimensions have no defaults | [src/digitalmodel/materials/line_pipe.py:L139-L160] [src/digitalmodel/materials/line_pipe.py:L273-L321] |
| `MaterialGrade` construction | MPa, kg/m³, dimensionless Poisson ratio | Elastic/density/class defaults in source; frozen record has no post-init value validation | [src/digitalmodel/materials/grades.py:L27-L97] |
| `lower_bound_toughness` | J, °C, mm, MPa; Pint quantities converted | Method defaults to auto, class to ferritic; reference routes require temperature and yield; method-specific reference thickness, fractile, cap and scope constants remain in source | [src/digitalmodel/materials/ffs_material_library.py:L84-L114] [src/digitalmodel/materials/ffs_material_library.py:L216-L230] [src/digitalmodel/materials/ffs_material_library.py:L267-L368] |
| `TrueStressStrainCurve` | MPa, °C; true/plastic strain dimensionless | Positive moduli/strengths; exponent strictly between 0 and 1; explicit uniform ultimate true strain required; temperature default is recorded only | [src/digitalmodel/materials/ffs_material_library.py:L395-L452] |
| Curve evaluation / tables | Tensile strain positive; stress MPa | Negative strain rejected; total/plastic tables default to 20 rows; plateau strain defaults to twice ultimate; minimum row counts 4/3 respectively | [src/digitalmodel/materials/ffs_material_library.py:L473-L556] |
| `Strengths`, `MaterialRecord`, `RepositoryProvenance` | MPa and repository metadata | Finite positive strengths, tensile at least yield; nonempty identities; mandatory digest and ISO date; schema permits a null revision; shipped records assign a full commit hash; unestablished links remain null | [src/digitalmodel/materials/database_schema.py:L20-L37] [src/digitalmodel/materials/database_schema.py:L52-L78] [src/digitalmodel/materials/database_schema.py:L91-L118] |

### Outputs

| API / output | Unit / format | Meaning and limit | Evidence |
| --- | --- | --- | --- |
| Grade and legacy views | MPa, psi, Pa or kN/m² according to helper suffix | Canonical fields and historical snapshots are distinct; compatibility views deliberately preserve historical values | [src/digitalmodel/materials/grades.py:L277-L309] [src/digitalmodel/materials/grades.py:L322-L376] [src/digitalmodel/materials/grades.py:L422-L474] |
| `LinePipe` properties | in, mm, in², kg/m, lb/ft, m³/m, bbl/ft; D/t dimensionless | ID, area, weight and bore volume; weight uses a fixed density rather than the selected grade density | [src/digitalmodel/materials/line_pipe.py:L126-L151] [src/digitalmodel/materials/line_pipe.py:L163-L215] |
| `ToughnessEstimate` | MPa√m; method, class, °C, mm and notes | `float(estimate)` returns toughness; notes retain extrapolation and capping dispositions | [src/digitalmodel/materials/ffs_material_library.py:L168-L184] [src/digitalmodel/materials/ffs_material_library.py:L329-L368] |
| Curve and tables | `(true_strain, MPa)` or `(MPa, plastic_strain)` | Elastic, hardening and ultimate plateau; no deck file or solver object is written here | [src/digitalmodel/materials/ffs_material_library.py:L484-L556] |
| Database records | Immutable mapping; stable string key | Specification, edition or explicit unknown token, designation, product level or inapplicable token; source-digest flag is separate from individual provenance | [src/digitalmodel/materials/database_schema.py:L120-L126] [src/digitalmodel/materials/database.py:L56-L86] |

These scalar material and section quantities have no coordinate origin or phase convention; the tensile curve rejects negative strains, and inputs carry units by suffix or Pint conversion rather than a geometry file format. [src/digitalmodel/materials/grades.py:L43-L64] [src/digitalmodel/materials/line_pipe.py:L139-L151] [src/digitalmodel/materials/ffs_material_library.py:L84-L110] [src/digitalmodel/materials/ffs_material_library.py:L484-L492]

## 3. Method and governing standards

All licensed references below mean **clause text not checked against the licensed copy**; standard table values and coefficients are intentionally left at their cited code locations. [docs/domains/materials/database-gaps.html:L39-L61]

| Method / reference | Edition and clause evidence | Implementation and disposition |
| --- | --- | --- |
| API 5L, Line Pipe | 46th edition, 9.3.1/Table 6 (PSL1) and 9.3.2/Table 7 (PSL2), SI | `_api5l` and `smts_mpa`: PSL1 SI tensile minima through X70 equal corresponding PSL2 minima; X80 has no PSL1 entry. Unsupported PSL1 and unknown API product levels raise. Legacy snapshots remain distinct. Licensed-text verification by the maintainer is required before #2726 closure. [src/digitalmodel/materials/grades.py:L149-L170] |
| IACS UR W11, Normal and Higher Strength Hull Structural Steels | Revision unspecified; no numbered clause/table identified | `_hull`, normal/high-strength registry, `_charpy_test_temperature`; unknown classes may yield null. [src/digitalmodel/materials/grades.py:L105-L121] [src/digitalmodel/materials/grades.py:L172-L191] [src/digitalmodel/materials/ffs_material_library.py:L562-L565] [src/digitalmodel/materials/ffs_material_library.py:L622-L626] |
| ASME B36.10M, Welded and Seamless Wrought Steel Pipe | Edition and numbered dimensional table unspecified | `NPS_OD_IN`, `SCHEDULE_WT_IN`, `od_for_nps`, `wt_for_schedule`; section calculations use annular area, area × density and bore area. [src/digitalmodel/materials/line_pipe.py:L12-L17] [src/digitalmodel/materials/line_pipe.py:L34-L51] [src/digitalmodel/materials/line_pipe.py:L163-L215] [src/digitalmodel/materials/line_pipe.py:L234-L270] |
| BS 7910, flaw assessment | Annex J; edition absent from this package; related register names 2019 while the offering plan names 2013, so applicable edition is unresolved | Charpy route delegates to `asset_integrity.assessment.crack_fad.kmat_from_charpy`; it is not an independent implementation here. [src/digitalmodel/materials/ffs_material_library.py:L17-L20] [src/digitalmodel/materials/ffs_material_library.py:L299-L306] [docs/domains/codes-register.md:L19-L20] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L26-L28] |
| API 579-1/ASME FFS-1, Fitness-For-Service | 2007 Annex F F.4.4 / F.2.3; 2016 and later Annex 3D 3D.4.4 / 3D.2.3; register names 2021 | `lower_bound_toughness`, `TrueStressStrainCurve`; the curve is an input-driven Ludwik model, not the standard's tabulated material model. [src/digitalmodel/materials/ffs_material_library.py:L10-L45] [src/digitalmodel/materials/ffs_material_library.py:L374-L412] [docs/domains/codes-register.md:L19-L20] |
| ASTM E1921, Master Curve; Wallin, Master curve analysis of the Euro fracture toughness dataset, 2002 | Source comments identify the paper and scope paragraph; standard edition unspecified | `_master_curve_mpa_sqrt_m`, Charpy-transition route and thickness adjustment; source coefficients are cited rather than reproduced. [src/digitalmodel/materials/ffs_material_library.py:L123-L144] [src/digitalmodel/materials/ffs_material_library.py:L197-L207] [src/digitalmodel/materials/ffs_material_library.py:L314-L347] |
| ASME BPVC XI, Nonmandatory Appendix A, A-4200; source comments name EPRI NP-719-SR and NUREG/CR-6609 | Standard edition unspecified; primary-source correspondence not established here | `_asme_curve_ksi_sqrt_in` and Kic/Kir routes, degree conversion, class/yield bounds and capping. [src/digitalmodel/materials/ffs_material_library.py:L146-L162] [src/digitalmodel/materials/ffs_material_library.py:L210-L213] [src/digitalmodel/materials/ffs_material_library.py:L349-L368] |
| EN 10025-2, Hot rolled structural steel; ASTM A312/A312M and A790/A790M, stainless steel pipe | Grade source omits editions and numbered tensile tables; register names EN 2019 | `_en`, `_austenitic`, `_duplex` and their registry rows. [src/digitalmodel/materials/grades.py:L124-L146] [src/digitalmodel/materials/grades.py:L193-L217] [docs/domains/codes-register.md:L120-L121] |
| Monotonic tensile model | Input-driven elastic relation, plastic-strain power law and ultimate plateau; explicit uniform true strain is required; Considere d(sigma)/d(eps) = sigma does not imply eps = n for the offset law | `stress_from_plastic_strain`, `stress` bisection, `plastic_strain`, `_plastic_grid`, `table`, `plastic_table`; convergence uses the relative bracket criterion in code. [src/digitalmodel/materials/ffs_material_library.py:L378-L412] [src/digitalmodel/materials/ffs_material_library.py:L473-L556] |
| Unit and compatibility views | Engineering conversions and explicitly preserved source snapshots | `MaterialGrade.smys_psi/smts_psi`; `smys_mpa/smts_mpa`, psi/Pa/kN/m² helpers and all seven `legacy_*` adapters; database is a projection, not a new strength dataset. [src/digitalmodel/materials/grades.py:L83-L97] [src/digitalmodel/materials/grades.py:L277-L376] [src/digitalmodel/materials/grades.py:L422-L474] [src/digitalmodel/materials/database.py:L95-L143] |

The package carries source strings and repository provenance, but no `Citation` sidecar is constructed by these APIs; sidecar adoption and edition qualification remain open under #1204 and issue pending (3). [src/digitalmodel/materials/grades.py:L59-L80] [src/digitalmodel/materials/database.py:L95-L114] [src/digitalmodel/materials/ffs_material_library.py:L299-L368]

The [current PSL1 worked example](../../../../docs/domains/materials/grade-matrix-validation-2026-06-28.md#worked-example-explicit-psl1-lookup)
shows grade and product-level inputs, direct/indexed answers, table provenance
and an unsupported request. `tests/materials/test_psl1_example.py` executes the
example. This supplementary regression coverage does not change the pinned
source above; original-standard qualification remains **NOT_EVALUATED** pending
the maintainer's licensed-text check.

## 4. Hand checks and regression examples

V1 checks a prescribed true-stress plateau and is an input-echo regression, not G2 evidence. V2 exercises the hardening branch against a hand calculation. V3–V6 exercise the existing closed-form toughness anchors; original-source qualification remains U2. [tests/materials/test_ffs_material_library.py:L269-L274]

| ID / kind | Independent basis / input | Expected / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 endpoint regression | Synthetic prescribed true ultimate, explicit ultimate strain, already-true input; no input file | 600 MPa at dimensionless strain 0.15 and 0.3; relative tolerances 1e-9 and 1e-12 are test numerical tolerances, not source uncertainty | `tests/materials/test_ffs_material_library.py::test_curve_explicit_ultimate_strain_and_true_ultimate_input` | BASE PASS; AFTER PASS [tests/materials/test_ffs_material_library.py:L269-L274] |
| V2 hardening hand check | Input-driven offset equation: 300 + (600 - 300) sqrt(0.025/0.1) = 450 MPa; E = 200000 MPa, eps_u = 0.103, n = 0.5, ep_u = 0.1; total strain = 0.025 + 450/200000 = 0.02725 | `stress_from_plastic_strain(0.025)` and `stress(0.02725)` = 450 MPa, absolute tolerance 1e-8 MPa; both true and engineering ultimate inputs | `tests/materials/test_audit_regressions.py::test_hardening_closed_form_anchor` | PASS; synthetic calculation, not material calibration |
| V3 toughness hand check | Master Curve median at T = T0, 1T thickness 25.4 mm; ASTM E1921 reference definition | `lower_bound_toughness(..., fractile=0.5).k_mat_mpa_sqrt_m` = 100 MPa√m; relative tolerance 0.01 | `tests/materials/test_ffs_material_library.py::test_master_curve_median_is_100_at_t0` | PASS; original-source qualification U2 |
| V4 toughness hand check | Master Curve 5% at T = T0: 25.2 + 36.6 exp(0) = 61.8; 1T thickness 25.4 mm | `lower_bound_toughness(...).k_mat_mpa_sqrt_m` = 61.8 MPa√m; relative tolerance 0.01 | `tests/materials/test_ffs_material_library.py::test_master_curve_5pct_lower_bound_at_t0` | PASS; original-source qualification U2 |
| V5 toughness hand check | ASME XI Appendix A, A-4200, K_Ic at T = RT_NDT: 33.2 + 20.734 = 53.934 ksi√in | `lower_bound_toughness(..., method="asme_kic").k_mat_mpa_sqrt_m` = 53.934 × 1.0988 MPa√m; relative tolerance 0.01 | `tests/materials/test_ffs_material_library.py::test_asme_kic_at_reference_temperature` | PASS; original-source qualification U2 |
| V6 toughness hand check | ASME XI Appendix A, A-4200, K_Ia at T = RT_NDT: 26.78 + 1.223 exp(0.0145 × 160) = 39.22 ksi√in (rounded) | `lower_bound_toughness(..., method="asme_kir").k_mat_mpa_sqrt_m` = 39.22 × 1.0988 MPa√m; relative tolerance 0.01 | `tests/materials/test_ffs_material_library.py::test_asme_kir_at_reference_temperature` | PASS; original-source qualification U2 |
| R1 regression, historical worked workflow | Existing grade/schedule construction, no independent source opened | Weight equals code metal area × code density, relative tolerance 1e-12; weight conversion relative tolerance 1e-6 | `tests/materials/test_line_pipe.py::test_weight_is_area_times_density` | BASE PASS; AFTER PASS [tests/materials/test_line_pipe.py:L56-L61] |
| R2 regression | Canonical/ISO aliases | 0 designation mismatches, dimensionless count | `tests/materials/test_grade_matrix.py::test_iso_designation_alias_resolves` | BASE PASS; AFTER PASS [tests/materials/test_grade_matrix.py:L109-L112] |
| R3 regression | Shared Charpy routine | 0 implementation-identity mismatches; relative output difference at most 1e-12, MPa√m quantity | `tests/materials/test_ffs_material_library.py::test_charpy_fallback_is_the_crack_fad_implementation` | BASE PASS; AFTER PASS [tests/materials/test_ffs_material_library.py:L43-L51] |
| R4 regression | Database projected from grade records | 0 strength/citation mismatches, MPa and dimensionless mismatch counts | `tests/materials/test_database.py::test_values_and_citations_equal_the_source_records` | BASE PASS; AFTER PASS [tests/materials/test_database.py:L62-L73] |
| R5 regression | Typed solver-table rows | 25 total-strain rows and 10 plastic rows; 0 nonmonotonic pairs, dimensionless counts | `tests/materials/test_ffs_material_library.py::test_curve_table_for_solver_decks` | BASE PASS; AFTER PASS [tests/materials/test_ffs_material_library.py:L277-L297] |

The existing dimensional and grade validation records claim comparison with published standards; those original sources were not opened for this pack, so their source qualification remains unresolved (U1) and they are not promoted into independent validation here. [docs/domains/materials/line-pipe-validation-2026-06-28.md:L22-L43] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L48-L64]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, with G5 required by section 9; the checks below are the proposed module-specific acceptance record, not independent gate signatures. (AQG r0, sections 9–11.)

| Gate | Applicability | Numeric pass criterion / unit | Executable check / evidence |
| --- | --- | --- | --- |
| G0 | Applicable: inputs, units, citations | C01–C06 below | Input, provenance and method tests |
| G1 | Applicable: Python imports and tests | C07: 0 collection errors and 0 failures, count | Python pytest route; no solver file is emitted by the package |
| G2 | Applicable: hand or published check | C08: V2 hardening error ≤ 1e-8 MPa; V3–V6 toughness relative error ≤ 0.01 | `stress_from_plastic_strain`, `stress`, `lower_bound_toughness`; V2–V6, not V1 |
| G3 | NOT_APPLICABLE: no dynamics integration in these scalar/material APIs | Not a numeric result | [src/digitalmodel/materials/line_pipe.py:L163-L215] [src/digitalmodel/materials/ffs_material_library.py:L473-L556] |
| G4 | NOT_APPLICABLE: no diffraction body or mesh in these APIs | Not a numeric result | [src/digitalmodel/materials/__init__.py:L15-L48] [src/digitalmodel/materials/ffs_material_library.py:L69-L77] |
| G5 | Applicable: evidence traceability | C09–C10 below; 0 unresolved result/citation associations, count | Pack receipt, digest and traceability checks; downstream report appendix not established |

| Check | Gate | Module-specific check and numeric criterion | Execution / evidence |
| --- | --- | --- | --- |
| C01 | G0 | `Strengths` rejects invalid, nonpositive, inverted or wrong-unit strengths: 0 unexpected acceptances, count; strengths MPa | `tests/materials/test_database.py::test_invalid_strength_is_rejected` and adjacent nonpositive/unit tests. [tests/materials/test_database.py:L220-L239] |
| C02 | G0 | `MaterialDatabase([record, record])` rejects duplicate keys with `duplicate material key`; `MaterialRecord`/`RepositoryProvenance` reject missing/malformed metadata: 0 unexpected acceptances | `tests/materials/test_database.py::test_duplicate_keys_are_rejected`, `test_missing_provenance_is_rejected`, `test_malformed_provenance_is_rejected`, `test_missing_strengths_are_rejected`, `test_unestablished_foreign_keys_are_rejected`. The duplicate and provenance tests were already separate on main; rejection-removal mutation is detected. [#2728](https://github.com/vamseeachanta/digitalmodel/issues/2728) |
| C03 | G0 | `line_pipe` rejects listed ambiguous diameter/wall and invalid catalogue inputs: 0 unexpected acceptances, count; geometry inches | `tests/materials/test_line_pipe.py::test_errors`; this does not test non-finite geometry or product-grade suitability. [tests/materials/test_line_pipe.py:L128-L140] |
| C04 | G0 | `lower_bound_toughness` and `TrueStressStrainCurve` wrong-unit, positional, class, missing-method-input and scope guards: 0 unexpected acceptances, count; J/°C/MPa | `tests/materials/test_ffs_material_library.py::test_toughness_wrong_unit_quantity_rejected`, `tests/materials/test_ffs_material_library.py::test_toughness_rejects_non_ferritic_material_class`, `tests/materials/test_ffs_material_library.py::test_toughness_requires_the_inputs_of_the_chosen_method`; scope and temperature tests in same range. `tests/materials/test_audit_regressions.py::test_curve_requires_explicit_uniform_strain` also asserts missing-strain rejection ([#2731](https://github.com/vamseeachanta/digitalmodel/issues/2731)). [tests/materials/test_ffs_material_library.py:L147-L227] |
| C05 | G0 | `grades.get`, `lookup_grade`, `MaterialDatabase.lookup`, `line_pipe` raise KeyError for unsupported qualified/unknown strings; exact canonical/ISO/hull aliases remain distinct: 0 unexpected acceptances or identity mismatches | `tests/materials/test_audit_regressions.py::test_qualified_or_unknown_grade_never_becomes_hull_grade`, `test_exact_grade_aliases_remain_available`; existing grade alias tests remain. [#2727](https://github.com/vamseeachanta/digitalmodel/issues/2727) |
| C06 | G0 | `load_database` output null links and absent editions remain explicit: 0 invented associations/editions, count | `tests/materials/test_database.py::test_missing_associations_are_explicitly_null`, `tests/materials/test_database.py::test_edition_metadata_is_not_inferred`; these do not qualify original standards. [tests/materials/test_database.py:L191-L205] |
| C07 | G1 | Import and execute `grades`, `load_database`, `line_pipe`, `lower_bound_toughness` and `TrueStressStrainCurve` through all collected materials nodes: 0 collection errors and 0 failed nodes, count | `python -m pytest tests/materials/ -q -rA -p no:cacheprovider`; imports are the actual Python load route. [tests/materials/test_database.py:L10-L13] [tests/materials/test_ffs_material_library.py:L25-L35] |
| C08 | G2 | `TrueStressStrainCurve.stress_from_plastic_strain` and `stress` return 450 MPa at V2 inputs within 1e-8 MPa; `lower_bound_toughness().k_mat_mpa_sqrt_m` meets V3–V6 anchors within 1% | V2–V6 test nodes in section 4; hardening-removal mutation fails both true/engineering-input cases. V1 remains an endpoint regression. [#2730](https://github.com/vamseeachanta/digitalmodel/issues/2730) |
| C09 | G5 | `normalised_source_digest` equals pinned provenance and load flag is true: 0 digest mismatches, count | `tests/materials/test_database.py::test_provenance_pins_the_repository_source`, `tests/materials/test_database.py::test_source_drift_requires_refreshed_provenance`; line-ending and deliberate-drift tests exercise detection. [tests/materials/test_database.py:L76-L86] [tests/materials/test_database.py:L266-L297] |
| C10 | G5 | `load_database` outputs match `grades.get` strengths/citations; PSL1 output provenance names 46th edition, clause 9.3.1: 0 mismatches | `tests/materials/test_database.py::test_values_and_citations_equal_the_source_records`; `tests/materials/test_audit_regressions.py::test_psl1_si_minima_and_table_provenance`. Pack citation ranges resolve at the assigned revision; no licensed qualification is inferred. |
| C11 | G1 | `TrueStressStrainCurve.table(n_points=25)` and `plastic_table(n_points=10)` have exactly 25/10 rows and no descending stress/strain pairs | `tests/materials/test_ffs_material_library.py::test_curve_table_for_solver_decks`; a missing interior plastic row fails `len(plastic) == 10`. [#2729](https://github.com/vamseeachanta/digitalmodel/issues/2729) |
| C12 | G0 | `lower_bound_toughness` auto method route and tagged-unit equivalence: 0 method mismatches; relative toughness difference ≤ 1e-6, dimensionless | `tests/materials/test_ffs_material_library.py::test_toughness_auto_method_selection`, `tests/materials/test_ffs_material_library.py::test_toughness_tagged_quantities_are_converted`. [tests/materials/test_ffs_material_library.py:L192-L227] |
| C13 | G1 | `LinePipe.weight_kg_m` and `weight_lb_ft` fixed-density conversion consistency: relative differences ≤ 1e-12 and ≤ 1e-6, dimensionless; kg/m and lb/ft quantities | R1. [tests/materials/test_line_pipe.py:L56-L61] |
| C14 | G1 | `legacy_smys_psi_dict` and `legacy_smts_psi_dict` equal their live consumer views: 0 differing entries, count | `tests/materials/test_grade_matrix.py::test_legacy_smys_psi_matches_corroded_pipe`, `tests/materials/test_grade_matrix.py::test_legacy_smts_psi_matches_dnv_rp_f101`. [tests/materials/test_grade_matrix.py:L98-L107] |
| C15 | G1 | `grades.smts_mpa` and `MaterialDatabase.lookup` agree with the repository's cited Table 6 SI entries and explicit product levels; absent PSL1/unknown API levels raise across public strength views; `load_database` emits no X80 PSL1 record | `tests/materials/test_database.py::test_psl_selection_matches_existing_reconciliation`; `tests/materials/test_audit_regressions.py::test_psl1_si_minima_and_table_provenance`, `test_psl1_unlisted_grades_raise`, `test_unknown_api_product_level_raises`; executable worked example: `tests/materials/test_psl1_example.py::test_psl1_worked_example`. [#2726](https://github.com/vamseeachanta/digitalmodel/issues/2726); maintainer licensed-text check pending |

Checks defined: G0 = 7, G1 = 5, G2 = 1, G5 = 2; total = 15, with G3/G4 inapplicable under the cited family mapping. (AQG r0, section 10.)

## 6. Known failure modes and pitfalls

sweep: 36 found

The sweep inventories 36 distinct default, nullable, unit, conversion or fallback sites below; guarded normal behavior is included so that a search match is not mistaken for a defect. [src/digitalmodel/materials/database_schema.py:L83-L118] [src/digitalmodel/materials/ffs_material_library.py:L84-L114] [src/digitalmodel/materials/line_pipe.py:L245-L270]

Exact text sweep, followed by reading all six package files and enclosing definitions:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|\b(float|int)\(|= 0([.]0)?\b|0[.]05|default|units|density|temperature|cache' src/digitalmodel/materials
rg -n 'except|continue|return|float\(|int\(|\.get\(|None|0\.0|0\.05|1970|write|cache' src/digitalmodel/materials --glob '*.py'
```

| Site | Symptom / disposition | Detection / tracking |
| --- | --- | --- |
| S01 `grades.get` | Exact canonical names/aliases only; blank and arbitrary suffix strings raise KeyError, including API-qualified strings outside the supported grammar | C05; `tests/materials/test_audit_regressions.py::test_qualified_or_unknown_grade_never_becomes_hull_grade`; corrected under [#2727](https://github.com/vamseeachanta/digitalmodel/issues/2727) |
| S02 `by_standard` | Empty substring returns all records; no-match substring returns empty list. [src/digitalmodel/materials/grades.py:L271-L274] | no detection check exists; issue pending (1) |
| S03 `smts_mpa` | API requests with absent PSL1 or unknown PSL tokens raise ValueError; no substitute product level | C15; corrected under [#2726](https://github.com/vamseeachanta/digitalmodel/issues/2726); product qualification still requires licensed verification |
| S04 `MaterialGrade` elastic defaults | Per-family modulus assumptions feed constructors without temperature calibration. [src/digitalmodel/materials/grades.py:L27-L32] [src/digitalmodel/materials/grades.py:L109-L146] | Regression constants exist; original qualification remains under #1204; no detection check exists for source accuracy |
| S05 `MaterialGrade.nu` | Common Poisson default applies unless replaced. [src/digitalmodel/materials/grades.py:L33-L33] [src/digitalmodel/materials/grades.py:L71-L73] | no detection check exists; #1204 |
| S06 `MaterialGrade.rho_kg_m3` | Generic density default and stainless overrides are inherited assumptions. [src/digitalmodel/materials/grades.py:L34-L36] [src/digitalmodel/materials/grades.py:L71-L73] [src/digitalmodel/materials/grades.py:L134-L146] | no detection check exists for original density qualification; #1204 |
| S07 `MaterialGrade.material_class` | Default ferritic class; direct construction has no numeric/class validation. [src/digitalmodel/materials/grades.py:L39-L97] | C04 covers FFS rejection only; no detection check exists for invalid direct grade records; issue pending (1) |
| S08 grade unit views | MPa/psi conversion is approximate; Pa/kN/m² helpers assume canonical MPa; SMTS legacy view rounds before `float`. [src/digitalmodel/materials/grades.py:L23-L25] [src/digitalmodel/materials/grades.py:L83-L97] [src/digitalmodel/materials/grades.py:L307-L309] [src/digitalmodel/materials/grades.py:L330-L363] | C14 historical parity tests exist; no independent conversion-accuracy check exists; issue pending (3) [tests/materials/test_grade_matrix.py:L98-L107] |
| S09 legacy snapshots | Per-consumer strength differences persist by design; substituting canonical values changes the engineering basis. [src/digitalmodel/materials/grades.py:L366-L419] | C14 covers the two FFS dictionaries only; no detection check exists for independent qualification of all preserved snapshots; convergence #1147 [tests/materials/test_grade_matrix.py:L98-L107] |
| S10 line-pipe unit constants | Inch/SI, mass/length and bore-volume factors are fixed assumptions. [src/digitalmodel/materials/line_pipe.py:L27-L31] [src/digitalmodel/materials/line_pipe.py:L183-L215] | C13 covers weight conversion; bore test only checks positivity/consistency; no detection check exists for independent barrel conversion; issue pending (3) [tests/materials/test_line_pipe.py:L76-L81] |
| S11 line-pipe density | Weight uses fixed source density regardless of grade. [src/digitalmodel/materials/line_pipe.py:L126-L127] [src/digitalmodel/materials/line_pipe.py:L197-L200] | C13 characterizes fixed-density behavior; no detection check exists for product-density mismatch; issue pending (1) |
| S12 `available_schedules` | Unsupported NPS returns an empty list via empty-dict default. [src/digitalmodel/materials/line_pipe.py:L245-L247] | no detection check exists; issue pending (1) |
| S13 `wt_for_schedule` | Missing table and unknown schedule raise rather than silently default. [src/digitalmodel/materials/line_pipe.py:L256-L270] | C03; guarded behavior |
| S14 `LinePipe.__post_init__`, factory | Comparison-only checks admit NaN; any grade is accepted before an API 5L label is built. [src/digitalmodel/materials/line_pipe.py:L153-L160] [src/digitalmodel/materials/line_pipe.py:L218-L223] [src/digitalmodel/materials/line_pipe.py:L297-L321] | C03 covers ordinary errors; no detection check exists for non-finite/product inputs; issue pending (1) |
| S15 `_edition` | No edition regex match produces None. [src/digitalmodel/materials/database.py:L89-L92] | C06; intentional unknown metadata; #1204 |
| S16 database lookup | `MaterialDatabase.lookup` validates product level through `grades.smts_mpa` before index selection | C15; unsupported requests raise; [#2726](https://github.com/vamseeachanta/digitalmodel/issues/2726) |
| S17 database digest flag | Direct indexes default to false; loader still returns records when digest drifts. [src/digitalmodel/materials/database.py:L38-L39] [src/digitalmodel/materials/database.py:L60-L68] [src/digitalmodel/materials/database.py:L123-L143] | C09; callers must inspect flag; #1204 |
| S18 `RepositoryProvenance` | Terms defaults describe repository reuse and leave original-source qualification unresolved; date parse exception re-raises. [src/digitalmodel/materials/database_schema.py:L52-L78] | C02/C09; no swallowed exception; #1204 |
| S19 `MaterialRecord.edition` | `continue` skips text checks only for intentional null edition. [src/digitalmodel/materials/database_schema.py:L102-L109] | C06; #1204 |
| S20 `sn_curve_ids` | Null default is absent grade-to-detail evidence, not an empty association list. [src/digitalmodel/materials/database_schema.py:L83-L99] | C02/C06; #1204 |
| S21 `standards_registry_id` | Null default is unresolved registry linkage. [src/digitalmodel/materials/database_schema.py:L83-L99] | C02/C06; #1204 |
| S22 `structural_shape_id` | Null default is unresolved shape linkage. [src/digitalmodel/materials/database_schema.py:L83-L99] | C02/C06; #1204 |
| S23 `Strengths.units` | MPa default with finite-positive and dimensional token validation. [src/digitalmodel/materials/database_schema.py:L20-L37] | C01; guarded behavior |
| S24 `_magnitude` | Bare reals assume suffix units; quantity conversion casts to float; string/bool/non-finite inputs rejected; dimensionality exception re-raises. [src/digitalmodel/materials/ffs_material_library.py:L84-L110] | C04/C12; no swallowed exception |
| S25 `_optional` | Missing input returns None; route-specific validation occurs later. [src/digitalmodel/materials/ffs_material_library.py:L113-L114] [src/digitalmodel/materials/ffs_material_library.py:L299-L324] | C04; guarded optional inputs |
| S26 automatic route | Multiple supplied reference inputs are resolved by fixed precedence, potentially leaving inputs unused. [src/digitalmodel/materials/ffs_material_library.py:L277-L287] | C12 covers ordinary route selection; no detection check exists for competing inputs; issue pending (2) |
| S27 fracture defaults | Fractile defaults to 0.05; reference thickness and class/yield/window limits are implicit source assumptions; source coefficients include a zero temperature shift. [src/digitalmodel/materials/ffs_material_library.py:L123-L165] [src/digitalmodel/materials/ffs_material_library.py:L216-L230] | C04 tests scope; no detection check exists for independent coefficient-source qualification; issue pending (3) |
| S28 extrapolation | Explicit extrapolation bypasses temperature-window rejection but appends a note. [src/digitalmodel/materials/ffs_material_library.py:L329-L337] | C04 temperature-window test; flagged behavior [tests/materials/test_ffs_material_library.py:L165-L172] |
| S29 toughness caps / ASME scope | Finite nonpositive caps are accepted; ASME yield only has upper-bound check and returned thickness lacks positivity check. [src/digitalmodel/materials/ffs_material_library.py:L344-L368] | no detection check exists for these inputs; issue pending (2) |
| S30 curve inputs | Explicit uniform true strain is required; assessment temperature remains recorded without thermal derating. No Considere inference from n is made for the offset law | `tests/materials/test_audit_regressions.py::test_curve_requires_explicit_uniform_strain` asserts rejection by `TrueStressStrainCurve` and `FFSMaterialInputs.true_stress_strain_curve`; [#2731](https://github.com/vamseeachanta/digitalmodel/issues/2731) |
| S31 curve strain conversion | `float()` on total/plastic strain has no finite or unit validation; plastic strain delegates through total stress. [src/digitalmodel/materials/ffs_material_library.py:L473-L509] | no detection check exists for NaN/infinity or textual strain; issue pending (2) |
| S32 total table | `float(strain_max)` lacks finite validation; absent maximum becomes twice ultimate; zero origin row is deliberate. [src/digitalmodel/materials/ffs_material_library.py:L517-L536] | C11 covers ordinary rows; no detection check exists for non-finite maxima; issue pending (2) |
| S33 plastic table | Same maximum default/conversion risk; zero initial plastic strain is deliberate. [src/digitalmodel/materials/ffs_material_library.py:L538-L556] | C11 covers ordinary rows; no detection check exists for non-finite maxima; issue pending (2) |
| S34 `_charpy_test_temperature` | Non-hull or missing class returns None; unknown table key also returns None. [src/digitalmodel/materials/ffs_material_library.py:L622-L626] | Existing hull lookup characterizes non-hull None; no detection check exists for malformed hull class; issue pending (2) [tests/materials/test_ffs_material_library.py:L375-L378] |
| S35 FFS input record / lookup | Default temperature records assessment conditions without derating; missing hardening exponent remains None and curve creation rejects it; `with_properties` copies without independent record validation. [src/digitalmodel/materials/ffs_material_library.py:L568-L613] [src/digitalmodel/materials/ffs_material_library.py:L629-L660] | C11 checks constructed curves; no detection check exists for invalid replacement records; issue pending (2) |
| S36 hull temperature tables | Unit-tagged table constants include zero entries; a test temperature is not an assessment-temperature toughness measurement. [src/digitalmodel/materials/ffs_material_library.py:L562-L565] [src/digitalmodel/materials/ffs_material_library.py:L622-L626] | Hull lookup test characterizes one entry only; no detection check exists for original table qualification; #1204 [tests/materials/test_ffs_material_library.py:L375-L378] |

No record-skipping loop, swallowed exception, integer parsing of external text, epoch-year fallback or cache write appears in the six-file package; the one `continue` and two exception handlers above implement nullable metadata or re-raise failures. [src/digitalmodel/materials/database_schema.py:L71-L78] [src/digitalmodel/materials/database_schema.py:L102-L109] [src/digitalmodel/materials/ffs_material_library.py:L92-L110]

Additional confirmed documentation defects are the historical grade-count mismatch and incomplete edition/provenance qualification; C06/C09 characterize the metadata boundary, while original-source and documentation freshness checks do not exist, issue pending (3). [tests/materials/test_grade_matrix.py:L140-L150] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L80-L84] [docs/domains/materials/database-gaps.html:L39-L61]

## 7. Lessons log

| Date | Entry | Lesson / pack change | Evidence | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-10-17 | Initial pack | Source qualification, regression parity and material applicability will be tracked separately; source snapshots will not be silently converged | [src/digitalmodel/materials/grades.py:L366-L376] [src/digitalmodel/materials/database.py:L123-L129] | Proposed record; issue pending (3) |

No historical clearance is asserted by this initial entry; future entries will retain the date, change reference and evidence without rewriting earlier rows.

## 8. Ownership

Proposed review assignment for this draft: engineering calculations quality manager; deputy: independent engineering calculations reviewer; producer responsibility: materials documentation and tests; unresolved engineering classifications will be referred to the family quality manager, with cross-family scope referred to the maintainer. These are review assignments for this pack, not claims of completed reviews. (AQG r0, section 11.)

## 9. Dependencies and implementation inventory

Upstream dependencies are Python standard-library utilities, the grade registry, Pint and `digitalmodel.units.Q_`, plus the delegated Charpy routine; database records depend on package source bytes, not an external service. [src/digitalmodel/materials/database.py:L10-L26] [src/digitalmodel/materials/database.py:L131-L143] [src/digitalmodel/materials/ffs_material_library.py:L56-L67] [src/digitalmodel/materials/line_pipe.py:L21-L25]

Every source mention of OrcaFlex, case-insensitive, is listed here: the unit-helper comment names linetype usage, the legacy table comment names `orcaflex.riser_config.PIPE_GRADES`, and the legacy adapter docstring repeats that dependency; these are text references and compatibility data, with no interface import, launch or version requirement. [src/digitalmodel/materials/grades.py:L341-L348] [src/digitalmodel/materials/grades.py:L403-L410] [src/digitalmodel/materials/grades.py:L451-L466]

Downstream grade-table consumers are documented in the migration matrix; legacy-preserved and canonical-derived routes remain distinct, with convergence tracked by #1147. [docs/domains/grade-table-migration-2026-06-28.md:L51-L74] [src/digitalmodel/materials/grades.py:L366-L376]

### Map basis and counted family

Map source: the repository-relative implementation map. Its embedded historical revision does not pin the current materials source. Map citations below resolve at the assigned revision; generated inventory claims retain their snapshot scope. [docs/registry/implementation-map.yaml:L1-L4]

The map snapshot lists `api-5ct-5l-grade-tables` as the in-package family. Its row counts are generated metadata, not a current completeness claim. [docs/registry/implementation-map.yaml:L24063-L24069] [docs/registry/implementation-map.yaml:L24216-L24225]

### Inventory boundary

The implementation map is a discovery index. Its embedded historical revision
and definition line numbers describe that generated snapshot; they are not a
current inventory or an independent qualification of its entries. The map's
materials row identifies the grade registry. Package methods outside that
family still require the associations described below.

The historical signals below were re-run with case-sensitive Python `re.search`
over each package Python file at the assigned revision. Each matching line is
assigned to its smallest enclosing function/class; module-level matches are
grouped once per file. This produced 34 enclosing definitions across six source
files. It is a text-discovery count, not a count of qualified implementations.
Repository-wide searches were repeated over `src`, `scripts`, `examples` and
`tests`. The earlier counted inventory and repository-wide file totals are
removed; text matches and generated map rows do not establish completeness.

```python
signals = [
    r'(?:API.?5[CLT]|_api5l|API5L|API5CT|MATERIAL_LIBRARY|STEEL_GRADES|GRADE_REGISTRY|_GRADES|steel_material|smys|SMYS)',
    r'API.?5CT|API.?5L|steel.grade|casing.grade',
    r'\b(?:J55|K55|N80|L80|P110|Q125|X42|X52|X60|X65|X70|X80)\b',
]
```

Source locators in this pack refer to the assigned revision, including map
metadata; no completeness or zero-miss claim is made for the generated map.

Map follow-up: pipe dimensions and section properties, IACS/EN/ASTM registry families, toughness/Charpy-transition/Master Curve/ASME routes and the tensile constitutive model need explicit family coverage or association; metadata projection and the delegated Charpy route will remain classified as helpers where applicable, issue pending (4). [src/digitalmodel/materials/line_pipe.py:L34-L215] [src/digitalmodel/materials/grades.py:L172-L217] [src/digitalmodel/materials/ffs_material_library.py:L197-L213] [src/digitalmodel/materials/ffs_material_library.py:L299-L368] [src/digitalmodel/materials/ffs_material_library.py:L473-L556]

## 10. Coverage and open items

### Read coverage

All six Python files in `src/digitalmodel/materials` were read: `__init__.py`, `grades.py`, `line_pipe.py`, `ffs_material_library.py`, `database_schema.py` and `database.py`; the inspected package has no nested Python subpackage. [src/digitalmodel/materials/__init__.py:L15-L48] [src/digitalmodel/materials/grades.py:L1-L17] [src/digitalmodel/materials/line_pipe.py:L1-L18] [src/digitalmodel/materials/ffs_material_library.py:L1-L53] [src/digitalmodel/materials/database_schema.py:L1-L5] [src/digitalmodel/materials/database.py:L1-L7]

Documentation found by searching for `digitalmodel.materials`, `src/digitalmodel/materials`, `materials/grades` and `material.grade matrix` in repository Markdown/HTML, with routing/map discovery added: the operator map, module-routing registry, implementation map and its Markdown index, codes register, grade-table migration record, the two materials validation records, materials database gaps page and the FFS offering plan. No package README was found in the package-file enumeration. [docs/maps/digitalmodel-operator-map.md:L79-L79] [docs/registry/module-routing.yaml:L729-L738] [docs/registry/implementation-map.md:L26-L26] [docs/domains/codes-register.md:L109-L121] [docs/domains/grade-table-migration-2026-06-28.md:L1-L21] [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L1-L25] [docs/domains/materials/line-pipe-validation-2026-06-28.md:L1-L20] [docs/domains/materials/database-gaps.html:L6-L23] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L138-L143]

The five materials test files were read and selected in full, including all parametrized cases; historical documentation counts are not used as current collection counts. [tests/materials/test_grade_matrix.py:L20-L52] [tests/materials/test_line_pipe.py:L18-L53] [tests/materials/test_database.py:L33-L50] [tests/materials/test_ffs_material_library.py:L1-L18]

Historical revision-0 offline receipt (not the current code pin): `PYTHONPATH=src uv run --no-sources python -m pytest tests/materials/ -q -rA -p no:cacheprovider --basetemp=<scratch>` was used before and after documentation edits, with the same test configuration and no addopts override. Each completed run collected 179 nodes: 179 passed, 0 failed, 0 skipped, 0 NOT_RUN; the passing node sets match exactly. By file: database 78, FFS 33, grades 29 and line pipe 39. The initial environment installation completed, but that first invocation exited 141 before collection; it is not counted as a test run. [tests/materials/test_database.py:L21-L30] [tests/materials/test_ffs_material_library.py:L1-L18] [tests/materials/test_grade_matrix.py:L20-L52] [tests/materials/test_line_pipe.py:L18-L53]

Historical revision-1 receipt: 212 materials tests passed, with 0 failures/skips. That count predates the provenance revisions and is not the current result. Its audit regressions plus the existing duplicate-key test produced 23 failures and 11 passes before implementation; mutations of duplicate acceptance, plastic-row construction and hardening produced four expected assertion failures.

Historical revision-2 receipt: `python -m pytest tests/materials -q -p no:cacheprovider` passes 245 nodes, with 0 failures/skips. The starting materials suite passed 238 nodes. Before documentation corrections, the database suite produced 2 expected failures and 93 passes. Configured-revision tests cover matching normalized bytes, altered bytes and missing Git history. Materials plus structure tests pass 299 nodes; the structure validator passes 18,053 paths. Licensed API verification and independent review remain pending.

Current revision-3 receipt: `python -m pytest tests/materials tests/repo_structure -q -rA -p no:cacheprovider` passes 301 nodes, with no failures or skips: 247 materials and 54 structure tests. Materials counts by file: database 97, audit 49, FFS 33, grade matrix 29 and line pipe 39. The structure validator passes 18,138 paths. The source revision is a main ancestor and its normalized grade-source digest matches the recorded digest. Configured-revision checks retain altered-byte and unavailable-history rejection; the new assertions guard assigned provenance, pack locator resolution and CI history. Original-source qualification and independent review remain pending.

### Open items

| Item | Scope / evidence needed | Tracking / responsible role |
| --- | --- | --- |
| Repository provenance | Assigned source revision and normalized digest identify the same grade text. Locators use that revision; generated inventory claims are bounded to their snapshot. CI retains full history and missing history fails the check. | #2778; materials maintainer |
| U1 | UNVERIFIED: original qualification of grade values, dimensions and physical defaults; publisher sources and editions have not been checked here; existing values stay referenced rather than reproduced. [docs/domains/materials/database-gaps.html:L39-L61] | #1204 and issue pending (3); materials maintainer |
| U2 | UNVERIFIED: correspondence of toughness coefficients and published-anchor tests to the named original publications; only the repository statements were inspected. [src/digitalmodel/materials/ffs_material_library.py:L123-L165] [tests/materials/test_ffs_material_library.py:L5-L17] | issue pending (3); engineering calculations quality manager |
| U3 | UNVERIFIED: calibrated, independent end-to-end FFS material assessment example; V1 covers a synthetic boundary; V2–V6 are formula anchors, not calibrated end-to-end assessments. [tests/materials/test_ffs_material_library.py:L269-L297] | issue pending (3); engineering calculations quality manager |
| Input guards | S01 grade identity corrected under #2727; remaining S02/S04–S14 gaps concern product labels, finite geometry and catalogue fallback. [src/digitalmodel/materials/grades.py:L237-L274] [src/digitalmodel/materials/line_pipe.py:L153-L160] [src/digitalmodel/materials/line_pipe.py:L297-L321] | issue pending (1); materials maintainer |
| FFS guards | S26/S29/S31–S35 gaps concerning unused reference inputs, cap/yield/thickness scope and finite evaluator/table inputs. [src/digitalmodel/materials/ffs_material_library.py:L277-L287] [src/digitalmodel/materials/ffs_material_library.py:L344-L368] [src/digitalmodel/materials/ffs_material_library.py:L473-L556] | issue pending (2); materials maintainer |
| Metadata and links | Null editions, grade-to-S–N associations, standards-registry and structural-shape links, original rights and dimensional-source readiness. [docs/domains/materials/database-gaps.html:L39-L78] | #1204; materials maintainer |
| Historical convergence | Consumer-specific snapshots require an explicit engineering-basis decision before convergence. [src/digitalmodel/materials/grades.py:L366-L376] | #1147; materials maintainer |
| Documentation and citations | Stale grade count, edition ambiguity, missing calculation sidecars and downstream report evidence integration. [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L80-L84] [src/digitalmodel/materials/database.py:L95-L114] [src/digitalmodel/materials/ffs_material_library.py:L299-L368] | issue pending (3); materials maintainer |
| Map follow-up | Section-9 uncounted methods need map rows before entering counted-family coverage. [src/digitalmodel/materials/line_pipe.py:L34-L215] [src/digitalmodel/materials/ffs_material_library.py:L473-L556] | issue pending (4); materials maintainer |
| Review disposition | This DRAFT will require independent A1–A8 review, actual issue numbers replacing draft references, and a gate disposition before clearance | issue pending (3); engineering calculations quality manager; AQG r0 section 11 |
