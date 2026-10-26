# Cathodic protection edition-path review

## Summary

The source review records implemented edition selection at commit
`d34bda5cf18d11be48b17c7ba94e5c8127af2bf2`. The record
contains 28 entries: 16 PASS and
12 FAIL under the criterion below. CP source
and calculator tests are unchanged. Numerical design results are not issued by
this review.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-25 | Record implemented edition branches, shared behavior and evidence gaps so edition claims can be limited to reviewed code. |
| 1 | 2026-10-26 | Add the existing F103 field-joint citation assertions and record the uncovered B401 retrofit clause; update dispositions to match the identified evidence. |

Table 1. Review revision history.

## Basis and acceptance criterion

The evidence is repository source and test code at the pinned commit. The record
assesses behavior implemented by these routes, rather than differences between
publications. No DNV document, licensed table dataset or external secondary
source is opened for this review. Source-code citation labels are recorded as
labels already carried by the implementation; their correctness against a
publication is not established here. Existing `verified-*` provenance strings
are software outputs, not a new source-validation result.

PASS requires an implemented path, an identified existing test that asserts the
stated behavior, and an attached table or clause label appropriate to that path.
A label may be a `Citation` on a returned value or an explicit rule string; the
record identifies the form. FAIL names a missing item. A covered neighboring
helper does not establish an untested caller branch. Routing-policy rows with
software labels but no clause citation remain FAIL for outward edition claims;
that disposition does not establish a defect in the routing policy.

PASS records establish the limited behavior described in each row. They do not
establish exhaustive input coverage, correctness of a standard, a case verdict,
or the magnitude of a design change. No reason for a publication change is
inferred. A FAIL row is excluded from edition claims until its named gap is
resolved and reviewed. A completed audit may contain FAIL rows; passing record
integrity tests does not promote those rows to PASS.

Public-eligible case-pack metadata is inspected only to identify route and
provenance context. No case-pack numerical result is an oracle for this record.
Client-derived cases and uncited legacy solver routes are excluded. No solver
campaign, chart or newsletter prose is produced. The ABS ship table route has a
fixed citation revision; an ABS edition comparison is not established.

The machine-readable companion is [edition-paths-review.json](edition-paths-review.json).
It is a list with one object per reviewed topic. `kind` distinguishes numerical
or label differences, software routing, shared behavior and the ABS scope limit.
`file`, `line`, `anchor` and `evidence` locate the observed source; `commit` pins
the bytes. `test` contains existing test references, not a claim that each named
test was executed in this review. `label` transcribes the code-carried edition
and clause identifiers without reproducing publication values. `missing` explains
every FAIL; `coverage` limits what the named assertions establish.

## Rechecked inventory locations

| Symbol | Inventory reference | Verified location at the pinned commit | Disposition |
| --- | --- | --- | --- |
| `_CATEGORY_IV_EDITIONS` | `116` | [`src/digitalmodel/cathodic_protection/b401_tables.py:116`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L116) | Matches the inspected ref. |
| `_ANODE_TEMPERATURE_EDITIONS` | `119` | [`src/digitalmodel/cathodic_protection/b401_tables.py:119`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L119) | Matches the inspected ref. |
| `design_driving_voltage` | `913` | [`src/digitalmodel/cathodic_protection/b401_tables.py:913`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L913) | Matches the inspected ref. |
| `Edition` | `27` | [`src/digitalmodel/cathodic_protection/_edition.py:27`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L27) | Matches the inspected ref. |
| `_run_b401` | `562` | [`src/digitalmodel/cathodic_protection/engine_adapter.py:562`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L562) | Matches the inspected ref. |

Table 2. Spec inventory locations rechecked against the available source ref.

## Review register

The shared-value entries state only that the implemented B401 routes carry the
same maps, factors, resistance functions and design-life handling for identical
inputs. Edition-dependent anode properties can still change output current.
Explicit overrides and component-local or phase-local lives remain separate
inputs. No equivalence of standard editions is inferred.

| Record | Code | Kind | Status | Primary source |
| --- | --- | --- | --- | --- |
| [b401-category-iv](#b401-category-iv) | B401 | difference | PASS | [`src/digitalmodel/cathodic_protection/b401_tables.py:116`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L116) |
| [b401-anode-temperature](#b401-anode-temperature) | B401 | difference | PASS | [`src/digitalmodel/cathodic_protection/b401_tables.py:119`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L119) |
| [b401-driving-voltage](#b401-driving-voltage) | B401 | difference | PASS | [`src/digitalmodel/cathodic_protection/b401_tables.py:913`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L913) |
| [b401-retrofit](#b401-retrofit) | B401 | difference | FAIL | [`src/digitalmodel/cathodic_protection/b401_structures_phases_retrofit.py:127`](../../../../src/digitalmodel/cathodic_protection/b401_structures_phases_retrofit.py#L127) |
| [b401-labels](#b401-labels) | B401 | difference | PASS | [`src/digitalmodel/cathodic_protection/b401_tables.py:76`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L76) |
| [b401-report-retrofit](#b401-report-retrofit) | B401 | difference | FAIL | [`src/digitalmodel/cathodic_protection/report_recommendations.py:98`](../../../../src/digitalmodel/cathodic_protection/report_recommendations.py#L98) |
| [b401-edition-routing](#b401-edition-routing) | B401 | routing | FAIL | [`src/digitalmodel/cathodic_protection/_edition.py:27`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L27) |
| [b401-f103-companion](#b401-f103-companion) | B401 | difference | FAIL | [`src/digitalmodel/cathodic_protection/dnv_rp_b401.py:93`](../../../../src/digitalmodel/cathodic_protection/dnv_rp_b401.py#L93) |
| [f103-linepipe](#f103-linepipe) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:786`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L786) |
| [f103-field-joints](#f103-field-joints) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:874`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L874) |
| [f103-temperature-bands](#f103-temperature-bands) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:278`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L278) |
| [f103-ambient](#f103-ambient) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:1138`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1138) |
| [f103-hot-potential-guard](#f103-hot-potential-guard) | F103 | difference | FAIL | [`src/digitalmodel/cathodic_protection/f103_tables.py:1198`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1198) |
| [f103-anode-sources](#f103-anode-sources) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:116`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L116) |
| [f103-utilisation-source](#f103-utilisation-source) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:1088`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1088) |
| [f103-labels](#f103-labels) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:87`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L87) |
| [f103-bank-equations](#f103-bank-equations) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/f103_tables.py:666`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L666) |
| [f103-engine-joints](#f103-engine-joints) | F103 | difference | PASS | [`src/digitalmodel/cathodic_protection/engine_adapter.py:844`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L844) |
| [f103-component-joints](#f103-component-joints) | F103 | difference | FAIL | [`src/digitalmodel/cathodic_protection/b401_component_zones.py:63`](../../../../src/digitalmodel/cathodic_protection/b401_component_zones.py#L63) |
| [f103-bank-joints](#f103-bank-joints) | F103 | difference | FAIL | [`src/digitalmodel/cathodic_protection/pipeline_anode_bank.py:45`](../../../../src/digitalmodel/cathodic_protection/pipeline_anode_bank.py#L45) |
| [f103-edition-routing](#f103-edition-routing) | F103 | routing | FAIL | [`src/digitalmodel/cathodic_protection/_edition.py:157`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L157) |
| [f103-pinned-key](#f103-pinned-key) | F103 | routing | FAIL | [`src/digitalmodel/cathodic_protection/engine_adapter.py:880`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L880) |
| [b401-current-densities](#b401-current-densities) | B401 | shared | PASS | [`src/digitalmodel/cathodic_protection/b401_tables.py:675`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L675) |
| [b401-utilisation](#b401-utilisation) | B401 | shared | PASS | [`src/digitalmodel/cathodic_protection/b401_tables.py:414`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L414) |
| [b401-resistance](#b401-resistance) | B401 | shared | FAIL | [`src/digitalmodel/cathodic_protection/anode_sizing.py:313`](../../../../src/digitalmodel/cathodic_protection/anode_sizing.py#L313) |
| [b401-design-life](#b401-design-life) | B401 | shared | FAIL | [`src/digitalmodel/cathodic_protection/engine_adapter.py:610`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L610) |
| [abs-single-edition](#abs-single-edition) | ABS | scope | PASS | [`src/digitalmodel/cathodic_protection/abs_ships_tables.py:13`](../../../../src/digitalmodel/cathodic_protection/abs_ships_tables.py#L13) |
| [f103-result-metadata](#f103-result-metadata) | F103 | routing | FAIL | [`src/digitalmodel/cathodic_protection/dnv_rp_f103.py:78`](../../../../src/digitalmodel/cathodic_protection/dnv_rp_f103.py#L78) |

Table 3. Record dispositions against the implementation, coverage and label criterion.

### b401-category-iv

**PASS — B401; tokens: 2005, 2010, 2017, 2021.** The implemented coating lookup accepts paint category IV only for the 2021 token; earlier tokens reject it.

**Source:** [`src/digitalmodel/cathodic_protection/b401_tables.py:116`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L116); [`src/digitalmodel/cathodic_protection/b401_tables.py:756`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L756).

**Existing tests:** [`tests/cathodic_protection/test_edition_crosswalk.py::test_b401_category_iv_only_in_2021`](../../../../tests/cathodic_protection/test_edition_crosswalk.py)

**Code-carried label:** Code-carried B401 labels: 2005 and 2010 use revision 2011, Table 10-; 2017 uses revision 2017-06, Table A-; 2021 uses revision 2021-05, Table 8-. These are observed labels, without source-document validation. Coating constants carry the selected table ending in -4.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### b401-anode-temperature

**PASS — B401; tokens: 2005, 2010, 2017, 2021.** The 2021 token selects stepwise anode-surface-temperature rows; earlier tokens reject temperatures above AMBIENT_ANODE_TEMPERATURE_C. This concerns anode properties, separately from fluid-temperature current-density bands.

**Source:** [`src/digitalmodel/cathodic_protection/b401_tables.py:119`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L119); [`src/digitalmodel/cathodic_protection/b401_tables.py:637`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L637); [`src/digitalmodel/cathodic_protection/b401_tables.py:611`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L611); [`src/digitalmodel/cathodic_protection/b401_tables.py:123`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L123).

**Existing tests:** [`tests/cathodic_protection/test_edition_crosswalk.py::test_b401_2021_hot_anode_changes_driving_voltage_and_capacity`](../../../../tests/cathodic_protection/test_edition_crosswalk.py); [`tests/cathodic_protection/test_b401_tables.py::test_ambient_editions_reject_anode_temperature_above_30`](../../../../tests/cathodic_protection/test_b401_tables.py); [`tests/cathodic_protection/test_b401_tables.py::test_table_8_6_rows_are_selected_stepwise`](../../../../tests/cathodic_protection/test_b401_tables.py)

**Code-carried label:** Code-carried B401 labels: 2005 and 2010 use revision 2011, Table 10-; 2017 uses revision 2017-06, Table A-; 2021 uses revision 2021-05, Table 8-. These are observed labels, without source-document validation. Anode values carry the selected table ending in -6.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### b401-driving-voltage

**PASS — B401; tokens: 2005, 2010, 2017, 2021.** The same subtraction uses the selected anode row and shared protective-potential value; the zinc seawater result differs between the earlier tokens and 2021.

**Source:** [`src/digitalmodel/cathodic_protection/b401_tables.py:913`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L913); [`src/digitalmodel/cathodic_protection/b401_tables.py:946`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L946); [`src/digitalmodel/cathodic_protection/b401_tables.py:423`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L423).

**Existing tests:** [`tests/cathodic_protection/test_edition_crosswalk.py::test_b401_zinc_seawater_potential_changes_in_2021`](../../../../tests/cathodic_protection/test_edition_crosswalk.py); [`tests/cathodic_protection/test_b401_tables.py::test_design_driving_voltage_seawater`](../../../../tests/cathodic_protection/test_b401_tables.py)

**Code-carried label:** Code-carried B401 labels: 2005 and 2010 use revision 2011, Table 10-; 2017 uses revision 2017-06, Table A-; 2021 uses revision 2021-05, Table 8-. These are observed labels, without source-document validation. Anode table ending in -6 with Sec. 5 (2005/2010), [5.4] (2017), or [2.4.2] (2021).

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### b401-retrofit

**FAIL — B401; tokens: 2005, 2010, 2017, 2021.** The 2021 branch requires all entries in _CONDITION_FIELDS. Earlier tokens require a condition mapping but do not execute that field loop. The method_basis label also branches by edition.

**Source:** [`src/digitalmodel/cathodic_protection/b401_structures_phases_retrofit.py:127`](../../../../src/digitalmodel/cathodic_protection/b401_structures_phases_retrofit.py#L127); [`src/digitalmodel/cathodic_protection/b401_structures_phases_retrofit.py:65`](../../../../src/digitalmodel/cathodic_protection/b401_structures_phases_retrofit.py#L65); [`src/digitalmodel/cathodic_protection/b401_structures_phases_schema.py:13`](../../../../src/digitalmodel/cathodic_protection/b401_structures_phases_schema.py#L13).

**Existing tests:** [`tests/cathodic_protection/test_b401_structures_phases.py::test_retrofit_project_practice_and_2021_condition_inputs_fail_closed`](../../../../tests/cathodic_protection/test_b401_structures_phases.py)

**Code-carried label:** Code-carried retrofit rule labels: 2005/2010 Sec. 7.13.3 (retrofit documentation); 2017 [7.13.3] (retrofit documentation); 2021 [7.4.1], [7.5.3] (retrofit evidence). These are strings in RULES, rather than Citation objects.

**Coverage limit:** The existing test removes potential_history under the default 2021 fixture and checks rejection. It does not establish the opposite branch or method_basis output.

**Missing item:** No identified regression checks omission of the condition fields under every earlier token or asserts both edition-dependent method_basis labels.

### b401-labels

**PASS — B401; tokens: 2005, 2010, 2017, 2021.** Source-map revision, table prefix, protective-potential clause and buried-density clause vary by token. The 2005 record carries revision 2011; token and citation revision are not interchangeable.

**Source:** [`src/digitalmodel/cathodic_protection/b401_tables.py:76`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L76); [`src/digitalmodel/cathodic_protection/b401_tables.py:482`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L482); [`src/digitalmodel/cathodic_protection/b401_structures_phases_schema.py:13`](../../../../src/digitalmodel/cathodic_protection/b401_structures_phases_schema.py#L13); [`src/digitalmodel/cathodic_protection/b401_structures_phases_schema.py:158`](../../../../src/digitalmodel/cathodic_protection/b401_structures_phases_schema.py#L158).

**Existing tests:** [`tests/cathodic_protection/test_b401_tables.py::test_every_result_carries_the_edition_citation`](../../../../tests/cathodic_protection/test_b401_tables.py); [`tests/cathodic_protection/test_b401_tables.py::test_table_label_follows_edition_numbering`](../../../../tests/cathodic_protection/test_b401_tables.py); [`tests/cathodic_protection/test_b401_tables.py::test_protection_potential`](../../../../tests/cathodic_protection/test_b401_tables.py); [`tests/cathodic_protection/test_b401_tables.py::test_buried_current_density`](../../../../tests/cathodic_protection/test_b401_tables.py)

**Code-carried label:** Code-carried B401 labels: 2005 and 2010 use revision 2011, Table 10-; 2017 uses revision 2017-06, Table A-; 2021 uses revision 2021-05, Table 8-. These are observed labels, without source-document validation. Buried labels are Sec. 6.3 (2005/2010), [6.3.8] (2017), [3.3.8] (2021).

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### b401-report-retrofit

**FAIL — B401; tokens: 2005, 2010, 2017, 2021.** The covered report behavior selects retrofit evidence for 2021 and retrofit documentation for earlier tokens; correctness of the emitted retrofit clause is not established.

**Source:** [`src/digitalmodel/cathodic_protection/report_recommendations.py:98`](../../../../src/digitalmodel/cathodic_protection/report_recommendations.py#L98); [`src/digitalmodel/cathodic_protection/report_recommendations.py:132`](../../../../src/digitalmodel/cathodic_protection/report_recommendations.py#L132).

**Existing tests:** [`tests/cathodic_protection/test_report_recommendations.py::test_verified_options_use_edition_specific_retrofit_subject`](../../../../tests/cathodic_protection/test_report_recommendations.py)

**Code-carried label:** RULES: 2005/2010 Sec. 7.13.3; 2017 [7.13.3]; 2021 [7.4.1], [7.5.3]. The report appends these clause labels to its source text.

**Coverage limit:** The named test asserts the edition-dependent subject text only; the emitted retrofit clause is not asserted.

**Missing item:** No identified assertion checks the emitted retrofit clause for every supported token.

### b401-edition-routing

**FAIL — B401; tokens: 2005, 2010, 2017, 2021, 2008, 2011.** Canonical tokens are defined by Edition. The 2008 alias maps to 2005, the 2011 alias maps to 2010, missing input warns and defaults to 2021, and unsupported tokens including 1993 are rejected.

**Source:** [`src/digitalmodel/cathodic_protection/_edition.py:27`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L27); [`src/digitalmodel/cathodic_protection/_edition.py:36`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L36); [`src/digitalmodel/cathodic_protection/_edition.py:127`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L127); [`src/digitalmodel/cathodic_protection/engine_adapter.py:562`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L562).

**Existing tests:** [`tests/cathodic_protection/test_edition_api_foundation.py::test_normalize_edition_accepts_supported_aliases`](../../../../tests/cathodic_protection/test_edition_api_foundation.py); [`tests/cathodic_protection/test_edition_api_foundation.py::test_normalize_edition_warns_and_defaults_when_missing`](../../../../tests/cathodic_protection/test_edition_api_foundation.py); [`tests/cathodic_protection/test_edition_api_foundation.py::test_normalize_edition_rejects_unknown_values`](../../../../tests/cathodic_protection/test_edition_api_foundation.py)

**Code-carried label:** Software routing labels in STANDARD_BY_EDITION; no normative clause is attached to alias/default policy.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

**Missing item:** Alias/default policy has software labels but no attached clause citation; it is not a standards-difference claim.

### b401-f103-companion

**FAIL — B401; tokens: 2005, 2010, 2017, 2021.** The protected_length wrapper maps 2005/2010 to F103 2010 and 2017/2021 to F103 2019; the downstream formula has no edition-dependent arithmetic.

**Source:** [`src/digitalmodel/cathodic_protection/dnv_rp_b401.py:93`](../../../../src/digitalmodel/cathodic_protection/dnv_rp_b401.py#L93); [`src/digitalmodel/cathodic_protection/dnv_rp_b401.py:369`](../../../../src/digitalmodel/cathodic_protection/dnv_rp_b401.py#L369).

**Existing tests:** No eligible covering test is identified.

**Code-carried label:** The wrapper docstring labels F103 Sec. 5.6.7, Eq. 14 only; no edition-specific Citation is returned.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

**Missing item:** No eligible covering test is used for this wrapper; existing riser benchmark coverage is outside this review basis. The numeric return has no attached edition-specific Citation.

### f103-linepipe

**PASS — F103; tokens: 2010, 2019.** Edition selects different coating rows and constants. The 2019 route can split rows by concrete-weight-coating flag and rejects some earlier coating identifiers; some newer coating identifiers are rejected under 2010.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:786`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L786); [`src/digitalmodel/cathodic_protection/f103_tables.py:829`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L829).

**Existing tests:** [`tests/cathodic_protection/test_edition_crosswalk.py::test_f103_flowline_60c_fbe_differs_between_2010_and_2019`](../../../../tests/cathodic_protection/test_edition_crosswalk.py); [`tests/cathodic_protection/test_edition_crosswalk.py::test_f103_2019_concrete_weight_coating_selects_the_fbe_row`](../../../../tests/cathodic_protection/test_edition_crosswalk.py); [`tests/cathodic_protection/test_f103_tables.py::test_coatings_absent_from_2019_table_raise`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_2019_only_coatings_raise_under_2010`](../../../../tests/cathodic_protection/test_f103_tables.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2. Coating constants carry Table A.1 or Table A-1.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-field-joints

**PASS — F103; tokens: 2010, 2019.** Edition selects different field-joint enums and rows. The older NONE maps to the newer bare-steel row; other older identifiers are rejected under 2019, and newer enum members are rejected under 2010. The 2019 resolver also distinguishes infill and amended identifier aliases.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:874`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L874); [`src/digitalmodel/cathodic_protection/f103_tables.py:1034`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1034).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_2010_none_maps_to_2019_bare_steel_row`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_other_2010_field_joint_ids_raise_under_2019`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_2019_field_joint_ids_raise_under_2010`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_resolve_2019_field_joint_3a_requires_infill`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_resolve_2019_field_joint_amended_name`](../../../../tests/cathodic_protection/test_f103_tables.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2. Field-joint constants carry Table A.2 or Table A-2.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-temperature-bands

**PASS — F103; tokens: 2010, 2019.** The 2019 fluid-temperature bands split the lowest 2010 band. The edition-specific mean-current-density maps also differ in overlapping bands; this is more than a band-label change.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:278`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L278); [`src/digitalmodel/cathodic_protection/f103_tables.py:328`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L328); [`src/digitalmodel/cathodic_protection/f103_tables.py:714`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L714).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_temperature_bands_per_edition`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_fluid_temperature_band_boundaries_2010`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_fluid_temperature_band_boundaries_2019`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_edition_crosswalk.py::test_f103_flowline_60c_fbe_differs_between_2010_and_2019`](../../../../tests/cathodic_protection/test_edition_crosswalk.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2. Current density carries Table 5-1 or Table 6-2.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-ambient

**PASS — F103; tokens: 2010, 2019.** The 2010 capacity lookup enforces the B401 ambient cap; the 2019 capacity lookup uses stepwise anode-surface-temperature rows. Potential and driving-voltage accessors share this guard, but coverage of each is assessed separately.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:1138`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1138); [`src/digitalmodel/cathodic_protection/f103_tables.py:1150`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1150).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_anode_temperature_above_30_rejected_under_2010`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_edition_crosswalk.py::test_f103_2019_buried_hot_anode_uses_table_6_3_row`](../../../../tests/cathodic_protection/test_edition_crosswalk.py)

**Code-carried label:** F103 2010 defers to B401 2010, revision 2011, Table 10-6; F103 2019 uses revision 2019-09, Table 6-3.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-hot-potential-guard

**FAIL — F103; tokens: 2010, 2019.** The 2010 potential and driving-voltage lookups also reject anode temperatures above the ambient cap; the 2019 lookups use anode-temperature rows.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:1198`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1198); [`src/digitalmodel/cathodic_protection/f103_tables.py:1253`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1253).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_anode_values_2010_defer_to_b401_2010`](../../../../tests/cathodic_protection/test_f103_tables.py)

**Code-carried label:** B401 2010 revision 2011, Table 10-6 with Sec. 5; F103 2019 revision 2019-09, Table 6-3 with [6.7.11].

**Coverage limit:** Ambient values and citations are tested; the separate capacity rejection test cannot establish every public accessor guard.

**Missing item:** No identified test directly checks hot-anode rejection by both the 2010 potential and driving-voltage accessors.

### f103-anode-sources

**PASS — F103; tokens: 2010, 2019.** The 2010 anode properties defer to the companion B401 2010 table; 2019 uses its own cited table. The zinc potential and derived driving voltage can differ even at ambient temperature.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:116`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L116); [`src/digitalmodel/cathodic_protection/f103_tables.py:110`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L110); [`src/digitalmodel/cathodic_protection/f103_tables.py:1233`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1233).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_anode_values_2010_defer_to_b401_2010`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_anode_values_2019_match_table_6_3`](../../../../tests/cathodic_protection/test_f103_tables.py)

**Code-carried label:** B401 revision 2011 Table 10-6 and Sec. 5 for F103 2010; F103 revision 2019-09 Table 6-3 and [6.7.11] for 2019.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-utilisation-source

**PASS — F103; tokens: 2010, 2019.** The value is shared, but 2010 defers to a B401 utilisation citation and 2019 attaches an F103 clause citation.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:1088`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L1088).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_bracelet_utilisation_factor_2010_defers_to_b401_table_10_8`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_bracelet_utilisation_factor_2019_cites_clause_6_4_2`](../../../../tests/cathodic_protection/test_f103_tables.py)

**Code-carried label:** B401 revision 2011 Table 10-8 for F103 2010; F103 revision 2019-09 [6.4.2] (anode utilisation factor) for 2019.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-labels

**PASS — F103; tokens: 2010, 2019.** Edition selects citation revision, current-density table label, linepipe table label and field-joint table label.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:87`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L87); [`src/digitalmodel/cathodic_protection/f103_tables.py:650`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L650).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_every_result_carries_the_edition_citation`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_edition_crosswalk.py::test_f103_flowline_60c_fbe_differs_between_2010_and_2019`](../../../../tests/cathodic_protection/test_edition_crosswalk.py); [`tests/cathodic_protection/test_f103_tables.py::test_field_joint_coating_constants_match_table_a2_2010`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_field_joint_coating_constants_match_table_a2_2019`](../../../../tests/cathodic_protection/test_f103_tables.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-bank-equations

**PASS — F103; tokens: 2010, 2019.** The anode-bank equation and arrangement labels are selected by edition; the circuit and metallic-drop kernels are shared. A separate companion B401 resistance citation changes with the F103 edition.

**Source:** [`src/digitalmodel/cathodic_protection/f103_tables.py:666`](../../../../src/digitalmodel/cathodic_protection/f103_tables.py#L666); [`src/digitalmodel/cathodic_protection/pipeline_anode_bank.py:282`](../../../../src/digitalmodel/cathodic_protection/pipeline_anode_bank.py#L282); [`src/digitalmodel/cathodic_protection/pipeline_anode_bank.py:327`](../../../../src/digitalmodel/cathodic_protection/pipeline_anode_bank.py#L327).

**Existing tests:** [`tests/cathodic_protection/test_pipeline_anode_bank.py::test_formula_references_separate_standard_and_circuit_relation`](../../../../tests/cathodic_protection/test_pipeline_anode_bank.py); [`tests/cathodic_protection/test_pipeline_anode_bank.py::test_2010_uses_renumbered_sections_and_companion_table`](../../../../tests/cathodic_protection/test_pipeline_anode_bank.py)

**Code-carried label:** F103 2010 revision 2010 Sec. 5.6 Eq. (8)-(14), Eq. (16)-(17); 2019 revision 2019-09 Sec. 6.7 Eq. (9)-(16), Eq. (18)/(20), Appendix D.8.5-D.8.10. Companion B401 revision 2011 Table 10-7 or revision 2017-06 Table A-7. formula_references is a separate display list.

**Coverage limit:** Existing tests assert selected section/equation/resistance strings. They do not assert every clause string or verify any underlying publication.

### f103-engine-joints

**PASS — F103; tokens: 2010, 2019.** The YAML adapter resolves newer field-joint identifiers and infill under 2019, and uses the older enum under 2010.

**Source:** [`src/digitalmodel/cathodic_protection/engine_adapter.py:844`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L844).

**Existing tests:** [`tests/cathodic_protection/test_engine_adapter.py::test_f103_2019_fbe_field_joints_resolve_by_infill`](../../../../tests/cathodic_protection/test_engine_adapter.py); [`tests/cathodic_protection/test_engine_adapter.py::test_f103_2010_field_joint_ids_unchanged`](../../../../tests/cathodic_protection/test_engine_adapter.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2. Downstream field-joint constants carry Table A.2 or Table A-2.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### f103-component-joints

**FAIL — F103; tokens: 2010, 2019.** The B401 component coating helper independently selects the F103 field-joint parser by the coating edition.

**Source:** [`src/digitalmodel/cathodic_protection/b401_component_zones.py:63`](../../../../src/digitalmodel/cathodic_protection/b401_component_zones.py#L63).

**Existing tests:** [`tests/cathodic_protection/test_b401_riser_components.py::test_f103_field_joint_coating_is_edition_cited`](../../../../tests/cathodic_protection/test_b401_riser_components.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2. Downstream coating constants supply the citation.

**Coverage limit:** The existing component test exercises the newer parser; table-level enum tests do not establish both component branches.

**Missing item:** The identified component test covers 2019; no identified component-level regression covers the older field-joint branch.

### f103-bank-joints

**FAIL — F103; tokens: 2010, 2019.** The bank-side field-joint parser chooses the older or newer enum before the shared coating lookup.

**Source:** [`src/digitalmodel/cathodic_protection/pipeline_anode_bank.py:45`](../../../../src/digitalmodel/cathodic_protection/pipeline_anode_bank.py#L45).

**Existing tests:** [`tests/cathodic_protection/test_pipeline_anode_bank.py::test_fjc_extension_uses_area_weighted_side_current_per_length`](../../../../tests/cathodic_protection/test_pipeline_anode_bank.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2. Downstream field-joint constants supply the citation.

**Coverage limit:** The existing field-joint extension test uses the default 2019 input.

**Missing item:** No identified bank-side regression exercises field-joint parsing under both edition tokens.

### f103-edition-routing

**FAIL — F103; tokens: 2010, 2019, 2016, 2021.** Missing input warns and defaults to 2019; 2021 is an alias of 2019 and 2016 maps to 2019 with a warning. These are implemented token mappings, without a claim about publication equivalence.

**Source:** [`src/digitalmodel/cathodic_protection/_edition.py:157`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L157); [`src/digitalmodel/cathodic_protection/_edition.py:85`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L85); [`src/digitalmodel/cathodic_protection/_edition.py:113`](../../../../src/digitalmodel/cathodic_protection/_edition.py#L113).

**Existing tests:** [`tests/cathodic_protection/test_f103_tables.py::test_2016_aliases_to_2019_with_a_warning`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_2021_alias_gives_the_2019_tables`](../../../../tests/cathodic_protection/test_f103_tables.py); [`tests/cathodic_protection/test_f103_tables.py::test_edition_none_warns_and_defaults_to_2019`](../../../../tests/cathodic_protection/test_f103_tables.py)

**Code-carried label:** Software routing labels in F103_STANDARD_BY_EDITION; no normative clause is attached to alias/default policy.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

**Missing item:** Alias/default policy has no attached clause citation; it is not a standards-difference claim.

### f103-pinned-key

**FAIL — F103; tokens: 2010, 2019.** The neutral F103 key uses the selected or default 2019 token. The dated compatibility key pins 2010, warns and rejects a conflicting explicit edition.

**Source:** [`src/digitalmodel/cathodic_protection/engine_adapter.py:880`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L880).

**Existing tests:** [`tests/cathodic_protection/test_engine_adapter.py::test_neutral_f103_key_without_edition_runs_the_2019_default`](../../../../tests/cathodic_protection/test_engine_adapter.py); [`tests/cathodic_protection/test_engine_adapter.py::test_f103_2010_alias_pins_2010_with_deprecation`](../../../../tests/cathodic_protection/test_engine_adapter.py); [`tests/cathodic_protection/test_engine_adapter.py::test_f103_2010_alias_accepts_a_matching_edition`](../../../../tests/cathodic_protection/test_engine_adapter.py); [`tests/cathodic_protection/test_engine_adapter.py::test_f103_2010_alias_rejects_a_conflicting_edition`](../../../../tests/cathodic_protection/test_engine_adapter.py)

**Code-carried label:** Code-carried F103 labels: 2010 revision 2010, Table 5-1 / Table A.1 / Table A.2; 2019 revision 2019-09, Table 6-2 / Table A-1 / Table A-2. Returned design quantities carry downstream table citations; compatibility-key policy itself has no normative clause.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

**Missing item:** The key-pinning policy has no attached clause citation; it is a software compatibility rule.

### b401-current-densities

**PASS — B401; tokens: 2005, 2010, 2017, 2021.** These implemented routes index the same current-density maps; the edition selects the citation label. Reinforcement and buried surfaces also use shared data. This does not establish equivalence of the publications.

**Source:** [`src/digitalmodel/cathodic_protection/b401_tables.py:675`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L675); [`src/digitalmodel/cathodic_protection/b401_tables.py:714`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L714); [`src/digitalmodel/cathodic_protection/b401_tables.py:979`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L979).

**Existing tests:** [`tests/cathodic_protection/test_edition_crosswalk.py::test_b401_current_demand_identical_across_editions`](../../../../tests/cathodic_protection/test_edition_crosswalk.py); [`tests/cathodic_protection/test_b401_tables.py::test_design_current_density_matches_table_1`](../../../../tests/cathodic_protection/test_b401_tables.py); [`tests/cathodic_protection/test_b401_tables.py::test_design_current_density_mean_matches_table_2`](../../../../tests/cathodic_protection/test_b401_tables.py); [`tests/cathodic_protection/test_b401_tables.py::test_reinforcement_current_density_matches_table_3`](../../../../tests/cathodic_protection/test_b401_tables.py); [`tests/cathodic_protection/test_b401_tables.py::test_buried_current_density`](../../../../tests/cathodic_protection/test_b401_tables.py)

**Code-carried label:** Code-carried B401 labels: 2005 and 2010 use revision 2011, Table 10-; 2017 uses revision 2017-06, Table A-; 2021 uses revision 2021-05, Table 8-. These are observed labels, without source-document validation. Current density tables end in -1/-2/-3; buried density uses the source-map buried clause.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### b401-utilisation

**PASS — B401; tokens: 2005, 2010, 2017, 2021.** These implemented routes return the same shape-keyed utilisation values; edition changes the attached table label. Explicit input overrides remain a separate design basis.

**Source:** [`src/digitalmodel/cathodic_protection/b401_tables.py:414`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L414); [`src/digitalmodel/cathodic_protection/b401_tables.py:878`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L878).

**Existing tests:** [`tests/cathodic_protection/test_b401_tables.py::test_utilisation_factor_matches_table_8`](../../../../tests/cathodic_protection/test_b401_tables.py)

**Code-carried label:** Code-carried B401 labels: 2005 and 2010 use revision 2011, Table 10-; 2017 uses revision 2017-06, Table A-; 2021 uses revision 2021-05, Table 8-. These are observed labels, without source-document validation. Utilisation tables end in -8.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

### b401-resistance

**FAIL — B401; tokens: 2005, 2010, 2017, 2021.** The resistance function and shared kernels accept geometry and resistivity, with no edition argument; the same numerical resistance follows for identical inputs. Anode potential and resulting current output can still differ by edition.

**Source:** [`src/digitalmodel/cathodic_protection/anode_sizing.py:313`](../../../../src/digitalmodel/cathodic_protection/anode_sizing.py#L313); [`src/digitalmodel/cathodic_protection/b401_tables.py:882`](../../../../src/digitalmodel/cathodic_protection/b401_tables.py#L882).

**Existing tests:** [`tests/cathodic_protection/test_kernels.py::TestTable107Resistance::test_long_slender_standoff_review_a2`](../../../../tests/cathodic_protection/test_kernels.py); [`tests/cathodic_protection/test_kernels.py::TestTable107Resistance::test_short_slender_standoff_formula`](../../../../tests/cathodic_protection/test_kernels.py); [`tests/cathodic_protection/test_kernels.py::TestTable107Resistance::test_long_flush`](../../../../tests/cathodic_protection/test_kernels.py); [`tests/cathodic_protection/test_kernels.py::TestTable107Resistance::test_short_flush_or_bracelet`](../../../../tests/cathodic_protection/test_kernels.py)

**Code-carried label:** The numeric function docstring carries B401 Table 10-7. The separate anode_resistance_citation helper selects Table 10-7, Table A-7 or Table 8-7 by edition; the function itself returns a float.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

**Missing item:** The function returns no Citation sidecar; edition-specific labels are available only through a separate helper.

### b401-design-life

**FAIL — B401; tokens: 2005, 2010, 2017, 2021.** The flat B401 adapter reads and validates the same design-life input/default for all tokens, then passes that life to the same coating and mass kernels. Component-local and phase-local lives remain explicit; no claim is made about a standard-required life.

**Source:** [`src/digitalmodel/cathodic_protection/engine_adapter.py:610`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L610); [`src/digitalmodel/cathodic_protection/engine_adapter.py:704`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L704); [`src/digitalmodel/cathodic_protection/_kernels.py:127`](../../../../src/digitalmodel/cathodic_protection/_kernels.py#L127).

**Existing tests:** [`tests/cathodic_protection/test_edition_crosswalk.py::test_b401_current_demand_identical_across_editions`](../../../../tests/cathodic_protection/test_edition_crosswalk.py); [`tests/cathodic_protection/test_b401_structures_phases.py::test_temporary_phase_has_its_own_design_life`](../../../../tests/cathodic_protection/test_b401_structures_phases.py)

**Code-carried label:** The mass kernel docstring labels B401 Eq. 2; RULES carries edition-specific active-time and mass labels. No source citation is attached to the flat adapter default design life.

**Coverage limit:** The crosswalk fixes life for a single jacket example; the phase test checks a separate temporary-life input.

**Missing item:** No identified regression varies design life across all supported tokens; the flat adapter default has no attached source citation.

### abs-single-edition

**PASS — ABS; tokens: 2017-12.** The rebuilt ship table helper carries one fixed citation revision and has no edition selector. The public route key contains a historical date misnomer; it does not establish another edition.

**Source:** [`src/digitalmodel/cathodic_protection/abs_ships_tables.py:13`](../../../../src/digitalmodel/cathodic_protection/abs_ships_tables.py#L13); [`src/digitalmodel/cathodic_protection/engine_adapter.py:139`](../../../../src/digitalmodel/cathodic_protection/engine_adapter.py#L139).

**Existing tests:** [`tests/cathodic_protection/test_abs_ships_rebuild.py::test_abs_table_values_are_cited`](../../../../tests/cathodic_protection/test_abs_ships_rebuild.py); [`tests/cathodic_protection/test_abs_tables.py::test_coating`](../../../../tests/cathodic_protection/test_abs_tables.py)

**Code-carried label:** ABS revision 2017-12; Section 2 Tables 1, 3, 4, 5 and Section 3 Tables 2, 4 are carried by the code. The record does not compare ABS editions.

**Coverage limit:** Existing tests check selected attached labels and the fixed revision against test metadata; no multi-edition behavior is claimed.

### f103-result-metadata

**FAIL — F103; tokens: 2010, 2019.** Deserializing a result without edition metadata labels it 2010, while a new design without an edition selects 2019. These are different software fallback contracts.

**Source:** [`src/digitalmodel/cathodic_protection/dnv_rp_f103.py:78`](../../../../src/digitalmodel/cathodic_protection/dnv_rp_f103.py#L78); [`src/digitalmodel/cathodic_protection/dnv_rp_f103.py:284`](../../../../src/digitalmodel/cathodic_protection/dnv_rp_f103.py#L284).

**Existing tests:** No eligible covering test is identified.

**Code-carried label:** The result validator supplies f103_standard_for_edition and edition_provenance; no normative clause is attached to metadata fallback policy.

**Coverage limit:** Existing assertions cover the stated behavior; exhaustive input coverage is not established.

**Missing item:** No identified test removes edition metadata from an F103 result and checks the fallback independently of the design default. Metadata fallback policy has no attached clause citation.

## Integrity and verification

[test_edition_paths_review.py](../../../../tests/cathodic_protection/test_edition_paths_review.py)
rejects malformed or duplicate-key JSON, missing required topics, invalid status
records, unpinned revisions, non-repository source paths, stale source bytes,
incorrect line anchors and nonexistent test definitions. Both primary and
supporting source references are checked at the recorded commit and HEAD; the
working source bytes must also match. The test references are resolved by Python
syntax inspection without importing calculators or opening citation resources.
Every PASS has a nonempty test list, label and no missing item. The added guards require
field-joint citation assertions for both F103 tokens, preserve the B401 retrofit
clause-coverage gap, and compare every Markdown disposition and the summary
totals with the JSON record.

The original review reports one pass and four missing-record errors before
the companion record is written. The initial focused regression run reports
14 passing tests. The original final focused run reports 131 passed tests, including
the five record-integrity tests, selected edition crosswalks, edition API
foundation, phased structures, a component-coating regression and shared kernels.
The direct file checks, engineering register, local Markdown links and repository
structure checks pass. Test counts measure verification activity, not qualified
engineering results. Tests that would open standard resources or use
excluded cases are outside this execution basis and are not run. Existing tests
of CP calculators and their assertions are unchanged.

For this revision, the starting integrity run passes five tests. With the three
new guards, the unchanged records produce two failures and six passes. After
correction, all eight integrity tests pass with `PYTHONUTF8` unset. These tests
inspect code and assertion definitions; the F103 calculator tests named in the
record are not executed by this run.

The source pins deliberately fail when a cited source file changes at HEAD or in
the working tree. A source change requires a new review of the affected behavior,
updated references and a new revision-history row; moving a line number alone
does not renew the review. Local links resolve against the frozen source bytes.

## Limitations and next checkpoint

This record cannot establish publication equivalence, the reasons for edition
changes, an unimplemented difference, numerical effects on cases, or engineering
qualification of a design. It does not replace independent review. The
maintainers can use PASS entries as code-behavior evidence within these limits;
FAIL entries require the named coverage or citation work before use in an edition
claim. No CP implementation is changed to resolve the findings in this review.

Lesson: enum coverage at a shared table helper does not establish coverage of a
separate adapter, component parser or bank parser that selects that helper.
