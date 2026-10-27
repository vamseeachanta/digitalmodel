# Lifting lug checklist gap analysis

## Summary

The lifting lug calculation covers nominal axial hole bearing and two-plane
tear-out. Eight other checklist items have incomplete coverage or no integrated
check. The atlas reproduces a restricted plate screening calculation; its
interpolation validation does not establish lifting qualification. Establish the
load direction and factor basis, then assess the connections and supporting
structure before using a nominal pass to inform a lifting decision.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Compare implemented checks and tests with a qualitative lifting checklist to identify bounded additions. |

Table 1. Initial issue of this gap analysis.

## Basis and scope

The [practitioner post](https://lnkd.in/p/gxbmUk2W) was accessed on 2026-10-27
(HTTP 200). It provides qualitative prompts, without calculation criteria. The
comment question about offshore wind lifts motivates explicit checks of lateral
loading and dynamic amplification. This note uses original wording and assesses
repository coverage; it supplies no new resistance criteria or standard values.

The primary scope is `src/digitalmodel/lifting_lug/`, its workflow, the default
`atlases/lifting_lug/` dataset and the associated tests. The related plane-stress
APDL generator and rigging utilities are considered separately below. The
[existing analysis guide](padeye-lift-point.md) describes the axial single-plate
and concentric direct-shear assumptions.

**COVERED** means an implemented and tested nominal check within those assumptions,
not approval of a lifting arrangement. **PARTLY** means a calculation or input
exists but an essential part of the checklist item is absent. **MISSING** means
no check is integrated into the lifting lug assessment. Existing tests cited for
partial items exercise only the stated implemented portion.

## Checklist comparison

| Item | Status | Implementation evidence and boundary | Test evidence |
| --- | --- | --- | --- |
| Load magnitude and direction | PARTLY | [`assessment.py:60`](../../../src/digitalmodel/lifting_lug/assessment.py#L60) accepts a scalar sling tension and [`assessment.py:197`](../../../src/digitalmodel/lifting_lug/assessment.py#L197) factors it. The axial assumption at [`mechanics.py:38`](../../../src/digitalmodel/lifting_lug/mechanics.py#L38) has no direction input or enforcement. No load-case basis check establishes weight, load share or alignment. | [`test_assessment.py:36`](../../../tests/lifting_lug/test_assessment.py#L36), `test_plate_and_weld_hand_calculation`, checks scalar demand; [`test_assessment.py:105`](../../../tests/lifting_lug/test_assessment.py#L105), `test_load_scaling_and_unloaded_case`, checks scaling. No directional admissibility test. |
| Plate strength and thickness | PARTLY | [`mechanics.py:60`](../../../src/digitalmodel/lifting_lug/mechanics.py#L60) uses thickness in nominal areas. [`assessment.py:81`](../../../src/digitalmodel/lifting_lug/assessment.py#L81) requires supplied limits; [`workflow.py:130`](../../../src/digitalmodel/lifting_lug/workflow.py#L130) adds cheek thickness without checking its load transfer. Plate bending, combined loading and applicability of resistance limits remain unresolved. | [`test_mechanics.py:11`](../../../tests/lifting_lug/test_mechanics.py#L11), `test_plate_demands_hand_calculation`; [`test_assessment.py:77`](../../../tests/lifting_lug/test_assessment.py#L77), `test_absence_of_limits_preserves_demands_without_pass`. No combined-strength or cheek-transfer test. |
| Hole bearing | COVERED | [`mechanics.py:60`](../../../src/digitalmodel/lifting_lug/mechanics.py#L60) calculates projected pin-area bearing; [`assessment.py:219`](../../../src/digitalmodel/lifting_lug/assessment.py#L219) compares it with a supplied limit. This covers nominal bearing only; contact peaks, pin bending and hoop stress are excluded at [`mechanics.py:40`](../../../src/digitalmodel/lifting_lug/mechanics.py#L40). | [`test_mechanics.py:11`](../../../tests/lifting_lug/test_mechanics.py#L11), `test_plate_demands_hand_calculation`, independently checks bearing demand; [`test_assessment.py:36`](../../../tests/lifting_lug/test_assessment.py#L36), `test_plate_and_weld_hand_calculation`, checks utilization. |
| Tear-out | COVERED | [`mechanics.py:57`](../../../src/digitalmodel/lifting_lug/mechanics.py#L57) requires a positive loaded-edge ligament and uses two shear planes; [`assessment.py:223`](../../../src/digitalmodel/lifting_lug/assessment.py#L223) compares nominal shear with a supplied limit. Coverage is restricted to the declared axial rectangular load path. | [`test_mechanics.py:27`](../../../tests/lifting_lug/test_mechanics.py#L27), `test_unequal_net_and_shear_areas`, distinguishes shear from net area; [`test_mechanics_boundaries.py:79`](../../../tests/lifting_lug/test_mechanics_boundaries.py#L79), `test_degenerate_ligaments_are_refused`, checks invalid ligaments. |
| Weld | PARTLY | [`assessment.py:108`](../../../src/digitalmodel/lifting_lug/assessment.py#L108) requires a concentric direct-shear weld. [`assessment.py:203`](../../../src/digitalmodel/lifting_lug/assessment.py#L203) calculates throat stress and required throat. Eccentricity, weld-group moments, mixed stresses and fabrication size limits have no check. | [`test_assessment.py:36`](../../../tests/lifting_lug/test_assessment.py#L36), `test_plate_and_weld_hand_calculation`; [`test_assessment.py:64`](../../../tests/lifting_lug/test_assessment.py#L64), `test_resized_weld_at_exact_limit_and_rule_gaps`. No eccentric weld test. |
| Supporting structure | MISSING | The result checks at [`assessment.py:218`](../../../src/digitalmodel/lifting_lug/assessment.py#L218) end at nominal plate and weld demands. No support geometry, stiffness, local strength or capacity input exists. The APDL base restraint at [`padeye.py:118`](../../../src/digitalmodel/ansys/padeye.py#L118) is an ideal boundary, not a support-capacity assessment. | No supporting-structure capacity test. [`test_padeye.py:24`](../../../tests/ansys/test_padeye.py#L24), `test_generated_apdl_has_required_fe_blocks`, checks generated restraint text only. |
| Shackle fit | PARTLY | [`mechanics.py:52`](../../../src/digitalmodel/lifting_lug/mechanics.py#L52) rejects a pin larger than its hole. It accepts equality. Jaw opening, assembled thickness, tolerances, articulation, shackle capacity and side-loading restrictions are absent. | [`test_assessment.py:131`](../../../tests/lifting_lug/test_assessment.py#L131), `test_invalid_inputs_rejected`, includes an oversized pin; [`test_mechanics_boundaries.py:57`](../../../tests/lifting_lug/test_mechanics_boundaries.py#L57), `test_equal_pin_and_hole_hand_demands`, demonstrates that equality passes. No assembly-fit test. |
| Sling angle and in-plane/out-of-plane components | PARTLY | [`assessment.py:61`](../../../src/digitalmodel/lifting_lug/assessment.py#L61) assumes angle is already included in tension. [`padeye.py:82`](../../../src/digitalmodel/ansys/padeye.py#L82) resolves two in-plane components in a separate generator. Neither supplies a local plate-normal force or checks lateral plate/weld demand; the nominal API has no angle conversion. | [`test_padeye.py:46`](../../../tests/ansys/test_padeye.py#L46), `test_load_components_follow_sling_angle`, checks generated in-plane loads only. No out-of-plane demand or rejection test in the assessment. |
| Dynamic amplification | PARTLY | [`assessment.py:72`](../../../src/digitalmodel/lifting_lug/assessment.py#L72) validates positive factors and a basis string; [`assessment.py:197`](../../../src/digitalmodel/lifting_lug/assessment.py#L197) applies them. [`workflow.py:127`](../../../src/digitalmodel/lifting_lug/workflow.py#L127) defaults an omitted dynamic factor to unity. Selection for lift conditions, applicability and prevention of double factoring are unevaluated. | [`test_assessment.py:36`](../../../tests/lifting_lug/test_assessment.py#L36), `test_plate_and_weld_hand_calculation`, and [`test_lifting_lug_design.py:33`](../../../tests/workflows/test_lifting_lug_design.py#L33), `test_lifting_lug_hand_calculation`, exercise multiplication. No lift-condition factor-selection test. |
| Complete load path | PARTLY | [`assessment.py:244`](../../../src/digitalmodel/lifting_lug/assessment.py#L244) combines nominal plate/weld results and unresolved qualification checks. [`workflow.py:157`](../../../src/digitalmodel/lifting_lug/workflow.py#L157) reports only plate screening. Neither connects sling/shackle resistance, weld reactions and support capacity under one identified load case. | [`test_assessment.py:64`](../../../tests/lifting_lug/test_assessment.py#L64), `test_resized_weld_at_exact_limit_and_rule_gaps`, preserves overall `NOT_EVALUATED` despite nominal passing checks; [`test_assessment.py:97`](../../../tests/lifting_lug/test_assessment.py#L97), `test_failure_has_priority_over_missing_checks`, preserves failure precedence. No complete-path acceptance test. |

Table 2. Coverage of ten design considerations and the tests supporting each finding.

## Atlas, surrogate and validation limits

The default pointer [`default.txt:1`](../../../atlases/lifting_lug/default.txt#L1)
selects atlas `90e306065a25`. Its
[`manifest.yaml:5`](../../../atlases/lifting_lug/90e306065a25/manifest.yaml#L5)
and [`surrogate.json:6`](../../../atlases/lifting_lug/90e306065a25/surrogate.json#L6)
declare only static load and plate thickness as axes. The response function
[`generate.py:387`](../../../src/digitalmodel/parametric/generate.py#L387)
fixes the remaining geometry, material and amplification/skew factors, then
returns the greatest of three legacy plate utilizations. It does not call the
typed weld assessment. A changed shackle, angle, supporting structure or factor
basis therefore requires more than an in-range query.

The surrogate uses linear grid interpolation
([`atlas.py:140`](../../../src/digitalmodel/parametric/atlas.py#L140)). Its stored
[`validation.json:2`](../../../atlases/lifting_lug/90e306065a25/validation.json#L2)
records eight held-out samples and a maximum relative error of 2.1 % against a
10.0 % tolerance. At [`generate.py:682`](../../../src/digitalmodel/parametric/generate.py#L682),
the reference response is the same function used to build the grid. These are
interpolation error measurements, not experimental, independent solver or
complete-load-path validation. The query provides range and confidence-band
checks at [`query.py:182`](../../../src/digitalmodel/parametric/query.py#L182),
without adding physical checks.

[`test_atlas_basis.py:14`](../../../tests/lifting_lug/test_atlas_basis.py#L14),
`test_refreshed_atlas_preserves_previous_grid`, checks freshness, stored-grid
regression and the recorded validation pass.
[`test_lifting_lug_design.py:92`](../../../tests/workflows/test_lifting_lug_design.py#L92),
`test_lifting_lug_atlas_query_has_current_basis`, exercises a current query.
Neither establishes the applicability of the legacy resistance limits.

Related rigging tools do not close these integration gaps. The separate
[`four_point_skew.py:165`](../../../src/digitalmodel/marine_ops/installation/four_point_skew.py#L165)
models three-dimensional sling equilibrium, exercised by
[`test_four_point_skew.py:105`](../../../tests/marine_ops/installation/test_four_point_skew.py#L105),
`test_unequal_stiffness_compatibility_and_full_force_balance`. The separate
[`installation_analysis.py:126`](../../../src/digitalmodel/orcaflex/installation_analysis.py#L126)
estimates system dynamics, and its sling routine at
[`installation_analysis.py:346`](../../../src/digitalmodel/orcaflex/installation_analysis.py#L346)
includes inclination. Their separate tests are
[`test_installation_analysis.py:60`](../../../tests/orcaflex/test_installation_analysis.py#L60),
`TestDAF.test_daf_increases_with_heave`, and
[`test_installation_analysis.py:239`](../../../tests/orcaflex/test_installation_analysis.py#L239),
`TestSlingConfig.test_steeper_angle_lower_tension`. No cited lifting lug test
transfers these results into a local plate frame and checks their factor history.
APDL text generation also does not establish a solved three-dimensional
connection model.

## Way forward

Prepare one bounded addition for each of the eight PARTLY or MISSING rows:
load-case admissibility; plate-strength scope and cheek transfer; weld-group
scope; support capacity; shackle assembly fit; local sling components; dynamic
factor provenance; and complete-path disposition. The corresponding issue drafts
are supplied with this change for the maintainers to open. Calculation behavior,
atlas data and existing tests remain unchanged.
