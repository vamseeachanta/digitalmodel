> Source: [2026-10-11 research note on issue 162](https://github.com/vamseeachanta/digitalmodel/issues/162#issuecomment-6115195050) and [2026-10-12 research note](https://github.com/vamseeachanta/digitalmodel/issues/162#issuecomment-6116520568).

# Drive-off and drift-off analysis methodologies — 2026-10 research note

This transcription records the first note's sections 1–12 and the second note's new gaps (section 7). Read and located statuses describe those sessions; no underlying paper was opened for this transcription. The author attribution is omitted. No paid standard text is included; standards remain designations only. Source IDs follow the first note's source-list order.

The claims of enough input data to reproduce (section 1) and complete vessel and riser inputs (section 8) are **historical and superseded**: the [merged 2017 fixture](../../../tests/drilling_riser/fixtures/drive_off_worked_example_2017.json) records missing inputs and modelling gaps; the reproduction remains skipped in [its test](../../../tests/drilling_riser/test_drive_off_worked_example.py). The 2026-10-12 note states that no open source found gives a complete input set. The earlier wording is retained as a historical statement, not an established reproduction capability.

## 1. Bottom line

1. The published method is a two-part calculation: (a) vessel offset and heading versus time after the failure, from a low-frequency manoeuvring model driven by wind, current, mean wave drift and whatever thrust remains or is wrongly applied; (b) riser response to that top-end motion, checked against a short list of limits. The first limit exceeded defines the point of disconnect; the red limit is the point of disconnect moved earlier by the emergency disconnect time, and the yellow limit earlier again by a preparation allowance. One fully worked open example exists (Liu et al. 2017 [R3]) with enough input data to reproduce.
2. Published methods disagree on four things that change the answer: fixed watch circles as a percentage of water depth versus circles computed from drift-off analysis and updated on the rig; coupled versus uncoupled vessel and riser; quasi-static versus dynamic riser response (the bottom of the riser lags the top); and full blackout versus partial loss of thrust as the design event.
3. On frequency versus severity the sources point in opposite directions and both can be right: a generic probabilistic study attributes over 90 % of emergency-disconnect risk to drift-off, while the drive-off paper states that damage is more likely in a drive-off. Human error dominates both in that study. The emergency disconnect time is not a constant: 58 s, about 70 s and about 4 minutes appear in the three records read.

## 2. Definitions (as used in the sources read)

| Term | Definition | Source |
|---|---|---|
| Drift-off | Loss of power or thrust; the vessel moves off location in the direction of the prevailing environment | Liu et al. 2017 [R3]; Bhalla and Cao 2005 abstract [R4] |
| Drive-off | A powered move away from the desired position, caused by thruster or control malfunction or wrong position information | Liu et al. 2017 [R3]; Bhalla and Cao 2005 abstract [R4] |
| Watch circles | Colour-coded offset thresholds centred on the surface position above the wellhead; radii depend on water depth, operation, environment, subsea equipment and the time required to disconnect | NASA and SAIC 2018; ABS 2015 |
| Point of disconnect | Offset at which any allowable limit is first exceeded | ABS 2015 [R1], slide 7 |
| Red limit | Offset at which the emergency disconnect sequence must be activated; drill pipe is sheared during the sequence | ABS 2015 [R1], slide 7 |
| Yellow limit | Offset at which preparations begin before the sequence | ABS 2015 [R1], slide 7 |
| Timing relation | T_red = T_pod minus disconnect time; T_yellow = T_pod minus disconnect time minus 90 s | ABS 2015 [R1], slide 6 |
| Well-specific operating guidelines | A per-well document defining the operator's actions when station-keeping capability changes, with green, advisory, yellow and red conditions each carrying an agreed response; limits agreed between contractor and operator before spud | Adamson, OGJ 2006 |

Origin of the well-specific guideline, per Adamson: a 1997 to 1998 Norwegian campaign, first version four pages long, which introduced the "advisory" level; an earlier 1982 UK guidance document already contained well-specific limits.

## 3. Methods for vessel excursion versus time

| Approach | What it does | Source |
|---|---|---|
| Fixed offset | Constant watch-circle radii, typically a percentage of water depth; no vessel simulation | ABS 2015 [R1], slide 10 |
| Uncoupled, static riser | Vessel drift simulated without the riser; trajectory applied to a static riser model | ABS 2015 [R1], slide 10 |
| Uncoupled, dynamic riser | Vessel drift without the riser; trajectory applied to a dynamic riser model | ABS 2015 [R1], slide 10; Bhalla and Cao 2005 (in-house vessel code, then commercial and in-house finite-element riser codes) |
| Fully coupled | Vessel and riser solved together in the time domain, including riser restoring force and riser dynamics; called "state of the art" [R1] | ABS 2015 [R1], slides 10 and 13; Liu et al. 2017 [R3]; a consultancy method statement (Stewart Technology Associates) using a commercial time-domain code |
| Partial loss of thrust | Remaining thrusters oppose the environment; argued to be far more likely than full loss | Josefson and Dincal 2014 (summary page) |
| Dynamic watch circles | Circles recomputed on the rig every 6 hours for prevailing and forecast weather, top tension and mud weight | ABS 2015 [R1], slide 11 |
| Probabilistic red limit | Red limit from probabilistic modelling of position-loss scenarios | Gjerde and Chen 2014, as described by Liu et al.; not read |

Open formulation (Liu et al. 2017 [R3], Mechanika 23(2), 259-264, CC BY 4.0). Three degrees of freedom (surge, sway, yaw) in a body-fixed frame with a rotation to the earth frame (their eq. 1). The equation of motion (eq. 2) balances the mass matrix including zero-frequency added mass (eq. 3) and the Coriolis and centripetal matrix (eq. 4) against current load, second-order wave load, wind load, thruster force and riser force. Current and wind loads are quadratic in relative velocity with direction-dependent coefficients (eqs. 5, 6); wave drift is from quadratic transfer functions and the wave spectrum (eq. 7); thruster force is a controller gain law (eq. 8); riser force on the vessel is computed from riser top tension and top angles (eq. 9). The riser is a tensioned beam with Morison loading above the mudline and soil springs on the conductor (eqs. 10 to 12). Vessel solver and riser model exchange motion and top force each step. Equation images did not survive text extraction; the structure above is from the paper's own description.

Note for the linked propeller-rudder work: none of the sources read models thrust build-up, propeller dynamics or thruster ramp rates. Liu et al. represent the drive-off as the control law driving to a wrong reference 100 m away. A thrust-versus-time input (ramp to full thrust on some or all units) is therefore an assumption to be sourced separately.

## 4. Riser, flex-joint and wellhead loading; disconnect timing

Limits checked (ABS 2015 [R1], slide 5): conductor bending stress; wellhead bending moment; lower flex-joint angle; upper flex-joint angle and moonpool contact; slip-joint stroke; tensioner stroke. Liu et al. cite an earlier conference paper (Ambrose et al.) for the same list. A consultancy method statement adds that conductor stress must not exceed yield in accidental conditions and that the lower package must lift clear on recoil.

Findings on which limit governs:

- Liu et al. (1,327 m riser, Beaufort 6): upper flex-joint angle and slip-joint stroke respond first; lower flex-joint angle, conductor stress and wellhead bending moment stay essentially constant for a lag period because of riser inertia. The upper flex-joint limit (12 degrees in their case) is reached at 50 s. Their critical values: upper 12 degrees, lower 6 degrees, stroke 9.905 m, wellhead bending moment 7.8 MN m.
- Liu et al.: the riser force first assists and then resists a drive-off along the environment, and always resists one against it; offset with the riser coupled is smaller than without.
- Sonawane, Koska and Campbell 2012 [R6] (5,000 ft, Gulf of Mexico, survival capacities, drift-off with failed disconnect): tensioners reach down-stroke limit at about 10 % of water depth; all components within design limits until then; the riser pipe fails in tension near the top at about 14 % offset. Lower flex-joint angle rises with offset and then falls as bending transfers into the conductor, governed by soil stiffness. Shallower water raises the risk of failure below the blowout preventer.
- ABS 2015 slide 9, one drift-off example (10 ppg mud, 1,455 kip top tension): point of disconnect 469, red offset 281, with 70 s between the red time (187 s) and the point-of-disconnect time (257 s). Offset units are not legible in the extracted slide.

Disconnect durations on record: 58 s (2007 drillship drive-off, regulator accident report); 70 s (ABS example); about 4 minutes with a disputed cause (2005 semi-submersible event, regulator investigation report).

## 5. Inputs and where the sources take them from

| Input | Published source of values |
|---|---|
| Vessel mass, added mass, yaw inertia | Liu et al. give a full set (mass 4.08e7 kg, surge and sway added mass 1.18e7 and 4.32e7 kg, yaw inertia 1.19e11 kg m2, yaw added inertia 1.38e11 kg m2) |
| Wind and current load coefficients | Direction-dependent drag coefficients; values not tabulated in the paper |
| Wave drift | Quadratic transfer functions with a wave spectrum |
| Environment | Liu: wind 13.8 m/s, wave 3 m and 6.7 s, surface current 0.5 m/s, collinear. ABS: prevailing or forecast weather for on-rig updates |
| Riser | Liu: length 1,327 m, top tension 3.02 MN, hydrodynamic diameter 0.5334 m, conductor 0.9144 m, drag 1.2, inertia 2, mass 280 kg (per unit length as printed) |
| Top tension and mud weight | Treated as variables that move the watch circles (ABS 2015) |
| Soil | Lateral soil springs per API RP 2A-WSD as cited by Liu et al. |
| Disconnect time, reaction time | Rig-specific; "the reaction time preferred by the client and drilling contractor" in the consultancy method statement |
| Component capacities | Scarce; taken from drawings, test data and vendor discussions (Sonawane et al.) |

## 6. Incident statistics and reviews

- Generic probabilistic risk study of a Class 3 drilling unit in the Gulf of Mexico (Thigpen, Boyer and Stewart, PSAM 14, 2018 [R2]): loss of position with emergency disconnect in fewer than 5 of every 100 wells; the normal operating environment contributes over 90 % of that risk; drift-off is over 90 % of the risk by end state; human error is over 80 % in normal operation; generators and thrusters under 10 %. Drive-off is driven by two human errors: a wrong offset entered, and a wrong response to a degraded control system. Results are generic, not for any specific vessel.
- Industry body annual review, IMCA M 269 (events reported for 2024): catalogue page read, statistics not shown there. Bulletin DPE 01/25 (16 Jan 2025): a record 217 event forms from 56 companies in 2024; top trigger causes electrical, mechanical and human; a "DP incident" is a loss of position or heading. No split between drive-off and drift-off is given in the pages read.
- Olubitan, Loughney, Wang and Bell, ESREL 2018 (repository record): from industry reports and an accident databank for 2000 to 2016, position-reference failures and thruster failures each caused 20.6 % of loss-of-position incidents; for 2012 to 2016, 71.2 % of thruster-failure events ended as "time loss" rather than drive-off or drift-off.
- Regulator accident report, drillship, 2 Dec 2007, 4,331 ft: corrupted data transferred between control consoles during routine maintenance; a different position reference became master; the system chased an erroneous position; the rig left its watch circle and exceeded the riser angle limit; disconnect took about 58 s; 550 bbl of brine released; a simulator programme for drift-off and drive-off was a corrective action.
- Regulator investigation report, semi-submersible, 5 Jul 2005: unable to hold station against about 3 knots of current and rising wind while displacing the riser; disconnect ordered at 175 ft from well centre; 710 bbl of synthetic mud released; station-keeping criteria and rig selection for the environment found inadequate. The report describes a program that advises the red zone from vessel characteristics, environment, footprint, drive-off scenarios, recovery and maximum riser angle.

## 7. Guidance by designation

Seen on pages read: ISO 13624-1 and ISO 13624-2 (named on the ABS slide as referencing fully coupled drift-off analysis); IMO MSC/Circ.645 (equipment classes; cited by the PSAM paper); API RP 16Q (riser tension requirements, per the consultancy method statement and this issue); API RP 2A-WSD (soil curves, per Liu et al.); US notice NTL 2005-G05 and 30 CFR 250.417(e) (as cited in the 2005 investigation report); IMCA M 269.

Seen only in search results, not opened: DNV-RP-E307; IMCA M 220; IMCA M 115 (said to contain frequency tables for drift-off and drive-off); IMO MSC.1/Circ.1580; MTS DP Operations Guidance (Parts 1 and 2); MTS TECHOP ODP 12. DNV-OS-E101, named in this issue, did not appear on any page read.

## 8. Open worked examples and benchmark candidates

1. Liu et al. 2017 [R3] (open licence): complete vessel and riser inputs, two drive-off directions, time histories and normalised limit utilisation. Best candidate for a reproduction test: time for the upper flex-joint angle to reach its limit (50 s), sign change of the riser force, and coupled offset smaller than uncoupled.
2. ABS 2015 slide 9: a single drift-off curve with point of disconnect, red offset and times; useful as a check of the timing arithmetic in section 2 only.
3. Sonawane et al. 2012: percentage-of-depth milestones (about 10 % stroke-out, about 14 % first failure) for a 5,000 ft case; a sanity range, not a benchmark.

## 9. Where published methods disagree

| Question | Position A | Position B |
|---|---|---|
| How to set watch circles | Fixed percentage of water depth | Computed from drift-off analysis and updated on the rig (ABS 2015) |
| Coupling | Uncoupled vessel drift, then riser | Coupled; the riser force changes the trajectory (Liu et al.); "makes a difference" (ABS 2015) |
| Riser response | Static riser under the offset | Dynamic; bottom response lags the top (Liu et al.) |
| Design event | Full loss of thrust | Partial loss is far more likely and the full-loss assumption causes unnecessary downtime (Josefson and Dincal) |
| Which event is worse | Drift-off dominates risk by frequency (PSAM 2018) | Damage more likely in drive-off (Liu et al., citing a conference paper not read) |
| Are the two events different for the riser | Similar from the riser's perspective, differing in vessel motion (consultancy statement) | Drive-off direction relative to the environment changes even the sign of the riser force (Liu et al.) |
| Governing limit | Upper flex joint and stroke first (Liu, 1,327 m) | Tensioner stroke, then riser tension near the top (Sonawane, 5,000 ft, survival basis) |
| Deterministic or probabilistic red limit | Deterministic point of disconnect minus disconnect time | Probabilistic red limit (Gjerde and Chen, not read) |

## 10. Suggested time-domain approach for an open implementation (assessment, not a finding)

Three-degree-of-freedom low-frequency model as in Liu et al.; quadratic wind and current loads; mean wave drift; a thrust-time input per thruster for drive-off or zero thrust for drift-off; riser top force as an optional feedback term; limits from section 4 evaluated on a quasi-static riser first, with a dynamic riser as the upgrade; output the three times (point of disconnect, red, yellow). First test: reproduce Liu et al.

## 11. Sources

Read (14), as recorded on 2026-10-11:

- [R1] ABS presentation to a regulator standards workshop, 8 May 2015 (14 slides)
- [R2] Thigpen, Boyer and Stewart, PSAM 14, 2018
- [R3] Liu, Chen, Ji and Fu, Mechanika 23(2), 2017, doi 10.5755/j01.mech.23.2.13780
- [R4] Bhalla and Cao, 2005 conference paper (abstract page only)
- [R5] Adamson, OGJ, 27 Nov 2006
- [R6] Sonawane, Koska and Campbell, Drilling Contractor, 15 Mar 2012
- [R7] Josefson and Dincal, 2014 (summary page only)
- [R8] Stewart Technology Associates riser analysis method page
- [R9] Olubitan et al., ESREL 2018 (repository record and abstract)
- [R10] IMCA M 269 catalogue page
- [R11] IMCA bulletin DPE 01/25
- [R12] regulator accident report of 2 Dec 2007
- [R13] regulator investigation report of the 5 Jul 2005 event (sections on disconnect, station-keeping guidelines, conclusions; not every page)
- [R14] Avram et al., Mining Revue 4/2015 (read, and found not to contain a drive-off method despite its abstract; excluded from conclusions).

Located, not read (17 stated; 15 enumerated groups), as recorded on 2026-10-11:

- [L1] O'Sullivan, Soles and Dib, OMAE2004-51631
- [L2] Gjerde and Chen, OMAE2014-24614
- [L3] Chen, Moan and Verhoeven, Reliability Engineering and System Safety 93, 1072-1090, 2008
- [L4] Verhoeven, Chen and Moan, 2004 conference paper (file opened, text not extractable)
- [L5] Bakken and Olsen 2001
- [L6] Olsen 2001
- [L7] Jenman 1998
- [L8] Ambrose, Keener and Farr, OTC 15236
- [L9] Hansen and Royce 2011
- [L10] Donnarumma 2012 (repository certificate error)
- [L11] two NTNU master's theses on drilling risers on DP vessels and on drift-off in deep water (repository pages would not render)
- [L12] Lang, World Oil, Oct 2017 (paywall preview)
- [L13] MTS DP Operations Guidance Part 1 (file opened, text not extractable)
- [L14] MTS TECHOP ODP 12 and a coast guard DP policy letter (access refused)
- [L15] a 2021 Australian regulator document on loss of position (timed out).

The stated 17 items and the enumerated 15 groups are retained: [L11] holds two theses and [L14] holds two inaccessible documents. No bibliographic details are inferred.

## 12. Could not verify

- Any numerical frequency of drive-off or drift-off per rig-year; the papers that carry them were not readable.
- Units of the ABS example offsets.
- Typical thruster ramp rates and thrust build-up; no source read covers them.
- Content of the ISO, API, IMO, DNV, IMCA and MTS documents beyond the fact that the pages read cite them.
- Whether "percentage of water depth" fixed circles have a single customary value; the ABS slide gives none.

## 13. Relation to existing repository code (differences listed, not resolved)

This mapping describes function names and docstrings on main. It does not supply missing physics or qualify a reproduction.

| Time-domain element or illustration | Existing code or absence |
|---|---|
| 3-DOF low-frequency model | **Absent**: no surge/sway/yaw time-domain vessel integrator. |
| Quadratic wind/current and mean drift | `environmental_force_n` in `src/digitalmodel/drilling_riser/drift_off.py`: scalar environmental load in newtons; no evolving relative-velocity trajectory. |
| Thrust-time input per thruster | **Absent**: no per-thruster ramp or shutdown history. |
| Söding/Brix rudder forces | `soding_forces` in `src/digitalmodel/hydrodynamics/propeller_rudder.py`: rudder interaction forces from instantaneous speed, rev/s and deflection; no thrust-time history. |
| Actuator-disk slipstream | `actuator_disk_velocity` in `src/digitalmodel/hydrodynamics/propeller_rudder.py`: axial velocity at the rudder plane; no thrust-time history. |
| Actuator-disk flat-plate rudder forces | `ad_flat_plate_forces` in `src/digitalmodel/hydrodynamics/propeller_rudder.py`: instantaneous rudder forces in that slipstream. |
| Riser top-force feedback | **Absent**: no feedback exchange with a vessel trajectory. |
| Quasi-static limit check | `watch_circle_limit` in `src/digitalmodel/drilling_riser/drift_off.py`: flex-joint angle and riser stress only, against the six limits in section 4. |
| Point of disconnect and red time | `drift_off_screen` in `src/digitalmodel/drilling_riser/drift_off.py`: `time_to_limit_s` is the limit time; `point_of_disconnect_m` is the EDS initiation offset; red lead time subtracts EDS duration and elapsed time. |
| Yellow time | **Absent**: no yellow output. |
| Screening qualification | `SCREENING_DISCLAIMER` in `src/digitalmodel/drilling_riser/drift_off.py`: constant-acceleration lower-bound disclaimer; no coupled dynamic result. |
| 2017 worked example | `test_reproduce_drive_off_worked_example` in `tests/drilling_riser/test_drive_off_worked_example.py`: incomplete-input, coupled reproduction stays skipped. |
| Constructed drift-off benchmark | `test_kinematics_constructed_benchmark_not_a_reproduction` in `tests/drilling_riser/test_drift_off_constructed_benchmark.py`: injected force and radius check kinematics, not the force model. |
| Force-balance illustration | `simulate` in `examples/domains/hydrodynamics/drive_off_balance.py`: one-degree-of-freedom prescribed force balance. |
| Force-balance illustration test | `test_drive_off_constant_force` in `tests/hydrodynamics/test_drive_off_balance_example.py`: independent arithmetic for the illustration. |

The operating-range caveat is the [propeller-rudder research note, section 1](propeller-rudder-research-note-2026-10.md#1-bottom-line): that note's modular form excludes start-up and bollard. This is a citation of the existing caveat, not new physics or a validation of the functions above. The [force-balance example documentation](../../../examples/domains/hydrodynamics/drive_off_balance.md) records evidence needed for ramp-up, drag and shutdown. Thrust ramp rates remain not established by the research notes; a time-domain implementation is later work.

## 14. New gaps from the 2026-10-12 note, section 7

Quoted from the second research note (2026-10-12); its worked-example values remain in the [constructed benchmark fixture](../../../tests/drilling_riser/fixtures/drift_off_constructed_benchmark.json).

> - The governing limit in W2006 is structural casing bending moment at 5.7 % of water depth, ahead of the flex-joint limits at 8.0 %. The module's limit search covers flex-joint angle and riser stress only, so it would return the larger radius.
> - The module has no yellow-circle output.
> - The module's environmental force and its limit radius are not exercised by this benchmark at all; both stay unvalidated until a source with vessel particulars is found.
