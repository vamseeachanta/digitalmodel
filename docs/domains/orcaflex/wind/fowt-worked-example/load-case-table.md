# Floating wind first worked-example case table

This is a proposed case-definition worksheet, not launcher input or a design-load-case compliance matrix.
The first platform is [INPUT: select L02 or K03 and exact representation].
All new numerical case inputs remain unselected. As-held inputs are transcribed below; their presence does not establish adequate duration, convergence or design coverage.
The [inventory](external-files-inventory.md) defines the source map and external-file gaps.

## As-held environment and stages

The values below are read from the raw models. Monolithic copies retain these line locators; the modular and specification representations are separate inputs and will require equivalence checks before use.

| Input | Units | L02 as held | K03 as held |
|---|---|---|---|
| Units system | model convention | SI ([L02 L14](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L14)) | SI ([K03 L11](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L11)) |
| Whole-system statics | enabled | Yes ([L02 L22](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L22)) | Yes ([K03 L19](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L19)) |
| Dynamics method | method | Implicit time domain ([L02 L24](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L24)) | Implicit time domain ([K03 L21](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L21)) |
| Constant time step | s | 0.1 ([L02 L26](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L26)) | 0.1 ([K03 L23](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L23)) |
| Log sample interval | s | 0.1 ([L02 L28](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L28)) | 0.1 ([K03 L25](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L25)) |
| Seabed depth | m | 200 ([L02 L85](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L85)) | 200 ([K03 L102](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L102)) |
| Wave type | spectrum | JONSWAP ([L02 L98](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L98)) | JONSWAP ([K03 L115](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L115)) |
| Wave significant height | m | 7 ([L02 L104](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L104)) | 4 ([K03 L121](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L121)) |
| Wave zero-crossing period | s | 8 ([L02 L105](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L105)) | 8.52552 ([K03 L122](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L122)) |
| Wave gamma | dimensionless | 1 ([L02 L106](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L106)) | 1 ([K03 L123](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L123)) |
| Wave direction | deg | 0 ([L02 L99](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L99)) | 180 ([K03 L116](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L116)) |
| Wave components | count | 200 ([L02 L107](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L107)) | 200 ([K03 L124](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L124)) |
| User-specified wave seeds | setting | No ([L02 L94](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L94)) | No ([K03 L111](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L111)) |
| Reference current speed | m/s | 0 ([L02 L123](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L123)) | 0 ([K03 L140](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L140)) |
| Reference current direction | deg | 180 ([L02 L124](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L124)) | 180 ([K03 L141](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L141)) |
| Wind type | definition | Constant ([L02 L134](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L134)) | NPD spectrum ([K03 L151](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L151)) |
| Wind speed | m/s | 15 ([L02 L137](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L137)) | 17.1 ([K03 L160](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L160)) |
| Wind direction | deg | 0 ([L02 L138](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L138)) | 180 ([K03 L161](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L161)) |
| Stage durations, in listed order | s | 8; 200 ([L02 L37](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L37), lines 38-39) | 12; 150 ([K03 L34](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L34), lines 35-36) |
| NPD wind parameters | model keys | Not applicable to Constant wind | elevation 150 m; frequency minimum 0.0016666666666666668 Hz; maximum 0.43 Hz; origin 0 s; seed 1796823655; components 1000 ([K03 L154](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L154) through [K03 L159](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L159)) |

Table: source values, not calculated results. Period is the model's WaveTz, not a substituted peak period.

The complete environment blocks are linked by the inventory's raw-source paths.
Current profiles are constant factors at the listed depths in each model; no external current file is named.
The durations above are source settings, not proposed decay-analysis windows.

## Source support and limits

- [DNV-OS-C301 / DNV-RP-0286 (line 39)](../../../../../src/digitalmodel/floating_wind/floaters.py#L39): Named as floating-wind stability/design practice; the code does not supply a six-axis free-decay protocol or operating/parked matrix.
- [DNV-RP-A203 (line 24)](../../../../../src/digitalmodel/floating_wind/qualification.py#L24): Named for technology qualification, not environmental case definitions.
- [DNV-RP-A203 (line 30)](../../../../../src/digitalmodel/floating_wind/collaboration.py#L30): Named for technology qualification / TRL, not load cases.
- [DNV-RP-0360 (line 61)](../../../../../src/digitalmodel/orcaflex/mooring_design_fowt.py#L61): Named for the cable minimum-bend-radius check; no turbine operating/parked matrix is defined.
- [DNV-OS-E301 / API RP 2SK (line 79)](../../../../../src/digitalmodel/orcaflex/mooring_design_fowt.py#L79): Named as mooring references, without a case definition for this worked example.
- [Existing workflow scope (line 3)](../../../../../examples/workflows/fowt-mooring/input.yml#L3): A watch-circle/cable feasibility screen; its example values are not inputs for these turbine models.
- [Existing solver-tier scope (line 10)](../../../../../src/digitalmodel/floating_wind/solver_tier.py#L10): A diffraction and motion/mooring preparation pipeline; it does not define this turbine case set.

These are repository citation findings only. No standard edition, clause, load-case identifier or requirement is adopted from them.
No paid-standard text is reproduced. The named standards have not been opened or used as public case-definition sources in this phase.

## Proposed cases

Every row below remains a proposed intent from the issue, pending the source selection stated in its last column.
The free-decay forcing, released coordinates, controller treatment and period-extraction method are [INPUT: define and review]; a label alone is not a runnable case.
No numerical limits, rotor states, wave suppression settings or initial offsets are implied.

| Case | Proposed intent / required output | Marked inputs before execution | Supporting case-definition reference |
|---|---|---|---|
| Statics | Establish equilibrium and record convergence, platform pose and mooring tensions. | [INPUT: equilibrium configuration, environmental treatment, tolerances and criterion] | reference to be selected: candidates must be public |
| Free decay: surge | Extract surge natural period and record the motion trace. | [INPUT: surge initial offset, units, release, duration and extraction criterion] | reference to be selected: candidates must be public |
| Free decay: sway | Extract sway natural period and record the motion trace. | [INPUT: sway initial offset, units, release, duration and extraction criterion] | reference to be selected: candidates must be public |
| Free decay: heave | Extract heave natural period and record the motion trace. | [INPUT: heave initial offset, units, release, duration and extraction criterion] | reference to be selected: candidates must be public |
| Free decay: roll | Extract roll natural period and record the motion trace. | [INPUT: roll initial offset, units, release, duration and extraction criterion] | reference to be selected: candidates must be public |
| Free decay: pitch | Extract pitch natural period and record the motion trace. | [INPUT: pitch initial offset, units, release, duration and extraction criterion] | reference to be selected: candidates must be public |
| Free decay: yaw | Extract yaw natural period and record the motion trace. | [INPUT: yaw initial offset, units, release, duration and extraction criterion] | reference to be selected: candidates must be public |
| Operating: source environment | Record mooring tensions, platform motions and tower-base loads under the selected as-held environment. | [INPUT: operating control state, settling window, retained window and criterion] | reference to be selected: candidates must be public |
| Operating: environmental variation | Compare response with the source-environment case. | [INPUT: wind/wave/current combination, direction, seed and criterion] | reference to be selected: candidates must be public |
| Parked: source environment | Record response under a separately defined parked state. | [INPUT: parked rotor, pitch and generator constraints, window and criterion] | reference to be selected: candidates must be public |
| Parked: environmental variation | Compare parked response across a selected environmental combination. | [INPUT: combination, direction, seed, parked controls and criterion] | reference to be selected: candidates must be public |

Table: proposed first-case worksheet. Each unsupported case definition retains the required reference-selection marker.

The issue's approximate campaign estimate is about 20 solves per platform set: 1 statics, 6 free decays and about 12 load cases.
The four operating/parked rows above are case families, not a claim that the approximate load-case set has been enumerated.
Expansion into individual cases will require the selected public reference, platform and reviewed manifest.
The [issue](https://github.com/vamseeachanta/digitalmodel/issues/2423) supplies the scope and estimate; it is not a technical case-definition source.

## Results and comparison record

Each executed case will record solver version, model and controller digests, controller state, environment, seed policy,
retained time window, output channel/unit/sign conventions and governing criterion.
The period comparison, mooring tension, platform motion and tower-base load results remain [RESULT: not calculated].
The comparison-reference limits and preconditions are recorded in the [inventory](external-files-inventory.md).
The [report skeleton](report-skeleton.html) will remain unissued until native-output readback and independent review establish each reported result.
