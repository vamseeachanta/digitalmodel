# Floating wind external-files inventory

Phase A records the vendor example models as held. Text inspection does not establish solver readiness, controller compatibility, or engineering qualification.
No model, controller, wind file or hydrodynamic file is downloaded or substituted.
Redistribution terms to be checked before any model file is shared outside.

## Source map

All source paths below are relative to the worktree root. Linked line locators refer to the inspected source text, not a solver readback.

- **L02 raw:** [docs/domains/orcaflex/examples/raw/L02/L02 OC4 Semi-sub.yml](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml).
- **L02 library:** [monolithic model](../../library/model_library/l02_oc4_semi_sub/monolithic/L02%20OC4%20Semi-sub.yml); [modular master](../../library/model_library/l02_oc4_semi_sub/modular/master.yml); [specification](../../library/model_library/l02_oc4_semi_sub/spec.yml).
- **K03 raw:** [docs/domains/orcaflex/examples/raw/K03/K03 15MW semi-sub FOWT.yml](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml).
- **K03 library:** [monolithic model](../../library/model_library/k03_15mw_semi_sub_fowt/monolithic/K03%2015MW%20semi-sub%20FOWT.yml); [modular master](../../library/model_library/k03_15mw_semi_sub_fowt/modular/master.yml); [specification](../../library/model_library/k03_15mw_semi_sub_fowt/spec.yml).

## External controller files

Each row is one distinct referenced file. Repeated uses and library representations are retained in the locator column.
Availability means an exact, case-insensitive basename match across the worktree; an installed file outside the worktree is not a holding.
The source column records only repository-named acquisition sources. A code symbol, model name or bare filename is not an acquisition source.

| Model | File | Model line and library locators | Purpose as stated in the model | Held status | Public source for missing file |
|---|---|---|---|---|---|
| L02 | `PythonController.py` | [L02 L53](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L53); [L02 L59](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L59); [monolithic L53](../../library/model_library/l02_oc4_semi_sub/monolithic/L02%20OC4%20Semi-sub.yml#L53); [monolithic L59](../../library/model_library/l02_oc4_semi_sub/monolithic/L02%20OC4%20Semi-sub.yml#L59); [modular L29](../../library/model_library/l02_oc4_semi_sub/modular/includes/20_generic_objects.yml#L29); [modular L35](../../library/model_library/l02_oc4_semi_sub/modular/includes/20_generic_objects.yml#L35); [spec L37499](../../library/model_library/l02_oc4_semi_sub/spec.yml#L37499); [spec L37507](../../library/model_library/l02_oc4_semi_sub/spec.yml#L37507) | External functions named Blade Pitch / Generator Torque; functions PitchController / TorqueController. | held elsewhere: [docs/domains/orcaflex/examples/raw/K01/PythonController.py](../../examples/raw/K01/PythonController.py); compatibility unverified | Not applicable: held elsewhere; no substitution authorised |
| K03 | `BladedControllerWrapper.dll` | [K03 L58](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L58); [monolithic L58](../../library/model_library/k03_15mw_semi_sub_fowt/monolithic/K03%2015MW%20semi-sub%20FOWT.yml#L58); [modular L34](../../library/model_library/k03_15mw_semi_sub_fowt/modular/includes/20_generic_objects.yml#L34); [spec L106430](../../library/model_library/k03_15mw_semi_sub_fowt/spec.yml#L106430) | External function named Controller; function BladedController. | not held (GAP) | not named in the repository |
| K03 | `libdiscon.dll` | [K03 L29795](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L29795); [monolithic L29795](../../library/model_library/k03_15mw_semi_sub_fowt/monolithic/K03%2015MW%20semi-sub%20FOWT.yml#L29795); [modular L106354](../../library/model_library/k03_15mw_semi_sub_fowt/modular/includes/20_generic_objects.yml#L106354); [spec L106423](../../library/model_library/k03_15mw_semi_sub_fowt/spec.yml#L106423) | Turbine tag ControllerDLL. | held elsewhere: [docs/domains/orcaflex/examples/raw/K02/libdiscon.dll](../../examples/raw/K02/libdiscon.dll); compatibility unverified | Not applicable: held elsewhere; no substitution authorised |
| K03 | `DISCON-UMaineSemi.IN` | [K03 L29796](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L29796); [monolithic L29796](../../library/model_library/k03_15mw_semi_sub_fowt/monolithic/K03%2015MW%20semi-sub%20FOWT.yml#L29796); [modular L106355](../../library/model_library/k03_15mw_semi_sub_fowt/modular/includes/20_generic_objects.yml#L106355); [spec L106424](../../library/model_library/k03_15mw_semi_sub_fowt/spec.yml#L106424) | Turbine tag InputFile. | not held (GAP) | not named in the repository |

Table: external controller references and repository holdings.

The L02 references are declarations, not proof that the turbine invokes them.
The turbine carries `GeneratorMode: Specified rotation` at [L02 L10200](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L10200),
`GeneratorMotionController: 6` at [L02 L10201](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L10201), and
`PitchController: (none)` at [L02 L10222](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L10222).
Phase B will resolve whether the declared functions are required for the chosen case, without assuming that controller removal is valid.
K03 assigns `Controller` to generator torque at [K03 L29373](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L29373)
and pitch at [K03 L29394](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L29394).

The whole-worktree search found the two held basenames only at the locations above.
They are not present alongside either target model; a matching basename does not establish the required controller version.
The missing wrapper and controller input have no repository-named acquisition source.

## Held modular include files

These are external references relative to each modular master, not missing vendor dependencies.

| Model | Referencing model line | Referenced file | Held status | Purpose as stated |
|---|---|---|---|---|
| L02 modular | [master L6](../../library/model_library/l02_oc4_semi_sub/modular/master.yml#L6) | [includes/01_general.yml](../../library/model_library/l02_oc4_semi_sub/modular/includes/01_general.yml) | held | includefile |
| L02 modular | [master L7](../../library/model_library/l02_oc4_semi_sub/modular/master.yml#L7) | [includes/03_environment.yml](../../library/model_library/l02_oc4_semi_sub/modular/includes/03_environment.yml) | held | includefile |
| L02 modular | [master L8](../../library/model_library/l02_oc4_semi_sub/modular/master.yml#L8) | [includes/20_generic_objects.yml](../../library/model_library/l02_oc4_semi_sub/modular/includes/20_generic_objects.yml) | held | includefile |
| K03 modular | [master L6](../../library/model_library/k03_15mw_semi_sub_fowt/modular/master.yml#L6) | [includes/01_general.yml](../../library/model_library/k03_15mw_semi_sub_fowt/modular/includes/01_general.yml) | held | includefile |
| K03 modular | [master L7](../../library/model_library/k03_15mw_semi_sub_fowt/modular/master.yml#L7) | [includes/03_environment.yml](../../library/model_library/k03_15mw_semi_sub_fowt/modular/includes/03_environment.yml) | held | includefile |
| K03 modular | [master L8](../../library/model_library/k03_15mw_semi_sub_fowt/modular/master.yml#L8) | [includes/20_generic_objects.yml](../../library/model_library/k03_15mw_semi_sub_fowt/modular/includes/20_generic_objects.yml) | held | includefile |

Table: modular composition references.

## Other reference classes

- Wind: L02 uses `WindType: Constant` ([L02 L134](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L134)); K03 uses `WindType: NPD spectrum` ([K03 L151](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L151)). Neither inspected model names an external wind file.
- Scripts: K03's user-defined generator-power result is `Embedded` ([K03 L43](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L43)); its body is held in the YAML and was not executed. This is not an external script-file dependency.
- Vessel and hydrodynamic data: the inspected raw, monolithic, modular and specification text carries inline data; no external vessel or hydrodynamic data filename was found.
- Model header `File` and specification `description` strings identify provenance, not runtime dependencies. The K03 specification description names `mono.yml` ([spec L3](../../library/model_library/k03_15mw_semi_sub_fowt/spec.yml#L3)); it is not an include directive.
- The search covered file-related keys, external-function definitions, controller tags, include directives, filename extensions and header URLs. It does not reveal transitive dependencies inside missing controller code.

## Comparison reference

L02's comment says its properties are based on an interpretation of the OC3/OC4 studies and are not necessarily correct ([L02 L10](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L10) to [L02 L12](../../examples/raw/L02/L02%20OC4%20Semi-sub.yml#L12)).
No particular comparison document is identified there: reference to be selected: candidates must be public.

K03 names `VolturnUS-S type` ([K03 L164](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L164)),
`VolturnUS-S` ([K03 L21444](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L21444)),
`15MW RWT` ([K03 L29366](../../examples/raw/K03/K03%2015MW%20semi-sub%20FOWT.yml#L29366)) and the controller input listed above.
Those strings do not identify a comparison document: reference to be selected: candidates must be public.
No benchmark identity, equivalence or numerical target is inferred from these names.

## Preconditions for phase B

- Obtain the missing wrapper and controller input from a verified source; resolve compatibility and deployment of the held controller basenames. Record versions, provenance, digests and transitive dependencies.
- Select the first platform and the exact raw or library representation; verify semantic equivalence before switching representations.
- Select public comparison and case-definition references; define offsets, controller states, environmental combinations, duration, seeds and acceptance criteria.
- Merge phase A after second-provider review, then obtain the separate licensed-run authority and record the solver version.
- Retain source inputs, case manifest, compact plotted arrays, metrics and native-output evidence against a versioned criterion before launch.
- Phase B remains left: about 20 solves per platform set (1 statics, 6 free decays, about 12 load cases), as estimated in [the issue](https://github.com/vamseeachanta/digitalmodel/issues/2423). This estimate is not an executed or fully defined case count.

Continue with the [load case table](load-case-table.md) and [report skeleton](report-skeleton.html).
