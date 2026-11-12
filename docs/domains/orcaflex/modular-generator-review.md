# OrcaFlex modular generator: structure coverage and defect review

Scope: `src/digitalmodel/solvers/orcaflex/modular_generator` (schema, builders,
routers, `riser_conversion.py`), its tests under
`tests/solvers/orcaflex/modular_generator` and the examples under
`docs/domains/orcaflex/examples/modular` and `docs/domains/orcaflex/library`.
Revision reviewed: `origin/main` at `78179a8c2`.

Method: the review reads the source end to end, runs the module test suite and
runs offline probes of the generator. No OrcaFlex licence is used, so no
statement below establishes that a generated file loads or solves in OrcaFlex.
Each item states what was checked and which check remains open.

Tracker: #3990.

## 1. How the generator builds a model

The root schema `ProjectInputSpec` (`schema/root.py`) accepts exactly one
domain section, with one exception (riser plus generic fallback):

| Domain section | Route | Builders or router |
| --- | --- | --- |
| `pipeline` (+ `equipment`) | typed builders | `general`, `environment`, `vardata`, `linetype`, `supports`, `morison`, `shapes`, `buoys`, `roller`, `tug`, `buoyancy`, `end_buoy`, `lines`, `groups`, `vessel_type`, `vessel`, `winch` |
| `riser` | typed builders | `riser_clumptype`, `riser_linetype`, `riser_vessel`, `riser_lines`, `riser_links` |
| `riser` + `generic` | `riser_conversion.render_riser` into the generic emitter | `generic_builder` |
| `mooring` | `routers/mooring_router.py` into the generic emitter | `generic_builder` |
| `generic` | pass-through of every OrcaFlex object section | `generic_builder` |

The generic route reproduces native OrcaFlex data objects through a
`properties` pass-through. The 57 specifications under
`library/model_library/*/spec.yml` all use it. They are conversions of public
OrcaFlex example models. A generic specification carries native data names,
so it is a template, not a parametric engineering input. In the matrix below,
"typed" means an engineering-unit schema whose fields drive the emitted
OrcaFlex data. "Generic only" means the structure can be carried through the
generator but not specified parametrically.

## 2. Coverage matrix

Columns: builder module; schema; example specification; test or golden; gap.
Paths are relative to the generator package unless they start with `docs/` or
`tests/`.

| Structure | Builder | Schema | Example spec | Test / golden | Gap |
| --- | --- | --- | --- | --- | --- |
| SCR (steel catenary riser) | `builders/riser_lines_builder.py`, `riser_linetype_builder.py` | `schema/riser.py` (`configuration: catenary`) | `library/templates/riser_catenary/spec.yml`; `library/campaigns/generic-scr-slwr/scr/spec.yml` | `test_riser_generation.py`, `test_riser_variant_semantic_proof.py`; no typed-output golden | Line types use the `General` category only; there is no homogeneous-pipe (steel grade, wall) path for a steel riser. Contents pressure is dropped (D3, #3983). |
| SLWR / lazy wave | as SCR | `schema/riser.py` (`configuration: lazy_wave`, `buoyancy_zone`, `clump_attachments`) | `library/templates/riser_lazy_wave/spec.yml`; `campaigns/generic-scr-slwr/slwr/spec.yml` | `test_riser_generation.py` | `buoyancy_zone` is accepted and never emitted (G2, #3973). The buoyant section works only when the author splits sections manually. |
| Steep wave | as SCR | `configuration: steep_wave` | `library/templates/riser_steep_wave/spec.yml` | `test_riser_generation.py` | As lazy wave (G2, #3973). A riser base is modelled only as an anchored end. |
| Pliant wave | as SCR + `riser_links_builder.py` (tether) | `RiserLink` | `library/templates/riser_pliant_wave/spec.yml` | `test_riser_generation.py` | Link names may reference only `Anchored`, the vessel or riser lines (`schema/riser.py:435`); a tether to a buoy cannot be expressed (G3, #3974). |
| Top-tensioned riser | none typed | none | `model_library/b01_drilling_riser` (generic) | generic round-trip tests only | No tensioner, stroke or tension-ring representation in the typed riser (G5, #3976); related analysis issues #3947, #3944. |
| Drilling riser | none typed | none | `model_library/b01_drilling_riser`, `b06_running_bop`, `library/templates/drilling_riser` (generic) | generic only | As top-tensioned (G5, #3976); related #138, #3944, #3958. |
| Flexible riser | `riser_linetype_builder.py` (`General` category) | `RiserLineType` | `riser_lazy_wave` template uses flexible properties | `test_riser_generation.py` | Bend stiffener and bend restrictor attachments are not in the typed schema; generic `stiffener_types` only. Related #2429, #3948. |
| Hybrid riser tower | none typed | none | `model_library/a03_jumper_to_high_tower` (generic) | generic only | Typed riser has no buoy object; the `buoy` end connection references an object that no typed builder creates (G3, #3974). Related #3949. |
| Catenary chain mooring | `routers/mooring_router.py` | `schema/mooring.py` | none (no `mooring:` specification exists under `docs/`) | `routers/test_mooring_router.py`, `test_routing_integration.py` | A fairlead cannot reference its host vessel: generation fails (G1, #3972). Chain hydrodynamic defaults (D2, #3982). |
| Taut / semi-taut | `mooring_router.py` | `MooringSegmentType.POLYESTER` | none | router unit tests | Unbuildable for the same reason (G1, #3972); polyester mass default about 14 times high (D1, #3981). |
| Synthetic rope | `mooring_router.py` | polyester only | none | router unit tests | No HMPE or nylon type and no load-dependent stiffness (G6, #3977). |
| Buoy-supported mooring | none typed | none | `model_library/c07_metocean_buoy`, `h01_chinese_lantern` (generic) | generic only | No line attachments (clump weights, submerged buoys) in the mooring schema (G4, #3975). |
| Buoys, CALM, SPM | none typed | none | `model_library/c05_single_point_mooring`, `c06_calm_buoy`, `c06_discretised_calm_buoy`, `library/templates/calm_buoy_moored` (generic) | `goldens/basefile/cases/calm_buoy_deep_water.yml`, `salm_wire_rope.yml` (template-generator base-file goldens, not modular output) | Typed mooring cannot host on a buoy (G1, #3972); no typed buoy (G3, #3974). |
| S-lay | `lines_builder.py`, `vessel_builder.py`, `vessel_type_builder.py`, `winch_builder.py`, `buoys_builder.py`, `roller_builder.py`, `slay_*.py` | `schema/pipeline.py`, `schema/equipment.py` (`vessel`, `stinger`, `tensioner`) | `library/templates/pipeline_installation/spec.yml`; `examples/modular/E01`-`E04` (generic) | `test_slay_builders.py`, `test_slay_schema.py`, `test_24in_spec.py` | Unknown material grade falls back to X65 (D4, #3984). |
| Floating pipeline installation (tugs) | `tug_builder.py`, `buoyancy_builder.py`, `end_buoy_builder.py`, `shapes_builder.py` | `equipment.tugs`, `equipment.buoyancy_modules` | `library/templates/pipeline_installation/spec.yml` | `builders/test_tug_builder.py`, `test_buoyancy_builder.py`, `test_end_buoy_builder.py` | The documented tug-span check is a no-op (D7, #3987). |
| J-lay | none | none | none | none | Not supported (G7, #3978). |
| Reel-lay | none | none | none | none | Not supported (G7, #3978). |
| Jumpers and spools | generic route via `marine_ops/installation/jumper_to_modular_spec.py` | `generic` | `model_library/mj01`-`mj03`, `f06_spool_piece_manoeuvring`, `library/templates/jumper_rigid_subsea` | `test_gom_tieback_jumper_pipeline.py`, `test_m_jumper_library_specs.py`, `test_jumper_*` | Existing open work: #476, #601, #602, #603. No new issue. |
| Umbilicals | none typed | none | `library/line_types/umb_*.yml` (line types only) | none | No typed umbilical route; the riser schema could carry it but has no umbilical cross-section helper (G8, #3979). |
| Towed and lifted objects | none typed | none | `model_library/e05_pipe_davit_lift`, `f01_lowered_cone`, `f07_suction_anchor_lowering`, `g04_anchor_last_deployment`, `j01_deployment_with_sub`, `i01_streamer_array` (generic) | generic only | Existing open work: #2427, #134. No new issue. |
| Floating wind turbines and their moorings | none typed (`GenericTurbine` pass-through) | `generic` | `model_library/k03_15mw_semi_sub_fowt`, `examples/modular/K01`, `K03` | generic only | Assigned to the floating wind work stream; related #2435. Not reviewed here. |
| Fixed structures | none typed | none | `model_library/k02_10mw_fixed_bottom_owt`, `h02_jacket_to_semisub` (generic) | generic only | No typed fixed-structure route; no new issue raised (low demand evidence). |

Cross-cutting gaps:

- G13. No golden YAML exists for typed riser, mooring or pipeline output; the
  only goldens (`goldens/basefile`) cover the template generator's base-file
  writer. Typed builders are checked by assertions on selected keys. Related
  #498.
- G14. `riser_conversion.extract_riser` rejects the native A01 catenary riser
  (`model_library/a01_catenary_riser/spec.yml`). The probe raises a
  `ValidationError` because native `EI` (`[124.87, ~]`) and `Cd`
  (`[Generic Drag, ~, 0.008]`) are lists. The module docstring limits the
  bridge to scalar properties; the consequence is that no public example
  riser can be upgraded to the typed route.

## 3. Defects

Each defect names the file and line on `78179a8c2`, the evidence and the
comparator.

| ID | Location | Defect | Evidence and comparator |
| --- | --- | --- | --- |
| D1 (#3981) | `routers/mooring_router.py:78`, `:90` | Wire-rope and polyester mass coefficients give masses far above the library line types the comments cite. | 76 mm wire: router 0.196 te/m against `library/line_types/wire_rope_76mm.yml` 0.025 te/m (ratio 7.9). 120 mm polyester: router 0.115 te/m against `polyester_rope_120mm.yml` 0.008 te/m (ratio 14.4). Existing tests compare the output to the same coefficient, so they cannot detect it. |
| D2 (#3982) | `routers/mooring_router.py:37`, `:175`, `:178`, `:193` | Chain defaults mix chain types and double the normal drag. | Mass uses the stud-link coefficient 0.0219 d² kg/m. EA (493,270 kN at 76 mm) lies between the DNV-OS-E302 studless R3 value (462,356 kN) and the stud-link value (557,077 kN). The hydrodynamic OD is 2d with Cd 2.4, so normal drag per metre is 2.0 times the DNV-OS-E301 studless value of Cd 2.4 referenced to the nominal diameter d. The axial Cd is 0.008 against 1.15. Source: SINTEF SIMA/RIFLEX documentation, "Chain cross-section", https://sima.sintef.no/docs/latest/riflex/sima/context/ChainCrossSection.html (retrieved 2026-11-12), quoting DNV-OS-E302 and DNV-OS-E301. Whether OrcaFlex applies the axial coefficient to the same reference diameter is not established offline. |
| D3 (#3983) | `builders/riser_lines_builder.py:102`-`103`; `schema/riser.py:158`-`159` | `contents.pressure` and `contents.flow_rate` are validated and then silently dropped. | The emitted line carries `ContentsDensity` only; native A01 lazy wave (`examples/modular/A01/.../07_lines.yml:68`-`69`) carries `ContentsPressure` and `ContentsFlowRate`. A pressurised riser is generated at zero internal pressure. |
| D4 (#3984) | `builders/linetype_builder.py:103`-`105` | An unsupported `pipeline.material` (for example `X80`) silently takes X65 properties. | `DNVSTF101Fy` becomes 450,000 kN/m² instead of failing; the supported set is X52, X60, X65, X70. |
| D5 (#3985) | `riser_conversion.py:28`-`31` | Typed `axial_drag_coefficient` is rendered as data name `CdAxial`, and a scalar `drag_coefficient` overwrites the native `Cd` list. | `CdAxial` occurs in no native example under `docs/domains/orcaflex/examples` (0 matches). Native `Cd` is `[normal, ~, axial]`. Rejection by OrcaFlex is not established offline. |
| D6 (#3986) | `builders/riser_vessel_builder.py:46` | `RiserVessel.orientation` pitch and roll are discarded; only the third component becomes `InitialHeading`. | A spec with `orientation: [0, 2, 0]` generates a level vessel without warning. |
| D7 (#3987) | `schema/root.py:137`-`145` | The docstring lists a tug-position check; the code computes it and then passes. | `max_tug_x > 1.5 * pipeline length` leads to `pass`. |
| D8 (#3988) | `routers/mooring_router.py:352`-`358` versus `builders/riser_lines_builder.py:187`-`188` | The word `anchor` means different things in the two domains. | A riser `anchor` end becomes `Anchored` (z measured from the seabed); a mooring `anchor` endpoint becomes `Fixed` (z global). The same spec value places the anchor at different depths. |
| D9 (#3989) | `builders/riser_linetype_builder.py:131`; `routers/mooring_router.py:198`, `:233`, `:267` | Seabed lateral friction is hard-coded (0.5 riser; 1.0, 0.5 and 0.3 mooring) with no schema field. | Silent default. `riser_linetype_builder.py:71`-`75` also has two identical branches for string and numeric Cd. |

Checked and not a defect: Line connection rows of eight values under the
nine-column `Connection, ..., ConnectionzRelativeTo` key
(`riser_lines_builder.py:90`-`92`, `:198`-`207`) match the native A01 files,
which also omit the trailing cell.

Documentation mismatch: `builders/shapes_builder.py:46` checks
`ramps is not None`, but `ramps` defaults to an empty list, so the condition is
always true. The default ramp shapes in `_build_default_ramps` are then
emitted for every floating installation without ramps declared. The S-lay
case returns no default shapes. The behaviour appears intended, but the
docstring states the opposite.

## 4. Test baseline

`python -m pytest tests/solvers/orcaflex/modular_generator tests/solvers/orcaflex/test_spec_upgrader_v2.py -q`
on `78179a8c2` (Windows, no licence): 1352 passed, 17 failed, 5 skipped, 34 errors in 349 s. The 34 errors are in `test_modular_vs_monolithic.py`, which needs a licensed OrcaFlex load. The review does not classify the 17 failures; PRs from this review compare touched test files against this baseline.

## 5. Issues and planned changes

| ID | Issue | Planned change |
| --- | --- | --- |
| G1 | #3972 | Enhancement PR: accept `mooring` + `generic` host objects |
| G2 | #3973 | Enhancement PR: emit the buoyancy zone as split sections |
| G3 | #3974 | Enhancement PR: typed riser buoys |
| G4 | #3975 | Enhancement PR: mooring clump and buoy attachments |
| G5 | #3976 | Issue only |
| G6 | #3977 | Issue only |
| G7 | #3978 | Issue only |
| G8 | #3979 | Issue only |
| G14 | #3980 | Issue only |
| D1 | #3981 | Fix PR |
| D2 | #3982 | Issue only (needs OrcaFlex axial-drag reference) |
| D3 | #3983 | Fix PR |
| D4 | #3984 | Fix PR |
| D5 | #3985 | Issue only (needs licensed load) |
| D6 | #3986 | Fix PR |
| D7 | #3987 | Issue only |
| D8 | #3988 | Issue only (convention decision) |
| D9 | #3989 | Issue only |
| G13 | #498 (comment) | Each enhancement PR adds a typed-output golden |

Items needing a licensed OrcaFlex load are packaged separately; this review
does not state that any generated model loads or converges.

## 6. Addendum: findings after the first issue set

- #4007. 24 of the 40 library line types under
  `docs/domains/orcaflex/library/line_types/` (all riser, pipe, flexible,
  hybrid and umbilical types) carry mass per unit length and EA about 1000
  times the OrcaFlex SI values (te/m, kN). The SI templates include them, for
  example `templates/risers/scr_hybrid/base/scr_base.yml:53`. After unit
  conversion, the steel EA values still differ from the steel area of the
  stated OD and ID by a factor of about 2.9 (10 in SCR: 785,000 kN against
  2.28e6 kN), so the data is not established by a units correction alone.
- Spring/damper links from `builders/riser_links_builder.py` carry
  `UnstretchedLength` and `Stiffness` without `LinearSpring` or
  `LinearDamper`. The native linear spring/damper in the public example
  "H03 Floating and stowed lines" states both. The schema documented the
  stiffness in kN for both link types; OrcaFlex spring/damper stiffness is
  per metre (kN/m). Both points are addressed with the top-tensioner change
  for #3976.
- The A05 single-line risers (`a05_catenary_spar`,
  `a05_lazy_wave_with_fpso`) use homogeneous-pipe line types. `RiserLineType`
  represents the General category only, so these examples cannot be upgraded
  to the typed route even after #3980.
