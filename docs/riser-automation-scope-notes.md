# Riser & installation analysis automation — scope notes

**Status: notes only.** No commitment, no schedule, no implementation. Written to
capture the intended direction and — more usefully — to record what already exists
and where the real obstacles are.

**Direction:** extend automated riser analysis to cover **drilling and completion
riser analysis across vendor systems**, and automate **installation analysis** the
same way.

---

## 1. What already exists

Worth stating plainly, because the gap is smaller than it looks in some places and
larger in others.

### Drilling riser — mature
`src/digitalmodel/drilling_riser/` is not a stub. It already carries stack-up and
section build (`stackup.py`, `section.py`, `assembly.py`, `schedule_assembly.py`),
response (`riser_response.py`, `response_correction.py`, `conductor_response.py`,
`damping.py`), operability (`operability.py`, `operability_screening.py`,
`operability_atlas.py`, `envelope.py`, `envelope_modes.yml`), drift-off
(`drift_off.py`), tool passage, TSJ workflow, and live-data hooks
(`metocean_inputs.py`, `telemetry_inputs.py`, `twin_loop.py`, `monitor_config.yml`).

### Riser data — a fail-closed database with a licensing route already solved
`src/digitalmodel/riser_database/` ships a fingerprint-verified loader, a
`config_catalog` (27 configurations seeded from `examples/riser_input/*.yml` and
`config/pipe/*riser*.yml`), a `standards_crosswalk` (`api-std-2rd`, `api-rp-16q`,
`dnv-os-f201`, `dnv-rp-c203`) and `material_sn_scf_dff`.

**The important part is the licensing pattern**: identifiers and clause *locators*
live in this public repo; the values resolve through `CitedValue` getters against
the **private** llm-wiki. Section 3 depends on reusing that pattern rather than
inventing one.

### Other riser lanes
`riser_fatigue/`, `subsea/vertical_riser/`, `subsea/catenary_riser/`,
`subsea/riser/`, plus base configs for `riser_screening`, `riser_stackup`,
`riser_fatigue`, `vertical_riser`, `simple_catenary_riser`. Scripts:
`run_riser_analysis.py`, `benchmark_riser_library.py`, `mesh_sensitivity_riser.py`,
`capture_riser_views.py`, `extract_riser_validation.py`.

### Installation — partial, and not riser-shaped
`installation/` covers `jumper_installation`, `pipelay`, `installation_pamphlet`.
`marine_ops/installation/` adds the genuinely reusable physics: `splash_zone.py`,
`crane_tip_motion.py`, `jumper_lift.py`, `go_no_go.py`, `operability.py`,
`realtime_feedback.py`, `suitability_report.py`, `jumper_to_modular_spec.py`.

---

## 2. The three real gaps

### Gap A — there is no vendor dimension
`grep vendor` across the riser modules returns only *"vendored component library"*
(software sense) and *"no vendor data"* (a licensing disclaimer). The data model has
**no** notion of manufacturer, joint family, connector type, or rated capacity.

Automating "across all vendors" is therefore not a coverage exercise — it needs a
schema extension first. Minimum fields a vendor-aware catalogue implies:

| Field | Why |
|---|---|
| `vendor`, `joint_family`, `series` | The identity a riser engineer actually selects on |
| `connector_type`, `preload`, `make-up torque` | Governs capacity and fatigue class; differs per family |
| `pressure_rating`, `tension_capacity`, `bending_capacity` | The acceptance envelope |
| `buoyancy_module` OD/ID/density/uplift | Drives effective tension and drag |
| `aux_lines` (C&K, booster, hydraulic) | Mass, drag, and the reason vendor stack-ups differ |
| `qualification_ref` | Which report/edition the rating traces to |

### Gap B — completion / workover riser is absent
There is `drilling_riser` and `vertical_riser`, but no completion, workover or
intervention lane — no CWOR, landing string, or open-water intervention riser
module. That is a genuinely different problem: different pressure regime, different
duration, different failure modes (disconnect, shear, hang-off), and a different
standard set from drilling.

### Gap C — installation analysis is jumper- and pipelay-shaped
The existing installation code handles lifts and lays. **Running a riser string is a
different operation**: stack-up sequencing, running weight and net downward margin
through the sequence, hang-off and slip-joint states, weather-window and workability
limits, recovery and contingency. The reusable pieces (`splash_zone`,
`crane_tip_motion`, `go_no_go`) exist and should be composed rather than duplicated.

---

## 3. The obstacle that decides the design

**This repository is public.** Vendor riser catalogues are largely proprietary, and
the repo already carries an explicit rule against holding standards clause text or
S-N parameter values here.

So a vendor catalogue **cannot** be a public data table of ratings. The route that
already works is the `riser_database` one:

- **Public:** vendor and joint-family *identifiers*, schema, units, provenance
  fields, and the code that consumes them.
- **Private (llm-wiki):** the rated values, resolved at run time through
  `CitedValue` getters, with the citation travelling alongside the number.
- **Fail-closed:** an unresolved vendor value must raise, never fall back to a
  guess — matching the existing loader's behaviour on unknown keys.

Anything that puts vendor ratings directly in this repo is the wrong shape,
regardless of how convenient it is.

---

## 4. Sketch of the work (notes, not a plan)

1. **Schema first.** Extend `riser_database` with the vendor dimension in Gap A,
   public identifiers only, values routed privately. Regenerate tables
   deterministically from seeds as the existing build does, keep the manifest
   fingerprints.
2. **Generalise the stack-up builder.** `drilling_riser/stackup.py` and
   `assembly.py` already build a string from sections; the extension is selecting
   sections from a vendor catalogue rather than a bare config.
3. **Add the completion/intervention lane** as a sibling to `drilling_riser`,
   reusing section/stack-up/response and replacing the operability and acceptance
   layers.
4. **Compose installation** from `marine_ops/installation` primitives into a
   riser-running workflow: sequence → net weight margin → workability → window.
5. **Benchmark harness.** `benchmark_riser_library.py` already exists; extend it so
   each vendor configuration has a regression case with a pinned expected result.

---

## 5. Open questions

1. **Which vendors, and from what source?** Public product literature gives
   configuration and capacity but rarely qualified values. Without a licensed data
   source, the catalogue can only hold what the private wiki can cite.
2. **Completion riser scope** — landing string only, or full CWOR including subsea
   test tree and open-water intervention?
3. **Installation scope** — riser running only, or the full campaign including
   hardware, weather windows and vessel suitability?
4. **What is the acceptance basis per lane?** Drilling risers are API RP 16Q
   territory; completion and intervention are not. Each lane needs its criteria
   fixed with an **edition**, not just a standard name.
5. **How much is verification?** A vendor-parameterised generator that is not
   benchmarked against known results is a liability, not an asset.

---

## 6. Note on scope discipline

Automating across vendors multiplies configurations; it does not by itself improve
any single answer. The value only appears if each generated configuration is
verified. Extending the benchmark harness alongside the generator — not after it —
is what keeps this an asset.
