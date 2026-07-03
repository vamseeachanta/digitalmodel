# Riser Database (#1245 / #1199a)

First slice of the riser domain database (parent #1199, exemplar of the
data-source flywheel epic llm-wiki#799). This slice ships the tabular
foundation: a fail-closed loader, three public reference tables, two
citation-emitting getters, and the leak gate.

## Tables (`data/riser_database/`)

Generated deterministically by `scripts/riser_database/build_tables.py` from
the externalized seed spec `scripts/riser_database/sources.yml` — regenerate
and `git diff --exit-code data/riser_database` to verify the committed tables
match the seeds. `manifest.yaml` carries per-table sha256 fingerprints; the
loader refuses a table whose fingerprint drifted.

| Table | Route | Content |
|---|---|---|
| `config_catalog` | public | Riser configuration envelopes seeded from `examples/riser_input/*.yml` (5) + `config/pipe/*riser*.yml` (22, explicit list). Depth/position for the pipe configs derive from the FILENAME (`<n>ft` → m × 0.3048); OD/WT are inches → mm × 25.4. A missing token yields an empty cell — never guessed. |
| `standards_crosswalk` | public | Clause **identifiers** + `wiki_path` only: `api-std-2rd` (there is no `api-rp-2rd` — API reclassified RP 2RD as STD 2RD), `api-rp-16q`, `dnv-os-f201`, `dnv-rp-c203`. `registry_revision` mirrors llm-wiki `data/standards-registry.json` verbatim. |
| `material_sn_scf_dff` | public | References into `fatigue/sn_curves.py` vocabulary (plain detail-category letters + environment — the REAL riser fatigue consumer, used by `riser_fatigue/workflow.py` and `touchdown.py`; NOT the composite ids of `sn_library_api`). `scf_ref`/`dff_ref` name getters. |

**Licensing rule:** no DNV/API clause text, table content, or S-N parameter
values in this public repo. Values come through `CitedValue` getters resolving
against the **private** llm-wiki (`.claude/rules/codes-standards-data-routing.md`
in workspace-hub). The schema tests enforce exact column allowlists plus
content-shape tripwires (clause-locator regex for `section`; length and
numeric-token-density caps on free text) so values cannot ride free-text
columns either.

## Loader

```python
from digitalmodel.riser_database import RiserDatabase

db = RiserDatabase.load()          # fail-closed: raises RiserDatabaseError
row = db.config("riser_scr")       # unknown key raises — never fuzzy-matches
```

Bounded reads (known filenames, 5 MB cap, `yaml.safe_load`), fingerprint
verification against `manifest.yaml`, exact column checks, duplicate-key
rejection. Escalation mirrors `parametric.atlas.Atlas.predict`: unknown key →
raise with reason.

## Citation getters

```python
from digitalmodel.riser_database import (
    get_riser_dff, get_riser_scf,
    get_tension_weight_factor, get_buoyancy_tension_factor, riser_citations,
)

get_riser_dff().value                  # 10.0 — DNV-OS-F201 (allowable damage = 1/DFF)
get_riser_scf().value                  # 1.0  — DNV-RP-C203 methodology (neutral default)
get_tension_weight_factor().value      # 1.05 — API RP 16Q (1st Ed. 1993) §3.3, f_wt
get_buoyancy_tension_factor().value    # 0.96 — API RP 16Q (1st Ed. 1993) §3.3, f_bt
get_riser_dff(override=3.0)            # user override wins; note records both values
riser_citations()                      # {dff, scf, f_wt, f_bt}; {} + RuntimeWarning standalone
```

Direct getter calls are **fail-closed** (`CitationResolutionError` carrying the
`code_id`); only the consumer helper degrades in standalone/`pip install` mode
(one-shot `RuntimeWarning`) — same contract as the mooring pilot. Values match
the riser literals (`touchdown.py` `dff=10.0`, `scf=1.0`; `stackup.py`
`F_WT_DEFAULT=1.05`, `F_BT_DEFAULT=0.96`) so the #1246 wiring has day-one
parity. `riser_fatigue/workflow.py` resolves DFF through `resolve_dff()`:
an explicit `dff` in settings wins; an absent `dff` resolves the cited
DNV-OS-F201 default in-context and **raises** standalone (never a silent
default on a pass/fail input).

Getter provenance discipline: a getter only cites a standard the code actually
sources the value from. #1246 adds the two API RP 16Q **1st Ed. 1993**
(reaffirmed 2001) §3.3 tension factors — the revision string is pinned to
`1993` to match the 1993-numbered clause locators (a 2nd Edition, 2017,
exists but is not the citation basis; the wiki page's `revision_note` records
the drift, mirroring the DNV-RP-C203 2021-vs-2024-10 precedent). The
top-tension safety factor (**1.25**) has **no** corresponding 16Q provision, so
it carries no getter and stays a project default in `stackup.py`; the Barlow-SF
likewise carries no standards attribution (generic formula, project default).

## Leak gate (suite 6 — hard merge gate)

`tests/riser_database/test_leak_gate.py`:

* **Layer A** (always runs, public CI included): regex structural checks —
  absolute paths, e-mail addresses, URLs — over every loaded public row and
  the raw data files. Patterns are generic and public-safe.
* **Layer B** (in-context): every public row matched against the PRIVATE
  workspace-hub `.legal-deny-list.yaml` with case/separator normalization.
  Skips loudly when workspace-hub is absent.

**Merge-gate procedure:** Layer B must run GREEN in a workspace-hub checkout
**before the branch is first pushed** (exposure happens at push, not merge).
Additionally run `workspace-hub/scripts/legal/legal-sanity-scan.sh
--repo=digitalmodel --diff-only` **while the changes are still uncommitted**
(the flag diffs the working tree against HEAD; at PR time it is vacuous) and
attach the output to the PR as evidence.

## Extension map

* **#1246** — *done*: added the two API RP 16Q §3.3 tension-factor getters
  (behind the llm-wiki `api-rp-16q` revision-`1993` pin), corrected the
  `stackup.py` attribution comments (1.25 has no 16Q provision), and wired
  `riser_fatigue/workflow.py` DFF through `resolve_dff()`.
* **#1247** — orcaflex riser citation surface (net-new; no parity baseline).
* **#1281a** — *done*: operating-envelope analytical tier (beam-column response
  model + sweep engine + von-Mises / flex-joint getters). See the
  operating-envelope section below. **#1281b** (AMJIG + WH-moment + data
  columns) and **#1281c** (solver-backed tier stub) follow.
* **#1199d** (T3, frontier-gated) — de-identified ACE result precedent,
  `private_sidecar` route; extends the leak gate to those rows.
* Future numeric response-surface atlases (metocean, VIV) live under
  `atlases/riser_database*` per `parametric.atlas` conventions — deliberately
  NOT used for these relational tables (binary Parquet is invisible to
  text-based leak scanning).

## riser_stackup + rig_riser_interface (#1259)

The stack-up layer publishes **handles + banded envelopes only**. Each row of
`riser_stackup` carries an `rsu_id` (resolving into the PRIVATE llm-wiki
stack-up registry via `wiki_path` — always the registry page itself, never
per-source pages), a `topology_class` from a controlled vocabulary (so
depth-collapsed queries are first-class), a coarse `water_depth_band`
(synthetic example: a dataset at an exact depth of, say, 1,234 m publishes
only `1000-1500m`), and the `rig_ref` **residency join**: a riser string is
stationed on a rig, so every stack-up row can join to a rig-class row in
`rig_riser_interface` (tensioner/TJ capability columns; `wed_rig_name` joins
the worldenergydata rig fleet and is populated only for rows sourced from
public rig specifications).

**De-id rules (enforced by tests):** no exact depths/masses/joint counts, no
unit-bearing values, no registry generic-name strings, and no provenance name
tokens or project numbers anywhere in the public artifacts (`tests/
riser_database/test_stackup_rig.py`, `test_provenance_tripwire.py` — the
latter auto-extracts tokens from the private registry's source pages, so it
runs in-context and is part of the merge gate). Banding is decided wiki-side,
on water depth only, one band per source dataset family.

**Cross-repo coupling (by design):** registering a new RSU id in the llm-wiki
registry makes the in-context reconciliation test fail here until the table
gains the row — a conscious update, never silent drift. Public issues, PRs
and commit messages about this table family carry bands and handles only.

## Assembly engine (#1280)

`drilling_riser/assembly.py` turns component records + counts into a
`RiserStackupModel` (parts from the public wed component vocabulary via
`from_component_counts`, or caller/wiki-side records via `from_records`) with
fail-closed aggregates — notably `gross_submerged_weight_and_uplift()`, which
REFUSES to degrade to net weight when a buoyed joint lacks uplift data
(non-conservative for the factored 16Q split). `minimum_top_tension_16q`
applies the tensioner-system factor N/(Rf·(N−n)) and fleet-angle allowance to
a minimum slip-ring tension; `check_rig_capability` evaluates the demand
against a `rig_riser_interface` row (wireline rigs count wires, direct-acting
count units; missing data → INSUFFICIENT_DATA, never guessed). The in-context
golden asserts the chain against the RSU-0007 calc contract (machine-readable
YAML on the private registry page — keys public, values wiki-side; tolerance
abs ≤ 0.5 t / rel ≤ 0.15%, an order of magnitude tighter than any
formula-omission error).

## Operating-envelope engine (#1281a, epic #1279 child C)

Two-tier, split into grandchild slices C1/C2/C3.

**C1 (this slice) — analytical screening tier.**
`drilling_riser/riser_response.py` solves the static tensioned beam-column BVP
`EI y'''' − T y'' = w(z)` (pinned flex-joint ends, top offset BC, uniform
current-drag load) in stable closed form → deflection, flex-joint angles, and
the bending moment `M(z)`. Provenance is standard tensioned-beam / beam-column
marine-riser mechanics (NOT any licensed project material); it is a **static
screening** model with no dynamic amplification.

`drilling_riser/envelope.py` (`compute_operating_envelope`) sweeps
**offset × current × seastate** and evaluates four limits — flex-joint angle,
von Mises utilisation, tensioner/TJ stroke, moonpool clearance. Offset and
current move the responses through the beam-column model (the von Mises check
feeds `orcaflex.code_check_engine.check_api_rp_2rd` the model's `M(z)`, so it is
bending-aware); seastate enters via a linear RAO motion contribution. The
operating mode (`OperatingMode` drilling / connected / hang-off) selects the
active limit set via `envelope_modes.yml`.

Criteria are fail-closed cited getters: flex-joint angle limits (mean 2° / max
4°) cite **API RP 16Q**; the von Mises design factor (0.67) cites **API STD
2RD** — both resolve against existing wiki pages (no paired wiki PR). Stroke
margin and moonpool clearance are **uncited project defaults** (no public
standards provision identified — the Barlow / top-tension-SF precedent). No new
public data columns, so the leak surface is unchanged.

**Deferred:** the wellhead/conductor-moment limit (needs a net-new rig data
column) and the AMJIG per-category extreme/survival criteria (licensed) are
**#1281b**. Standard selection (16Q vs AMJIG) is a query-time *threshold*, not a
sweep axis.

**C3 (#1346, #1281c) — solver-backed dynamic tier STUB.**
`scripts/parametric/build_drilling_riser_envelope_library.py` builds a
`parametric.library` atlas (`drilling_riser_envelope`) keyed on **operating
mode** (categorical) × offset/current/Hs/Tp (continuous), storing a **von Mises
dynamic amplification factor** (DAF). It ships as a STUB (`solver.licensed=False`,
`version="STUB"`) with synthetic values until a licensed OrcaFlex time-domain
run per mode replaces them; bumping the `LIBRARY_EXPECTATIONS` version off "STUB"
makes the committed stub read stale → the query escalates → a real run is
prompted. `compute_operating_envelope(dynamic=True)` loads the atlas once,
multiplies the **von Mises** utilisation by the DAF (the one response the static
tier leaves untreated — flex-joint angle and stroke already carry a linear RAO
term, so they are NOT re-amplified), escalates (never extrapolates) outside
coverage, and populates `EnvelopeResult.dynamic_provenance` with the STUB marker
+ disclaimer so a dynamically-amplified verdict never leaves unlabeled. The
`_provenance` surfacing of the solver block was extended for all library atlases
(also closes the same gap for the OrcaFlex/diffraction libraries).
