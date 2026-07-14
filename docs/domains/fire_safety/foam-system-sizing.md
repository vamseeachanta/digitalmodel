# Foam/fire-suppression system sizing — screening calc

## What this is

`digitalmodel.foam_system` and the routed workflow basename
`foam_system_sizing` provide a deterministic **screening** calculation for
low-expansion foam fire-suppression systems (deck foam, monitor/deluge
protection, hose allowances) of the kind found on MODUs, drilling units and
tankers:

1. **Foam solution demand** — protected areas x application rate, with the
   application-rate criteria supplied as a config table keyed by
   standard+edition;
2. **Foam concentrate quantity** — demand x discharge time x
   concentration, plus a reserve policy (e.g. 100% reserve);
3. **Proportioner duty check** — design solution flow and concentrate
   injection rate against the proportioner's rated envelope (vendor data);
4. **Hydraulic screening** — Hazen-Williams or Darcy-Weisbach
   (Swamee-Jain) pipe-run pressure drop through a simple tree distribution
   network, static head, and pump head/flow margin.

## Screening posture — read this first

This is a screening/sizing sanity calc, **not** a detail design tool:

- the network must be a tree (no loops, no balancing);
- foam solution is treated hydraulically as water (config density);
- fittings enter as equivalent lengths only;
- proportioner/eductor pressure losses are not modelled — only the rated
  flow envelope is checked;
- no discharge-device K-factor modelling; terminals state a required flow
  and pressure directly.

**Final foam-system designs are governed by the authority having
jurisdiction (AHJ) and/or the classification society plan review against
the applicable edition of the referenced standards** (e.g. NFPA 11,
SOLAS II-2 / FSS Code, ABS MODU rules, CAP 437 for helidecks). This module
exists to screen sizing before/alongside that review, not to replace it.

## Citation-carrying criteria table

Application rates and discharge times are **not** hard-coded. They come in
via the config `criteria` table, and every entry must carry a citation
(`standard`, `edition`, `clause`; optional `note`). Entries without a full
citation are rejected — a rate value that cannot be traced to a code
edition is not usable in a deliverable. Citation strings are echoed into
the demand CSV so the output is self-documenting.

An **illustrative** example set (values must be verified against the
purchased/governing edition before use — the clause fields below are
deliberately generic):

```yaml
criteria:
  helideck_foam:
    application_rate_lpm_per_m2: 6.5
    discharge_time_min: 10.0
    citation:
      standard: "NFPA 11"
      edition: "2021"
      clause: "example helideck monitor rate"
      note: "illustrative example value — verify against the purchased edition"
  modu_deck_foam:
    application_rate_lpm_per_m2: 6.5
    discharge_time_min: 15.0
    citation:
      standard: "NFPA 11"
      edition: "2021"
      clause: "example hydrocarbon spill-fire rate"
      note: "illustrative example value — verify against the purchased edition"
  tanker_deck_foam:
    application_rate_lpm_per_m2: 0.6
    discharge_time_min: 20.0
    citation:
      standard: "IMO FSS Code"
      edition: "as amended"
      clause: "Ch. 14 deck foam (example)"
      note: "illustrative example value — verify against the governing edition"
```

The repo's Codes & Regs register (`docs/domains/codes-register.md`) is the
place to keep the digested, edition-pinned values that feed this table.

## Starter cited criteria library (#1586)

Commonly used rates ship with the module as a YAML library
(`src/digitalmodel/foam_system/data/foam_criteria_library.yml`) so users
select entries by key instead of re-typing cited values per job:

- **NFPA 11 (2021)** — fixed-roof tank surface application (flash-point
  variants), open-top floating-roof top-of-seal (foam dam), hydrocarbon
  spill (portable);
- **SOLAS FSS Code** (Res. MSC.98(73) as amended, 2015 consolidated
  edition) — the three fixed deck foam rate bases (cargo deck area 0.6,
  largest tank section 6.0, largest monitor area 3.0 L/min/m2);
- **IMO MODU Code (2009)** — helideck foam application.

Every entry carries the mandatory Citation fields and an explicit edition.
Entries flagged `verify_against_source: true` (all NFPA clause numbering
and the MODU helideck basis) must be confirmed against the purchased /
governing edition before use in a deliverable — the loader appends a
`VERIFY AGAINST SOURCE` marker to the citation note so the flag follows
the value into every output CSV.

Selection API: `digitalmodel.foam_system.criteria_library`
(`load_criteria_library`, `get_criterion`, `list_criteria`), or the
workflow config key `criteria_library` (list of keys, merged with any
inline `criteria`; key collisions are an error — library values are never
silently overridden). Tests:
`tests/fire_safety/test_foam_criteria_library.py`.

## Report-pack demo (#1587)

`examples/workflows/foam-system-report-pack/` wires a sizing run into the
standard report pack end to end (same pattern as the fatg_spectral_fatigue
demo): `foam_system_sizing.yml` (synthetic tanker deck-foam fixture using
library keys) -> `data/foam_system_sizing_*.csv` -> `input.yml`
(`report_pack`) -> md/html(/pdf) report with citations sidecar and
report-layer provenance manifest. Tests:
`tests/report_pack/test_foam_report_pack.py` (asserts the checked-in CSVs
regenerate byte-identically and the pack renders and validates).

## Method

- **Demand**: per area `Q = A x rate`. `demand_policy: max` treats the
  protected areas as alternative single-fire scenarios (largest governs);
  `sum` discharges all simultaneously. Hose-stream allowances
  (`flow x count`) always add on top of the governing area demand.
- **Concentrate**: governing-area solution volume (`Q x discharge time`,
  each area using its own criterion's time) x concentration, plus
  hose-stream concentrate (`flow x count x duration x concentration`),
  all x `(1 + reserve_percent/100)`. Injection rate =
  design solution flow x concentration.
- **Proportioner**: design solution flow within
  `[min_solution_flow_lpm, max_solution_flow_lpm]`; injection rate below
  `max_concentrate_flow_lpm`; concentrate percentage equal to the
  proportioner rating. Utilization reported. Vendor proportioner curves
  are a client-supplied intake item — only the rated envelope is checked.
- **Hydraulics** (single discharge scenario, all listed terminals
  flowing): run flow = sum of downstream terminal flows;
  - Hazen-Williams (SI): `h_f = 10.67 L Q^1.852 / (C^1.852 d^4.87)`
    (Q m3/s, d m), default `C = 120`;
  - Darcy-Weisbach: Swamee-Jain friction factor
    `f = 0.25 / log10(eps/3.7D + 5.74/Re^0.9)^2` (laminar `f = 64/Re`);
  - required pump head per terminal = path friction + static lift +
    required pressure converted at config density; worst terminal governs;
    pump head and flow margins reported.

Units: SI throughout — m2, L/min, minutes, m, mm, bar.

## Validation

`tests/fire_safety/test_foam_system_sizing.py` carries a fully synthetic
MODU-like deck fixture (round numbers: 300 m2 helideck, 400 m2 drill
floor, 2 x 400 L/min hose stations, three-run tree network) with the
complete hand calculation reproduced in the test docstring: design demand
3400 L/min, concentrate 3300 L at 3% with 100% reserve, proportioner
utilization 85%, required pump head 99.61 m vs 120 m rated (margin
20.39 m). The Darcy-Weisbach path is checked against the exact
Hagen-Poiseuille solution in the laminar regime and cross-checked against
Hazen-Williams in the turbulent regime. No client data anywhere;
project-specific calibrations stay private.

## Usage

```yaml
basename: foam_system_sizing
foam_system_sizing:
  criteria:
    modu_deck_foam:
      application_rate_lpm_per_m2: 6.5
      discharge_time_min: 15.0
      citation: {standard: "NFPA 11", edition: "2021",
                 clause: "example hydrocarbon spill-fire rate",
                 note: "illustrative — verify against purchased edition"}
  protected_areas:
    - {name: drill floor, area_m2: 400.0, criterion: modu_deck_foam}
  demand_policy: max
  hose_streams:
    - {name: foam hose stations, flow_lpm: 400.0, count: 2, duration_min: 20.0}
  concentrate: {concentration_percent: 3.0, reserve_percent: 100.0}
  proportioner:
    min_solution_flow_lpm: 500.0
    max_solution_flow_lpm: 4000.0
    max_concentrate_flow_lpm: 150.0
    rated_concentration_percent: 3.0
  hydraulics:
    method: hazen_williams        # or darcy_weisbach
    hazen_williams_c: 120.0
    pump: {node: PUMP, rated_flow_lpm: 3600.0, rated_head_m: 120.0}
    runs:
      - {from: PUMP, to: J1, length_m: 100.0, diameter_mm: 200.0}
      - {from: J1, to: MONITOR, length_m: 50.0, diameter_mm: 150.0}
    terminals:
      - {node: MONITOR, flow_lpm: 2600.0, required_pressure_bar: 7.0,
         elevation_m: 15.0}
  output_dir: results
```

Outputs: per-area demand CSV (with citation strings), hydraulic run CSV
(flow, velocity, head loss per pipe run), terminal path CSV (path,
friction/static/pressure heads, required pump head), and a summary dict in
the returned config (`demand`, `concentrate_result`,
`proportioner_result`, `hydraulics_result`). Feeds the standard report
pack (`report_pack`).

## Gaps / follow-ups

- Looped/gridded network balancing (Hardy Cross or solver-based) — out of
  screening scope by design.
- Proportioner and discharge-device pressure-loss models — pending vendor
  curves (client-supplied intake item).
- Medium/high-expansion foam and water-spray/sprinkler density checks —
  add as further criteria kinds when a job needs them.
