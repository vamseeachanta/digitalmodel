# Alternative-fuel (LH2) ship-design sizing toolkit

## What this is

`digitalmodel.alt_fuel_ship_sizing` and the routed workflow basename
`alt_fuel_ship_sizing` are a parametric, YAML-config-driven sizing toolkit
for liquid-hydrogen (LH2) / wind-assisted ship concepts. Four calculators
compose:

1. **LH2 fuel-chain sizing** — required tank volume from route endurance +
   powering demand + fuel-cell efficiency chain; boil-off gas (BOG) rate
   from tank geometry / insulation heat leak; net vs gross tank volume
   with ullage and design margins.
2. **Wind-assist integration** — a user-supplied thrust matrix (net thrust
   vs true wind speed/angle; vendor polar or model-test data) applied over
   a route wind rose to reduce the required propulsion power; outputs the
   expected power saving and fuel reduction.
3. **Endurance / fuel-cost trade-study generator** — sweeps design
   variables (service speed, wind-assist on/off) and emits a comparison
   table (CSV) that drops straight into the `report_pack` workflow's
   `results.tables` input.
4. **Concept-stage GT/NT tonnage estimate** — International Convention on
   Tonnage Measurement of Ships, 1969 (ITC 69), Annex I, Regulation 3
   (`GT = K1·V`, `K1 = 0.2 + 0.02·log10 V`) and Regulation 4
   (`NT = K2·Vc·(4d/3D)² + K3·(N1 + N2/10)`) with all Regulation 4
   provisos (factor ≤ 1, cargo term ≥ 0.25 GT, N1+N2 < 13 → 0,
   NT ≥ 0.30 GT).

**Posture: concept-design screening only.** Public physics and published
data only — every physical constant is cited in
`src/digitalmodel/alt_fuel_ship_sizing/constants.py` (liquid parahydrogen
at NBP: density ~70.85 kg/m3, latent heat ~446 kJ/kg, per NBS Monograph
168 / NIST Chemistry WebBook; hydrogen LHV 119.96 MJ/kg per the DOE
Hydrogen Analysis Resource Center). The module carries no project
calibration; results are uncalibrated screening estimates, not design
values.

## Method

### Fuel chain (LHV basis throughout)

```
electrical demand = shaft power / eta_drivetrain + hotel load
fuel LHV power    = electrical demand / eta_fuel_cell
fuel mass flow    = fuel LHV power / LHV
voyage fuel       = mass flow x endurance x (1 + margin)
net tank volume   = required fuel mass / liquid density
gross tank volume = net / (1 - ullage fraction)
```

Shaft power at the design speed is an **input** — bare-hull resistance and
propulsion prediction, weight groups, and stability/damage load-case prep
are separate capability lanes whose outputs feed this workflow; they are
not reimplemented here.

### Boil-off (BOG)

Concept-stage tank: plain cylinder (flat ends) of gross volume `V` and
slenderness `L/D` (`V = πD²L/4`, `A = πDL + πD²/2`). Heat leak
`Q = q·A` from a specified insulation heat flux (W/m², the usual
characterisation of a vacuum-insulated LH2 tank) or `U·(T_amb − T_tank)`.
BOG mass rate `= Q / h_vap`; the conventional boil-off rate (%/day) is
referenced to the full-load liquid mass. `bog_handling: consumed` routes
BOG to the fuel-cell plant (normal LH2 arrangement; flags if generation
ever exceeds the average draw); `bog_handling: lost` adds the vented mass
to the required fuel and converges the tank volume by fixed-point
iteration.

### Wind assist

Per wind-rose bin, the equivalent shaft-power saving of a thrust `T` along
track is `ΔP = T·V_ship / η_D` (towrope power divided by the
quasi-propulsive coefficient of the propulsion chain it displaces), capped
at the required shaft power. Bilinear interpolation inside the thrust
matrix; wind angles fold port/starboard into [0, 180°]; above the top
tabulated wind speed the thrust clamps (device depowered), below the
bottom row it scales with `(tws/tws_min)²` (dynamic pressure), reaching
zero in a calm. Probability mass not covered by the rose is treated as
calm.

### Trade study

Speed × wind-assist sweep through the full fuel chain. Shaft power vs
speed comes from a user `speed_power_curve` (piecewise-linear between
calm-water prediction points) or, absent one, the classic cube law
`P = P_ref·(V/V_ref)³` — acceptable only for small speed deltas at
screening stage. Optional fuel price adds a voyage fuel-cost column.

## Validation

- All four calculators carry hand-verified round-number expected values in
  the test docstrings (`tests/naval_architecture/test_alt_fuel_ship_sizing.py`):
  the synthetic ~40k DWT bulker-like fixture (4000 nm at 12 kn, 4750 kW
  shaft + 500 kW hotel, 50 % fuel cell × 95 % drivetrain, LHV 120 MJ/kg
  round-number override) gives exactly 330 kg/h, 121 000 kg voyage fuel
  and 1 856.3 m³ gross tank; the resulting BOR of ~0.25 %/day sits in the
  published range for vacuum-insulated LH2 tanks of that size.
- ITC 69 tonnage: exact-arithmetic cases exercise every Regulation 4
  proviso (both floors, the unity cap on `(4d/3D)²`, and the <13-passenger
  rule).
- The fixture YAML
  (`tests/naval_architecture/fixtures/alt_fuel_ship_sizing_bulker40k.yml`)
  runs end-to-end through the router and the CSV outputs.

**Gap (pending, private):** calibration against real LH2/wind-assist hulls
(tank-sizing / BOG / electrical / fuel-cost workbooks and vendor thrust
matrices) is recorded privately once the source-workbook extraction lane
delivers; a NavCad cross-check of the powering inputs rides the resistance
lane. Until then treat all outputs as uncalibrated screening numbers.

## Usage

```yaml
basename: alt_fuel_ship_sizing
alt_fuel_ship_sizing:
  route: {distance_nm: 4000.0, service_speed_kn: 12.0}
  powering:
    shaft_power_kw: 4750.0        # from the resistance/propulsion lane
    hotel_load_kw: 500.0
  efficiency: {fuel_cell_lhv: 0.50, electric_drivetrain: 0.95}
  fuel: {margin_fraction: 0.10}   # LH2 property defaults are cited in constants.py
  tank:
    ullage_fraction: 0.08
    length_to_diameter: 5.0
    insulation_heat_flux_w_per_m2: 1.5
    bog_handling: consumed        # consumed | lost
  wind_assist:                    # optional
    enabled: true
    propulsive_efficiency: 0.70
    thrust_matrix:
      tws_mps: [5.0, 10.0]
      twa_deg: [0.0, 90.0, 180.0]
      thrust_kn: [[0.0, 50.0, 20.0], [0.0, 100.0, 40.0]]
    wind_rose:
      - {tws_mps: 10.0, twa_deg: 90.0, probability: 0.5}
  trade_study: {speeds_kn: [10.0, 12.0, 14.0], wind_assist: [false, true]}
  tonnage:                        # optional, ITC 69 Annex I Regs 3-4
    total_enclosed_volume_m3: 60000.0
    cargo_volume_m3: 47000.0
    moulded_draught_m: 10.0
    moulded_depth_m: 15.0
  economics: {fuel_price_per_kg: 6.0}
  output_dir: results
```

Outputs (CSV under `output_dir`): `<stem>_fuel_chain_summary.csv`,
`<stem>_wind_assist_bins.csv`, `<stem>_trade_study.csv` (feeds
`report_pack`), `<stem>_tonnage.csv`.
