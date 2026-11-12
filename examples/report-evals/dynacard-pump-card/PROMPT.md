# Run it yourself: surface card to pump card

Article: https://www.aceengineer.com/newsletter/ (the article URL is set at publication).

For decision makers: this prompt lets your team, or your AI assistant, turn a surface dynamometer card into a pump card with public code, first on a public example well and then on a well of your own. It lists the well data the calculation needs and says what happens when an item is missing.

For practitioners: paste the block below into an AI assistant that can run git and Python and use the internet. It needs git, Python 3.11 or later and the uv package tool. The key results are in `results.json` next to this file. `eval.py` grades them.

## Prompt (copy all of it)

```text
Task: turn a surface dynamometer card of a sucker-rod pumped well into a pump
(downhole) card with the wave equation, and read pump stroke, fluid load and
fillage from it. Do it first on the public example well, then on my card.

Inputs (all public, no account needed):
- Code and data: https://github.com/vamseeachanta/digitalmodel at commit
  484b674ade72817359dc135344dfd1f2411b488b, MIT licence.
- The public example well:
  tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json in that
  repository. It is an anonymised field well: a measured surface card, a
  two-taper steel rod string, the pump depth and plunger diameter, the pumping
  speed, and the pump card stored by the source system's own finite-difference
  calculation, for comparison.
- Public reference card shapes: the ten load curves of the diagnostic plate at
  https://doi.org/10.17632/hnfwm9mjs6.1, CC0 1.0, retrieved 2026-10-11, kept in the
  repository under tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate.

Steps:
1. git clone https://github.com/vamseeachanta/digitalmodel
   cd digitalmodel
   git fetch origin 484b674ade72817359dc135344dfd1f2411b488b || git fetch origin refs/pull/3969/head
   git checkout 484b674ade72817359dc135344dfd1f2411b488b
   uv sync --extra test
2. Run the public example and keep its output and figure:
   PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/analysis.py --out analysis.json --figure figure.png
3. Rebuild every key result and grade it against results.json:
   PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py
4. For my card: ask me for the items listed under "Ask me before you go on if".
   Write them into a copy of reader-card-template.json and run analysis.py with
   that file as its argument. Keep every ASSUMED input, the sensitivity table
   and the HYPOTHESIS label on every shape output in what you report.

What the shape label is worth: the shape-only model labelled 5 of 8 public
plate shapes with a literal label, and 6 of 10 when the two assumed label
mappings are included. Its accuracy on field cards is NOT_EVALUATED, because no
independently labelled field card set is available. Report the label as a
hypothesis to check at the well, never as a diagnosis.

Expected public-case key results:
| Quantity | Value | Unit |
| --- | --- | --- |
| Surface stroke | 53.99 | in |
| Peak polished-rod load | 12240 | lbf |
| Minimum polished-rod load | 7296 | lbf |
| Pump stroke, gross plunger travel | 39.58 | in |
| Pump/surface stroke ratio | 0.733 | ratio |
| Maximum pump load | 4660 | lbf |
| Minimum pump load | 83 | lbf |
| Fluid load, mean phase difference | 3309 | lbf |
| Pump fillage | 60.08 | % |
| Effective plunger travel | 23.78 | in |
| Fillage-adjusted displacement | 40.03 | bbl/day |
| Gross displacement | 66.62 | bbl/day |
| Pump-card work per stroke | 7904.9 | ft-lbf |

Basis: 24 h/day and 100 % volumetric efficiency.

Simple reader layout (see reader-card-template.json for the complete public card):
- surface_card.position_in and surface_card.load_lbf: equal-length numeric arrays,
  inches and lbf, over one stroke in acquisition order; uniform time spacing is required.
- strokes_per_minute: positive pumping speed in SPM.
- rod_string: nonempty array, surface first, of diameter_in and length_ft.
  material is steel; omitted material is ASSUMED steel. Optional section fields:
  modulus_psi, density_lb_ft3, weight_lb_ft (including couplings), wave_speed_ft_s,
  damping_rate_s_inv (the prescribed frequency-domain rate in inverse seconds).
  Omitted properties use package defaults recorded as ASSUMED. Other materials
  require a separate input interface and are rejected by this reader.
- pump.diameter_in and pump.depth_ft: positive plunger diameter and setting depth.
- fluid_density_lb_ft3 and tubing_pressure_psi: optional nonnegative numbers;
  omitted values and their effects are recorded as ASSUMED.
- tubing_anchored: optional true; unanchored tubing motion is not modelled and
  false is rejected rather than silently interpreted as an anchored installation.
- survey: optional object with measured_depth_ft, inclination_deg and azimuth_deg
  arrays, paired from surface through the rod string, with increasing measured depth.
  The default solver retains the survey; the frequency-domain comparison remains vertical.
Unknown keys are rejected. A repeated closing position/load pair is excluded from
frequency-domain harmonic counting. The reader reports an error when the usable
sample count is insufficient; source timing cannot be established from loop shape.

Ask me before you go on if:
- The surface card: position and load samples over one full stroke, with units: The wave equation is solved from the measured polished-rod position and load. Without it there is no calculation; stop. The samples are ASSUMED evenly spaced in time. Supply: Export the card from the pump-off controller or dynamometer as position (in) and load (lbf) columns, in acquisition order.
- The rod string as run: diameter, length and material of each taper, surface first: Rod stretch, wave travel time and buoyant rod weight come from the taper geometry. Without diameter or length there is no calculation; stop. A missing material is ASSUMED steel. Rod length: On the public example well a 1 % error in it changes: pump stroke −0.35, fluid load 0.87 (stepped), fillage 0.46, maximum pump load −1.64, displacement 0.11 (% per %). Rod diameter: On the public example well a 1 % error in it changes: pump stroke 0.58, fluid load −0.17 (stepped), fillage −0.94, maximum pump load −3.14, displacement −0.36 (% per %). Supply: Take it from the rod tally or the last workover ticket; a remembered string is weaker evidence than the tally.
- Pump setting depth and plunger diameter: The pump depth closes the rod string; the plunger diameter converts plunger travel to volume. Without either there is no calculation; stop. Plunger diameter: On the public example well a 1 % error in it changes: pump stroke < 0.01 in magnitude, fluid load < 0.01 in magnitude, fillage < 0.01 in magnitude, maximum pump load < 0.01 in magnitude, displacement 2.00 (% per %). Supply: Take both from the pump or completion record.
- Pumping speed in strokes per minute: Speed sets the time of one stroke and so the dynamic part of the rod load. Without it there is no calculation; stop. On the public example well a 1 % error in it changes: pump stroke −0.06, fluid load 0.93 (stepped), fillage < 0.01 in magnitude, maximum pump load 0.04, displacement 0.93 (% per %). Supply: Read it from the controller, or time ten strokes.
- Load-cell calibration date and method: Every pump load is the surface load less the buoyant rod weight, so a load-cell error passes straight into it. If not supplied, the surface loads are ASSUMED correct as recorded. On the public example well a 1 % error in it changes: pump stroke −0.29, fluid load 0.94 (stepped), fillage 0.47, maximum pump load 2.57, displacement 0.18 (% per %). The effect of a zero offset is NOT_EVALUATED. Supply: Give the date of the last calibration and whether the cell was zeroed unloaded.
- Fluid density, or water cut with oil and water densities: Fluid density sets the buoyancy of the rod string. If not supplied, fluid density is ASSUMED 62.4 lb/ft3 (fresh water). On the public example well a 1 % error in it changes: pump stroke 0.00, fluid load 0.00, fillage 0.00, maximum pump load 0.22, displacement 0.00 (% per %). Supply: Give the produced-fluid density or the water cut and the two phase densities.
- Whether the tubing is anchored: Unanchored tubing stretches with the fluid load and shortens the plunger travel relative to the barrel. If not supplied, anchored tubing is ASSUMED; pump stroke, fillage and displacement rest on it. Unanchored tubing is not modelled and its effect is NOT_EVALUATED. Supply: State anchored or unanchored, with the anchor depth.
- Deviation survey: In a deviated well, rod weight and friction act along the well path. If not supplied, a vertical well is ASSUMED. The frequency-domain cross-check is always vertical. The effect of deviation is NOT_EVALUATED without a survey. Supply: Give measured depth, inclination and azimuth from surface to the pump.
- Tubing pressure: It is used for pump intake pressure, which this report does not give. If not supplied, 100 psi is ASSUMED; none of the reported card results uses it. Supply: Read it at the wellhead.

Use these public-case sensitivity factors to explain which results missing or
uncertain inputs weaken. Recalculate the factors on my own card; a local factor
for the public example well is not an uncertainty bound for a different card.
| Input | Pump stroke | Fluid load | Fillage | Maximum pump load | Displacement |
| --- | --- | --- | --- | --- | --- |
| Rod length and pump depth | −0.35 | 0.87* | 0.46 | −1.64 | 0.11 |
| Rod diameter | 0.58 | −0.17* | −0.94 | −3.14 | −0.36 |
| Rod modulus | 0.32 | −0.27* | −0.47 | 0.03 | −0.14 |
| Pumping speed | −0.06 | 0.93* | < 0.01 | 0.04 | 0.93 |
| Surface load scale | −0.29 | 0.94* | 0.47 | 2.57 | 0.18 |
| Surface position scale | 1.29 | 0.06* | −0.47 | 0.05 | 0.82 |
| Fluid density | 0.00 | 0.00 | 0.00 | 0.22 | 0.00 |
| Pump diameter | < 0.01 | < 0.01 | < 0.01 | < 0.01 | 2.00 |

Sensitivity factors on the public example well: % change in the result per 1 % change in the input, from the ±1 % steps. * stepped response: the ±1 % and ±5 % factors differ by more than 0.10, or the result moves the same way for a decrease and an increase; use the full table in the report. < 0.01 marks a nonzero magnitude below the resolution.

You cannot:
- Name a pump fault with a stated field accuracy: every shape label is a HYPOTHESIS; field accuracy is NOT_EVALUATED because independent labelled field cards are unavailable.
- Analyse a deviated well as vertical without saying so; the frequency-domain cross-check is vertical.
- Correct a mis-calibrated load cell without independent calibration evidence.
- Give production from a card alone: operating runtime and plunger/valve slip are needed.
- Establish a published-example check without an openly readable worked example with tabulated card values; it is NOT_EVALUATED.

Output: report stroke, loads, fillage, effective travel, displacement basis and
work per stroke; the second-solver cross-check; the complete assumptions
register; shape hypotheses with probabilities and recorded plate counts; input
sensitivity; and one physical-card/reference-shape figure. For the public-case
arithmetic comparison, write my-results.json using the numeric result ids in
results.json, in the form {"results": [{"id": "pump_stroke_in", "value": 39.58}]}.
Include every graded result in that file, then run:
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py --compare my-results.json
The comparison checks reproduction of the public example, not the validity of
my own card. An ASSUMED item is never graded PASS. A shape label remains
HYPOTHESIS, and unestablished matters remain NOT_EVALUATED with missing evidence.
```

## Check your answer

```text
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py --validate-only
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py --compare my-results.json
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py
```

The comparison grades a reader's public-case reconstruction. The rebuild reruns the package calculations from the retained public card. PASS refers to numerical reconstruction within rounding tolerances, conditional on the recorded assumptions.

## Adapt it to your project

Start with `reader-card-template.json`, replace the surface samples and the as-run equipment and operating inputs, then run `analysis.py` on the new file. The template's rod weights and material properties belong to the public example well; replace them with the as-run values or omit unknown optional properties and retain the resulting ASSUMED entries. Actual production needs runtime and slip evidence in addition to a card.
