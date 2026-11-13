"""Build the calculation note and reusable prompt from one analysis dictionary."""
from pathlib import Path
import argparse

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
REPORT = ROOT / 'docs/domains/artificial_lift/dynacard-pump-card-report.md'
from report_contract import METRICS as DISPLAY_METRICS, TITLE, REPO, PULL_REQUEST

METRICS = tuple((key, *spec) for key, spec in DISPLAY_METRICS.items())
INPUT_LABELS = {
    'rod_length_depth': 'Rod length and pump depth', 'rod_diameter': 'Rod diameter',
    'rod_modulus': 'Rod modulus', 'pumping_speed': 'Pumping speed',
    'surface_load_scale': 'Surface load (load-cell gain)', 'surface_position_scale': 'Surface position (stroke scale)',
    'fluid_density': 'Fluid density', 'pump_diameter': 'Plunger diameter',
}
OUTPUT_LABELS = {'pump_stroke_in': 'Pump stroke', 'fluid_load_lbf': 'Fluid load',
                 'fillage_pct': 'Fillage', 'maximum_pump_load_lbf': 'Maximum pump load',
                 'pump_displacement_bbl_day': 'Displacement'}


def number(value, decimals=2):
    if value is None:
        return 'NOT_EVALUATED'
    if abs(value) < .5 * 10 ** -decimals:
        value = 0.0
    return f'{value:.{decimals}f}'.replace('-', '−')


def small(value, decimals=2):
    """A nonzero value that rounds to zero is printed as below the resolution."""
    if value != 0 and abs(value) < .5 * 10 ** -decimals:
        return f'< {10 ** -decimals:.{decimals}f}'
    return number(value, decimals)


def table(headers, rows, caption):
    def line(row):
        return '| ' + ' | '.join(str(v).replace('|', '/') for v in row) + ' |'
    return '\n'.join([line(headers), line(['---'] * len(headers)),
                      *(line(row) for row in rows), '', caption, ''])


def key_table(data, caption):
    rows = [(label, number(data['key_results'][key], dp), unit)
            for key, label, unit, dp in METRICS]
    return table(('Quantity', 'Value', 'Unit'), rows, caption)


def value_text(value):
    if isinstance(value, float):
        return number(value, 3)
    if isinstance(value, dict):
        return '; '.join(k.replace('_', ' ') + ': ' + value_text(v) for k, v in value.items())
    if isinstance(value, list):
        return '; '.join(value_text(v) for v in value)
    return str(value)


def design_tables(data):
    inputs = data['inputs']
    rows = [('Pumping speed', number(inputs['strokes_per_minute']), 'SPM'),
            ('Pump depth', number(inputs['pump']['depth_ft']), 'ft'),
            ('Plunger diameter', number(inputs['pump']['diameter_in'], 3), 'in'),
            ('Fluid density used', number(inputs['fluid_density_lb_ft3'], 3), 'lb/ft³'),
            ('Tubing pressure', number(inputs['tubing_pressure_psi']), 'psi')]
    rows.extend([('Total supplied rod length', number(data['geometry_basis']['rod_length_ft']), 'ft'),
                 ('Pump depth minus supplied rod length', number(data['geometry_basis']['unrepresented_length_ft']), 'ft')])
    for index, rod in enumerate(inputs['rod_string'], start=1):
        rows.extend([(f'Rod taper {index}, diameter', number(rod['diameter_in'], 3), 'in'),
                     (f'Rod taper {index}, length', number(rod['length_ft']), 'ft'),
                     (f'Rod taper {index}, modulus', number(rod['modulus_psi'], 0), 'psi'),
                     (f'Rod taper {index}, weight per length', number(rod['weight_lb_ft'], 3), 'lb/ft'),
                     (f'Rod taper {index}, prescribed wave speed', number(rod['wave_speed_ft_s'], 0), 'ft/s')])
    design = table(('Input', 'Value', 'Unit'), rows,
                   'Table 1. Public example well design and operating inputs, surface-first taper order.')
    rows = [(row['id'].replace('_', ' ').capitalize(), row['status'],
             value_text(row['value']), row['why']) for row in data['assumptions']]
    return design + '\n' + table(('Assumption', 'Status', 'Value or basis', 'Reason'), rows,
        'Table 2. Assumptions retained by the calculation; ASSUMED is not a passing check.')


def solver_table(data):
    cross = data['solver_cross_check']
    return table(('Actual card', 'Reference card', 'Load nRMSE (%)'), (
        ('Frequency domain', 'Default finite difference', number(cross['gibbs_default_load_nrmse_pct'], 3)),
        ('Default finite difference', 'Stored card, source finite difference', number(cross['default_reference_load_nrmse_pct'], 3)),
        ('Frequency domain', 'Stored card, source finite difference', number(cross['gibbs_reference_load_nrmse_pct'], 3))),
        'Table 4. Card comparisons aligned by traversal and sample origin; each reference load range normalises its error.')


def shape_section(data):
    shape = data['shape_hypothesis']
    score = shape['plate_score']
    literal, total = score['literal'], score['including_assumed']
    text = (f"The shape-only model labelled {literal['correct']} of {literal['count']} literal public plate shapes "
            f"and {total['correct']} of {total['count']} including the assumed crosswalks. "
            'These are recorded plate counts, with one exemplar per shape.\n\n')
    rows = [(index, row['label'], number(row['probability'] * 100), 'HYPOTHESIS')
            for index, row in enumerate(shape['top_three'], start=1)]
    text += table(('Rank', 'Shape-only label', 'Model probability (%)', 'Status'), rows,
                  'Table 5. Shape-only synthetic-model probabilities; classification is a hypothesis.')
    rows = [(row['rank'], row['label'], number(row['distance'], 4), 'HYPOTHESIS')
            for row in shape['nearest_three']]
    return text + '\n' + table(('Rank', 'Nearest public plate shape', 'Loop distance (dimensionless)', 'Status'),
        rows, 'Table 6. Minimum mean distance after independent axis normalisation, resampling and traversal alignment.')


def sensitivity_section(data):
    steps = data['settings']['perturbation_steps_pct']
    rows = []
    for row in data['sensitivity']:
        by_step = {item['input_change_pct']: item['output_change_pct'] for item in row['steps']}
        for output, label in OUTPUT_LABELS.items():
            rows.append((INPUT_LABELS[row['input']], label,
                         *(small(by_step[step][output]) for step in steps),
                         small(row['factor_1pct'][output]), small(row['factor_5pct'][output]),
                         'stepped' if output in row['stepped_outputs'] else 'smooth'))
    headers = ('Input', 'Output', *(f'{number(s, 0)} %' for s in steps),
               'Factor at ±1 %', 'Factor at ±5 %', 'Response')
    return ('Each input family is changed independently about the public example well. '
            'Rod length and pump depth are scaled together; all taper diameters are scaled together '
            'while weight per length is scaled with area to preserve effective density. '
            'Each entry is the output change as a percentage of its base value. '
            'The sensitivity factor is the difference of the positive and negative output changes '
            'divided by the difference of the input percentages. A zero is printed as 0.00; '
            'the wider central difference exposes nonlinearity. A response is marked stepped '
            'where the two factors differ by more than 0.10, or where the result moves the same way, '
            'by more than 0.01 %, for a decrease and an increase of the input. Such a result moves in jumps, '
            'because the corner points that define it shift between samples; its factor '
            'is not a reliable slope.\n\n' +
            table(headers, rows, 'Table A1. Output changes (%) and central sensitivity factors (% output per % input).'))


def source_table(data):
    rows = [(s['what'], s['source'], s['licence'], s['date'], f"[Source]({s['url']})")
            for s in data['sources']]
    return table(('Data', 'Source', 'Licence', 'Version or date', 'Link'), rows,
                 'Table B1. Retained data sources; input and source digests are recorded by the analysis and eval.')


def methodology(data):
    harmonics = data['settings']['harmonic_count']
    return f'''Surface displacement and tensile load provide the boundary conditions for the damped axial rod wave equation. The default finite-difference adapter marches the displacement field through the tapered rod string and reconstructs pump load from strain, following Everitt and Jennings (1992), SPE-18189-PA. The coupled frequency-domain transfer is a separate numerical method: it propagates both displacement and force through each ordered taper as a frequency-domain solution of the rod model of Gibbs (1963), [SPE-588-PA](https://doi.org/10.2118/588-PA), with {harmonics} harmonics.

The finite-difference solver receives a repeated closing position/load pair; the frequency-domain solver receives unique phases. Both computed cards use unique phases for metrics and comparison. An exact closing pair in the input is removed before either representation is constructed, so open and closed representations give identical results. The stored comparison card has a different unique-phase count and is interpolated periodically onto the measured card's uniform phase grid before comparison.

The [package calculations](../../../src/digitalmodel/marine_ops/artificial_lift/dynacard/calculations.py) define gross plunger travel as the position range, effective travel as the bottom-right corner position relative to the minimum, and fillage as effective travel divided by gross travel, expressed as a percentage. Fluid load is the mean upstroke load minus the mean downstroke load between the detected corner phases. The figure marks these mean levels rather than substituting the extreme load range. The [card-area calculation](../../../src/digitalmodel/marine_ops/artificial_lift/dynacard/power_consumption.py) closes the polygon and converts its enclosed area to work per stroke.

The package displacement calculation uses plunger area, travel and pumping speed. The fillage-adjusted and gross values below use {data['settings']['runtime_hours']} h/day and {data['settings']['volumetric_efficiency_pct']} % volumetric efficiency as an explicit calculation basis. Load nRMSE uses [compare_cards](../../../src/digitalmodel/marine_ops/artificial_lift/dynacard/comparison.py). Numerical reproduction checks depend on the ASSUMED inputs in Table 2.
'''


def report_introduction(data):
    key = data['key_results']
    limitations = '\n'.join('- ' + text for text in data['cannot_do'] if 'published-example' not in text)
    geometry = data['geometry_basis']
    return f'''<!-- Generated by examples/report-evals/dynacard-pump-card/build_report.py. -->
# {TITLE}

## Summary

The public example well gives a pump stroke of {number(key['pump_stroke_in'])} in from a measured surface stroke of {number(key['surface_stroke_in'])} in, a fluid load of {number(key['fluid_load_lbf'], 0)} lbf and a card-derived fillage of {number(key['fillage_pct'])} %. Effective plunger travel is {number(key['effective_travel_in'])} in. At {data['settings']['runtime_hours']} h/day and {data['settings']['volumetric_efficiency_pct']} % volumetric efficiency, fillage-adjusted displacement is {number(key['pump_displacement_bbl_day'])} bbl/day.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-11-12 | Initial calculation note with public-card results, assumptions and sensitivity. |
| 1 | 2026-11-12 | Correct cycle endpoints, refresh derived results and declare metadata precision for reproducible comparison. |

## 1 Introduction

### 1.1 Background

A measured surface dynamometer card contains rod-string weight, elastic stretch and dynamic effects. The wave-equation conversion separates those effects to calculate displacement and load at the pump.

### 1.2 Scope of work

One public field card is converted by the default solver and, as a cross-check by a different numerical method, by a coupled frequency-domain solver. Stroke, fluid load, fillage, displacement, work per stroke and input sensitivity are calculated. Public shape references provide a separate hypothesis ranking.

### 1.3 Limitations

The analysis cannot:

{limitations}

{data['validation']['statement']}

The wave solutions terminate at the supplied rod-string length of {number(geometry['rod_length_ft'], 0)} ft; the stated pump depth is {number(geometry['pump_depth_ft'], 0)} ft, leaving {number(geometry['unrepresented_length_ft'], 0)} ft unrepresented by the supplied taper inventory. Identification of the rod-string endpoint card as the pump card is ASSUMED; qualification at the stated pump depth requires a reconciled as-run rod and completion tally.

The well record of the public example well carries two notes that bear on the results. Its own system reports that corner detection failed on this card, so the fillage and fluid load here rest on this package's corner detection alone. Its last well test gives a liquid rate below the displacement computed here at the stated basis; displacement from a card is not production.

Model probabilities describe synthetic class separation and are not calibrated field probabilities. The public plate is dependent shape evidence because it also constrains the synthetic generators used for training.

The assumed damping, anchoring, time sampling, material properties and load calibration are registered in Table 2. Reproduction within rounding tolerance establishes arithmetic consistency for this governing case; it does not establish those assumptions.

'''


def report_calculations(data):
    return f'''## 2 Design data

{design_tables(data)}
## 3 Methodology

{methodology(data)}
## 4 Results

{key_table(data, 'Table 3. Public example well results from the default finite-difference solver, conditional on Table 2.')}
{solver_table(data)}
The measured surface card and the two computed pump cards are shown below with the pump card stored in the well record. The stored card comes from the source system's own finite-difference calculation, with damping coefficients close to those of the default solver, so its agreement with the default solver is a regression check; only the frequency-domain solver is a different method. The reference shapes are displayed independently on normalised axes.

![Surface card and computed pump card, public example well](../../../examples/report-evals/dynacard-pump-card/figure-1-surface-and-pump-card.png)

Figure 1. Measured and computed physical cards with annotated travel and mean phase fluid load; nearest public shapes are hypotheses.

{shape_section(data)}
'''


def report_appendices(data, grade):
    return f'''## 5 References

- Gibbs (1963), *Predicting the Behavior of Sucker-Rod Pumping Systems*, SPE-588-PA, [doi:10.2118/588-PA](https://doi.org/10.2118/588-PA).
- Everitt and Jennings (1992), *An Improved Finite-Difference Calculation of Downhole Dynamometer Cards for Sucker-Rod Pumps*, [SPE-18189-PA](https://doi.org/10.2118/18189-PA).
- Package implementation: [coupled transfer equations and checks](../../../src/digitalmodel/marine_ops/artificial_lift/dynacard/gibbs-transfer.md), [finite-difference adapter](../../../src/digitalmodel/marine_ops/artificial_lift/dynacard/everitt_jennings/adapter.py), and the calculation modules linked in Section 3.

## Appendix A Sensitivity analysis

{sensitivity_section(data)}
## Appendix B Reusable prompt

The [reusable prompt](../../../examples/report-evals/dynacard-pump-card/PROMPT.md) specifies the public-case rebuild, the reader input layout, missing-data questions and the comparison output. The [reader template](../../../examples/report-evals/dynacard-pump-card/reader-card-template.json) contains the same public example well in the simple layout.

{source_table(data)}
## Appendix C Checks

```text
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py --validate-only
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py
```

The [last-run record](../../../examples/report-evals/dynacard-pump-card/last-run.json) grades **{grade}** against the printed results and their half-last-digit tolerances. This grade checks numerical reconstruction; ASSUMED inputs and HYPOTHESIS labels retain their separate statuses.

The manifest rounds numeric calculation metadata to 12 decimal places to bound insignificant variation between numeric libraries. Identifiers, source digests and labels remain exact; graded result values retain their separately declared display tolerances.

The recorded pin predates the endpoint correction. Corrected results are evaluated in candidate mode at the pull request head; pinned reproduction requires a new pin after the corrected revision is published. The legacy file named last-run-pinned.json is refreshed as candidate evidence and records this limitation explicitly.
'''


def build_report(data: dict, grade='PASS') -> str:
    return report_introduction(data) + report_calculations(data) + report_appendices(data, grade)


def reader_layout():
    return '''Simple reader layout (see reader-card-template.json for the complete public card):
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
'''


def sensitivity_prompt(data):
    rows = []
    for row in data['sensitivity']:
        rows.append((INPUT_LABELS[row['input']],
                     *(small(row['factor_1pct'][key])
                       + ('*' if key in row['stepped_outputs'] else '')
                       for key in OUTPUT_LABELS)))
    return table(('Input', 'Pump stroke', 'Fluid load', 'Fillage',
                  'Maximum pump load', 'Displacement'), rows,
                 'Sensitivity factors on the public example well: % change in the result per '
                 '1 % change in the input, from the ±1 % steps. * stepped response: the ±1 % '
                 'and ±5 % factors differ by more than 0.10, or the result moves the same way, by more than 0.01 %, for a decrease and an increase; use the full table in the report. < 0.01 marks a nonzero magnitude below the resolution.')


def prompt_start(data):
    score = data['shape_hypothesis']['plate_score']
    literal, total = score['literal'], score['including_assumed']
    pin = data['commit']
    plate = next(row for row in data['sources'] if row['what'] == 'Public reference shapes')
    return f'''# Run it yourself: surface card to pump card

Article: https://www.aceengineer.com/newsletter/ (the article URL is set at publication).

For decision makers: this prompt lets your team, or your AI assistant, turn a surface dynamometer card into a pump card with public code, first on a public example well and then on a well of your own. It lists the well data the calculation needs and says what happens when an item is missing.

For practitioners: paste the block below into an AI assistant that can run git and Python and use the internet. It needs git, Python 3.11 or later and the uv package tool. The key results are in `results.json` next to this file. `eval.py` grades them.

## Prompt (copy all of it)

```text
Task: turn a surface dynamometer card of a sucker-rod pumped well into a pump
(downhole) card with the wave equation, and read pump stroke, fluid load and
fillage from it. Do it first on the public example well, then on my card.

Inputs (all public, no account needed):
- Code and data: {REPO} at commit
  {pin}, MIT licence.
- The public example well:
  tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json in that
  repository. It is an anonymised field well: a measured surface card, a
  two-taper steel rod string, the pump depth and plunger diameter, the pumping
  speed, and the pump card stored by the source system's own finite-difference
  calculation, for comparison.
- Public reference card shapes: the ten load curves of the diagnostic plate at
  {plate['url']}, {plate['licence']}, retrieved {plate['date']}, kept in the
  repository under tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate.

Steps:
1. git clone {REPO}
   cd digitalmodel
   git fetch origin refs/pull/{PULL_REQUEST}/head
   git checkout FETCH_HEAD
   (The recorded pin predates the endpoint correction. These corrected values
   require the pull request head in candidate mode until a new pin is published.)
   uv sync --extra test
2. Run the public example and keep its output and figure:
   PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/analysis.py --out analysis.json --figure figure.png
3. Rebuild every key result and grade it against results.json:
   PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py
4. For my card: ask me for the items listed under "Ask me before you go on if".
   Write them into my-card.json, a copy of
   examples/report-evals/dynacard-pump-card/reader-card-template.json, with
   positions in inches and loads in lbf (convert a metric card first), and run:
   PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/analysis.py my-card.json --out my-analysis.json --figure my-figure.png
   Keep every ASSUMED input, the sensitivity table and the HYPOTHESIS label on
   every shape output in what you report.

What the shape label is worth: the shape-only model labelled {literal['correct']} of {literal['count']} public
plate shapes with a literal label, and {total['correct']} of {total['count']} when the two assumed label
mappings are included. Its accuracy on field cards is NOT_EVALUATED, because no
independently labelled field card set is available. Report the label as a
hypothesis to check at the well, never as a diagnosis.

'''


def prompt_results(data):
    questions = '\n'.join('- ' + row['what'] + ': ' + row['why'] + ' '
                          + row['effect'] + ' Supply: ' + row['how_to_supply']
                          for row in data['missing_data'])
    cannot = '\n'.join('- ' + text for text in data['cannot_do'])
    basis = (f"Basis, ASSUMED: {data['settings']['runtime_hours']} h/day and "
             f"{data['settings']['volumetric_efficiency_pct']} % volumetric efficiency.")
    return f'''Expected public-case key results:
{key_table(data, basis)}
{reader_layout()}
Ask me before you go on if:
{questions}

Use these public-case sensitivity factors to explain which results missing or
uncertain inputs weaken. Recalculate the factors on my own card; a local factor
for the public example well is not an uncertainty bound for a different card.
{sensitivity_prompt(data)}
You cannot:
{cannot}

Output: report stroke, loads, fillage, effective travel, displacement basis and
work per stroke; the second-solver cross-check; the complete assumptions
register; shape hypotheses with model scores (not field probabilities) and
recorded plate counts; input
sensitivity; and one figure of the surface and pump cards beside the reference
shapes. Only if you computed the public-case results without running analysis.py:
for the arithmetic comparison, write my-results.json using the numeric result ids in
results.json, in the form {{"results": [{{"id": "pump_stroke_in", "value": {number(data['key_results']['pump_stroke_in'])}}}]}}.
Include every graded result in that file, then run:
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py --compare my-results.json
The comparison checks reproduction of the public example, not the validity of
my own card. An ASSUMED item is never graded PASS. A shape label remains
HYPOTHESIS, and unestablished matters remain NOT_EVALUATED with missing evidence.
```

'''


def prompt_closing():
    return '''## Check your answer

```text
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py --validate-only
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py --compare my-results.json
PYTHONPATH=src uv run --no-sync python examples/report-evals/dynacard-pump-card/eval.py
```

The comparison grades a reader's public-case reconstruction. The rebuild reruns the package calculations from the retained public card. PASS refers to numerical reconstruction within rounding tolerances, conditional on the recorded assumptions.

## Adapt it to your project

Start with `reader-card-template.json`, replace the surface samples and the as-run equipment and operating inputs, then run `analysis.py` on the new file. The template's rod weights and material properties belong to the public example well; replace them with the as-run values or omit unknown optional properties and retain the resulting ASSUMED entries. Actual production needs runtime and slip evidence in addition to a card.
'''


def build_prompt(data: dict) -> str:
    return prompt_start(data) + prompt_results(data) + prompt_closing()


def main():
    from analysis import run
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report', type=Path, default=REPORT)
    parser.add_argument('--prompt', type=Path, default=HERE / 'PROMPT.md')
    args = parser.parse_args()
    data = run()
    args.report.write_text(build_report(data), encoding='utf-8')
    args.prompt.write_text(build_prompt(data), encoding='utf-8')


if __name__ == '__main__':
    main()
