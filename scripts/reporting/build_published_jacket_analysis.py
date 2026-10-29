# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build only the public 84.1 m jacket analysis from the unchanged fixture."""

from __future__ import annotations

import argparse
import copy
import json
import math
import re
from xml.etree import ElementTree as ET
from pathlib import Path
from typing import Any

import yaml

from cp_review_document import SECTION_TITLES
from build_cp_review_portfolio import verify_pack, write_receipt
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.cathodic_protection.report_elevation import _depth_data
from digitalmodel.citations import Citation, validate_citation
from digitalmodel.reporting.engine import write_report
from digitalmodel.reporting.spec import FigureBlock, Section, TableBlock, TextBlock

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / 'tests/fixtures/cathodic_protection/workflow_inputs/published_jacket_84m.yml'
DESTINATION = ROOT / 'docs/domains/cathodic_protection/reports/published-jacket-84m'
STEM = 'cathodic-protection-analysis'
ASSUMPTION = 'assumed, based on previous experience'
CRITERION = ('Potential must be more negative than −0.800 V versus Ag/AgCl/seawater; '
             'stated assumption, not a value printed in the paper or established by a standard citation.')
STATUS = ('This report is a worked example of the published 84.1 m jacket, calculated by the workflow. '
          'The paper’s printed values are compared with the workflow results in Appendix A. '
          'Hand calculations check the three phase-current sums. Tool checks compare the saved currents '
          'and anode counts with a fresh calculation and check the report rebuild. '
          'The checkpoint table compares six printed quantities; it does not establish local protection. '
          'Temperature and resistivity sensitivity is not part of this report; it is a separate reviewed issue.')


def write_json(path: Path, payload: Any) -> None:
    """Write finite portable metadata as UTF-8 with LF line endings."""
    path.write_text(json.dumps(payload, indent=2, sort_keys=True, ensure_ascii=False,
                               allow_nan=False) + '\n', encoding='utf-8', newline='\n')


def validate_inputs(cfg: dict[str, Any]) -> None:
    """Reject incomplete decision inputs before any status is calculated."""
    def finite(value: Any) -> None:
        if isinstance(value, dict):
            for item in value.values():
                finite(item)
        elif isinstance(value, list):
            for item in value:
                finite(item)
        elif value is None or (isinstance(value, (float, int)) and not math.isfinite(value)):
            raise ValueError('NOT_EVALUATED: missing or non-finite input')

    required = {
        'design_data': ('design_life', 'edition', 'structure_type'),
        'environment': ('seawater_temperature_C', 'seawater_resistivity_ohm_m'),
        'anode': ('individual_anode_mass_kg', 'length_m', 'count', 'material', 'type'),
        'structure': ('zones', 'connected_current_allowances', 'verification_evidence', 'report_geometry'),
    }
    inputs = cfg.get('inputs', {})
    for group, fields in required.items():
        if not isinstance(inputs.get(group), dict) or any(key not in inputs[group] for key in fields):
            raise ValueError(f'NOT_EVALUATED: missing {group} input')
    geometry = inputs['structure']['report_geometry']
    try:
        _depth_data(geometry)
    except (KeyError, TypeError, ValueError, OverflowError) as error:
        raise ValueError('NOT_EVALUATED: invalid criterion, basis or monitoring input') from error
    if geometry['criterion_V'] != -0.8:
        raise ValueError('NOT_EVALUATED: this report requires its stated −0.800 V criterion')
    targets = cfg['inputs']['structure']['verification_evidence']['targets']
    required = {'initial_demand', 'mean_demand', 'final_demand', 'initial_output', 'final_output', 'count'}
    for target in targets:
        value = target.get('printed')
        if (not target.get('locator') or isinstance(value, bool) or
                not isinstance(value, (int, float)) or not math.isfinite(value) or value <= 0):
            raise ValueError('NOT_EVALUATED: invalid published checkpoint')
    if {target['quantity'] for target in targets} != required:
        raise ValueError('NOT_EVALUATED: incomplete published checkpoints')
    finite(cfg['inputs'])


def calculate(cfg: dict[str, Any] | None = None) -> dict[str, Any]:
    """Run the repository workflow with the original numerical inputs."""
    cfg = copy.deepcopy(cfg) if cfg is not None else yaml.safe_load(FIXTURE.read_text(encoding='utf-8'))
    validate_inputs(cfg)
    return run_cathodic_protection(cfg)


def numerical_record(cfg: dict[str, Any]) -> dict[str, Any]:
    """Retain full calculation precision independently of display precision."""
    result = cfg['results']
    ver = result['current_output_verification']
    return {
        'current_A': {phase: result['current_demand_A'][f'total_{phase}_A']
                      for phase in ('initial', 'mean', 'final')},
        'anode_counts': dict(zip(('installed', 'mass', 'initial', 'final', 'recommended'),
                                (ver[k] for k in ('anode_count', 'count_by_mass',
                                 'count_by_initial_current', 'count_by_final_current',
                                 'recommended_anode_count')))),
    }


def current_chart(cfg: dict[str, Any]) -> FigureBlock:
    """Plot phase demand against the capacity limit with native hover labels."""
    result = cfg['results']
    verification = result['current_output_verification']
    phases = ('initial', 'final')
    demands = [result['current_demand_A'][f'total_{phase}_A'] for phase in phases]
    capacity = [verification['anode_count'] * verification[f'anode_current_output_{phase}_A']
                for phase in phases]
    scale = 180 / max(*demands, *capacity)
    parts = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 620 300" '
             'role="img" aria-label="Installed independent estimate: phase current">',
             '<text x="20" y="25">Current (A); bars: demand; dashed line: available output limit</text>']
    points = []
    for i, (phase, demand, limit) in enumerate(zip(phases, demands, capacity)):
        x = 160 + 280 * i
        y = 230 - demand * scale
        parts += [f'<rect x="{x-35}" y="{y:.1f}" width="70" height="{demand*scale:.1f}" fill="#336d89">',
                  f'<title>Published jacket; {phase}; demand {demand:.1f} A; '
                  f'independent output limit {limit:.1f} A</title></rect>',
                  f'<text x="{x-45}" y="255">{phase.capitalize()}</text>',
                  f'<text x="{x-40}" y="{y-8:.1f}">{demand:.1f} A</text>']
        points.append(f'{x},{230-limit*scale:.1f}')
    parts += [f'<polyline points="{" ".join(points)}" fill="none" stroke="#b44031" '
              'stroke-width="3" stroke-dasharray="6 4"><title>Provided initial and final '
              'current must meet or exceed phase demand; independent cylinder estimate.</title></polyline>',
              '<text x="20" y="285">Hover over a bar for its demand, limit and units.</text></svg>']
    return FigureBlock(title='Installed current demand and independent output limit',
                       figure_id='installed-current', svg=''.join(parts),
                       caption='The initial demand exceeds the installed independent-estimate output limit. '
                               'The comparison uses the unchanged workflow resistance model [2]. '
                               'The adjacent table gives the separate printed-output basis [1].')


def build_document(cfg: dict[str, Any] | None = None):
    """Use the report adapter's numerical tables with public engineering prose."""
    cfg = calculate() if cfg is None else copy.deepcopy(cfg)
    validate_inputs(cfg)
    # These display-only fields carry no numerical input and are not exported verbatim.
    view = copy.deepcopy(cfg)
    geometry = view['inputs']['structure']['report_geometry']
    geometry['attribution'] = 'Published paper (2023) [1]'
    geometry['criterion_source'] = 'stated assumption; not printed in the paper'
    history = Section(key='revision-history', title='Revision history', blocks=[
        TableBlock(title='Revision history', columns=['Revision', 'Date', 'Change and reason'], rows=[
            ['00', '2026-10-22', 'Public worked example with explicit assumptions and reproducible results.']])])
    section_order = list(SECTION_TITLES)
    section_order.insert(1, history.title)
    reference_number = section_order.index('References') + 1
    locator = f'[1]; see Section {reference_number}.2'
    geometry['citation'] = locator
    spec = anode_design_report(view)
    tables = {b.title: b for s in spec.sections for b in s.blocks if isinstance(b, TableBlock)}
    figure = next(b for b in spec.figure_blocks() if b.svg)
    ET.register_namespace('', 'http://www.w3.org/2000/svg')
    diagram = ET.fromstring(figure.svg)
    for node in diagram.iter():
        if node.text:
            node.text = re.sub(r'-(?=\d)', '−', node.text)
    figure.svg = ET.tostring(diagram, encoding='unicode')
    figure.caption = ('Depth-true monitoring diagram from the published elevations and potentials [1]. '
                      'Member dimensions and anode distribution are not established. ' + CRITERION)
    for table in tables.values():
        table.source = 'Published inputs [1]; other inputs are ' + ASSUMPTION + '.'
        for row in table.rows:
            for i, value in enumerate(row):
                if value == 'based on previous project experience':
                    row[i] = ASSUMPTION
    measured = tables['Measured potentials at the stated time of measurement']
    measured.columns += ['Criterion and basis']
    measured.units += ['-']
    for row in measured.rows:
        row.append(CRITERION)
    checkpoints = tables['Published checkpoints and calculated differences']
    # Keep raw values in results.json; the report uses engineering display precision.
    for row in checkpoints.rows:
        decimals = 2 if 'output' in str(row[0]).lower() else 1
        for index in (1, 2):
            row[index] = (str(int(float(row[index]))) if row[0] == 'Anode count'
                          else f'{float(row[index]):.{decimals}f}')
    for row in tables['Design data'].rows:
        if row[0] == 'Anode net mass':
            row[1] = str(cfg['inputs']['anode']['individual_anode_mass_kg'])
        elif row[0] == 'Anode equivalent radius (fresh)':
            row[1] = f'{float(row[1]):.3f}'
    for row in tables['Independent estimate: anode mass and count'].rows:
        if row[0] == 'Individual anode net mass':
            row[1] = f"{cfg['inputs']['anode']['individual_anode_mass_kg']:.1f}"
    aggregate = tables['Installed arrangement: published outputs and independent estimate']
    aggregate.columns += ['Criterion and basis']
    for row in aggregate.rows:
        row.append('Provided capacity ≥ computed demand; printed unit outputs [1], '
                   'independent cylinder estimate [2], or lifetime mass on the stated assumptions.')
    spec.document.title = 'Cathodic protection analysis: published jacket at 84.1 m'
    spec.document.revision = '00'
    spec.document.number = 'B0000-RPT-001-00'
    spec.document.project = 'Published jacket example'
    spec.document.client = 'Not applicable'
    spec.input_echo = {}
    spec.citations = [{
        'code_id': 'dnv-rp-b401', 'publisher': 'DNV', 'revision': '2021-05',
        'section': 'Anode mass; Anode resistance; Anode current output',
        'wiki_path': 'wikis/engineering-standards/wiki/standards/dnv-rp-b401-2021.md',
        'source_sibling': 'generic', 'source_project': None,
        'note': 'Method citation only; the potential criterion is a stated assumption.',
    }]
    paragraphs = [
        ('The jacket is a fixed steel structure in 84.1 m water depth. Analysis is performed to '
         'determine aggregate anode requirements and compare published monitoring readings. '
         'The key results are summarized below.\n\n' + STATUS + '\n\n'
         '- Aggregate sizing is conditional on the stated inputs.\n'
         '  - The installed count is 569; the independent cylinder estimate requires 575 anodes.\n'
         '- Local protection requires separate attention.\n'
         '  - The two lowest monitored levels fail the stated potential criterion at day 518. '
         'Arrange a retrofit assessment and repeat surveys after intervention.'),
        ('The published arrangement is analysed with the repository calculation workflow. '
         'Printed demand, unit output and count values are compared with calculated values. '
         'Differences in assumed anode geometry are retained.'),
        ('The four published area groups and effective phase densities are used without a separate '
         'coating deterioration law. Nine connected wells add 18 A at every phase. '
         'The resistivity conversion is 22 ohm.cm × 0.01 = 0.22 ohm.m. '
         'Design life is 25 years and temperature is 10 °C; these are ' + ASSUMPTION + '. '
         'Capacity, utilisation, net alloy interpretation and the two electrode potentials are also '
         'assumed. Equivalent-cylinder radius is calculated from nominal mass, length and '
         'an assumed alloy density of 2750 kg/m³. The −0.800 V protection criterion is a stated assumption. '
         'The paper gives a 0.25 V driving voltage, but not the separate electrode potentials. '
         'No standard citation is used to qualify the assumed −0.800 V criterion. '
         'The depth diagram shows the available monitoring data; a realistic member elevation '
         'cannot be established from the published information.'),
        ('Current demand is calculated as area multiplied by effective density, plus connected '
         'allowances. Lifetime net mass equals mean current multiplied by service hours, divided '
         'by capacity and utilisation. Initial and final current outputs equal driving voltage '
         'divided by anode resistance. Required count is the largest of the mass, initial-output '
         'and final-output counts. DNV-RP-B401, May 2021 [2], supplies the method headings '
         'Anode mass, Anode resistance and Anode current output. No standard table is reproduced. '
         'Aggregate capacity passes only when provided capacity is at least the corresponding demand. '
         + CRITERION + ' Missing or invalid inputs stop report generation with a reason.'),
        ('The calculated phase currents and anode counts are presented in Appendix A. '
         'Printed outputs give passing aggregate checks for the installed arrangement. '
         'The independent equivalent-cylinder estimate fails initial current output at 569 anodes '
         'and requires 575. These are different output bases, not interchangeable verdicts.'),
        ('The printed demand totals are reproduced within 0.1 %. Aggregate capacity does not '
         'establish protection at each member. At day 518, the two lowest monitored levels '
         'remain more positive than the stated −0.800 V criterion. The measured-potential '
         'table prints the criterion and assumption beside each status.'),
        ('Member dimensions, buried geometry, sediment resistivity, per-level anode allocation '
         'and present condition are not established. Local protection, intervention timing and '
         'a per-level retrofit split are not evaluated. Temperature and resistivity sensitivity '
         'is not part of this report; it is a separate reviewed issue. The report does not '
         'establish a qualified installation design.'),
        ('Retrofit assessment is recommended for the two lowest monitored levels because the '
         'day-518 readings fail the stated potential criterion. Assess clamped anodes at these '
         'levels, as proposed in [1]. Establish allocation, electrical continuity and remaining '
         'anode condition before selecting quantities and an intervention date. Repeat potential '
         'surveys at the same positions after intervention. A reading at or more positive than '
         '−0.800 V triggers investigation under the stated assumption. For aggregate design '
         'using the cylinder estimate, increase the count by six, to 575; recheck geometry, '
         'attachments and distribution before use.'),
        ('The public paper and standard edition are identified below.\n\n'
         f'## {reference_number}.1 Publications\n\n'
         '[1] Study on Implementation and Optimization of Cathodic Protection for Offshore Oil '
         'Platform Jacket. Engineering Advances 3(3), 2023, 241–245. '
         'https://doi.org/10.26855/ea.2023.06.017.\n\n'
         '[2] DNV-RP-B401, Cathodic protection design, May 2021. '
         'https://www.dnv.com/energy/standards-guidelines/dnv-rp-b401-cathodic-protection-design/.\n\n'
         f'## {reference_number}.2 Source locators\n\n'
         '[1], p. 242, Tables 1–2 and Section 2: areas, effective densities, connected allowance, '
         'anode size, installed count, resistivity, driving voltage and printed checkpoints. '
         'Pages 243–244, Section 3: monitoring. Page 244, Section 5: proposed remedy. '
         '[2], Anode mass; Anode resistance; Anode current output: calculation methods. '
         'The potential criterion is a stated assumption, not attributed to either publication.'),
        ('Symbols and units are defined here. Cathodic protection limits steel corrosion by supplying '
         'current. A is ampere; kg is kilogram; m is metre; yr is year; Ah/kg is ampere-hour per '
         'kilogram; ohm.cm and ohm.m are resistivity units. Ag/AgCl is the silver/silver-chloride '
         'reference electrode in seawater. Lower-case i denotes current density; upper-case I '
         'denotes current demand. The coating multipliers f_ci, f_cm and f_cf apply to initial, '
         'mean and final phases, respectively; all are unity for the effective-density inputs. '
         'PASS means that the adjacent criterion is met on the '
         'stated basis. FAIL means that it is not met. NOT_EVALUATED means required evidence is '
         'missing, invalid or not applicable.'),
    ]
    spec.sections = [Section(key=f'analysis-{i}', title=title, blocks=[TextBlock(markdown=text)])
                     for i, (title, text) in enumerate(zip(SECTION_TITLES, paragraphs), 1)]
    spec.sections[2].blocks.append(figure)
    spec.sections[4].blocks.append(current_chart(cfg))
    # Preserve the adapter's numerical data and comparisons, without its use-status prose.
    names = ('Design data', 'Published zone areas and effective-density treatment',
             'Current density, coating breakdown and demand by zone',
             'Independent estimate: anode mass and count',
             'Installed arrangement: published outputs and independent estimate',
             'Published checkpoints and calculated differences',
             'Measured potentials at the stated time of measurement')
    appendix = [tables[name] for name in names]
    # Locators follow the numbered References section, including revision history.
    for table in appendix:
        table.source = 'Published quantities [1]; assumed inputs and method [2] as stated in Design Data.'
        locator_columns = [i for i, name in enumerate(table.columns) if name in ('Locator', 'Source or note')]
        for row in table.rows:
            for i in locator_columns:
                row[i] = locator
            for i, cell in enumerate(row):
                if isinstance(cell, str):
                    row[i] = re.sub(r'-(?=\d)', '−', cell)
                    if cell.startswith('[1], p.'):
                        row[i] = locator
                elif isinstance(cell, (float, int)) and cell < 0:
                    row[i] = f'{cell:g}'.replace('-', '−')
    spec.appendices = [
        Section(key='calculation-data', title='Calculation data', label='Appendix A', blocks=appendix),
        Section(key='quality-evidence', title='Quality evidence', label='Appendix B', blocks=[
            TextBlock(markdown='The three independent hand-calculation anchors check phase-current sums. '
                      'The saved-report tests are consistency checks against the workflow. '
                      'They do not independently validate every reported value. The build validates '
                      'the citation-only edition record and creates release metadata and checksums. '
                      'Browser interaction and a PDF derivative have not been checked. '
                      'Input precision is retained in the fixture and results JSON.')]),
    ]
    spec.sections.insert(1, history)
    for record in spec.citations:
        validate_citation(Citation(**record), repo_root=ROOT)
    return spec, numerical_record(cfg)


def build(destination: Path) -> list[Path]:
    """Regenerate the report and release metadata with the repository writers."""
    spec, results = build_document()
    write_report(spec, destination, STEM, pdf='off', public_presentation=True)
    write_json(destination / 'results.json', results)
    write_json(destination / 'changes.json', {
        'revision': '00', 'changes': [
            'Published phase demands and anode requirements are presented with their calculation basis.',
            'Measured potentials retain failures against a stated assumed criterion.',
            'Resistivity units and unavailable sensitivity results are stated explicitly.',
        ]})
    write_receipt(destination)
    verify_pack(destination)
    return sorted(p for p in destination.iterdir() if p.is_file())


def main() -> None:
    """Build this case only, optionally into a separate output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=DESTINATION)
    args = parser.parse_args()
    build(args.output)


if __name__ == '__main__':
    main()
