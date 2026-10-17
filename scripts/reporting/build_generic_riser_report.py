# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the generic riser report from qualified per-case summaries only."""
from __future__ import annotations

import argparse
import csv
import importlib.util
import json
import math
from pathlib import Path
import re
from typing import Any

from bs4 import BeautifulSoup
import yaml

from digitalmodel.drilling_riser import generic_campaign
from digitalmodel.drilling_riser.generic_campaign import METHOD, digest, write_text
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.provenance import DataSource, Provenance
from digitalmodel.reporting.spec import DocumentMeta, ReportSpec, Section, TableBlock, TextBlock

TITLE = 'Drilling riser analysis: generic deepwater drilling riser, vessel offset and current sweep'
FORBIDDEN = re.compile(r'\b(vendor|example|template|B01|benchmark|pass|fail|verdict|review|script|repository|campaign)\b', re.I)


def driver_module() -> Any:
    """Load the shared extraction and acceptance contract without execution."""
    path = Path(__file__).resolve().parents[1] / 'orcaflex/run_generic_drilling_riser_campaign.py'
    spec = importlib.util.spec_from_file_location('generic_riser_driver', path)
    if spec is None or spec.loader is None:
        raise ValueError('Unable to load the extraction contract')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def text(value: str) -> TextBlock:
    """Construct a report prose block."""
    return TextBlock(markdown=value)


def section(key: str, title: str, *blocks: Any) -> Section:
    """Construct one report section."""
    return Section(key=key, title=title, blocks=list(blocks))


def ring_reference_description(table: dict) -> str:
    """Describe the initial elevation recorded for each prepared case."""
    elevations = [(case['id'], float(case['ring_initial_position_m']['value'][2]))
                  for case in table['cases']]
    if len({value for _, value in elevations}) == 1:
        return f'{elevations[0][1]:g} m for every case'
    return '; '.join(f'{case_id}: {value:g} m' for case_id, value in elevations)


def build_spec(campaign: Path, results: Path, *, criteria_file: Path | None = None, allow_synthetic: bool = False) -> ReportSpec:
    """Build a report specification after checking complete matched inputs."""
    driver = driver_module()
    table = yaml.safe_load((campaign / 'case-table.yml').read_text(encoding='utf-8'))
    if table != generic_campaign.case_table(Path(__file__).resolve().parents[2]):
        raise ValueError('Unsupported design basis; regenerate the prepared inputs')
    criteria, criteria_identity = generic_campaign.load_criteria(criteria_file, table)
    cases = table['cases']
    if len(cases) != 24 or len({case['id'] for case in cases}) != 24:
        raise ValueError('Expected 24 unique cases')
    paths = sorted(results.glob('*.summary.json'))
    if len(paths) != 24:
        raise ValueError('Expected 24 completed summaries')
    summaries = {}
    for path in paths:
        item = json.loads(path.read_text(encoding='utf-8'))
        key = item.get('case_id')
        if key not in {case['id'] for case in cases} or key in summaries:
            raise ValueError('Unexpected or duplicate case identity')
        if (item.get('status') != 'complete' or item.get('extraction_version') != driver.EXTRACTION_VERSION
                or item.get('input_sha256') != driver.input_digest(campaign, key)
                or item.get('criteria_sha256') != criteria_identity):
            raise ValueError('Result identity does not match prepared inputs')
        driver.validate_result(item)
        summaries[key] = item
    versions = {item['solver_version'] for item in summaries.values()}
    if len(versions) != 1:
        raise ValueError('Mixed solver versions')
    version = next(iter(versions))
    if version == 'synthetic' and not allow_synthetic:
        raise ValueError('Synthetic data is restricted to explicit test builds')
    screen_path = campaign / 'screens/column-screen.csv'
    basis = json.loads((campaign / 'screens/basis.json').read_text(encoding='utf-8'))
    expected = {'template_sha256': table['template_sha256'],
        'case_table_sha256': digest(campaign / 'case-table.yml'),
        'screen_csv_sha256': digest(screen_path),
        'calculation_fingerprints': generic_campaign.calculation_fingerprints()}
    if any(basis.get(key) != value for key, value in expected.items()):
        raise ValueError('Screen identity does not match current inputs and calculation')
    with screen_path.open(encoding='utf-8', newline='') as stream:
        screen_rows = list(csv.DictReader(stream))
    if len(screen_rows) != 24 or {row['case_id'] for row in screen_rows} != set(summaries):
        raise ValueError('Incomplete screen coverage')
    screens = {row['case_id']: row for row in screen_rows}
    for row in screen_rows:
        for key, value in row.items():
            if key not in ('case_id', 'scope') and not math.isfinite(float(value)):
                raise ValueError('Nonfinite screen result')
    all_rows, within, window = [], [], []
    for case in cases:
        result = summaries[case['id']]
        rows = driver.criterion_rows(result, criteria)
        all_rows.extend([[case['id'], *row] for row in rows])
        if all(row[-1] <= 1 for row in rows):
            within.append(case)
    for speed in sorted({case['current_mps']['value'] for case in cases}):
        window.append([speed, 'production held; none established'])
    governing = []
    count = len(driver.criterion_rows(next(iter(summaries.values())), criteria))
    for index in range(count):
        candidates = all_rows[index::count]
        governing_row = (min(candidates, key=lambda item: item[3]-item[4])
                         if index in (4, 5, 6, 7, 8) else
                         max(candidates, key=lambda item: item[-1]))
        governing.append(governing_row[:-1])
    comparisons = []
    for case in cases:
        result, screen = summaries[case['id']], screens[case['id']]
        for label, metric, column in (
                ('maximum stress (MPa)', 'stress_max_mpa', 'axial_plus_bending_stress_mpa'),):
            reference = float(screen[column])
            demand = result[metric]
            difference = (demand - reference) / abs(reference) * 100 if abs(reference) > 1e-9 else 'not defined'
            comparisons.append([case['id'], label, demand, reference, difference, demand - reference])
    criterion_table = [[key.replace('_', ' '), item['value'], item['unit'],
                        item['source'].split('; src/')[0]] for key, item in criteria.items()]
    tension_margins = [[case['id'], summaries[case['id']]['support_intact_min_kn']-criteria['tsrmin_kn']['value'],
        summaries[case['id']]['support_loss_min_kn']-criteria['tsrmin_kn']['value'],
        summaries[case['id']]['bottom_sum_min_kn']-0.05*criteria['ws_kn']['value'],
        summaries[case['id']]['member_min_kn'], summaries[case['id']]['table_clearance_min_m']] for case in cases]
    load_cases = [[case['id'], case['offset_pct']['value'], case['offset_m']['value'],
                   case['current_mps']['value']] for case in cases]
    ring_elevations = ', '.join(f'{height:g} m' for height in sorted({
        float(case['ring_initial_position_m']['value'][2]) for case in cases}))
    intro = (f"Supplied mean angle ≤ {criteria['mean_angle_deg']['value']:g}°, "
             f"maximum angle ≤ {criteria['max_angle_deg']['value']:g}°. "
             'Production remains held; nothing released to production. '
             'No operating window is established. Check A requires intact slip-ring support '
             'at least T_SRmin, the separate End B sum at least 0.05 Ws, and every '
             'member minimum effective tension at least zero. Check B requires the '
             'three-of-four vertical link share less the carried weight at least T_SRmin. '
             'Every link shall stay inside its spring table. Both checks bind every case. '
             'Native channel qualification, equipment capacity and held-edition confirmation '
             'of API RP 16Q clause text remain pending.')
    sections = [
        section('summary', '1. Executive Summary', text(intro), TableBlock(title='Production hold',
            columns=['Current (m/s)', 'Release status'], rows=window)),
        section('introduction', '2. Introduction', text('Connected-mode response is assessed for a generic deepwater '
            'drilling riser over vessel mean offset and uniform current. Results are compared with an approximate '
            'static riser-column calculation. No geographic site is represented.')),
        section('design', '3. Design Data', text('3.1 Site data: water depth 1,020 m; no site.\n\n'
            '3.2 Structure data: 1,020 m main riser, 0.650 m outside diameter, 0.600 m inside diameter; '
            'steel density 7,850 kg/m³, elastic modulus 212 GPa. Slip-joint model length is 10 m. '
            'Drill string, choke and kill lines, lower flex joint and casing remain in the connected model.\n\n'
            '3.3 Environmental data: uniform current 0–1.5 m/s towards 180°; no wave is applied. '
            'One mud density, 2,200 kg/m³.\n\n'
            '3.4 Vessel data: generic semisubmersible, length 90 m, operating draught 24.38 m; four spring tensioners.\n\n'
            '3.5 Criteria: joint angle criteria, yield strength and stress design factor are supplied separately '
            'with their sources. The stress limit is their assumed uniaxial product. '
            '8,345.7 kN is a legacy screen reading, not graded, not the modelled tension. '
            'T_SRmin includes flooded steel weight of the main tube, choke and kill, with fluid head counted once. '
            'The mechanical/hydraulic part of Rf is assumed unity, a known limit. Stroke limits are independent '
            'assumptions, not capacities inferred from slip-joint length.\n\n'
            f"{table['tension_acceptance_basis']}\n\n"
            '3.6 Method: see Section 4.\n\n3.7 Load cases: six offsets and four current speeds; '
            'offsets and profile are assumed, based on previous experience. Build-up 8 s and main stage 42 s.'),
            TableBlock(title='Criteria and sources', columns=['Criterion', 'Limit', 'Unit', 'Basis'], rows=criterion_table),
            TableBlock(title='Load cases', columns=['Case', 'Offset (%)', 'Offset (m)', 'Current (m/s)'], rows=load_cases)),
        section('method', '4. Analysis Methodology', text(f'{METHOD}.\n\nOrcaFlex version {version}. '
            'Statics precede dynamics. Statistics use the main stage and exclude build-up. Angles are mean absolute '
            'and maximum absolute rotations at the compliant connections: upper slip-joint vessel end and lower '
            'rigid-piece fixed end. Tension acceptance uses simultaneous global link forces less ring weight and '
            'the slip-joint End B global reaction, with separate bottom and member-profile comparisons. '
            'The minimum over static equilibrium and main-stage dynamics is graded. Vertical stroke is tensioner-ring '
            f'elevation relative to each case\'s initial elevation ({ring_elevations}), read from the case table; '
            'vessel elevation is fixed. Maximum von Mises stress is '
            'taken over the main riser. Native channel qualification is required before engineering acceptance.\n\n'
            'The static comparison uses a pinned beam with constant assumed top tension and uniform drag. '
            'Both joint-angle comparisons are omitted: constant tension can introduce an error of about a factor of two. '
            'The above-water column is treated as submerged, underestimating weight by about 1.0 %. Its '
            'axial-plus-bending stress omits pressure stress. It is an approximate screening estimate, not an assembly calculation.')),
        section('results', '5. Detailed Results', text('The governing demand is stated with its criterion, component and '
            'limit. Percentage differences use the static screen as reference: 100 × (dynamic demand − screen) / |screen|. '
            'For |screen| ≤ 10⁻⁹ in its stated unit, percentage difference is not defined and absolute difference is given. '
            'Comparison arrays are in the appendix. Mechanical tensioner stroke is not represented by geometric extension.'),
            TableBlock(title='Governing envelopes', columns=['Case', 'Criterion', 'Component', 'Demand', 'Limit', 'Unit'], rows=governing),
            TableBlock(title='Binding tension margins; nonnegative meets each comparison',
                columns=['Case', 'Check A margin (kN)', 'Check B margin (kN)', 'End B margin (kN)',
                         'Member minimum (kN)', 'Spring-table clearance (m)'], rows=tension_margins)),
        section('findings', '6. Key Findings', text(f'{len(within)} of 24 cases meet the tabulated numerical comparisons. Production remains held. '
            'The governing envelope table identifies the controlling cases. Static-screen agreement does not '
            'establish dynamic adequacy; pressure stress, attached components and compliant support effects differ.')),
        section('limitations', '7. Limitations', text(f'The basis is a generic configuration. {METHOD}. '
            'No drift-off, recoil, VIV or fatigue; one mud weight. Native extraction qualification and confirmation of '
            'all supplied and assumed acceptance limits are required. The riser-column tension estimate does not establish the '
            'required tension of the complete connected assembly. '
            'Old-setting End B moment: 1,858 kN.m native against a 1,363-1,440 kN.m hand estimate; '
            'explanation pending the checker. New-setting native results remain unqualified.')),
        section('recommendations', '8. Recommendations', text('Confirm equipment capacities and operating criteria, '
            'qualify the native result channels, and evaluate the excluded operating conditions before operational use.')),
        section('references', '9. References', text(f'OrcaFlex, version {version}: connected finite-element analysis.\n\n'
            + '.\n\n'.join(generic_campaign.REFERENCES.values())
            + '.\n\nNumerical criteria are supplied separately with their sources; bibliographic references '
            'alone do not establish compliance.')),
        section('symbols', '10. Acronyms and Symbols', text('VIV: vortex-induced vibration. ρ: density (kg/m³). θ: joint angle (°). σ: stress (MPa).')),
    ]
    return ReportSpec(document=DocumentMeta(number='G0000-RPT-001-00', revision='00', title=TITLE,
        project='Generic drilling riser', client='Generic engineering basis'), sections=sections,
        appendices=[section('appendices', '11. Appendices',
            TableBlock(title='Screen comparisons', columns=['Case', 'Quantity', 'Demand', 'Screen reference',
                'Difference (%)', 'Absolute difference'], rows=comparisons),
            TableBlock(title='Complete envelopes', columns=['Case', 'Criterion', 'Component', 'Demand', 'Limit',
                'Unit', 'Comparison score'], rows=all_rows))],
        provenance=Provenance(sources=[DataSource(kind='design data', identifier='generic-connected-basis',
            digest=table['template_sha256']), DataSource(kind='analytical results', identifier='column-screen',
            digest=digest(screen_path)), DataSource(kind='acceptance criteria', identifier='runtime-criteria',
            digest=criteria_identity)] + [DataSource(kind='dynamic results', identifier=p.stem.split('.')[0],
                digest=digest(p)) for p in paths]))


def arrange_report(document: BeautifulSoup) -> None:
    """Place engine metadata in the appendix and captions below tables."""
    appendix = document.select_one('#appendices')
    if appendix is None:
        raise ValueError('Missing report appendix')
    for key in ('references-cited', 'data-sources', 'input-echo'):
        metadata = document.select_one('#' + key)
        if metadata is None:
            raise ValueError('Missing report metadata')
        heading = metadata.select_one('.l1head')
        if heading is None:
            raise ValueError('Missing metadata heading')
        subheading = document.new_tag('h3')
        subheading.string = heading.get_text(' ', strip=True)
        heading.replace_with(subheading)
        metadata.name = 'div'
        appendix.append(metadata.extract())
        for link in document.select(f'nav.toc a[href="#{key}"]'):
            entry = link.find_parent('li')
            if entry is not None:
                entry.decompose()
    for paragraph in appendix.select('p.prose'):
        if paragraph.get_text(strip=True) == 'No external references cited.':
            paragraph.string = 'Bibliographic references are listed in Section 9; supplied criterion sources are listed in Section 3.'
        elif paragraph.get_text(strip=True) == 'No inputs echoed.':
            paragraph.string = 'Design inputs and assumptions are listed in Section 3.'
    for table in document.select('.tbl'):
        caption, wrapper = table.select_one('.cap'), table.select_one('.tbl-wrap')
        if caption is not None and wrapper is not None:
            wrapper.insert_after(caption.extract())


def render(campaign: Path, results: Path, *, criteria_file: Path | None = None, allow_synthetic: bool = False) -> str:
    """Render the matched results and reject prohibited visible wording."""
    html = render_html(build_spec(campaign, results, criteria_file=criteria_file, allow_synthetic=allow_synthetic), pdf_status='HTML only')
    document = BeautifulSoup(html, 'html.parser')
    arrange_report(document)
    if FORBIDDEN.search(document.get_text(' ', strip=True)):
        raise ValueError('Forbidden visible report text')
    return str(document)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--campaign', type=Path, required=True)
    parser.add_argument('--results', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--criteria', type=Path, required=True)
    args = parser.parse_args()
    write_text(args.output, render(args.campaign, args.results, criteria_file=args.criteria))
