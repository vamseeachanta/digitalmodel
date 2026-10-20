# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report prepared connected-riser inputs and screens without solver results."""

from __future__ import annotations

import argparse
import csv
import json
import math
from pathlib import Path

import yaml
from bs4 import BeautifulSoup

from digitalmodel.drilling_riser import generic_campaign as campaign
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.provenance import DataSource, Provenance
from digitalmodel.reporting.spec import (
    DocumentMeta,
    ReportSpec,
    RevisionRow,
    Section,
    TableBlock,
    TextBlock,
)

ROOT = Path(__file__).resolve().parents[2]
PUBLIC_LOADS = (
    'https://www.orcina.com/webhelp/OrcaFlex/Content/html/'
    'Linetheory,Hydrodynamicandaerodynamicloads.htm'
)
PUBLIC_STATICS = (
    'https://www.orcina.com/webhelp/OrcaFlex/Content/html/Generaldata,Statics.htm'
)
NOT_EVALUATED = 'NOT_EVALUATED'


def read_prepared(folder: Path) -> tuple[dict, list[dict]]:
    """Reject stale inputs, incomplete coverage and changed analytical values.

    Digests bind the existing preparation artifacts. Recalculation also checks
    that each row belongs to its declared case and implements the current
    screen; updating a CSV digest alone cannot qualify edited screen values.
    """
    table_path = folder / 'case-table.yml'
    table = yaml.safe_load(table_path.read_text(encoding='utf-8'))
    if table != campaign.case_table(ROOT):
        raise ValueError('Unsupported design basis; regenerate prepared inputs')
    screen_path = folder / 'screens/column-screen.csv'
    basis = json.loads((folder / 'screens/basis.json').read_text(encoding='utf-8'))
    identities = {
        'template_sha256': table['template_sha256'],
        'case_table_sha256': campaign.digest(table_path),
        'screen_csv_sha256': campaign.digest(screen_path),
        'calculation_fingerprints': campaign.calculation_fingerprints(),
    }
    if any(basis.get(key) != value for key, value in identities.items()):
        raise ValueError('Prepared input identity does not match current calculation')
    expected = {row['case_id']: row for row in campaign.screen_rows(ROOT, table)}
    with screen_path.open(encoding='utf-8', newline='') as stream:
        reader = csv.DictReader(stream)
        columns = reader.fieldnames or []
        required = set(next(iter(expected.values())))
        if set(columns) != required or len(columns) != len(required):
            raise ValueError('Unexpected screen columns')
        rows = list(reader)
    if len(rows) != len(expected) or {r['case_id'] for r in rows} != set(expected):
        raise ValueError('Incomplete or duplicate screen coverage')
    for row in rows:
        reference = expected[row['case_id']]
        if set(row) != set(reference):
            raise ValueError('Unexpected screen columns')
        for key, value in reference.items():
            if isinstance(value, str):
                if row[key] != value:
                    raise ValueError('Unsupported screen scope')
            else:
                try:
                    actual = float(row[key])
                except (TypeError, ValueError) as error:
                    raise ValueError('Invalid screen value') from error
                if not math.isfinite(actual) or actual != value:
                    raise ValueError('Changed screen value; regenerate prepared inputs')
    return table, rows


def paragraph(value: str) -> TextBlock:
    """Construct report prose."""
    return TextBlock(markdown=value)


def build_spec(folder: Path) -> ReportSpec:
    """Build a preparation-only analysis; acceptance is never inferred."""
    table, screens = read_prepared(folder)
    cases = table['cases']
    load_rows = [[
        case['id'], f"{case['offset_pct']['value']:.1f}",
        f"{case['offset_m']['value']:.1f}", f"{case['current_mps']['value']:.1f}",
        case['offset_pct']['source'], case['offset_m']['source'],
        case['current_mps']['source'],
    ] for case in cases]
    screen_rows = [[row['case_id'], *[
        f'{float(row[key]):.1f}' for key in (
            'submerged_weight_kn', 'legacy_screen_kn_not_graded',
            'axial_plus_bending_stress_mpa', 'geometric_extension_m',
        )
    ]] for row in screens]
    pending = [[case['id'], *([NOT_EVALUATED] * 5)] for case in cases]
    references = (
        'Orcina WebHelp, Line theory: Hydrodynamic and aerodynamic loads, Drag '
        f'(accessed 2026-10-20): {PUBLIC_LOADS}. The normal drag term reduces '
        'to q = rho Cd D U² / 2 per unit length for steady transverse flow. '
        'This source supplies the equation, not a drag coefficient.\n\n'
        'Orcina WebHelp, General data: Statics '
        f'(accessed 2026-10-20): {PUBLIC_STATICS}. Static equilibrium is iterative; '
        'YAML validation cannot establish equilibrium or dynamic adequacy.\n\n'
        'MIT OpenCourseWare, Structural Mechanics (2013), Lecture 6, '
        'equations (6.4)–(6.6): constant axial force, transverse equilibrium '
        'and moment-curvature relation give the tensioned-beam equation. '
        'Lecture 4, equation (4.30): sigma = N/A + Mz/I; set z = D/2 '
        'at the outer fibre for the reported stress screen. '
        'https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/pages/lecture-notes/ '
        '(accessed 2026-10-20). Chord extension follows directly from '
        'the Pythagorean theorem, as written out in the worked example.\n\n'
        f'Input geometry, densities, stiffness and drag coefficient: {campaign.SPEC.as_posix()}. '
        'Case coordinates and assumed load grid: case-table.yml, cases and basis. '
        'Numerical screen outputs: screens/column-screen.csv; identity: screens/basis.json. '
        'Equations and boundary conditions: src/digitalmodel/drilling_riser/riser_response.py, '
        'section.py and stackup.py; preparation adapter: generic_campaign.py. '
        'The constant-tension screen is an analytical model assumption, not a code criterion.'
    )
    sections = [
        Section(key='summary', title='Summary', blocks=[paragraph(
            f'{len(cases)} connected-mode offset and current cases are prepared. '
            'The tables below contain analytical column screens only. '
            'Solver version, flex-joint response, assembly tension, mechanical stroke '
            'and dynamic stress comparisons are NOT_EVALUATED. '
            'No operating window is established. Production remains held.'
        )]),
        Section(key='revision-history', title='Revision history', blocks=[TableBlock(
            title='Issue history', columns=['Revision', 'Date', 'Change and reason'],
            rows=[['00', '2026-10-20', 'Initial preparation report to expose inputs, '
                   'analytical screens and outstanding qualification without solver results.']],
        )]),
        Section(key='basis', title='Design basis and case table', blocks=[
            paragraph(table['method'] + '. Offsets and current speeds are '
                      'assumed, based on previous experience. Each grid value carries '
                      'its recorded source below. Geometry and material values remain '
                      'owned by the generic specification; no vendor model is redistributed.'),
            TableBlock(title='Offset and current cases',
                       columns=['Case', 'Offset (%)', 'Offset (m)', 'Current (m/s)',
                                'Offset percentage source', 'Offset distance source',
                                'Current source'], rows=load_rows),
        ]),
        Section(key='method', title='Analytical method and sources', blocks=[
            paragraph('The existing stack-up screen computes submerged column weight '
                      'and an assumed top tension. The pinned constant-tension beam '
                      'solves EI y\'\'\'\' − T y\'\' = q with zero end moments and specified '
                      'end offset. Axial-plus-bending stress is T/A + |M|D/(2I). '
                      'Geometric extension is sqrt(L² + X²) − L. '
                      'These are analytical screen equations; no new engineering '
                      'constant or acceptance limit is introduced.'),
            paragraph(references),
            paragraph('The connected drilling model and the running-BOP model were '
                      'inspected as context. Running BOP is a different operation; '
                      'its results are not a connected-mode reference. Unqualified '
                      'drilling-model groups are excluded.'),
        ]),
        Section(key='screens', title='Analytical screening results', blocks=[
            TableBlock(title='Analytical column screens', columns=[
                'Case', 'Submerged column weight (kN)',
                'Legacy tension reading, not graded (kN)',
                'Axial-plus-bending stress (MPa)', 'Geometric extension (m)',
            ], rows=screen_rows, source='screens/column-screen.csv'),
            paragraph(table['screen_limitations'] + '\n\n'
                      'Geometric extension is not mechanical tensioner stroke. '
                      'The legacy tension reading is not modelled assembly tension. '
                      'Joint-angle comparisons are omitted because tension varies '
                      'along the column. No stress acceptance is inferred.'),
        ]),
        Section(key='pending', title='Connected-mode results and way forward', blocks=[
            TableBlock(title='Pending connected-mode comparisons', columns=[
                'Case', 'Flex-joint angles', 'Binding tension checks',
                'Mechanical stroke', 'Dynamic stress', 'Comparison with screens',
            ], rows=pending),
            paragraph('All pending cells are NOT_EVALUATED because this report consumes '
                      'no solver results or runtime acceptance criteria. It does not '
                      'inspect stored summaries. Supply qualified angle, stress and '
                      'equipment criteria with sources; qualify native channels and '
                      'complete the prepared cases using one recorded solver version. '
                      'Then use build_generic_riser_report.py to check result identities '
                      'and compare matched results against criteria and column screens. '
                      'Confirm the held API RP 16Q edition and applicable clause text '
                      'before code acceptance. Shielding, soil representation and '
                      'End B moment interpretation remain unresolved. '
                      'Waves, drift-off, recoil, VIV and fatigue are outside this sweep.'),
        ]),
    ]
    return ReportSpec(
        document=DocumentMeta(
            number='G0000-RPT-002-00', revision='00',
            title='Drilling riser analysis: preparation and analytical screens',
            project='Generic drilling riser', client='Generic engineering basis',
            date='2026-10-20', revision_history=[RevisionRow(
                rev='00', date='2026-10-20', description='Initial preparation analysis',
            )],
        ), sections=sections,
        provenance=Provenance(sources=[DataSource(
            kind='prepared inputs', identifier='case-table.yml',
            digest=campaign.digest(folder / 'case-table.yml'),
        ), DataSource(
            kind='analytical screen', identifier='screens/column-screen.csv',
            digest=campaign.digest(folder / 'screens/column-screen.csv'),
        )]),
    )


def render(folder: Path) -> str:
    """Render deterministic standard HTML without a browser or solver."""
    document = BeautifulSoup(
        render_html(build_spec(folder), pdf_status='HTML only'), 'html.parser',
    )
    # Public sources are documented in the method; the engine citation model
    # accepts only wiki sources. Keep its empty-state message consistent.
    references = document.select_one('#references-cited p.prose')
    if references is None:
        raise ValueError('Missing standard report references section')
    references.string = 'Public references and equations are listed in Analytical method and sources.'
    return str(document)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--campaign', type=Path, default=ROOT / campaign.DEST)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    campaign.write_text(args.output, render(args.campaign))
