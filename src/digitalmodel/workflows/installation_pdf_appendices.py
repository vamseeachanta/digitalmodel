"""Semantic screening checks and ordered trailing full-report appendices."""
from io import BytesIO

from reportlab.lib.pagesizes import A4
from reportlab.platypus import SimpleDocTemplate

from digitalmodel.workflows.vessel_capability_layout import (
    bind_screening, _criterion_status, _finite,
)


def validate_pdf_screening(summary, payload):
    """Permit explicit conditional preview while retaining causal fit and score checks."""
    mode = payload.get('demo', {}).get('default_mode')
    if mode not in ('history_only', 'wave_preview'):
        raise ValueError('Full report requires an explicit supported forecast mode')
    return bind_screening(summary, payload, allow_wave_preview=mode == 'wave_preview')


def _criterion_rows(cases, criteria):
    rows = []
    for case in cases:
        cells = []
        for criterion in criteria:
            status = _criterion_status(case, criterion['id'])
            label = str(criterion.get('label', criterion['id']))
            wording = {'WITHIN_ASSUMPTIONS': f'Acceptable against {label}',
                       'EXCEEDS_ASSUMPTIONS': f'Not acceptable against {label}',
                       'NOT_EVALUATED': 'Not evaluated'}[status]
            utilization = max((check['utilization'] for check in case['checks']
                if check['id'] == criterion['id'] and _finite(check.get('utilization'))), default=None)
            cells.append(wording + (f' ({utilization:.3f})' if utilization is not None else ''))
        rows.append([f"CASE-{case['index']:03d}", f"{case['hs_m']:g}", f"{case['tp_s']:g}"] + cells)
    return rows


def render_trailing_appendices(cases, payload, sensitivity):
    """Render C then D after the separately embedded Appendix B snapshot."""
    from digitalmodel.workflows.installation_full_report_pdf import _section, _table, _sensitivity
    story = []
    _section(story, 'Appendix C. Per-cell verdicts against provisional criteria',
        'Each verdict is conditional on component capacities, load-path mapping and the criteria basis. '
        'Unity is the utilization threshold. Numerical failures are Not evaluated. '
        'Engineering acceptance: NOT EVALUATED.')
    criteria = payload['criteria']
    headers = ['Case', 'Hs (m)', 'Tp (s)'] + [c.get('label', c['id']) for c in criteria]
    widths = [63, 43, 43] + [358 / len(criteria)] * len(criteria) if criteria else [169] * 3
    _table(story, headers, _criterion_rows(cases, criteria), widths,
           'Table C-1. Conditional per-cell verdicts for every planned cell.')
    if sensitivity is not None:
        _sensitivity(story, sensitivity)
    stream = BytesIO()
    SimpleDocTemplate(stream, pagesize=A4, leftMargin=44, rightMargin=44,
                      topMargin=44, bottomMargin=49).build(story)
    return stream
