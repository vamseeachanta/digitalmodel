"""Shared document control and contents for installation engineering reports."""
from html import escape
from digitalmodel.workflows.installation_partial_report import _table


def appendix_labels(screening, sensitivity):
    labels = [('a', 'detailed results'), ('b', 'provenance and unresolved checks')]
    if screening is not None:
        labels.append(('c', 'per-cell verdicts against provisional criteria'))
    if sensitivity is not None:
        labels.append(('d', 'time-step and seed sensitivity'))
    return labels


def report_cover(config, created, *, appendices=None):
    fields = [('Document', config.get('document_id', 'Installation engineering assessment')),
              ('Revision', config.get('revision', 'Not recorded')), ('Issue purpose', config.get('issue_purpose', 'Technical review')),
              ('Prepared by', config.get('prepared_by', 'Automated report generation')), ('Reviewed by', config.get('reviewed_by', 'Not assigned')),
              ('Approved by', config.get('approved_by', 'Not assigned')), ('Generated UTC', created)]
    table = _table(['Control field', 'Entry'], [[escape(str(a)), escape(str(b))] for a, b in fields])
    headings = ['Introduction', 'Summary and conclusions', 'Design data and assumed criteria',
                'Analysis methodology', 'Results — conditional screening', 'Validation status', 'Conclusions and recommendations',
                'References and revision history']
    toc = ''.join(f'<li><a href="#section-{i}">{i} {name}</a></li>' for i, name in enumerate(headings, 1))
    appendices = appendices or [('a', 'detailed results'), ('b', 'provenance and unresolved checks')]
    appendix_links = ' · '.join(f'<a href="#appendix-{letter}">Appendix {letter.upper()}: {escape(title)}</a>'
                               for letter, title in appendices)
    return (f'<header><div class="tag">Engineering assessment / technical review</div><h1>{escape(config.get("title", "Jumper installation engineering report"))}</h1>'
            f'<p>{escape(config.get("subtitle", "Recorded demand, assumed-criteria envelopes and simulated monitoring"))}</p></header>'
            '<section><h2>Document control</h2>' + table + '<p class="caption">Table 0-1. Document control; no review or approval signature is assigned.</p>'
            '<h2>Contents</h2><ol style="list-style:none">' + toc + '</ol>' + appendix_links + '</section>')

