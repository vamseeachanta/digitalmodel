"""Standalone report-native diagram of analysis and evidence qualification."""
from html import escape


def _node(name, x, y, title, lines, pending=False):
    fill = '#fff4dd' if pending else '#edf4f8'
    border = ' stroke-dasharray="6 4"' if pending else ''
    labels = [title, *lines]
    texts = ''.join(
        f'<text x="{x + 110}" y="{y + 25 + index * 21}" text-anchor="middle" '
        f'style="font:{"600 " if index == 0 else ""}14px sans-serif;fill:#203549">'
        f'{escape(label)}</text>' for index, label in enumerate(labels))
    return (f'<g id="workflow-{name}"><rect x="{x}" y="{y}" width="220" height="94" '
            f'rx="8" fill="{fill}" stroke="#536c7e"{border}/>{texts}</g>')


def _edge(source, target, path):
    return (f'<path data-from="{source}" data-to="{target}" d="{path}" fill="none" '
            'stroke="#536c7e" stroke-width="2" marker-end="url(#workflow-arrow)"/>')


def workflow_schematic():
    """Keep forecasting separate from the operating-window qualification path."""
    nodes = [
        ('design', 10, 15, 'Design / source evidence', ['Arrangement and capacities', 'Edition / mappings pending']),
        ('master', 260, 15, 'Master + change files', ['Hs steps 0.25 m; Tp 1 s', 'Hashes and seeded cases']),
        ('solve', 510, 15, 'Static equilibrium', ['Irregular-wave dynamics', 'Saved simulation + receipt']),
        ('extract', 760, 15, 'Verified extraction', ['Units / channels / extrema', 'Hashes and sample coverage']),
        ('demand', 260, 175, 'Component demand', ['Crane / sling / jumper', 'Governing case + location']),
        ('slack', 510, 175, 'Slack / re-tension', ['Signed-tension diagnostics', 'Snap / interference pending']),
        ('forecast', 760, 175, 'Simulated 120 s forecast', ['History-only vs baselines', 'Oracle input labelled if used']),
        ('qualification', 385, 325, 'Qualification pending', ['Criteria + capacity mapping', 'Convergence / seed / RAO'], True),
        ('window', 385, 460, 'Conditional Hs–Tp window', ['Pending qualified boundaries', 'No approved limits issued'], True),
        ('report', 385, 595, 'Reviewed report', ['Evidence + limitations', 'Unresolved sections retained']),
        ('pamphlet', 385, 730, 'Pamphlet evidence', ['Reviewed supported claims', 'Simulation scope retained']),
    ]
    edges = [('design', 'master', 'M230 62H260'), ('master', 'solve', 'M480 62H510'),
             ('solve', 'extract', 'M730 62H760'),
             ('extract', 'demand', 'M870 109V140H370V175'),
             ('extract', 'slack', 'M870 109V140H620V175'),
             ('extract', 'forecast', 'M870 109V175'),
             ('demand', 'qualification', 'M370 269V297H450V325'),
             ('slack', 'qualification', 'M620 269V297H540V325'),
             ('qualification', 'window', 'M495 419V460'),
             ('window', 'report', 'M495 554V595'),
             ('forecast', 'report', 'M870 269V642H605'),
             ('report', 'pamphlet', 'M495 689V730')]
    return _wrap_svg(''.join(_edge(*edge) for edge in edges) + ''.join(_node(*node) for node in nodes))


def _wrap_svg(content):
    return ('<figure style="margin:16px 0"><svg xmlns="http://www.w3.org/2000/svg" '
            'viewBox="0 0 1000 885" role="img" aria-labelledby="workflow-title workflow-desc" '
            'style="max-width:1000px;width:100%;height:auto">'
            '<title id="workflow-title">Installation analysis and evidence workflow</title>'
            '<desc id="workflow-desc">Component demand and slack assessment feed pending qualification '
            'before any operating window. Forecast evidence feeds the report separately and does not expand limits. '
            'Missing evidence retains pending status; failed runs require resolution before qualification.</desc>'
            '<defs><marker id="workflow-arrow" markerWidth="8" markerHeight="8" refX="7" refY="4" '
            'orient="auto"><path d="M0 0L8 4L0 8Z" fill="#536c7e"/></marker></defs>' + content +
            '<text x="740" y="405" text-anchor="middle" style="font:14px sans-serif;fill:#203549">'
            '<tspan x="740">Forecast evidence only</tspan><tspan x="740" dy="23">Live operational validation remains pending</tspan></text>'
            '<text x="500" y="865" text-anchor="middle" style="font:14px sans-serif;fill:#203549">'
            'Missing evidence retains pending status; failed runs require resolution.</text></svg>'
            '<figcaption class="caption">Figure M1. Analysis workflow. Dashed boxes identify pending qualification '
            'and operating-window assessment. Forecasting is a separate simulation demonstration; '
            'it does not establish or expand operating limits.</figcaption></figure>')
