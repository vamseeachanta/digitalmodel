from digitalmodel.workflows.installation_report_sections import front_sections, pending_sections, marketing_section
import xml.etree.ElementTree as ET


def test_standard_sections_and_recorded_design_settings():
    summary = {'counts': {'VERIFIED': 2, 'RUNNING': 1}, 'cases': [
        {'status': 'VERIFIED', 'hs_m': 1, 'tp_s': 8, 'seed': 17,
         'heading_degrees': 180, 'settings': {'duration_s': 900, 'buildup_s': 90,
                                            'sample_interval_s': .2}}]}
    html = front_sections(summary)
    for name in ('Introduction', 'Summary and conclusions', 'Design data', 'Analysis methodology'):
        assert name in html
    assert '900' in html and '0.2' in html
    assert '600 s' not in html
    assert 'Evidence required' in html


def test_placeholders_persist_with_no_results_and_no_marketing_claim():
    html = pending_sections({'counts': {}, 'cases': []}) + marketing_section()
    for label in ('Operating window', 'Capacity utilisation', 'Snap loads', 'Convergence',
                  'Forecast validation', 'Pamphlet evidence'):
        assert label in html
    assert 'Not evaluated' in html
    assert 'Pending analysis' in html
    assert 'No approved operating limits' in html


def test_design_values_are_escaped():
    html = front_sections({'counts': {}, 'cases': [
        {'status': 'VERIFIED', 'settings': {}, 'seed': '<unsafe>'}]})
    assert '&lt;unsafe&gt;' in html and '<unsafe>' not in html


def test_workflow_accessible_and_qualification_is_not_forecast_driven():
    html = front_sections({'counts': {}, 'cases': []})
    svg = ET.fromstring(html[html.index('<svg'):html.index('</svg>') + 6])
    assert svg.attrib['role'] == 'img'
    identifiers = {node.attrib.get('id') for node in svg.iter()}
    assert set(svg.attrib['aria-labelledby'].split()) <= identifiers
    text = ' '.join(svg.itertext())
    for label in ('Master + change files', 'Static equilibrium', 'Verified extraction',
                  'Component demand', 'Slack / re-tension', 'Simulated 120 s forecast',
                  'Qualification pending', 'Conditional Hs–Tp window', 'Reviewed report',
                  'Pamphlet evidence', 'Missing evidence retains pending status'):
        assert label in text
    edges = {(node.attrib.get('data-from'), node.attrib.get('data-to')) for node in svg.iter()}
    assert ('forecast', 'report') in edges
    assert ('forecast', 'qualification') not in edges
    assert ('forecast', 'window') not in edges
    assert ('demand', 'qualification') in edges
    assert ('slack', 'qualification') in edges
    assert 'Live operational validation remains pending' in text
    assert 'http' not in ET.tostring(svg, encoding='unicode').replace('http://www.w3.org/2000/svg', '')
