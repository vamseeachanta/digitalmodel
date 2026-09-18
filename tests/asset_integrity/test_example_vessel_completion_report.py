"""Report assertions for honest conditional assessment presentation."""
from digitalmodel.asset_integrity.assessment.example_vessel_completion_report import render_report


def test_missing_results_do_not_fabricate_acceptance():
    html = render_report({'basis': {}, 'level12': [], 'fea': [], 'verification': {}})
    assert 'NOT EVALUATED' in html
    assert 'NUMERICAL ELASTIC CHECKS PASS' not in html
    assert 'No finite-element result supplied' in html


def test_numeric_comparators_and_qualification_are_separate():
    row = dict(case_id='c-reduced', case='C', formulation='solid', pressure_mpa=.8,
               local_pitch_mm=12.5, radial_layers=4, acceptance=True,
               demands_mpa={'membrane': 110., 'membrane_plus_bending': 170., 'local_failure': 250.},
               limits_mpa={'membrane': 120., 'membrane_plus_bending': 180., 'local_failure': 480.},
               pressure_limit_mpa=.85)
    html = render_report({'fea': [row], 'verification': {'mesh': 'PENDING'}})
    assert '110.000' in html and '120.000' in html
    assert 'NUMERICAL ELASTIC CHECKS PASS' in html
    assert 'Mesh and boundary qualification' in html and 'PENDING' in html
    assert 'common operating pressure' in html


def test_sources_assumptions_diagrams_and_escaping():
    html = render_report({'basis': {'material': {'screening_stress_mpa': 120}},
                          'figures': [{'src': 'plot.png', 'caption': '<script>bad</script>'}]})
    assert 'API 579-1/ASME FFS-1:2007' in html
    assert 'assumed material properties' in html
    assert html.count('<svg') == 4
    assert '1100' in html and '1000' in html and '15.300' in html
    assert '<script>bad</script>' not in html


def test_inconsistent_comparator_cannot_display_pass():
    row = dict(acceptance=True, demands_mpa={'membrane': 150.},
               limits_mpa={'membrane': 120.}, pressure_mpa=1.5)
    html = render_report({'fea': [row]})
    assert 'NUMERICAL ELASTIC CHECKS PASS' not in html
    assert 'INCOMPLETE OR INCONSISTENT' in html


def test_four_area_flow_grids_and_decision_matrix():
    grids = [dict(area_id=a, bounds=[-1, 1, -1, 1], cells=[[-1, 1, -1, 1, 10]],
                  nominal_mm=16, csv_href=f'{a}.csv') for a in 'ABCD']
    html = render_report({'grid_previews': grids, 'decisions': [
        {'area': 'C', 'route': 'L1 → L2 → selected elastic Level 3',
         'pressure_mpa': .65, 'conclusion': 'Conditional numerical pass; verification pending'}]})
    for a in 'ABCD':
        assert f'Area {a} thickness grid' in html
    assert 'Area decision matrix' in html and '0.650' in html
    assert 'Linear elastic analysis' in html
    assert 'Area D: L1 → L2 → elastic L3 → repair' in html
    assert 'Vessel location schematic' in html
    assert 'Insert geometry and load path' in html
