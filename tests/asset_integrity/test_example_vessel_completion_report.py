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


def test_professional_structure_and_document_control_without_false_signoff():
    html = render_report({'document': {'revision': '01', 'date': '2026-09-18'}})
    assert '<nav' in html and 'href="#fea-comparisons"' in html
    assert 'Document control' in html and 'Nomenclature' in html
    assert 'INTERNAL TECHNICAL REVIEW' in html and 'EXAMPLE DATA' in html
    assert 'Approved by' not in html
    assert '@media print' in html and 'Appendix A' in html


def test_fea_images_are_inside_numerical_comparison_section_and_bound_to_case():
    row = dict(case_id='solid-c-reduced', case='C', formulation='solid',pressure_mpa=.65,
               acceptance=True, local_pitch_mm=12.5,radial_layers=4,
               demands_mpa=dict(membrane=105.,membrane_plus_bending=156.,local_failure=255.),
               limits_mpa=dict(membrane=120.,membrane_plus_bending=180.,local_failure=480.))
    image = dict(case_id='solid-c-reduced',src='native/c.png',caption='Native contour',
                 pressure_mpa=.65,
                 quantity='Element von Mises stress',units='MPa',averaging='Unaveraged PLESOL',
                 source_rst_sha256='a'*64,sha256='b'*64,result_set='last',deformation='undeformed')
    html = render_report({'fea':[row],'fea_images':[image]})
    section=html.split('id="fea-comparisons"',1)[1].split('id="verification"',1)[0]
    assert 'native/c.png' in section and '0.650 MPa' in section
    assert 'Unaveraged PLESOL' in section and 'linearized' in section
    assert 'b'*64 in html
    bad = render_report({'fea':[row],'fea_images':[dict(image,case_id='unrelated')]})
    assert 'native/c.png' not in bad


def test_basis_uses_engineering_labels_instead_of_raw_dictionary_dump():
    html=render_report({'basis':dict(inside_radius_mm=1000, nominal_mm=16,
        material=dict(elastic_modulus_mpa=195000,screening_stress_mpa=120))})
    assert 'Elastic modulus' in html and 'Nominal shell thickness' in html
    assert '{&quot;elastic_modulus_mpa&quot;' not in html


def test_head_basis_repair_numbering_and_print_expansion_are_explicit():
    html=render_report({'head':dict(head_nominal_mm=20,inside_height_mm=500,assumed_joint_efficiency=1)})
    assert 'Nominal head thickness' in html and '20.000' in html
    assert 'Internal head height' in html and '500.000' in html
    assert 'Figure 8.1.' in html and 'Figure 8.2.' in html
    assert "addEventListener('beforeprint'" in html
    assert html.count('<table>') == html.count('<div class="table-wrap"><table>')


def test_unbound_native_image_is_not_presented_as_evidence():
    html=render_report({'fea':[dict(case_id='C',case='C',formulation='solid',radial_layers=4,pressure_mpa=1.5)],'fea_images':[
        dict(case_id='C',src='unverified.png',caption='Looks convincing')]})
    assert 'unverified.png' not in html
    assert 'failed provenance binding' in html


def test_prose_uses_supplied_basis_and_diagnostics_do_not_become_refined_cases():
    html=render_report({'basis':dict(uncertainty_mm=.3,future_loss_mm=.7,assessment_temperature_c=80),
                       'fea':[dict(case_id='intact',case='intact',formulation='shell',pressure_mpa=1.5)]})
    assert '0.300 mm uncertainty and 0.700 mm future loss' in html
    assert '80.000 °C' in html
    assert 'No refined finite-element result supplied' in html
