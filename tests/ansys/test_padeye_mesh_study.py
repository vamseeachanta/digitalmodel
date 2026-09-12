"""Prespecified diagnostic criteria; synthetic cases are not engineering evidence."""
import pytest

from tests.ansys.padeye_mesh_study import assess_mesh_pair


def mesh(size, count, stress=100.0, x=160.0):
    return dict(mesh_size_mm=size, mesh_node_count=count, mesh_element_count=count,
                loaded_node_count=10, max_seqv_mpa=stress, allowable_mpa=212.5749,
                uc=round(stress / (355 / 1.67), 5), peak_node=1,
                peak_x_mm=x, peak_y_mm=220.0, peak_z_mm=0.0,
                applied_fx_n=0.0, applied_fy_n=50000.0,
                reaction_fx_n=0.0, reaction_fy_n=-50000.0, force_residual_n=0.0)


def test_stable_pair_never_establishes_full_qualification():
    result = assess_mesh_pair(mesh(10, 100), mesh(5, 400))
    assert result['status'] == 'diagnostic_checks_passed'
    assert not result['native_qualification_complete']
    assert not result['convergence_demonstrated']


@pytest.mark.parametrize('change', ['growth', 'movement', 'no_refinement', 'load',
                                  'nonfinite', 'overload', 'unbalanced', 'zero_nodes'])
def test_adverse_pair_is_not_promoted(change):
    fine = mesh(5, 400)
    updates = {'growth': {'max_seqv_mpa': 100.001},
               'movement': {'peak_x_mm': 159.99},
               'no_refinement': {'mesh_node_count': 100},
               'load': {'applied_fy_n': 5000.0},
               'nonfinite': {'peak_y_mm': float('nan')},
               'overload': {'max_seqv_mpa': 220.0, 'uc': 1.034928},
               'unbalanced': {'reaction_fy_n': -49900.0, 'force_residual_n': 100.0},
               'zero_nodes': {'loaded_node_count': 0}}
    fine.update(updates[change])
    result = assess_mesh_pair(mesh(10, 100), fine)
    assert result['status'] == 'unqualified_investigation_required'
    assert result['findings']
    assert not result['native_qualification_complete']
