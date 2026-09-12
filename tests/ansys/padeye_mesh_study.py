"""Frozen two-mesh diagnostics for the 8 mm / 50 kN screening candidate.

These checks cannot qualify physical hardware or establish convergence.
Peak growth or coordinate movement is an investigation trigger, not a proof
of mathematical singularity. No subset of the stress field replaces the peak.
"""
import math

from tests.ansys.golden_acceptance import (
    _scientific_rounding, close, number, validate_equilibrium,
)


def _check_mesh(digest, size):
    for value in digest.values():
        number(value)
    close(digest['mesh_size_mm'], size, 0)
    for key in ('loaded_node_count', 'mesh_node_count', 'mesh_element_count', 'peak_node'):
        value = digest[key]
        assert value > 0 and value == int(value), f'invalid {key}'
    assert 0 <= digest['peak_x_mm'] <= 400 and 0 <= digest['peak_y_mm'] <= 300
    close(digest['peak_z_mm'], 0, 0)
    close(digest['applied_fx_n'], 0, 0)
    close(digest['applied_fy_n'], 50000, 0)
    validate_equilibrium('padeye', digest, {})
    allowable = 355 / 1.67  # Frozen example inputs, not a standards-derived rule.
    close(digest['allowable_mpa'], allowable, 0.00005)
    peak = digest['max_seqv_mpa']
    assert peak > 0 and peak + 0.00005 < min(355, allowable)
    assert 0 <= digest['uc'] and digest['uc'] + 0.000005 < 1
    close(digest['uc'], peak / allowable, 0.000005 + 0.00005 / allowable)


def assess_mesh_pair(coarse, fine):
    """Return an auditable disposition; never promote a golden or readiness."""
    findings = []
    for label, digest, size in (('coarse', coarse, 10), ('fine', fine, 5)):
        try:
            _check_mesh(digest, size)
        except (AssertionError, KeyError, ValueError, OverflowError) as error:
            findings.append(f'{label}: {error}')
    metrics = {}
    if not findings:
        for key in ('mesh_node_count', 'mesh_element_count'):
            if fine[key] <= coarse[key]:
                findings.append(f'No demonstrated refinement in {key}')
        growth = fine['max_seqv_mpa'] - coarse['max_seqv_mpa']
        metrics['peak_growth_mpa'] = growth
        if growth > 0.0001 + 1e-12:  # Combined F12.4 half-last-place intervals.
            findings.append('Global peak grew beyond recorded print resolution')
        displacement = []
        for key in ('peak_x_mm', 'peak_y_mm', 'peak_z_mm'):
            delta = fine[key] - coarse[key]
            displacement.append(delta)
            rounding = _scientific_rounding(fine[key]) + _scientific_rounding(coarse[key])
            if abs(delta) > rounding + 1e-12:
                findings.append(f'Global peak moved in {key}')
        metrics['peak_displacement_mm'] = math.hypot(*displacement)
    return {'status': 'unqualified_investigation_required' if findings else 'diagnostic_checks_passed',
            'native_qualification_complete': False, 'convergence_demonstrated': False,
            'findings': findings, 'metrics': metrics,
            'scope': 'Two-mesh sensitivity only; no physical lug rating or golden promotion'}
