# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic thickness solve checked by an independent forward evaluation."""

from dataclasses import replace

import pytest

from tests.subsea.on_bottom_stability.test_e305_parity import case


def test_inverse_concrete_thickness_recomputes_diameter_and_forward_margin():
    from digitalmodel.subsea.on_bottom_stability.e305_simplified import (
        Pipe, weight_build_up, evaluate_case, inverse_concrete_thickness,
    )

    pipe = Pipe(1, 0.1, 0, 2, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    visited = []

    def build_cases(updated_pipe, weight):
        visited.append((updated_pipe.concrete_m, weight.hydrodynamic_diameter_m))
        # Synthetic diameter-dependent factor forces recomputation at each step.
        first = case(R=1, Fw=1+weight.hydrodynamic_diameter_m, current_m_s=1,
                     cd=1, cl=0, target_safety_factor=1)
        return (first, replace(first, role='extreme_current'))

    root = inverse_concrete_thickness(pipe, build_cases, g=10,
                                      lower_m=0, upper_m=0.5, tolerance_m=1e-7)
    assert root.status == 'converged'
    assert root.converged
    assert 0 < root.thickness_m < 0.5
    assert len(set(visited)) > 2
    forward_pipe = replace(pipe, concrete_m=root.thickness_m)
    weight = weight_build_up(forward_pipe, g=10)
    for value in build_cases(forward_pipe, weight):
        result = evaluate_case(value, diameter_m=weight.hydrodynamic_diameter_m,
                               rho_water_kg_m3=1, available_weight_n_m=weight.submerged_weight_n_m)
        assert result.utilization <= 1
        assert result.utilization == pytest.approx(1, abs=1e-4)
    assert root.residual_n_m >= 0
    assert root.qualified is False
    assert inverse_concrete_thickness(pipe, build_cases, g=10, lower_m=0.4,
                                      upper_m=0.5).status == 'lower_feasible'
    assert inverse_concrete_thickness(pipe, build_cases, g=10, lower_m=0,
                                      upper_m=0.00001).status == 'unbracketed'
    assert inverse_concrete_thickness(pipe, build_cases, g=10, lower_m=0,
                                      upper_m=0.5, max_iterations=1).status == 'not_converged'
