# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Pressure-loss regression against retained calculation values.

The comparison basis is assumed, based on previous experience. It is not a
published worked example or an independently qualified engineering design.
The cached head is retained at full precision for the numerical assertion.
"""

import pytest

from digitalmodel.subsea.pipeline.pressure_loss import Pressureloss


@pytest.mark.parametrize("diameter, head, reynolds, head_tolerance, re_tolerance", [
    (4.5, 293.64590379313415, 8842.3477823008961, 0.004, 0.005),
    (4.0, 513.93844053973726, 9947.6412550885088, 0.011, 0.008),
])
def test_pressure_loss_matches_retained_head_without_plot_output(
    tmp_path, monkeypatch, diameter, head, reynolds, head_tolerance, re_tolerance,
):
    """Catch rounded-pressure drift and the unrelated fixed-path plot write."""
    monkeypatch.chdir(tmp_path)
    cfg = {"pipe_data": {
        "water_depth": 10000, "acceleration": 9.81, "Nominal_Id": diameter,
        "roughness": 0.045, "mud_weight": 15, "viscosity": 60, "flow_rate": 10,
    }}
    result = Pressureloss().run(cfg)["pressure_loss"]["flow_parameters"]
    # Retained head (m); criterion bounds the existing intermediate rounding.
    assert result["friction_loss"] == pytest.approx(head, rel=head_tolerance)
    assert result["reynolds_number"] == pytest.approx(reynolds, rel=re_tolerance)
    assert result["potential_energy"] == 3048.0
    assert list(tmp_path.iterdir()) == []
