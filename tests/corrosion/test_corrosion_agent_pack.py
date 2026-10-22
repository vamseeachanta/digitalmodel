# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic with synthetic screening inputs, not standard limits.

The public MIL-STD-889C table could not be read. No numeric metal-table
anchor is claimed here. Boundary tests check software semantics only.
"""

from digitalmodel.corrosion.galvanic_screening import verdict_for_delta


def test_synthetic_ok_boundary():
    """Synthetic indices 0.8 and 0.6 V: |0.8-0.6|=0.2 V <= 0.2 V."""
    assert verdict_for_delta(abs(0.8 - 0.6), 0.2, 0.1) == "OK"


def test_synthetic_marginal_boundary():
    """Synthetic indices 0.9 and 0.6 V: difference=0.3 V = 0.2+0.1 V."""
    assert verdict_for_delta(abs(0.9 - 0.6), 0.2, 0.1) == "MARGINAL"


def test_synthetic_protect():
    """Synthetic indices 1.0 and 0.6 V: difference=0.4 V > 0.2+0.1 V."""
    assert verdict_for_delta(abs(1.0 - 0.6), 0.2, 0.1) == "PROTECT"
