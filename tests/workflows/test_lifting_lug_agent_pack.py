# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic arithmetic anchors for the pinned nominal lug model.

No AISC edition/clause is specified by workflow.py. Its factor provenance is
UNVERIFIED; these tests establish arithmetic only, not code compliance.
Dimensions, load, and the chosen yield stress are synthetic caller inputs.
"""

import pytest

from digitalmodel.lifting_lug.workflow import design_load_kn, lug_checks


def test_design_load_literal():
    """Synthetic factors: P = 100 kN × 2 × 1.5 = 300 kN."""
    assert design_load_kn(100, 2, 1.5) == pytest.approx(300)


def test_net_section_literal():
    """t=10 mm, hole=20 mm, R=30 mm: A=2×10×(30-20/2)=400 mm².

    P=20 kN=20000 N; stress=20000/400=50 MPa. Synthetic Fy=250 MPa;
    code factor TENSION_FACTOR (workflow.py; AISC edition/clause unspecified,
    UNVERIFIED) gives allowable=150 MPa; utilization=1/3.
    """
    check = lug_checks(20, 10, 20, 20, 30, 250)[1]
    assert check.demand_mpa == pytest.approx(50)
    assert check.allowable_mpa == pytest.approx(150)
    assert check.utilization == pytest.approx(1 / 3)
    assert check.passes


def test_tearout_literal():
    """Same synthetic geometry: two nominal planes A=2×10×20=400 mm².

    P=20000 N: tau=50 MPa. Code SHEAR_FACTOR (workflow.py; AISC
    edition/clause unspecified, UNVERIFIED) gives allowable=100 MPa for
    synthetic Fy=250 MPa; utilization=50/100=0.5. No empirical lug factors.
    """
    check = lug_checks(20, 10, 20, 20, 30, 250)[2]
    assert check.demand_mpa == pytest.approx(50)
    assert check.allowable_mpa == pytest.approx(100)
    assert check.utilization == pytest.approx(0.5)
    assert check.passes
