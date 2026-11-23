# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""The worked example returns the independently calculated documented answers."""

import runpy
from pathlib import Path


def test_study_value_example():
    """Independent hand checks, with synthetic operands from the guide.

    NIST SP 811 (2008), Appendix B.8: 10*0.3048 = 3.048 m.
    Length additivity: 12.5+7.5+10 = 30 m.
    OpenStax UP1 (2016), 14.1, eq. 14.4: 1000*10*2.5 = 25000 Pa.
    OpenStax UP1 (2016), 12.3, eq. 12.36:
    (2000*2)/(200000000*0.01) = 4000/2000000 = 0.002 m.
    No native solver or geometry result is implied by these hand checks.
    """
    script = (
        Path(__file__).resolve().parents[2] / "examples/reporting/study_value_checks.py"
    )
    result = runpy.run_path(str(script))["build_example"]()
    assert result["dimension_m"] == 3.048
    assert result["riser_total_m"] == 30.0
    assert result["pressure_pa"] == 25000.0
    assert result["bar_elongation_m"] == 0.002
    assert '>25000.0</span> <span class="units">Pa</span>' in result["pressure_html"]
    assert result["native_bar_solution"] == "NOT_EVALUATED"
    assert result["native_box_export"] == "NOT_EVALUATED"
