"""Characteristic-yield regressions for the F201 external-pressure criterion."""

from dataclasses import asdict

import numpy as np
import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignFactors, DesignLoads, FabricationType, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import (
    DnvStF101Strategy,
)
from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f201 import (
    DnvStF201Strategy,
)
from digitalmodel.structural.pipe_capacity.custom.DNV_OS_F201 import (
    dnv_os_f201_lrfd_wsd as legacy,
)


@pytest.mark.parametrize("alpha_u", [0.96, 1.0])
@pytest.mark.parametrize("fabrication_type", list(FabricationType))
@pytest.mark.parametrize("thickness", [0.012, 0.022225, 0.05])
def test_external_collapse_uses_same_characteristic_yield_as_other_resistances(
    alpha_u, fabrication_type, thickness,
):
    geometry = PipeGeometry(0.5334, thickness, corrosion_allowance=0.003)
    material = PipeMaterial("screening", 552e6, 621e6,
                            fabrication_type=fabrication_type)
    original_material = asdict(material)
    loads = DesignLoads(2e6, 20e6, 1e6, 2e6)
    strategy = DnvStF201Strategy(alpha_U=alpha_u)
    utilisation, details = strategy.run_checks(
        geometry, material, loads, DesignFactors())["combined"]
    diameter, t2 = geometry.outer_diameter, geometry.t2
    fy = material.smys * alpha_u
    pp = fy * material.alpha_fab * 2 * t2 / diameter
    assert details["p_p"] == pp
    assert details["collapse_smys"] == material.smys
    assert details["collapse_f_y"] == details["f_y"] == fy
    assert asdict(material) == original_material
    # Independent cubic roots in MPa, rather than the production bisection.
    pel = 2 * material.youngs_modulus * (t2 / diameter)**3 / (1 - material.poissons_ratio**2)
    elastic, plastic = pel / 1e6, pp / 1e6
    coeff = elastic * plastic * details["f_0"] * diameter / t2
    roots = np.roots([1, -elastic, -plastic**2 - coeff, elastic * plastic**2])
    pc = min(root.real for root in roots if abs(root.imag) < 1e-12 and root.real > 0) * 1e6
    assert details["p_c"] == pytest.approx(pc, abs=1.0)
    expected = legacy.combined_loading_external_overpressure(
        loads.bending_moment, details["Mk"], loads.effective_tension,
        details["Tk"], loads.external_pressure, loads.internal_pressure, pc,
        gamma_sc=details["gamma_sc"], gamma_m=details["gamma_m"],
    )
    assert utilisation == pytest.approx(expected.utilisation, rel=1e-7)
    # F201's adaptation shall not change the shared F101 strategy or caller.
    _, shared = DnvStF101Strategy()._check_collapse(
        geometry, material, loads, DesignFactors())
    assert shared["p_p"] == material.smys * material.alpha_fab * 2 * t2 / diameter
