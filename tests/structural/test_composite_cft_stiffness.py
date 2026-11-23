# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""CFT stiffness from independent concentric-circle and polygon integrals.

All material and geometry inputs below are synthetic test choices, not design
data. With MPa = N/mm^2 and dimensions in mm, EA is in N, EI in N mm^2,
and the steel-transformed second moment I = EI/E_s is in mm^4.
"""

import importlib.util
import json
import math
from pathlib import Path
from unittest.mock import Mock

import matplotlib.figure
import pytest
import sectionproperties.analysis
from sectionproperties.analysis import Section
from sectionproperties.pre import Material
from sectionproperties.pre.geometry import CompoundGeometry


SCRIPT = (
    Path(__file__).resolve().parents[2]
    / "scripts/integrations/sectionproperties_composite_poc.py"
)


def _load_composite():
    spec = importlib.util.spec_from_file_location("composite_cft_poc", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_composite_import_is_inert(tmp_path, monkeypatch, capsys):
    """Import must neither mesh/solve nor create output anywhere."""
    monkeypatch.chdir(tmp_path)
    operations = [
        (CompoundGeometry, "create_mesh"),
        (sectionproperties.analysis, "Section"),
        (Path, "mkdir"),
        (Path, "write_text"),
        (matplotlib.figure.Figure, "savefig"),
    ]
    guards = []
    for target, name in operations:
        guard = Mock(side_effect=AssertionError(f"Import must not call {name}"))
        monkeypatch.setattr(target, name, guard)
        guards.append(guard)

    module = _load_composite()

    assert callable(module.run_cft_case)
    for guard in guards:
        guard.assert_not_called()
    assert list(tmp_path.iterdir()) == []
    assert capsys.readouterr().out == ""


@pytest.mark.parametrize(
    "diameter,thickness,steel_modulus,concrete_modulus",
    [(80.0, 8.0, 210_000.0, 35_000.0), (60.0, 6.0, 180_000.0, 24_000.0)],
)
def test_cft_effective_stiffness_hand_integrals(
    diameter, thickness, steel_modulus, concrete_modulus, tmp_path, monkeypatch
):
    """Both angular and triangular refinement retain the derived error bound.

    Independent hand integrals: A = integral(r dr dphi) = pi R^2 and
    I_x = integral(r^3 sin(phi)^2 dr dphi) = pi R^4/4. Subtract inner
    from outer circles for steel; use the inner circle for concrete.

    For an inscribed regular n-gon, theta = 2*pi/n. Integrating its n
    centre-to-edge triangles gives A_n = n R^2 sin(theta)/2 and
    I_n = n R^4 sin(theta) (2 + cos(theta))/24. Hence the polygon/circle
    ratios are sin(theta)/theta and sin(theta)/theta*(2+cos(theta))/3.
    The same factors apply to each concentric material and their E-weighted
    sums. From sin(theta)/theta >= 1-theta^2/6 and cos(theta) >= 1-theta^2/2,
    the relative deficits are at most theta^2/6 for EA and theta^2/3 for I.
    These bounds apply here since theta < 1 and the lower factors are positive.
    """
    # Stop the old eager import before it meshes or solves any example.
    with monkeypatch.context() as import_guard:
        import_guard.setattr(
            CompoundGeometry,
            "create_mesh",
            Mock(side_effect=AssertionError("Import must not create a mesh")),
        )
        module = _load_composite()

    # Poisson ratios, strengths and densities are arbitrary synthetic choices;
    # geometric EA/EI depends only on the supplied elastic moduli and shapes.
    steel = Material("Synthetic steel", steel_modulus, 0.3, 300.0, 1.0, "blue")
    concrete = Material(
        "Synthetic concrete", concrete_modulus, 0.2, 30.0, 1.0, "grey"
    )
    for name in ("calculate_warping_properties", "calculate_plastic_properties"):
        monkeypatch.setattr(
            Section, name, Mock(side_effect=AssertionError("Only geometry is needed"))
        )

    outer_radius = diameter / 2
    inner_radius = outer_radius - thickness
    steel_area = math.pi * (outer_radius**2 - inner_radius**2)
    concrete_area = math.pi * inner_radius**2
    steel_i = math.pi * (outer_radius**4 - inner_radius**4) / 4
    concrete_i = math.pi * inner_radius**4 / 4
    expected_ea = steel_modulus * steel_area + concrete_modulus * concrete_area
    expected_ei = steel_modulus * steel_i + concrete_modulus * concrete_i
    expected_transformed_i = expected_ei / steel_modulus

    # Floating-point allowance only: ten times sqrt(machine epsilon), separate
    # from the geometric bound. Both meshes have only a few thousand elements.
    roundoff = 10 * math.sqrt(math.ulp(1.0))
    previous_errors = None
    previous_nodes = None
    # Synthetic discretizations: halve edge angle and quarter triangle area.
    for sides, mesh_area in ((32, 16.0), (64, 4.0)):
        output_dir = tmp_path / f"polygon-{sides}"
        section, results = module.run_cft_case(
            outer_diameter=diameter,
            wall_thickness=thickness,
            steel_material=steel,
            concrete_material=concrete,
            n=sides,
            mesh_sizes=(mesh_area, mesh_area),
            output_dir=output_dir,
        )

        theta = 2 * math.pi / sides
        area_ratio = math.sin(theta) / theta
        inertia_ratio = area_ratio * (2 + math.cos(theta)) / 3
        assert results["EA_N"] == pytest.approx(expected_ea * area_ratio, rel=roundoff)
        for axis in ("xx", "yy"):
            # EI has force-length-squared units; transformed I has length^4.
            assert results[f"EI{axis}_Nmm2"] == pytest.approx(
                expected_ei * inertia_ratio, rel=roundoff
            )
            assert results[f"I{axis}_steel_mm4"] == pytest.approx(
                expected_transformed_i * inertia_ratio, rel=roundoff
            )
            assert results[f"EI{axis}_Nmm2"] == pytest.approx(
                steel_modulus * results[f"I{axis}_steel_mm4"], rel=roundoff
            )

        ea_error = abs(results["EA_N"] / expected_ea - 1)
        i_error = abs(results["Ixx_steel_mm4"] / expected_transformed_i - 1)
        assert ea_error <= theta**2 / 6 + roundoff
        assert i_error <= theta**2 / 3 + roundoff
        if previous_errors is not None:
            assert ea_error < previous_errors[0]
            assert i_error < previous_errors[1]
            assert len(section.mesh_nodes) > previous_nodes
        previous_errors = ea_error, i_error
        previous_nodes = len(section.mesh_nodes)

        assert "EJ_eff_mm4" not in results
        saved = output_dir / "sectionproperties_cft_results.json"
        assert json.loads(saved.read_text()) == results
        assert list(output_dir.iterdir()) == [saved]
