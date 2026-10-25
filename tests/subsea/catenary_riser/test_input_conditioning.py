# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic input controls; E1-E4 in docs/engineering-basis.md.

Mass times configured acceleration must scale every weight by the same ratio.
Geometric limits below are definitions, not material acceptance limits.
"""

from dataclasses import replace
import math

import pytest
from click.testing import CliRunner

from digitalmodel.subsea.catenary_riser import (
    BuoyancyModule,
    EffectiveWeightCalculator,
    FluidProperties,
    LazyWaveConfiguration,
    MaterialProperties,
    RiserConfiguration,
    SimpleCatenaryAnalyzer,
    AIR,
    SEAWATER,
    get_fluid,
)
from digitalmodel.subsea.catenary_riser.cli import cli
from digitalmodel.subsea.catenary_riser.legacy.catenary_equation import CatenaryCalculator


@pytest.fixture
def riser():
    return RiserConfiguration(
        "synthetic", 2, .5, 20, MaterialProperties("synthetic", 1, 0, 4),
        FluidProperties("contents", 1), FluidProperties("external", 2),
        coating_thickness=.1, coating_density=3, water_depth=2, horizontal_offset=3,
        gravity=2,
    )


@pytest.mark.parametrize("field,value", [
    ("outer_diameter", 0), ("outer_diameter", math.nan), ("wall_thickness", 0),
    ("wall_thickness", 1), ("length", -1), ("gravity", 0),
    ("coating_thickness", -.1), ("coating_density", -1),
    ("water_depth", 0), ("horizontal_offset", math.inf), ("top_angle", 90),
])
def test_constructor_ranges_are_named(riser, field, value):
    with pytest.raises(ValueError, match=field):
        replace(riser, **{field: value})


def test_missing_coating_density_is_rejected(riser):
    with pytest.raises(ValueError, match="coating_density"):
        replace(riser, coating_density=None)


@pytest.mark.parametrize("density", [-1, math.inf, math.nan])
def test_density_invalid(density):
    with pytest.raises(ValueError, match="density"):
        FluidProperties("synthetic", density)


def test_configured_gravity_and_calculator_override(riser):
    """OD=2, ID=1, coated OD=2.2: Asteel=.75pi, Ai=.25pi, Acoat=.21pi.

    At g=2: steel=6pi, coating=1.26pi, contents=.5pi,
    displacement=4.84pi and net=2.92pi N/m. At g=4 every term doubles.
    """
    result = EffectiveWeightCalculator().calculate(riser)
    assert result.steel_weight == pytest.approx(6 * math.pi)
    assert result.coating_weight == pytest.approx(1.26 * math.pi)
    assert result.contents_weight == pytest.approx(.5 * math.pi)
    assert result.buoyancy == pytest.approx(4.84 * math.pi)
    assert result.effective_weight == pytest.approx(2.92 * math.pi)
    override = EffectiveWeightCalculator(gravity=4).calculate(riser)
    assert override.effective_weight == pytest.approx(5.84 * math.pi)
    assert riser.gravity == 2
    # At a=3 and d=2, s=4. Changing g doubles w, H and T, preserving shape.
    solved = SimpleCatenaryAnalyzer(gravity=4).analyze_riser(
        riser, horizontal_tension=3 * 5.84 * math.pi,
    )
    assert solved.arc_length == pytest.approx(4)


def test_module_gravity_and_duplicate_keys(riser):
    module = BuoyancyModule("foam", 2, 3, 1, 1)
    # Annulus area=(9-4.84)*pi/4=1.04pi; (rho_foam-rho_water)*g=-2.
    calc = EffectiveWeightCalculator()
    result = calc.calculate_with_buoyancy_module(riser, module)
    assert result.buoyancy_module_force == pytest.approx(-2.08 * math.pi)
    profile = LazyWaveConfiguration(riser, [module]).get_effective_weight_profile()
    assert profile["foam"] == pytest.approx(.84 * math.pi)
    with pytest.raises(ValueError, match="duplicate"):
        LazyWaveConfiguration(riser, [module, module])
    with pytest.raises(ValueError, match="reserved"):
        LazyWaveConfiguration(riser, [replace(module, name="bare_riser")])
    with pytest.raises(ValueError, match="overlap"):
        LazyWaveConfiguration(riser, [module, replace(module, name="other")])
    with pytest.raises(ValueError, match="length"):
        LazyWaveConfiguration(riser, [replace(module, start_length=19)])


@pytest.mark.parametrize("alias,expected", [
    ("water", SEAWATER), ("WATER", SEAWATER), ("empty", AIR), ("Empty", AIR),
])
def test_fluid_aliases_preserve_preset_identity(alias, expected):
    # Alias compatibility only; preset densities remain unqualified metadata.
    assert get_fluid(alias) is expected


@pytest.mark.parametrize("gravity", [2, 4])
def test_coated_module_gravity_hand_balance(riser, gravity):
    """Synthetic coated diameters: riser=2.2, foam=3, module=3.2 m.

    Annular areas are 1.04pi (foam), .31pi (outer coating), 1.35pi
    (displaced fluid). Densities 1, 3, 2 give net module force
    (1.04 + .93 - 2.70)pi*g = -.73pi*g N/m. Bare weight is
    1.46pi*g N/m, so the combined section weighs .73pi*g N/m.
    """
    module = BuoyancyModule(
        "foam", 2, 3, 1, 1, outer_coating_thickness=.1, outer_coating_density=3,
    )
    configured = replace(riser, gravity=gravity)
    expected_force = -.73 * math.pi * gravity
    expected_section = .73 * math.pi * gravity
    assert module.buoyancy_force_per_length(
        configured.external_fluid, 2.2, gravity=gravity,
    ) == pytest.approx(expected_force)
    for calculator, source in (
        (EffectiveWeightCalculator(), configured),
        (EffectiveWeightCalculator(gravity=gravity), riser),
    ):
        result = calculator.calculate_with_buoyancy_module(source, module)
        assert result.effective_weight == pytest.approx(1.46 * math.pi * gravity)
        assert result.buoyancy_module_force == pytest.approx(expected_force)
        assert result.effective_weight_with_buoyancy == pytest.approx(expected_section)
    profile = LazyWaveConfiguration(configured, [module]).get_effective_weight_profile()
    assert profile["bare_riser"] == pytest.approx(1.46 * math.pi * gravity)
    assert profile["foam"] == pytest.approx(expected_section)
    assert riser.gravity == 2


@pytest.mark.parametrize("thickness,density,module_force", [
    (None, None, -2.5), (0, None, -2.5), (.1, 0, -3.74),
])
def test_absent_or_zero_density_coating_weight(riser, thickness, density, module_force):
    uncoated = replace(riser, coating_thickness=thickness, coating_density=density)
    assert uncoated.coating_weight_per_length == 0
    # Bare riser OD=2. Module foam area=1.25pi; g=2, densities=1 and 2.
    # A zero-density .1 m shell adds .31pi displaced area, hence -1.24pi N/m.
    module = BuoyancyModule(
        "foam", 2, 3, 1, 1,
        outer_coating_thickness=thickness, outer_coating_density=density,
    )
    assert module.buoyancy_force_per_length(
        riser.external_fluid, 2, gravity=2,
    ) == pytest.approx(module_force * math.pi)


@pytest.mark.parametrize("angle", [0, 90, -1, math.nan, math.inf])
def test_legacy_invalid_angles_fail(angle):
    with pytest.raises(ValueError, match="q"):
        CatenaryCalculator().calculate({"q": angle, "d": 2})


@pytest.mark.parametrize("data,error", [
    ({}, ValueError), ({"F": None, "q": None, "X": None}, ValueError),
    ({"X": 1}, NotImplementedError),
])
def test_legacy_no_branch_is_loud(data, error):
    from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import catenaryEquation
    with pytest.raises(error):
        catenaryEquation(data)


def test_explicit_force_requires_no_offset_and_rejects_ambiguity(riser):
    analyzer = SimpleCatenaryAnalyzer()
    no_offset = replace(riser, horizontal_offset=None)
    result = analyzer.analyze_riser(no_offset, horizontal_tension=30)
    assert result.converged
    with pytest.raises(ValueError, match="one"):
        analyzer.analyze_riser(riser, horizontal_tension=30, top_tension=50)
    with pytest.raises(ValueError, match="offset"):
        analyzer.analyze_riser(no_offset)


@pytest.mark.parametrize("command", ["simple", "weight", "lazy-wave"])
def test_cli_invalid_geometry_never_writes(command, tmp_path):
    args = [command, "--diameter", "1", "--thickness", "1"]
    if command != "weight":
        args += ["--length", "20", "--water-depth", "2", "--offset", "3"]
    output = tmp_path / "absent.json"
    result = CliRunner().invoke(cli, args + ["--output", str(output)])
    assert result.exit_code != 0
    assert "wall_thickness" in result.output
    assert not output.exists()


def test_zero_top_tension_is_not_ignored():
    result = CliRunner().invoke(cli, [
        "simple", "--diameter", ".508", "--thickness", ".025", "--length", "1500",
        "--water-depth", "1000", "--offset", "500", "--top-tension", "0",
    ])
    assert result.exit_code != 0
    assert "top_tension" in result.output


def test_legacy_inch_areas_and_configured_gravity():
    """Hand arithmetic for the legacy inch-area boundary, basis E4.

    International inch=0.0254 m exactly (NIST SP 811, 2008, Appendix B.8,
    length conversion table; the public PDF is linked in route-qualification.md).
    Synthetic areas 3,1,4 inch² and densities 4,1,2 kg/m³ at g=2:
    mass=13*(.0254²) kg/m, displaced=8*(.0254²) kg/m;
    net weight=10*(.0254²) N/m. No pipe geometry inference is reused.
    """
    from digitalmodel.subsea.catenary_riser.legacy.pipeProperties import equivalentPipe
    cfg = {
        "SteelSection": {"A": 3, "Ai": 1},
        "InsulationSection": {"A": 0, "Ao": 4},
        "Material": {"Steel": {"Rho": 4}, "ExternalCoating": {"Rho": 0},
                     "SeaWater": {"Rho": 2}},
        "geometry": {"Strakes": {"BaseThickness": None}},
        "default": {"Constants": {"g": 2}},
    }
    out = equivalentPipe(cfg, FluidDensity=1)["equivalentPipe"]["WithoutBuoyancy"]
    assert out["massPerUnitLength"] == pytest.approx(13 * .0254**2)
    assert out["weightPerUnitLength"] == pytest.approx(10 * .0254**2)


@pytest.mark.parametrize("analysis", ["Extreme", "Fatigue"])
@pytest.mark.parametrize("state", ["disabled", "no_loads", "no_riser"])
def test_inactive_export_refuses_before_output(analysis, state, tmp_path):
    from digitalmodel.subsea.catenary_riser.legacy.orcaflexModel import build_model
    cfg = {"default": {"Analysis": {
        "SCR": state != "no_riser", "SLWR": False, analysis: state != "disabled",
    }}, "EnvironmentLoad": {analysis: [] if state == "no_loads" else [{}]}}
    with pytest.raises(ValueError, match="export"):
        build_model(analysis, cfg)
    assert not list(tmp_path.iterdir())
