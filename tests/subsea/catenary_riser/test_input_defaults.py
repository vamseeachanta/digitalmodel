"""Offline input contracts; synthetic quantities, no solver qualification."""

from copy import deepcopy
from dataclasses import replace
import importlib
import math

import pandas as pd
import pytest

from digitalmodel.subsea.catenary_riser import (
    BuoyancyModule, EffectiveWeightCalculator, FluidProperties, LazyWaveAnalyzer,
    MaterialProperties, RiserConfiguration, SimpleCatenaryAnalyzer,
)


def test_section_six_input_contracts_bind_regressions():
    """Structure check only; the cited package calls establish behavior."""
    import ast
    import json
    from pathlib import Path
    root = Path(__file__).resolve().parents[3]
    docs = root / "src/digitalmodel/subsea/catenary_riser/docs"
    inventory = json.loads((docs / "qualification.json").read_text(encoding="utf-8"))
    records = [r for r in inventory["claims"] if "input_contract" in r]
    assert len(records) == 33
    for record in records:
        assert record["status"] == "TESTED"
        assert record["input_contract"]
        for node in record["tests"]:
            file, *names = node.split("::")
            source = ast.parse((root / "tests/subsea/catenary_riser" / file).read_text(encoding="utf-8"))
            for name in names:
                source = next(n for n in source.body
                              if isinstance(n, (ast.FunctionDef, ast.ClassDef)) and n.name == name)


@pytest.mark.parametrize("arguments", [(), ("case.v2.yml",)])
@pytest.mark.parametrize("module,default,prefix", [
    ("catenaryRiserSummary", "dataManager/catenarySummary.yml", "dataManager"),
    ("getFatigueLoading", "dataManager/fatigueLoadingTemplate.yml", "dataManager/catenary"),
])
def test_legacy_configuration_selection(module, default, prefix, arguments):
    package = importlib.import_module(
        f"digitalmodel.subsea.catenary_riser.legacy.{module}"
    )
    selected, name = package.configuration_inputs(list(arguments))
    assert selected == (f"{prefix}/case.v2.yml" if arguments else None)
    assert name == ("case.v2" if arguments else default.split("/")[-1][:-4])


@pytest.mark.parametrize("arguments", [[""], [None], ["a.yml", "b.yml"], ["a.txt"]])
@pytest.mark.parametrize("module", ["catenaryRiserSummary", "getFatigueLoading"])
def test_invalid_legacy_argument_never_selects_default(module, arguments):
    package = importlib.import_module(
        f"digitalmodel.subsea.catenary_riser.legacy.{module}"
    )
    with pytest.raises(ValueError, match="argument"):
        package.configuration_inputs(arguments)


@pytest.fixture
def fatigue_config():
    return {"EnvironmentLoadTemplate": {"Fatigue": [{"Wave": {"WaveTrains": [
        {"WaveDirection": 45}
    ]}}]}, "RANDOM250": [10, 20], "SimFilePrefix": "synthetic_"}


def test_fatigue_positive_bins_and_numeric_units(fatigue_config):
    from digitalmodel.subsea.catenary_riser.legacy.getFatigueLoading import prepareFatigueLoading
    # Two positive probabilities, one true zero. Heights in m, periods in s;
    # periods retain the legacy one-decimal convention, without locale coercion.
    frame = pd.DataFrame([[0, 20], [80, 0]], index=[1.5, 2.5], columns=[6.24, 8.26])
    before = deepcopy(fatigue_config)
    loads, rows = prepareFatigueLoading(frame, fatigue_config)
    waves = [load["Wave"]["WaveTrains"][0] for load in loads]
    assert [(w["WaveHs"], w["WaveTp"], w["WaveSeed"]) for w in waves] == [
        (2.5, 6.2, 10), (1.5, 8.3, 20),
    ]
    assert rows["Probability"].tolist() == [80, 20]
    assert fatigue_config == before
    waves[0]["WaveHs"] = 999
    assert waves[1]["WaveHs"] == 1.5


@pytest.mark.parametrize("field,value", [
    ("probability", -1), ("probability", math.nan), ("probability", math.inf),
    ("height", "1,5"), ("height", 0), ("height", math.nan),
    ("period", "6,2"), ("period", 0), ("period", math.inf),
    ("seed", 1.5), ("seed", math.nan), ("seed", True), ("seed", "10"),
])
def test_fatigue_invalid_inputs_fail_without_mutation(fatigue_config, field, value):
    from digitalmodel.subsea.catenary_riser.legacy.getFatigueLoading import prepareFatigueLoading
    frame = pd.DataFrame([[100.0]], index=[1.5], columns=[6.2])
    if field == "probability":
        frame.iloc[0, 0] = value
    elif field == "height":
        frame.index = [value]
    elif field == "period":
        frame.columns = [value]
    else:
        fatigue_config["RANDOM250"] = [value]
    before = deepcopy(fatigue_config)
    with pytest.raises(ValueError, match=field):
        prepareFatigueLoading(frame, fatigue_config)
    assert fatigue_config == before


def test_fatigue_empty_bins_and_seed_exhaustion(fatigue_config):
    from digitalmodel.subsea.catenary_riser.legacy.getFatigueLoading import prepareFatigueLoading
    loads, rows = prepareFatigueLoading(pd.DataFrame([[0]], index=[1], columns=[6]), fatigue_config)
    assert loads == [] and rows.empty
    fatigue_config["RANDOM250"] = []
    with pytest.raises(ValueError, match="seed"):
        prepareFatigueLoading(pd.DataFrame([[100]], index=[1], columns=[6]), fatigue_config)


@pytest.mark.parametrize("cfg", [None, {}, {"meta": None}, {"meta": []},
    {"calculation": "unknown"}, {"calculation": "catenary_riser_typo"}])
def test_router_refuses_missing_or_unsupported_calculation(cfg):
    from digitalmodel.subsea.catenary_riser.legacy.catenary import Catenary
    with pytest.raises(ValueError, match="calculation"):
        Catenary().router(cfg)


@pytest.mark.parametrize("metadata", [False, True])
def test_router_retains_supported_angle_calculation(metadata):
    from digitalmodel.subsea.catenary_riser.legacy.catenary import Catenary
    cfg = {"q": math.degrees(math.atan(3/4)), "d": 2}
    cfg.update({"meta": {"calculation": "catenary"}} if metadata else {"calculation": "catenary"})
    result = Catenary().router(cfg)
    assert result["S"] == pytest.approx(4)
    assert result["X"] == pytest.approx(3 * math.log(3))


@pytest.mark.parametrize("inputs", [(100, 20, 1000), (100, 20, 0), (math.nan, 20, 1000)])
def test_uncited_arch_estimate_is_quarantined(inputs):
    with pytest.raises(NotImplementedError, match="uncited"):
        LazyWaveAnalyzer().estimate_required_buoyancy(*inputs)


@pytest.mark.parametrize("analyzer", [EffectiveWeightCalculator, SimpleCatenaryAnalyzer, LazyWaveAnalyzer])
@pytest.mark.parametrize("gravity", [0, -1, math.nan, math.inf])
def test_analyzer_gravity_ranges(analyzer, gravity):
    with pytest.raises(ValueError, match="gravity"):
        analyzer(gravity=gravity)


def test_lazy_gravity_default_is_explicit_and_none_is_not_an_override():
    assert LazyWaveAnalyzer().g == 9.81
    with pytest.raises(ValueError, match="gravity"):
        LazyWaveAnalyzer(gravity=None)


@pytest.mark.parametrize("field,value", [
    ("youngs_modulus", 0), ("density", 0), ("poissons_ratio", -1),
    ("poissons_ratio", .5), ("yield_strength", -1), ("ultimate_strength", math.inf),
])
def test_material_constructor_ranges(field, value):
    with pytest.raises(ValueError, match=field):
        replace(MaterialProperties("synthetic", 100, .3, 4), **{field: value})


@pytest.mark.parametrize("field,value", [
    ("kinematic_viscosity", 0), ("kinematic_viscosity", math.nan), ("bulk_modulus", -1),
])
def test_fluid_optional_constructor_ranges(field, value):
    with pytest.raises(ValueError, match=field):
        FluidProperties("synthetic", 0, **{field: value})


@pytest.mark.parametrize("field,value", [
    ("name", ""), ("name", None), ("length", 0), ("outer_diameter", -1),
    ("start_length", -1), ("buoyancy_material_density", math.nan),
    ("outer_coating_thickness", .1),
])
def test_module_constructor_ranges(field, value):
    message = "outer_coating_density" if field == "outer_coating_thickness" else field
    with pytest.raises(ValueError, match=message):
        replace(BuoyancyModule("synthetic", 2, 3, 1, 0), **{field: value})


def test_simple_configured_gravity_and_default_override_hand_case():
    # E1/E4: steel area=.75pi, internal=.25pi, external=pi m².
    # Densities=4,1,2 kg/m³: net mass=1.25pi kg/m.
    # At g=2, w=2.5pi N/m; choose a=3 m, z=2 m: s=4 m,
    # H=7.5pi N and T=12.5pi N. At g=4 every force doubles.
    riser = RiserConfiguration("synthetic", 2, .5, 20,
        MaterialProperties("synthetic", 1, 0, 4), FluidProperties("internal", 1),
        FluidProperties("external", 2), water_depth=2, gravity=2)
    for analyzer, gravity in [(SimpleCatenaryAnalyzer(), 2), (SimpleCatenaryAnalyzer(4), 4)]:
        result = analyzer.analyze_riser(riser, horizontal_tension=3 * 1.25 * math.pi * gravity)
        assert result.effective_weight == pytest.approx(1.25 * math.pi * gravity)
        assert result.arc_length == pytest.approx(4)
        assert result.top_tension == pytest.approx(6.25 * math.pi * gravity)
    assert riser.gravity == 2
    assert replace(riser, gravity=9.81).gravity == 9.81


@pytest.fixture
def legacy_buoyancy():
    # Synthetic inch geometry with SI densities and force per length.
    return {"InsulationSection": {"OD": 2},
        "Material": {"SeaWater": {"Rho": 4}, "Buoyancy": {"Rho": 2}},
        "default": {"Constants": {"g": 2}},
        "commonDefinition": {"UniformBuoyancy": {"Thickness": None, "BuoyancyFactor": .5},
                             "DiscreteBuoyancy": {"BuoyancyCoverage": 25}},
        "catenaryResult": {"Reference_FluidFilled": {"weightPerUnitLength": 8 * math.pi * .0254**2},
                           "FluidFilled": {"weightPerUnitLength": 12 * math.pi * .0254**2}}}


@pytest.mark.parametrize("gravity", [2, 4])
def test_legacy_buoyancy_balance_uses_factor_and_gravity(legacy_buoyancy, gravity):
    from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import buoyancyProperties
    cfg = legacy_buoyancy
    cfg["default"]["Constants"]["g"] = gravity
    # Requested uplift=.5*8pi*inch²=4pi*inch² N/m.
    # A=uplift/[(4-2)*g]=2pi*inch²/g m²; OD²=4+8/g inch².
    # At 25% coverage the annular area is multiplied by four.
    # The legacy caller passes reference-fluid weight, not actual-fluid weight.
    result = buoyancyProperties(cfg, 8 * math.pi * .0254**2)
    uniform = result["commonDefinition"]["UniformBuoyancy"]
    assert uniform["BuoyancyOD"] == pytest.approx(math.sqrt(4 + 8 / gravity))
    assert result["commonDefinition"]["DiscreteBuoyancy"]["BuoyancyOD"] == pytest.approx(
        math.sqrt(4 + 32 / gravity))
    assert result["lazyWaveCatenaryResult"]["WeightPerUnitLengthWithBuoyancy"] == pytest.approx(
        8 * math.pi * .0254**2)


def test_legacy_buoyancy_thickness_branch(legacy_buoyancy):
    from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import buoyancyProperties
    cfg = legacy_buoyancy
    cfg["commonDefinition"]["UniformBuoyancy"].update(Thickness=.5, BuoyancyFactor=None)
    # OD=3 inch; annulus=1.25pi inch²; uplift=(4-2)*2*1.25pi=5pi inch² N/m.
    result = buoyancyProperties(cfg, 8 * math.pi * .0254**2)
    assert result["commonDefinition"]["UniformBuoyancy"]["BuoyancyFactor"] == pytest.approx(5/8)
    assert result["lazyWaveCatenaryResult"]["WeightPerUnitLengthWithBuoyancy"] == pytest.approx(
        7 * math.pi * .0254**2)


@pytest.mark.parametrize("field,value", [("g", 0), ("g", math.nan), ("BuoyancyCoverage", 0),
    ("BuoyancyCoverage", 101), ("BuoyancyFactor", -.1), ("BuoyancyFactor", math.inf)])
def test_legacy_buoyancy_ranges(legacy_buoyancy, field, value):
    from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import buoyancyProperties
    if field == "g":
        legacy_buoyancy["default"]["Constants"][field] = value
    elif field == "BuoyancyCoverage":
        legacy_buoyancy["commonDefinition"]["DiscreteBuoyancy"][field] = value
    else:
        legacy_buoyancy["commonDefinition"]["UniformBuoyancy"][field] = value
    before = deepcopy(legacy_buoyancy)
    with pytest.raises(ValueError, match=field):
        buoyancyProperties(legacy_buoyancy, 1)
    assert legacy_buoyancy == before


@pytest.mark.parametrize("lazy", [False, True])
def test_legacy_plot_origin_is_hangoff(monkeypatch, lazy):
    from digitalmodel.subsea.catenary_riser.legacy import catenaryMethods as module
    monkeypatch.setattr(module, "genericPlot", lambda data: None)
    # E1 translated to hangoff x=0, z=-5 below MSL. a=3, d=2,
    # x=3*acosh(5/3); touchdown z=-7. Plotting makes no force claim.
    x = 3 * math.acosh(5/3)
    if lazy:
        data = {"LazyWaveCatenaryDefinition": {"HangoffBelowMeanSeaLevel": 5,
                "HogBendAboveSeabed": 4, "SagBendElevationAboveSeabed": 2,
                "VerticalDistance": 10},
            "Analysis": {"result_folder": "", "file_name": "unused"},
            "lazyWaveCatenaryResult": {
                "HangOff": {"BendRadius": 3, "X": x, "d": 2},
                "SagToBuoyancy": {"X": 1}, "BuoyancyToHog": {"X": 1, "BendRadius": 3},
                "Summary": {"HangOffToBuoyancy": {"X": x},
                            "Buoyancy": {"X": 2}, "BuoyancyToTouchDown": {"X": x},
                            "HangoffToTDP": {"X": 2*x+2}},
                "BuoyancyToTouchDown": {"X": x, "BendRadius": 3},
                "HogToBuoyancy": {"BendRadius": 3, "X": 1},
                "BuoyancyToSag": {"X": 1},
                "TouchDown": {"BendRadius": 3, "X": x, "d": 2}}}
        module.lazyWavePlot(data)
        points = data["lazyWaveCatenaryResult"]["Summary"]["HangOffToBuoyancy"]["PlotData"]
    else:
        data = {"catenaryResult": {"BendRadius": 3, "X": x, "d": 2},
                "commonDefinition": {"HangoffBelowMeanSeaLevel": 5},
                "Analysis": {"result_folder": "", "file_name": "unused"}}
        module.simple_catenary_plot(data)
        points = data["catenaryResult"]["PlotData"]
    assert points["X"][0] == 0
    assert points["Y"][0] == pytest.approx(-5)


@pytest.mark.parametrize("value", [True, "2", None, complex(2, 0)])
def test_dimensional_inputs_are_real_numbers(value):
    with pytest.raises(ValueError, match="density"):
        FluidProperties("invalid", value)


def test_legacy_declination_grid_preserves_fractional_angles():
    from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import gethorizontalDistanceHangofftoTDPrange
    from digitalmodel.subsea.catenary_riser.legacy.catenary_equation import CatenaryCalculator
    target = CatenaryCalculator().calculate({"d": 10, "q": .75})["X"]
    cfg = {"simpleCatenaryDefinition": {"verticalDistance": 10, "axialLineForce": None,
        "horizontalDistanceHangofftoTDP": {"range": [target]}},
        "catenaryResult": {"X": 1, "q": .85},
        "commonDefinition": {"TDPToAnchor": {"SCR": 200}, "LengthBeforeHangoff": 0}}
    with pytest.warns(DeprecationWarning):
        rows = gethorizontalDistanceHangofftoTDPrange(cfg)
    assert rows[0]["q"] == .75
    # Returned coordinates are rounded to three decimal places by the legacy API.
    assert rows[0]["X"] == pytest.approx(target, abs=.0005)


@pytest.mark.parametrize("data", [
    {"OD": 0, "ID": 0}, {"OD": 1, "ID": -1},
    {"OD": 1, "ID": 2}, {"OD": math.inf, "ID": 0},
])
def test_legacy_section_ranges(data):
    from digitalmodel.subsea.catenary_riser.legacy.pipeProperties import sectionProperties
    with pytest.raises(ValueError):
        sectionProperties(data)


@pytest.mark.parametrize("gravity", [0, math.inf, math.nan])
def test_legacy_equivalent_pipe_rejects_invalid_gravity(gravity):
    from digitalmodel.subsea.catenary_riser.legacy.pipeProperties import equivalentPipe
    cfg = {"default": {"Constants": {"g": gravity}}}
    with pytest.raises(ValueError, match="g"):
        equivalentPipe(cfg, 1)


@pytest.mark.parametrize("branch", ["bare", "strakes", "buoyancy"])
def test_legacy_equivalent_pipe_unit_branches(branch):
    from digitalmodel.subsea.catenary_riser.legacy.pipeProperties import equivalentPipe
    cfg = {"SteelSection": {"A": 3, "Ai": 1},
        "InsulationSection": {"A": 0, "Ao": 4}, "BuoyancySection": {"A": 1, "Ao": 5},
        "Material": {"Steel": {"Rho": 4}, "ExternalCoating": {"Rho": 0},
                     "SeaWater": {"Rho": 2}, "Buoyancy": {"Rho": 1},
                     "Strakes": {"MassPerUnitLength": .5, "WeightPerUnitLength": .3}},
        "geometry": {"Strakes": {"BaseThickness": .1 if branch == "strakes" else None}},
        "default": {"Constants": {"g": 2}}}
    output = equivalentPipe(cfg, FluidDensity=1, Buoyancy=branch == "buoyancy")["equivalentPipe"]
    values = output["WithBuoyancy" if branch == "buoyancy" else "WithoutBuoyancy"]
    # Inch-square conversion u=.0254²; steel/contents mass=12u+1u.
    # Bare displacement=8u; net force=2*(13-8)u=10u N/m.
    # Foam adds 1u mass and changes displacement to 10u: force=8u N/m.
    # Explicit strake values are already kg/m and N/m and are added once.
    u = .0254**2
    mass = 14*u if branch == "buoyancy" else 13*u
    force = 8*u if branch == "buoyancy" else 10*u
    if branch == "strakes":
        mass += .5
        force += .3
    assert values["massPerUnitLength"] == pytest.approx(mass)
    assert values["weightPerUnitLength"] == pytest.approx(force)


def test_cli_errors_and_quarantine_do_not_replace_existing_output(tmp_path):
    from click.testing import CliRunner
    from digitalmodel.subsea.catenary_riser.cli import cli
    output = tmp_path / "existing.json"
    output.write_text("preserved")
    for command in ["simple", "weight", "lazy-wave"]:
        args = [command, "--diameter", "1", "--thickness", "1"]
        if command != "weight":
            args += ["--length", "20", "--water-depth", "2", "--offset", "3"]
        result = CliRunner().invoke(cli, args + ["--output", str(output)])
        assert result.exit_code == 1 and "wall_thickness" in result.output
        assert output.read_text() == "preserved"
    result = CliRunner().invoke(cli, ["lazy-wave", "--diameter", "1", "--thickness", ".1",
        "--length", "2000", "--water-depth", "1000", "--offset", "500",
        "--buoy-length", "200", "--buoy-diameter", "2", "--buoy-start", "300",
        "--output", str(output)])
    assert result.exit_code == 1 and "quarantined" in result.output
    assert output.read_text() == "preserved"
