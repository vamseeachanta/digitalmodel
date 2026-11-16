# ABOUTME: Regression tests for CPIP density provenance and pressure disclosure.
# ABOUTME: Pressure expectations use a known fluid load, pump area and depth.

import json

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard import calculations
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults,
    CardData,
    CPIPAnalysis,
    DynacardAnalysisContext,
    FluidLoadAnalysis,
    InputParameters,
    PumpProperties,
    WellTestData,
)


@pytest.fixture
def card():
    return CardData(position=[0, 0, 100, 100], load=[0, 1000, 1000, 0])


@pytest.fixture
def context(card):
    return DynacardAnalysisContext(
        api14="CPIP-TEST", surface_card=card, rod_string=[], spm=10,
        pump=PumpProperties(diameter=2, depth=1440),
        input_params=InputParameters(strokes_per_minute=10, tubing_pressure=120),
    )


@pytest.fixture(autouse=True)
def known_fluid_load(monkeypatch):
    monkeypatch.setattr(
        calculations, "calculate_fluid_load",
        lambda *args, **kwargs: FluidLoadAnalysis(fluid_load=250),
    )


@pytest.mark.parametrize("density", [72.0, DynacardAnalysisContext.model_fields["fluid_density"].default])
def test_constructor_density_overrides_conflicting_well_test(context, card, density):
    supplied = DynacardAnalysisContext(
        **context.model_dump(exclude_unset=True), fluid_density=density,
        well_test=WellTestData(water_cut=100),
    )
    result = calculations.calculate_cpip(supplied, card)
    expected_pdp = 120 + density / 144 * 1440
    assert result.fluid_density == density
    assert result.tubing_gradient == pytest.approx(density / 144)
    assert result.pump_discharge_pressure == pytest.approx(expected_pdp)
    assert result.pump_intake_pressure == pytest.approx(expected_pdp - 250 / context.pump_area)
    assert result.density_source == "context"


@pytest.mark.parametrize("supply", ["assignment", "copy"])
def test_density_supplied_after_construction(context, card, supply):
    assert "fluid_density" not in context.model_fields_set
    if supply == "assignment":
        context.fluid_density = 72
    else:
        context = context.model_copy(update={"fluid_density": 72})
    result = calculations.calculate_cpip(context, card)
    assert result.fluid_density == 72
    assert result.tubing_gradient == 0.5
    assert result.density_source == "context"


@pytest.mark.parametrize("water_cut, source", [(80, "well_test_mixture"), (None, "assumed_50pct_mixture")])
def test_omitted_density_keeps_mixture(context, card, water_cut, source):
    if water_cut is not None:
        context.well_test = WellTestData(water_cut=water_cut)
    assert "fluid_density" not in context.model_fields_set
    wc = (50 if water_cut is None else water_cut) / 100
    mixed_sg = 1.03 * wc + (141.5 / (35 + 131.5)) * (1 - wc)
    result = calculations.calculate_cpip(context, card)
    assert result.fluid_density == pytest.approx(62.4 * mixed_sg)
    assert result.tubing_gradient == pytest.approx(0.433 * mixed_sg)
    expected_pdp = 120 + 0.433 * mixed_sg * 1440
    assert result.pump_discharge_pressure == pytest.approx(expected_pdp)
    assert result.pump_intake_pressure == pytest.approx(expected_pdp - 250 / context.pump_area)
    assert result.density_source == source


@pytest.mark.parametrize("density", [0, -1, float("nan"), float("inf"), -float("inf")])
def test_invalid_supplied_density_is_rejected(context, card, density):
    context.fluid_density = density
    context.well_test = WellTestData(water_cut=80)
    with pytest.raises(ValueError, match="fluid_density.*finite.*positive"):
        calculations.calculate_cpip(context, card)


@pytest.mark.parametrize("basis", ["context", "well_test_mixture", "assumed_50pct_mixture"])
@pytest.mark.parametrize("pip", [-25.0, 0.0, 25.0])
def test_pressure_sign_disclosed_for_every_basis(context, card, monkeypatch, basis, pip):
    # Zero depth isolates the sign boundary from density-gradient rounding.
    context.pump.depth = 0
    if basis == "context":
        context.fluid_density = 72
    elif basis == "well_test_mixture":
        context.well_test = WellTestData(water_cut=80)
    fluid_load = (120 - pip) * context.pump_area
    monkeypatch.setattr(
        calculations, "calculate_fluid_load",
        lambda *args, **kwargs: FluidLoadAnalysis(fluid_load=fluid_load),
    )
    result = calculations.calculate_cpip(context, card)
    assert result.pump_intake_pressure == pytest.approx(pip)
    assert result.density_source == basis
    if pip < 0:
        assert result.pump_intake_pressure < 0  # Preserve the diagnostic value.
        assert result.pressure_status == "negative_calculated_pressure"
        assert any("physical intake pressure is not established" in w for w in result.warnings)
    else:
        assert result.pressure_status == "calculated"
        assert result.warnings == []


def test_combined_calculations_serialize_cpip_provenance(context, card, monkeypatch):
    context.fluid_density = 72
    monkeypatch.setattr(
        calculations, "calculate_fluid_load",
        lambda *args, **kwargs: FluidLoadAnalysis(fluid_load=10000),
    )
    combined = calculations.run_p1_calculations(context, card)
    result = AnalysisResults(**combined)
    payload = json.loads(result.model_dump_json())["cpip"]
    assert payload["density_source"] == "context"
    assert payload["fluid_density"] == 72
    assert payload["tubing_gradient"] == 0.5
    assert payload["pump_intake_pressure"] == pytest.approx(840 - 10000 / context.pump_area)
    assert payload["pressure_status"] == "negative_calculated_pressure"
    assert any("physical intake pressure is not established" in w for w in payload["warnings"])
    assert CPIPAnalysis.model_validate(payload) == combined["cpip"]


def test_manually_constructed_results_do_not_claim_provenance_or_status():
    legacy = CPIPAnalysis.model_validate({"pump_intake_pressure": -25})
    assert legacy.density_source == "unestablished"
    assert legacy.pressure_status == "unestablished"
    assert legacy.warnings == []
    legacy.warnings.append("separate instance")
    assert CPIPAnalysis().warnings == []
