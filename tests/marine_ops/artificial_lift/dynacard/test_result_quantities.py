# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Result contracts for synthetic cards; expected values are derived by hand."""

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard import (
    CardData,
    DynacardAnalysisContext,
    FeatureExtractor,
    PumpProperties,
    RodSection,
    SurfaceUnit,
    calculate_card_geometry,
    calculate_card_perimeter,
    calculate_power_consumption,
    calculate_shape_similarity,
    estimate_neutral_point,
    generate_ideal_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import AnalysisResults


@pytest.fixture
def context():
    """Synthetic inputs, not measured equipment or engineering defaults."""
    return DynacardAnalysisContext(
        api14="synthetic",
        surface_card=CardData(position=[0, 0, 12, 12], load=[0, 100, 100, 0]),
        rod_string=[RodSection(diameter=1, length=100)],
        pump=PumpProperties(diameter=1, depth=100),
        surface_unit=SurfaceUnit(stroke_length=12),
        spm=10,
        runtime=6,
    )


def test_physical_perimeter_is_unavailable(context):
    """Inches and pounds have no Euclidean sum; unavailable is JSON null."""
    geometry = calculate_card_geometry(context, raise_on_error=True)
    assert geometry.perimeter is None
    assert geometry.model_dump()["perimeter"] is None
    assert calculate_card_perimeter(context.surface_card.position,
                                    context.surface_card.load) is None
    # Hand calculation: travel = 2*12 in, load excursion = 2*100 lb,
    # normalized perimeter = 2*(12/12 + 100/100) = 4.
    assert geometry.position_path_length == pytest.approx(24)
    assert geometry.load_path_length == pytest.approx(200)
    assert geometry.normalized_perimeter == pytest.approx(4)


def test_legacy_classifier_perimeter_is_preserved(context):
    """Hand calculation: legacy feature = 2*(12+100)/(2*(12+100)) = 1.

    This is an arbitrary trained feature, never a reported physical length.
    """
    geometry = calculate_card_geometry(context, raise_on_error=True)
    features = FeatureExtractor.extract_analysis_features(
        AnalysisResults(ctx=context, card_geometry=geometry)
    )
    assert features[7] == pytest.approx(1)
    assert "legacy_perimeter_feature" not in geometry.model_dump()


def test_surface_extrema_cannot_produce_depth():
    """A time-cycle's two extrema contain no depth-resolved load profile."""
    assert estimate_neutral_point(1000, -100, 500, rod_length=100) is None


def test_similarity_preserves_load_sign():
    """Hand calculation: centered profiles v and -v have cosine -1.

    Dot(v, -v) / (norm(v)*norm(-v)) = -sum(v*v)/sum(v*v) = -1.
    Reference: NIST Dataplot, Cosine Similarity definition (2018 update).
    """
    card = CardData(position=[0, 1, 2, 1], load=[0, 1, 2, 0])
    inverted = CardData(position=card.position, load=[2 - x for x in card.load])
    assert calculate_shape_similarity(card, card) == pytest.approx(1)
    assert calculate_shape_similarity(card, inverted) == pytest.approx(-1)


def test_similarity_uses_positions():
    """Same load sequence with a shifted interior vertex is a different profile."""
    card = CardData(position=[0, 1, 2, 1], load=[0, 1, 2, 0])
    shifted = CardData(position=[0, 0.5, 2, 1], load=card.load)
    assert calculate_shape_similarity(card, shifted) < 1


def test_geometry_changes_centroid(context):
    """Hand calculation by subtracting a square from a rectangle.

    Rectangle: A=4*4=16, C=(2,2). Remove upper-right 2*2 square:
    A=12, Cx=Cy=(16*2-4*3)/12=5/3. See polygon centroid (1988).
    """
    rectangle = CardData(position=[0, 4, 4, 0], load=[0, 0, 4, 4])
    l_card = CardData(position=[0, 4, 4, 2, 2, 0], load=[0, 0, 2, 2, 4, 4])
    rect = calculate_card_geometry(context, rectangle, raise_on_error=True)
    l_shape = calculate_card_geometry(context, l_card, raise_on_error=True)
    assert rect.centroid_position == pytest.approx(2)
    assert l_shape.centroid_position == pytest.approx(5 / 3)
    assert l_shape.centroid_load == pytest.approx(5 / 3)


def test_failed_power_analysis_is_not_zero_work(context):
    """An empty card has not been integrated: no work or power was evaluated."""
    context.surface_card = CardData(position=[], load=[])
    result = calculate_power_consumption(context)
    assert result.card_area is None
    assert result.polished_rod_horsepower is None
    assert result.warnings


def test_ideal_similarity_agrees_with_public_function(context):
    """The result and direct API compare the same two branch profiles."""
    ideal = generate_ideal_card(context, fluid_load=100, raise_on_error=True)
    card = CardData(position=ideal.ideal_surface_position, load=ideal.ideal_surface_load)
    assert ideal.shape_similarity == pytest.approx(
        calculate_shape_similarity(context.surface_card, card)
    )


def test_zero_similarity_is_evaluated_orthogonality():
    """Hand calculation: common ramp v and opposite branch tents w.

    At each position, the two w loads cancel. The dot product with v is
    therefore zero, while both norms are positive. Signed cosine is zero.
    """
    ramp = CardData(position=[0, 1, 2, 1], load=[-1, 0, 1, 0])
    tents = CardData(position=ramp.position, load=[0, 1, 0, -1])
    assert calculate_shape_similarity(ramp, tents) == pytest.approx(0)


def test_similarity_ignores_start_and_edge_sampling():
    """Hand calculation: both profiles are constant 100 up and zero down.

    Centered and normalized profiles are identical, so their cosine is one.
    Vertical transitions belong to neither position-resolved stroke branch.
    """
    card = CardData(position=[0, 0, 12, 12], load=[0, 100, 100, 0])
    resampled = CardData(position=[12, 6, 0, 0, 3, 6, 12],
                         load=[0, 0, 0, 100, 100, 100, 100])
    assert calculate_shape_similarity(card, resampled) == pytest.approx(1)


@pytest.fixture(params=[(False, 0), (False, 3), (True, 0), (True, 3)])
def interior_transfer_cards(request):
    """The same oriented polygon before and after inserting collinear points."""
    reflected, offset = request.param
    position = [0, 50, 50, 100, 100, 0]
    load = [0, 0, 100, 100, 0, 0]
    sampled_position = [0, 49, 50, 50, 51, 100, 100, 0]
    sampled_load = [0, 0, 0, 100, 100, 100, 0, 0]
    cards = []
    for positions, loads in ((position, load), (sampled_position, sampled_load)):
        if reflected:
            positions = [100 - value for value in positions]
        cards.append(CardData(position=positions[offset:] + positions[:offset],
                              load=loads[offset:] + loads[:offset]))
    return cards


def test_similarity_preserves_interior_vertical_transfer(interior_transfer_cards):
    """Identical directed edges give identical centered vectors: cosine = 1."""
    card, sampled = interior_transfer_cards
    assert calculate_shape_similarity(card, sampled) == pytest.approx(1)


def test_deviation_preserves_interior_vertical_transfer(context, interior_transfer_cards):
    """Equal loads at every branch position give RMS sqrt(mean(0**2)) = 0 lb."""
    from digitalmodel.marine_ops.artificial_lift.dynacard import IdealCardCalculator

    card, sampled = interior_transfer_cards
    context.surface_card = card
    calculator = IdealCardCalculator(context)
    calculator.result.ideal_surface_position = sampled.position
    calculator.result.ideal_surface_load = sampled.load
    calculator.result.ideal_stroke_length = 100
    calculator._calculate_deviation_from_measured()
    assert calculator.result.load_deviation_rms == pytest.approx(0)
    assert calculator.result.shape_similarity == pytest.approx(1)


@pytest.mark.parametrize("reflected", [False, True])
def test_interior_transfer_uses_one_sided_edge_loads(reflected):
    """At a jump use the edge on its higher-position side, for either stroke.

    The retained 64-point grid samples integer positions from 0 to 63.
    Upstroke loads jump from 0 to 100 lb at 21 in; reflection moves the
    downstroke jump to 42 in. The other stroke has load zero throughout.
    """
    from digitalmodel.marine_ops.artificial_lift.dynacard.ideal_card import (
        _comparison_branch_profiles,
    )

    position = [0, 21, 21, 63, 63, 0]
    if reflected:
        position = [63 - value for value in position]
    up, down = _comparison_branch_profiles(position, [0, 0, 100, 100, 0, 0])
    if reflected:
        assert up == pytest.approx([0] * 64)
        assert down == pytest.approx([100] * 42 + [0] * 22)
    else:
        assert up == pytest.approx([0] * 21 + [100] * 43)
        assert down == pytest.approx([0] * 64)


def test_reused_buckling_calculator_does_not_retain_thresholds(context):
    """Missing wellbore geometry on a second call must yield unknown verdicts."""
    from digitalmodel.marine_ops.artificial_lift.dynacard import RodBucklingCalculator

    calculator = RodBucklingCalculator(context)
    first = calculator.calculate(context.surface_card, inclination_deg=30,
                                 tubing_id=2, load_datum="net_pump_load")
    assert first.sinusoidal_critical_load is not None
    second = calculator.calculate(context.surface_card, load_datum="net_pump_load")
    assert second.sinusoidal_critical_load is None
    assert second.sinusoidal_buckling_detected is None


def test_worked_example_answers():
    """Check the independent rectangle and signed-cosine hand calculations.

    Area=12*100, centroid=(12/2,100/2), travel=2*12, excursion=2*100,
    normalized perimeter=4; reflection cosine=-1, unsupported depth=None.
    """
    from pathlib import Path
    from runpy import run_path

    script = Path(__file__).resolve().parents[4] / "examples/domains/artificial_lift/dynacard_quantities.py"
    answer = run_path(str(script))["run_example"]()
    assert answer == pytest.approx({
        "area_lb_in": 1200,
        "centroid_position_in": 6,
        "centroid_load_lb": 50,
        "position_path_length_in": 24,
        "load_path_length_lb": 200,
        "normalized_perimeter": 4,
        "physical_perimeter": None,
        "reflected_load_similarity": -1,
        "neutral_point_depth_ft": None,
    })


def test_ambiguous_strokes_keep_only_legacy_classifier_score(context):
    """Multiple position reversals are ambiguous, but the trained feature survives.

    The sign sequence (+,-,+,-) cannot identify a single up/down pair, so
    the engineering comparison is unavailable. The frozen legacy feature
    retains its original domain (0,1] and is excluded from engineering JSON.
    """
    context.surface_card = CardData(position=[0, 12, 6, 12, 0],
                                    load=[0, 100, 50, 0, 0])
    result = generate_ideal_card(context, fluid_load=100, raise_on_error=True)
    assert result.shape_similarity is None
    assert result.load_deviation_rms is None
    assert 0 < result.legacy_shape_feature <= 1
    assert "legacy_shape_feature" not in result.model_dump()


@pytest.mark.parametrize(
    "fixture_number, downhole_available",
    [(1, False), (2, False), (3, True), (4, False), (5, False)],
)
def test_similarity_availability_on_recorded_cards(fixture_number, downhole_available):
    """Surface cycles are monotone; four downhole cycles have extra reversals.

    An accepted card compared with itself has signed cosine one. Additional
    position reversals make the two stroke branches ambiguous, even when
    comparing a card with itself; unavailable must remain distinct from zero.
    """
    import json
    from pathlib import Path

    fixture = (Path(__file__).resolve().parents[1] / "test_data"
               / f"cleansed_well_{fixture_number:03d}.json")
    data = json.loads(fixture.read_text(encoding="utf-8"))
    surface = CardData(position=data["cardData"]["Position"],
                       load=data["cardData"]["Load"])
    downhole = CardData(position=data["downholeCard"]["Position"],
                        load=data["downholeCard"]["Load"])

    assert calculate_shape_similarity(surface, surface) == pytest.approx(1)
    if downhole_available:
        assert calculate_shape_similarity(downhole, downhole) == pytest.approx(1)
    else:
        assert calculate_shape_similarity(downhole, downhole) is None
