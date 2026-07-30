# ABOUTME: Holds each synthetic card generator against a canonical plate card.
# ABOUTME: Guards generator shape from silently drifting off the reference set.
"""Shape regression: synthetic generators vs the CC0 diagnosis plate.

The generators in ``card_generators`` are the *only* thing the 18-mode
classifier was ever trained on. Nothing previously checked that the shapes they
draw are the shapes real pumps make, and four of them were not: gas
interference had no rounded lower-right corner at all, fluid pound put its load
drop on the wrong side of the card, rod parting drew a Lissajous figure-eight
enclosing no area, and pump tagging drew one class where there are two.

This module pins every generator that has a canonical exemplar to that
exemplar, using a distance that ignores where a card starts and which way round
it is traversed -- the two things about a closed loop that carry no information.

The exemplars, their DOI and their licence are documented in
``testdata/README.md``.

Calibration, measured on the plate itself (see
:func:`test_plate_exemplars_are_mutually_distinguishable`): distances between
genuinely different canonical classes run min 0.035, median 0.117, max 0.249.
A generator sitting well under that median from its own exemplar is as close as
"the same condition" gets on this plate.
"""

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)

_FIXTURE = Path(__file__).parent / "testdata" / "plate_reference_cards.json"

# Points to resample each closed loop to before comparing.
_LOOP_POINTS = 128

# Seeds drawn from each generator; the reported distance is their median, so a
# single unlucky noise draw cannot move the verdict.
_SEEDS = range(20)

# Distances between different canonical classes on the plate itself.
_PLATE_MIN_SEPARATION = 0.035
_PLATE_MEDIAN_SEPARATION = 0.117


# ---------------------------------------------------------------------------
# Shape metric
# ---------------------------------------------------------------------------


def _normalise(a: np.ndarray) -> np.ndarray:
    """Scale one axis onto 0-1. The plate carries no engineering units."""
    lo, hi = float(np.min(a)), float(np.max(a))
    return (a - lo) / (hi - lo) if hi > lo else np.zeros_like(a)


def _resample_loop(x: np.ndarray, y: np.ndarray, m: int = _LOOP_POINTS) -> np.ndarray:
    """Resample a closed loop to ``m`` points at uniform arc length."""
    px = np.append(x, x[0])
    py = np.append(y, y[0])
    step = np.hypot(np.diff(px), np.diff(py))
    s = np.concatenate([[0.0], np.cumsum(step)])
    if s[-1] == 0:
        return np.zeros((m, 2))
    t = np.linspace(0, s[-1], m, endpoint=False)
    return np.stack([np.interp(t, s, px), np.interp(t, s, py)], axis=1)


def prepare(position, load) -> np.ndarray:
    """Per-axis 0-1 normalised, arc-length resampled closed loop."""
    return _resample_loop(
        _normalise(np.asarray(position, dtype=float)),
        _normalise(np.asarray(load, dtype=float)),
    )


def loop_distance(a: np.ndarray, b: np.ndarray) -> float:
    """Phase-invariant distance between two closed loops.

    Minimum mean point-to-point distance over every circular shift of ``b``
    and both traversal directions. A card has no canonical start sample and no
    canonical direction, so a metric that cared about either would be measuring
    the recorder rather than the pump.
    """
    best = np.inf
    for oriented in (b, b[::-1]):
        for shift in range(len(oriented)):
            d = float(
                np.linalg.norm(a - np.roll(oriented, shift, axis=0), axis=1).mean()
            )
            best = min(best, d)
    return best


def jaggedness(loop: np.ndarray) -> float:
    """Mean |second difference| of load around a prepared loop.

    A texture statistic: high for a card whose trace rattles from sample to
    sample, near zero for a smooth one. It is measured on the *prepared* loop
    so that it does not simply report how finely the source was sampled -- the
    plate exemplars carry between 21 and 190 points each.

    This is what separates sand abrasion from every other class, and it is not
    a property of the card's outline.
    """
    return float(np.mean(np.abs(np.diff(loop[:, 1], n=2))))


# ---------------------------------------------------------------------------
# Fixture
# ---------------------------------------------------------------------------


def _load_plate() -> dict:
    with open(_FIXTURE, encoding="utf-8") as fh:
        return json.load(fh)["cards"]


PLATE = _load_plate()

# Plate class -> the generator that should be nearest it.
EXEMPLAR_OF = {entry["mode"]: name for name, entry in PLATE.items()}

# Sand abrasion is deliberately absent from the nearest-exemplar check; see
# ``test_sand_abrasion_is_jagged_like_the_plate`` for what replaces it and why.
SHAPE_TESTED_MODES = sorted(set(EXEMPLAR_OF) - {"SAND_ABRASION"})

PREPARED_PLATE = {name: prepare(e["position"], e["load"]) for name, e in PLATE.items()}


def _generator_loops(mode: str) -> list[np.ndarray]:
    gen = ALL_GENERATORS[mode]
    return [prepare(*(lambda c: (c.position, c.load))(gen(seed=s))) for s in _SEEDS]


def _distances_to_plate(mode: str) -> dict[str, float]:
    loops = _generator_loops(mode)
    return {
        name: float(np.median([loop_distance(ref, loop) for loop in loops]))
        for name, ref in PREPARED_PLATE.items()
    }


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


def test_every_plate_exemplar_has_a_generator():
    """The crosswalk is not allowed to rot when modes are added or renamed."""
    missing = sorted(m for m in EXEMPLAR_OF if m not in ALL_GENERATORS)
    assert not missing, f"plate exemplars with no generator: {missing}"


def test_plate_exemplars_are_mutually_distinguishable():
    """Calibrates the scale every other assertion in this module is read on."""
    names = list(PREPARED_PLATE)
    off_diagonal = [
        loop_distance(PREPARED_PLATE[a], PREPARED_PLATE[b])
        for i, a in enumerate(names)
        for b in names[i + 1:]
    ]
    assert min(off_diagonal) == pytest.approx(_PLATE_MIN_SEPARATION, abs=0.005)
    assert np.median(off_diagonal) == pytest.approx(
        _PLATE_MEDIAN_SEPARATION, abs=0.010
    )


@pytest.mark.parametrize("mode", SHAPE_TESTED_MODES)
def test_generator_is_nearest_its_own_exemplar(mode):
    """The real test: no generator may resemble another class more than its own.

    Gas interference used to fail this outright -- it sat 0.050 from the
    plate's *normal* card and 0.150 from the plate's gas card, three times
    closer to normal than to its own class.
    """
    own = EXEMPLAR_OF[mode]
    d = _distances_to_plate(mode)
    nearest = min(d, key=d.get)
    runner_up = sorted(d.values())[1]
    assert nearest == own, (
        f"{mode} is nearest the {nearest!r} exemplar ({d[nearest]:.4f}), "
        f"not its own {own!r} ({d[own]:.4f})"
    )
    assert runner_up - d[own] > 0.005, (
        f"{mode} separates its own exemplar from the next by only "
        f"{runner_up - d[own]:.4f}"
    )


@pytest.mark.parametrize("mode", SHAPE_TESTED_MODES)
def test_generator_is_close_to_its_own_exemplar(mode):
    """And it has to be *near* it, not merely nearest.

    The bar is the median separation between genuinely different canonical
    classes: a generator further from its own exemplar than two different
    conditions are from each other is not drawing that condition.
    """
    own = EXEMPLAR_OF[mode]
    d = _distances_to_plate(mode)[own]
    assert d < _PLATE_MEDIAN_SEPARATION, (
        f"{mode} sits {d:.4f} from its own exemplar, past the "
        f"{_PLATE_MEDIAN_SEPARATION} median separation between different classes"
    )


def test_sand_abrasion_is_jagged_like_the_plate():
    """Sand abrasion is checked on texture, not outline.

    Its diagnostic feature is a stochastic one -- grain-by-grain rattle in the
    load trace -- and a point-correspondence loop distance cannot match one
    noise realisation to another: the arc-length parameterisation of a jagged
    loop is spent on the rattle rather than on the card. So the loop distance
    is only required to be sane here, and the jaggedness statistic carries the
    actual assertion.
    """
    reference = jaggedness(PREPARED_PLATE["sand"])
    plate_others = [
        jaggedness(loop) for name, loop in PREPARED_PLATE.items() if name != "sand"
    ]
    # Calibration: on the plate, sand rattles an order of magnitude more than
    # any other class.
    assert reference > 10 * np.median(plate_others)

    ours = np.median([jaggedness(loop) for loop in _generator_loops("SAND_ABRASION")])
    assert 0.5 * reference < ours < 2.5 * reference, (
        f"sand jaggedness {ours:.4f} against the plate's {reference:.4f}"
    )

    smooth = [
        np.median([jaggedness(loop) for loop in _generator_loops(mode)])
        for mode in SHAPE_TESTED_MODES
    ]
    assert ours > 2.5 * np.median(smooth)

    # The outline still has to be in the right neighbourhood, even if it
    # cannot win the nearest-exemplar contest.
    assert _distances_to_plate("SAND_ABRASION")["sand"] < _PLATE_MEDIAN_SEPARATION


def test_card_corners_are_rounded_like_the_plate():
    """Load transfer straddles the turnaround, so card corners are cut away.

    The generators used to build the transfer as a five-sample linear ramp
    starting *at* the turnaround, which draws a squarer card than any pump
    makes -- corner clearance 0.03-0.05 against the plate's 0.12-0.15. Bezerra
    vertical-projection features read exactly this geometry, so it is not
    cosmetic.
    """
    def clearances(loop):
        return [
            float(np.min(np.linalg.norm(loop - np.array(c), axis=1)))
            for c in ((0, 0), (1, 0), (1, 1), (0, 1))
        ]

    plate = clearances(PREPARED_PLATE["normal"])
    assert min(plate) > 0.05 and max(plate) < 0.20

    ours = np.median([clearances(loop) for loop in _generator_loops("NORMAL")], axis=0)
    assert min(ours) > 0.09, f"normal card corners still too sharp: {ours}"
    assert max(ours) < 0.20, f"normal card corners over-rounded: {ours}"


def test_gas_interference_sheds_load_late_on_the_downstroke():
    """The defining feature, asserted directly rather than only via distance.

    Free gas must be compressed before the travelling valve can open, so the
    load transfers late and the lower-right corner is gone. The old generator
    subtracted ``sin(t)`` across the whole downstroke, which dips the *middle*
    and leaves a symmetric sag: lower-right clearance 0.099 against lower-left
    0.097, where the plate reads 0.650 against 0.153.
    """
    def corner(loop, c):
        return float(np.min(np.linalg.norm(loop - np.array(c), axis=1)))

    plate = PREPARED_PLATE["gas_interference"]
    assert corner(plate, (1, 0)) > 4 * corner(plate, (0, 0))

    ours = _generator_loops("GAS_INTERFERENCE")
    br = np.median([corner(loop, (1, 0)) for loop in ours])
    bl = np.median([corner(loop, (0, 0)) for loop in ours])
    assert br > 0.45, f"lower-right corner not gutted: {br:.3f}"
    assert br > 4 * bl, f"sag is symmetric, not a corner: BR={br:.3f} BL={bl:.3f}"


def test_fluid_pound_holds_load_into_the_downstroke_then_collapses():
    """Load must fall *late*, and the lower branch must rise with position.

    The old generator dropped to the low load immediately at the top of the
    stroke, punched a notch below the baseline near 78% of position, then
    ramped back linearly -- lower-branch slope -0.073 where the plate reads
    +0.504, the opposite sign.
    """
    def lower_branch_slope(loop):
        low = loop[loop[:, 1] < np.median(loop[:, 1])]
        return float(np.polyfit(low[:, 0], low[:, 1], 1)[0])

    assert lower_branch_slope(PREPARED_PLATE["insufficient_supply_fluid_pound"]) > 0.3
    ours = np.median([lower_branch_slope(l) for l in _generator_loops("FLUID_POUND")])
    assert ours > 0.3, f"fluid pound lower branch slope {ours:.3f}, expected > 0.3"


def test_rod_parting_encloses_a_card():
    """Not a figure-eight, and not full-load.

    ``pos=(1-cos t)/2`` against ``load=mean+A*sin(2t)`` is a 1:2 Lissajous
    curve: it crosses itself and encloses essentially nothing (fill ratio
    0.011 against the plate's 0.731). A parted string still traces a loop --
    friction opens it and rod inertia tilts it. What marks it out is scale:
    load range against mean load, 0.24 on the plate against 1.9 for normal.
    """
    def fill_ratio(loop):
        x, y = loop[:, 0], loop[:, 1]
        area = 0.5 * abs(np.dot(x, np.roll(y, -1)) - np.dot(np.roll(x, -1), y))
        return float(area)  # bounding box is 1x1 after normalisation

    plate_fill = fill_ratio(PREPARED_PLATE["rod_part"])
    ours = [ALL_GENERATORS["ROD_PARTING"](seed=s) for s in _SEEDS]
    fills = [fill_ratio(prepare(c.position, c.load)) for c in ours]
    assert np.median(fills) == pytest.approx(plate_fill, abs=0.15)
    assert np.median(fills) > 0.5

    spans = [
        (max(c.load) - min(c.load)) / abs(np.mean(c.load)) for c in ours
    ]
    assert np.median(spans) < 0.5, "rod parting card is not a low-load card"


def test_pump_tagging_up_and_down_are_distinct_classes():
    """The split is real: the impact is at opposite ends of the stroke."""
    up = [ALL_GENERATORS["PUMP_TAGGING_UP"](seed=s) for s in _SEEDS]
    down = [ALL_GENERATORS["PUMP_TAGGING_DOWN"](seed=s) for s in _SEEDS]

    # Tagging up peaks at maximum position; tagging down troughs at minimum.
    for card in up:
        pos = np.asarray(card.position)
        peak = pos[int(np.argmax(card.load))]
        assert peak > 0.9 * pos.max(), "tagging-up spike is not at top of stroke"
    for card in down:
        pos = np.asarray(card.position)
        trough = pos[int(np.argmin(card.load))]
        assert trough < 0.1 * pos.max(), "tagging-down dip is not at bottom of stroke"

    # And the old generator was about three times too violent: normalised
    # load-range-to-mean 2.59 where the plate reads 0.91.
    for cards in (up, down):
        spans = [(max(c.load) - min(c.load)) / abs(np.mean(c.load)) for c in cards]
        assert np.median(spans) < 1.6


def test_plunger_out_of_barrel_loses_the_top_right_corner():
    """Fluid load is dumped mid-upstroke, so the card never reaches (1, 1)."""
    def corner(loop, c):
        return float(np.min(np.linalg.norm(loop - np.array(c), axis=1)))

    assert corner(PREPARED_PLATE["plunger_out_of_barrel"], (1, 1)) > 0.4
    ours = np.median(
        [corner(loop, (1, 1)) for loop in _generator_loops("PLUNGER_OUT_OF_BARREL")]
    )
    assert ours > 0.35, f"top-right corner still present: {ours:.3f}"
