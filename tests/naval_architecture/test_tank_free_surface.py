"""Small-angle rectangular free-surface checks against hand integration.

USNA, EN400 Principles of Ship Performance (Summer 2020), §4.8.3 and
§4.8.3.1: FSC = rho_t i_t / displacement_mass, i_t = L B³ / 12.
For a synthetic 10 m by 4 m surface, integrating y² over -2..2 gives
i_t = 10 [y³/3](-2..2) = 160/3 m⁴. At rho = 800 kg/m³,
FSM = 128000/3 kg m, and for displacement 1000000 kg the GM loss
is 16/375 m. These are synthetic inputs, not sourced material values.
"""

import math
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.naval_architecture.tank_gauging import (
    CalibrationTable,
    centre_sounding,
    density_at_temperature,
    gm_loss_m,
    liquid_mass_kg,
    rectangular_free_surface_moment_kg_m,
    sounding_to_ullage,
)


def test_rectangular_hand_integral():
    fsm = rectangular_free_surface_moment_kg_m(10, 4, 800, fill_state="slack")
    assert fsm == pytest.approx(128000 / 3)
    assert gm_loss_m(fsm, 1000000) == pytest.approx(16 / 375)
    assert 1 - gm_loss_m(fsm, 1000000) == pytest.approx(359 / 375)


@pytest.mark.parametrize("state", ["empty", "pressed_full"])
def test_no_free_surface_when_empty_or_pressed_full(state):
    assert rectangular_free_surface_moment_kg_m(10, 4, 800, fill_state=state) == 0
    assert gm_loss_m(0, 1000000) == 0


@pytest.mark.parametrize(
    "length,breadth,density,state",
    [
        (0, 4, 800, "slack"),
        (10, -4, 800, "slack"),
        (10, 4, 0, "slack"),
        (math.inf, 4, 800, "slack"),
        (10, math.nan, 800, "slack"),
        (10, 4, math.inf, "slack"),
        (10, 4, 800, "full"),
        (10, 4, 800, None),
    ],
)
def test_invalid_free_surface(length, breadth, density, state):
    with pytest.raises(ValueError):
        rectangular_free_surface_moment_kg_m(length, breadth, density, fill_state=state)


@pytest.mark.parametrize(
    "fsm,displacement",
    [
        (-1, 1000000),
        (math.nan, 1000000),
        (math.inf, 1000000),
        (100, 0),
        (100, -1),
        (100, math.nan),
        (100, math.inf),
    ],
)
def test_invalid_gm_loss(fsm, displacement):
    with pytest.raises(ValueError):
        gm_loss_m(fsm, displacement)


def test_complete_box_tank_example():
    """Reproduce the HTML sense check using independently evaluated values.

    Box 10 x 4 x 3 m: V=40h, u=3-h. Level h=1.5 -> u=1.5,
    V=60. At +1 degree and x=2, h_c=1.465089870143565 gives
    V=58.6035948057426 by symmetry of the unclipped plane.
    Using synthetic rho15=800, beta=0.001 and temperature 40:
    rho40=32000/41; equal-volume masses are 48000 and 1920000/41 kg.
    The level slack surface gives FSM=128000/3 and FSC=16/375 m.
    Geometry, density and expansion sources are cited in the other test
    docstrings; EN400 (2020) §4.8.3 supplies the free-surface basis.
    """
    table = CalibrationTable(((0, 0), (1, 40), (2, 80), (3, 120)), "sounding", 3)
    assert sounding_to_ullage(1.5, 3) == 1.5
    assert table.volume_at(1.5) == 60
    assert table.depth_at_volume(60) == 1.5
    centre = centre_sounding(1.5, 3, x_gauge_m=2, trim_deg=1)
    assert table.volume_at(centre) == pytest.approx(58.6035948057426)
    assert liquid_mass_kg(60, 800) == 48000
    assert liquid_mass_kg(
        60, density_at_temperature(800, 15, 40, 0.001)
    ) == pytest.approx(1920000 / 41)
    assert gm_loss_m(
        rectangular_free_surface_moment_kg_m(10, 4, 800, fill_state="slack"), 1000000
    ) == pytest.approx(16 / 375)


class ExampleParser(HTMLParser):
    """Extract the trusted example code and displayed table cells."""

    def __init__(self):
        super().__init__()
        self.in_code = False
        self.in_cell = False
        self.code = []
        self.cells = []

    def handle_starttag(self, tag, attrs):
        if tag == "pre":
            self.in_code = True
        if tag == "td":
            self.in_cell = True
            self.cells.append("")

    def handle_endtag(self, tag):
        if tag == "pre":
            self.in_code = False
        if tag == "td":
            self.in_cell = False

    def handle_data(self, data):
        if self.in_code:
            self.code.append(data)
        if self.in_cell:
            self.cells[-1] += data


def test_published_example_executes_and_matches_hand_values(capsys):
    """Check the actual report code and table against the hand example above."""
    report = (
        Path(__file__).parents[2]
        / "docs"
        / "domains"
        / "naval-architecture"
        / "tank-gauging-box-example.html"
    )
    parser = ExampleParser()
    parser.feed(report.read_text(encoding="utf-8"))
    namespace = {}
    exec(compile("".join(parser.code), report.name, "exec"), namespace)
    results = list(namespace["results"].values())
    expected = [
        1.5,
        60,
        1.5,
        1.465089870143565,
        58.6035948057426,
        48000,
        1920000 / 41,
        128000 / 3,
        16 / 375,
        359 / 375,
    ]
    assert results == pytest.approx(expected)
    assert namespace["table_corrected_volume"] == pytest.approx(58.6035948057426)
    assert namespace["ullage_corrected_volume"] == pytest.approx(58.6035948057426)
    # The final table has quantity/hand/code columns; both numeric columns
    # must reproduce the three-significant-figure results, not stale prose.
    result_cells = parser.cells[-30:]
    for i, expected_value in enumerate(expected):
        for cell in result_cells[i * 3 + 1 : i * 3 + 3]:
            assert float(cell.replace(",", "")) == float(format(expected_value, ".3g"))
    assert len(capsys.readouterr().out.splitlines()) == len(expected)
