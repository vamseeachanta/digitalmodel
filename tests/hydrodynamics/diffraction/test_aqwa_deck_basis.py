"""Reading a physical basis out of an AQWA deck.

Checked against the shipped ship-RAO example deck and a small synthetic deck
pair that differs only in additional roll and pitch damping. A project deck
pair with the same property is kept out of this public repository; set
DM_AQWA_DAMPING_DECK_DIR to the folder holding F_FST1_L00_DAMPAD.dat and
F_FST1_L00_DAMPNO.dat to also run the check against it.
"""

from __future__ import annotations

import math
import os
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_deck_basis import (
    read_aqwa_basis,
)

REPO = Path(__file__).resolve().parents[3]
SHIP = (REPO / "docs" / "domains" / "aqwa" / "examples" / "03_dat"
        / "001_ship_raos" / "001_ship_raos.dat")
# Private project data: absent unless DM_AQWA_DAMPING_DECK_DIR points at it.
_DAMPING_DIR = os.environ.get("DM_AQWA_DAMPING_DECK_DIR", "")
DAMPAD = (Path(_DAMPING_DIR) / "F_FST1_L00_DAMPAD.dat") if _DAMPING_DIR else None
DAMPNO = (Path(_DAMPING_DIR) / "F_FST1_L00_DAMPNO.dat") if _DAMPING_DIR else None

ship_only = pytest.mark.skipif(not SHIP.exists(), reason="ship deck absent")


@ship_only
def test_ship_mass_and_inertia():
    b = read_aqwa_basis(SHIP)
    assert b.mass == pytest.approx(45184268.0)
    assert b.inertia is not None
    ixx, ixy, ixz, iyy, iyz, izz = b.inertia
    assert ixx == pytest.approx(8.35728e9, rel=1e-9)
    assert iyy == pytest.approx(1.1296e11, rel=1e-9)
    assert izz == pytest.approx(1.2218e11, rel=1e-9)
    assert (ixy, ixz, iyz) == (0.0, 0.0, 0.0)


@ship_only
def test_ship_centre_of_gravity():
    b = read_aqwa_basis(SHIP)
    assert b.cog_node == 98000
    assert b.centre_of_gravity is not None
    x, y, z = b.centre_of_gravity
    assert x == pytest.approx(108.882)
    assert y == pytest.approx(1.6289e-3)
    assert z == pytest.approx(8.5)


@ship_only
def test_ship_environment():
    b = read_aqwa_basis(SHIP)
    assert b.water_depth == pytest.approx(500.0)
    assert b.water_density == pytest.approx(1025.0)
    assert b.gravity == pytest.approx(9.80665)
    assert b.waterline_z == pytest.approx(0.0)


@ship_only
def test_ship_frequency_grid_converted_from_hz():
    """The deck carries HRTZ cards in Hz; the basis reports rad/s."""
    b = read_aqwa_basis(SHIP)
    assert len(b.frequencies_rad_s) == 8
    assert min(b.frequencies_rad_s) == pytest.approx(0.1, abs=1e-3)
    assert max(b.frequencies_rad_s) == pytest.approx(2.2521, abs=1e-3)
    # 1.5915e-2 Hz is 0.1 rad/s
    assert b.frequencies_rad_s[0] == pytest.approx(
        2 * math.pi * 1.5915e-2, rel=1e-6)


@ship_only
def test_ship_headings():
    b = read_aqwa_basis(SHIP)
    assert b.headings_deg == pytest.approx(
        (-180.0, -135.0, -90.0, -45.0, 0.0, 45.0, 90.0, 135.0, 180.0))


@ship_only
def test_ship_has_no_additional_damping():
    """Deck 7 is empty in this example; it is pure potential flow."""
    b = read_aqwa_basis(SHIP)
    assert not b.has_additional_damping
    assert b.additional_damping is None


@ship_only
def test_ship_derived_quantities():
    b = read_aqwa_basis(SHIP)
    rx, ry, rz = b.radii_of_gyration
    assert rx == pytest.approx(13.60, abs=0.01)
    assert ry == pytest.approx(50.00, abs=0.01)
    assert rz == pytest.approx(52.00, abs=0.01)
    # Mass over density is the volume the hull must displace to float here.
    assert b.displaced_volume_if_balanced == pytest.approx(44082.21, abs=0.02)


@ship_only
def test_ship_panels_and_options():
    b = read_aqwa_basis(SHIP)
    assert b.panel_count > 5000
    assert "LHFR" in b.options
    assert b.unit_system is not None and "kg" in b.unit_system


@ship_only
def test_volume_as_mass_is_the_trap_this_guards():
    """The L01 OrcaWave companion used 44082.20 as a mass in tonnes.

    That number is the displaced volume in cubic metres. Reading the deck gives
    the mass directly and makes the substitution visible.
    """
    b = read_aqwa_basis(SHIP)
    mass_tonnes = b.mass / 1000.0
    volume = b.displaced_volume_if_balanced
    assert mass_tonnes == pytest.approx(45184.268, abs=1e-3)
    assert volume == pytest.approx(44082.21, abs=0.02)
    # They differ by the density factor, which is what was dropped.
    assert mass_tonnes / volume == pytest.approx(1.025, rel=1e-4)


def _fidp_deck(tmp_path, name: str, roll: float, pitch: float):
    """A one-structure deck whose only body data is a Deck 7 FIDP matrix.

    Generic values, not taken from any project: the record layout is the
    AQWA fixed-column one (keyword in columns 7-10, ten-column fields).
    """
    diag = {4: roll, 5: pitch}
    cards = []
    for mode in range(1, 7):
        row = [0.0] * 6
        if mode in diag:
            row[mode - 1] = diag[mode]
        cards.append("      FIDP" + f"{mode:10d}"
                     + "".join(f"{v:10.3e}" for v in row))
    text = "\n".join([
        "          JOB AQWA  LINE",
        "          FDR1",
        *cards,
        "          END",
        "",
    ])
    deck = tmp_path / name
    deck.write_text(text, encoding="ascii")
    return deck


def test_additional_damping_is_read_from_a_synthetic_pair(tmp_path):
    """Same property as the project pair: FIDP differs on roll and pitch."""
    added = read_aqwa_basis(_fidp_deck(tmp_path, "ad.dat", 2.0e9, -5.0e10))
    none = read_aqwa_basis(_fidp_deck(tmp_path, "no.dat", 1.0e5, -1.0e6))
    assert added.has_additional_damping
    assert none.has_additional_damping
    assert added.additional_damping[3][3] == pytest.approx(2.0e9)
    assert added.additional_damping[4][4] == pytest.approx(-5.0e10)
    assert added.additional_damping[0][0] == 0.0
    assert added.additional_damping[3][3] > 100 * none.additional_damping[3][3]


@pytest.mark.skipif(
    not (DAMPAD and DAMPNO and DAMPAD.exists() and DAMPNO.exists()),
    reason="private damping deck pair absent (set DM_AQWA_DAMPING_DECK_DIR)")
def test_additional_damping_is_read_when_present():
    """The pair differs in FIDP on roll and pitch; both are read."""
    added = read_aqwa_basis(DAMPAD)
    none = read_aqwa_basis(DAMPNO)
    assert added.has_additional_damping
    assert none.has_additional_damping
    # Row 4 is roll. The two decks carry very different values there.
    roll_added = added.additional_damping[3][3]
    roll_none = none.additional_damping[3][3]
    assert roll_added > 100 * roll_none, (
        f"expected a large roll damping difference, got {roll_added} "
        f"against {roll_none}")


def test_missing_fields_are_reported_not_guessed(tmp_path):
    deck = tmp_path / "bare.dat"
    deck.write_text("JOB AQWA  LINE\n          GLOB\n      DPTH      100.\n"
                    "  END\n")
    b = read_aqwa_basis(deck)
    assert b.water_depth == pytest.approx(100.0)
    assert b.mass is None
    assert b.inertia is None
    assert any("MATE" in w for w in b.warnings)
    assert any("PMAS" in w for w in b.warnings)
    assert b.radii_of_gyration is None
    assert b.displaced_volume_if_balanced is None


def test_describe_runs_on_a_sparse_deck(tmp_path):
    deck = tmp_path / "bare.dat"
    deck.write_text("JOB AQWA  LINE\n")
    text = read_aqwa_basis(deck).describe()
    assert "AQWA basis" in text

