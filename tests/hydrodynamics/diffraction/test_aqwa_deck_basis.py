"""Reading a physical basis out of an AQWA deck.

Checked against the two decks in the repository whose numbers are known: the
shipped ship-RAO example, and the LNGC pair that differs only in additional
roll damping.
"""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_deck_basis import (
    read_aqwa_basis,
)

REPO = Path(__file__).resolve().parents[3]
SHIP = (REPO / "docs" / "domains" / "aqwa" / "examples" / "03_dat"
        / "001_ship_raos" / "001_ship_raos.dat")
DAMPAD = (REPO / "docs" / "domains" / "aqwa" / "scripts"
          / "wlng_diffraction_analysis" / "raos" / "F_FST1_L00_DAMPAD.dat")
DAMPNO = (REPO / "docs" / "domains" / "aqwa" / "scripts"
          / "wlng_diffraction_analysis" / "raos" / "F_FST1_L00_DAMPNO.dat")

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


@pytest.mark.skipif(not (DAMPAD.exists() and DAMPNO.exists()),
                    reason="LNGC damping pair absent")
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

