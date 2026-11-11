"""Hand checks for the berthing-energy and fender-selection calculator.

Every fixture value is a generic or fictional test input (a Supramax-class
bulk carrier described only by round-number particulars, and fictional
fender ratings). None is a project, client or manufacturer value. Expected
values are worked by hand in the comments, independently of the code.
"""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.berthing_energy import (
    FENDER_CATALOGUE_COLUMNS,
    abnormal_impact_factor,
    added_mass_factor_ueda,
    added_mass_factor_ukc,
    added_mass_factor_vasco_costa,
    berthing_energy,
    block_coefficient,
    contact_geometry,
    eccentricity_factor,
    load_fender_catalogue,
    radius_of_gyration,
    select_fenders,
)

FIXTURE = Path(__file__).parent / "fixtures" / "generic_fender_catalogue.csv"

# Generic Supramax-class bulk carrier (round numbers, not a real ship)
DISPLACEMENT_T = 60_000.0
LBP_M = 180.0
BEAM_M = 32.0
DRAFT_M = 12.0


# --------------------------------------------------------------------------
# Energy equation, PIANC 2002 section 4.2.1: E = 0.5 M v^2 Ce Cm Cs Cc
# --------------------------------------------------------------------------


def test_energy_hand_check():
    # 0.5 * 60000 * 0.12^2 = 432.0; * 0.5 = 216.0; * 1.8 = 388.8 kNm
    e = berthing_energy(DISPLACEMENT_T, 0.12, ce=0.5, cm=1.8)
    assert e == pytest.approx(388.8, rel=1e-12)


def test_energy_reproduces_deterministic_spreadsheet_method():
    # The common spreadsheet layout: every coefficient typed in, Cs = Cc = 1.
    # 0.5 * 60000 * 0.0144 = 432.0; * 0.736 = 317.952; * 1.5 = 476.928 kNm
    e = berthing_energy(DISPLACEMENT_T, 0.12, ce=0.736, cm=1.5, cs=1.0, cc=1.0)
    assert e == pytest.approx(476.928, rel=1e-12)
    # utilisation against one fictional 700 kNm fender, then shared by two
    assert e / 700.0 == pytest.approx(0.681326, rel=1e-5)
    assert e / 700.0 / 2 == pytest.approx(0.340663, rel=1e-5)


def test_energy_scales_with_velocity_squared():
    e1 = berthing_energy(DISPLACEMENT_T, 0.10, ce=0.5, cm=1.5)
    e2 = berthing_energy(DISPLACEMENT_T, 0.20, ce=0.5, cm=1.5)
    assert e2 / e1 == pytest.approx(4.0)


def test_softness_and_berth_configuration_apply():
    # 388.8 * 0.9 * 0.9 = 314.928
    e = berthing_energy(DISPLACEMENT_T, 0.12, ce=0.5, cm=1.8, cs=0.9, cc=0.9)
    assert e == pytest.approx(314.928, rel=1e-12)


@pytest.mark.parametrize("bad", [dict(displacement_t=0.0), dict(velocity_m_s=-0.1)])
def test_energy_rejects_bad_inputs(bad):
    args = dict(displacement_t=DISPLACEMENT_T, velocity_m_s=0.12, ce=0.5, cm=1.5)
    args.update(bad)
    with pytest.raises(ValueError):
        berthing_energy(**args)


# --------------------------------------------------------------------------
# Hull form, radius of gyration and eccentricity (PIANC 2002 section 4.2.4)
# --------------------------------------------------------------------------


def test_block_coefficient_hand_check():
    # 60000 / 1.025 = 58536.585 m3; / (180 * 32 * 12 = 69120) = 0.846883
    cb = block_coefficient(DISPLACEMENT_T, LBP_M, BEAM_M, DRAFT_M)
    assert cb == pytest.approx(0.846883, rel=1e-5)


def test_radius_of_gyration_hand_check():
    # (0.19 * 0.8 + 0.11) * 180 = 0.262 * 180 = 47.16 m
    assert radius_of_gyration(0.8, LBP_M) == pytest.approx(47.16, rel=1e-12)


def test_eccentricity_quarter_point_perpendicular():
    # K = 47.16, R = 45 (quarter point of 180 m), phi = 90 deg
    # K^2 = 2224.0656; R^2 = 2025; Ce = 2224.0656 / 4249.0656 = 0.523425
    ce = eccentricity_factor(47.16, 45.0)
    assert ce == pytest.approx(0.523425, rel=1e-5)


def test_eccentricity_uses_r_squared_in_numerator():
    # (K^2 + R^2 cos^2 phi) / (K^2 + R^2) with phi = 60 deg, cos^2 = 0.25
    # (2224.0656 + 2025 * 0.25) / 4249.0656 = 2730.3156 / 4249.0656 = 0.642569
    ce = eccentricity_factor(47.16, 45.0, phi_deg=60.0)
    assert ce == pytest.approx(0.642569, rel=1e-5)
    # The variant with K^2 in place of R^2 in the numerator gives a different,
    # wrong value; the function must not reproduce it.
    wrong = (47.16**2 + 47.16**2 * 0.25) / (47.16**2 + 45.0**2)
    assert wrong == pytest.approx(0.654283, rel=1e-5)
    assert ce != pytest.approx(wrong, rel=1e-3)


def test_eccentricity_midship_contact_is_one():
    assert eccentricity_factor(47.16, 0.0) == pytest.approx(1.0)


def test_contact_geometry_hand_check():
    # Contact 45 m aft of the bow on a 180 m ship: 45 m forward of midships.
    # Hull side offset B/2 = 16 m; berthing angle 6 deg, bow in.
    # R^2 = 45^2 + 16^2 = 2025 + 256 = 2281; R = 47.759816
    # R cos(phi) = 45 sin 6 + 16 cos 6 = 4.703764 + 15.912350 = 20.616114
    # cos(phi) = 20.616114 / 47.759816 = 0.431662 -> phi = 64.427 deg
    r, phi = contact_geometry(LBP_M, BEAM_M, x_from_bow_m=45.0, berthing_angle_deg=6.0)
    assert r == pytest.approx(47.759816, rel=1e-6)
    assert phi == pytest.approx(64.427, abs=2e-3)
    # Ce = (2224.0656 + 20.616114^2) / (2224.0656 + 2281)
    #    = (2224.0656 + 425.024) / 4505.0656 = 0.588025
    ce = eccentricity_factor(47.16, r, phi)
    assert ce == pytest.approx(0.588025, rel=1e-5)


def test_contact_geometry_without_offset_or_angle_is_perpendicular():
    r, phi = contact_geometry(LBP_M, 0.0, x_from_bow_m=45.0, berthing_angle_deg=0.0)
    assert r == pytest.approx(45.0)
    assert phi == pytest.approx(90.0)


# --------------------------------------------------------------------------
# Added mass (PIANC 2002 section 4.2.5)
# --------------------------------------------------------------------------


@pytest.mark.parametrize(
    "depth, expected",
    [
        (13.2, 1.8),  # UKC 1.2 m = 0.1 D
        (12.6, 1.8),  # UKC 0.6 m < 0.1 D
        (18.0, 1.5),  # UKC 6.0 m = 0.5 D
        (30.0, 1.5),  # UKC > 0.5 D
        (15.6, 1.65),  # UKC 3.6 m = 0.3 D: 1.8 - 0.3 * (0.3 - 0.1) / 0.4 = 1.65
    ],
)
def test_added_mass_from_keel_clearance(depth, expected):
    assert added_mass_factor_ukc(DRAFT_M, depth) == pytest.approx(expected, rel=1e-12)


def test_added_mass_rejects_grounded_vessel():
    with pytest.raises(ValueError, match="keel clearance"):
        added_mass_factor_ukc(DRAFT_M, 11.0)


def test_added_mass_ueda_hand_check():
    # 1 + pi * 12 / (2 * 0.8 * 32) = 1 + 37.699112 / 51.2 = 1.736311
    assert added_mass_factor_ueda(DRAFT_M, BEAM_M, 0.8) == pytest.approx(
        1.736311, rel=1e-6
    )


def test_added_mass_vasco_costa_hand_check():
    # 1 + 2 * 12 / 32 = 1.75
    assert added_mass_factor_vasco_costa(DRAFT_M, BEAM_M) == pytest.approx(1.75)


# --------------------------------------------------------------------------
# Abnormal impact (PIANC 2002 Table 4.2.5)
# --------------------------------------------------------------------------


@pytest.mark.parametrize(
    "vessel_type, largest, expected",
    [
        ("tanker_bulk", True, 1.25),
        ("tanker_bulk", False, 1.75),
        ("container", True, 1.5),
        ("container", False, 2.0),
        ("general_cargo", True, 1.75),
        ("ro_ro_ferry", True, 2.0),
        ("tug_workboat", True, 2.0),
    ],
)
def test_abnormal_impact_table(vessel_type, largest, expected):
    assert abnormal_impact_factor(vessel_type, largest=largest) == expected


def test_abnormal_impact_unknown_type():
    with pytest.raises(KeyError):
        abnormal_impact_factor("submarine")


# --------------------------------------------------------------------------
# Fender catalogue (read by path and schema) and selection
# --------------------------------------------------------------------------


def test_catalogue_loads_fixture_by_schema():
    cat = load_fender_catalogue(FIXTURE)
    assert len(cat) == 6
    first = cat[0]
    assert first.fender_id == "GEN-PN-1"
    assert first.rated_energy_knm == pytest.approx(150.0)
    assert first.source.startswith("fictional")


def test_catalogue_rejects_missing_column(tmp_path):
    bad = tmp_path / "bad.csv"
    cols = [c for c in FENDER_CATALOGUE_COLUMNS if c != "reaction_kn"]
    bad.write_text(",".join(cols) + "\n", encoding="utf-8")
    with pytest.raises(ValueError, match="reaction_kn"):
        load_fender_catalogue(bad)


def test_select_smallest_adequate_fender():
    # required 388.8 kNm, single contact, no reductions:
    # GEN-PN-3 (400) is the lowest rating >= 388.8 -> utilisation 0.972
    cat = load_fender_catalogue(FIXTURE)
    picks = select_fenders(cat, required_energy_knm=388.8)
    assert picks[0].fender.fender_id == "GEN-PN-3"
    assert picks[0].utilisation == pytest.approx(0.972, rel=1e-12)
    assert all(p.utilisation <= 1.0 for p in picks)


def test_select_applies_tolerance_and_angular_reduction():
    # GEN-PN-3: 400 * (1 - 0.10) * 0.95 = 342.0 < 388.8 -> rejected
    # GEN-PN-4: 700 * 0.9 * 0.95 = 598.5 -> utilisation 388.8 / 598.5 = 0.649624
    cat = load_fender_catalogue(FIXTURE)
    picks = select_fenders(
        cat, required_energy_knm=388.8, energy_tolerance=0.10, angular_factor=0.95
    )
    assert picks[0].fender.fender_id == "GEN-PN-4"
    assert picks[0].characteristic_energy_knm == pytest.approx(598.5)
    assert picks[0].utilisation == pytest.approx(0.649624, rel=1e-5)


def test_select_filters_on_hull_pressure_and_reaction():
    cat = load_fender_catalogue(FIXTURE)
    # GEN-PN-4 (hull pressure 210 kPa) fails a 200 kPa limit; GEN-PN-5 passes
    picks = select_fenders(cat, required_energy_knm=500.0, max_hull_pressure_kpa=200.0)
    ids = [p.fender.fender_id for p in picks]
    assert "GEN-PN-4" not in ids
    assert ids[0] == "GEN-PN-5"
    # a reaction cap of 1500 kN removes GEN-PN-5 (1600 kN) as well
    picks = select_fenders(
        cat,
        required_energy_knm=500.0,
        max_hull_pressure_kpa=200.0,
        max_reaction_kn=1500.0,
    )
    assert [p.fender.fender_id for p in picks] == ["GEN-FM-6"]


def test_select_fenders_sharing_divides_energy_only():
    # Two fenders in contact share 388.8 kNm equally -> 194.4 kNm each.
    # GEN-PN-2 (250) is the smallest adequate; utilisation 0.7776.
    cat = load_fender_catalogue(FIXTURE)
    picks = select_fenders(cat, required_energy_knm=388.8, fenders_in_contact=2)
    assert picks[0].fender.fender_id == "GEN-PN-2"
    assert picks[0].utilisation == pytest.approx(0.7776, rel=1e-12)
    # hull pressure is a property of each fender's own deflection: not divided
    assert picks[0].hull_pressure_kpa == pytest.approx(
        picks[0].fender.hull_pressure_kpa
    )


def test_select_returns_empty_when_nothing_adequate():
    cat = load_fender_catalogue(FIXTURE)
    assert select_fenders(cat, required_energy_knm=1.0e6) == []


def test_end_to_end_generic_supramax():
    # Cb from particulars, K, quarter-point Ce (phi = 90), Cm from UKC,
    # abnormal factor for the largest bulk carrier, then select a fender.
    cb = block_coefficient(DISPLACEMENT_T, LBP_M, BEAM_M, DRAFT_M)  # 0.846883
    k = radius_of_gyration(cb, LBP_M)  # (0.19 * 0.846883 + 0.11) * 180 = 48.763
    ce = eccentricity_factor(k, LBP_M / 4)
    cm = added_mass_factor_ukc(DRAFT_M, 13.8)  # UKC 1.8 = 0.15 D -> 1.7625
    e_normal = berthing_energy(DISPLACEMENT_T, 0.12, ce=ce, cm=cm)
    e_design = e_normal * abnormal_impact_factor("tanker_bulk", largest=True)
    # hand: K = 48.76300; K^2 = 2377.830; Ce = 2377.830 / 4402.830 = 0.540070
    assert k == pytest.approx(48.76300, rel=1e-5)
    assert ce == pytest.approx(0.540070, rel=1e-5)
    assert cm == pytest.approx(1.7625)
    # E = 432 * 0.540070 * 1.7625 = 411.2142; * 1.25 = 514.0178 kNm
    assert e_normal == pytest.approx(411.2142, rel=1e-5)
    assert e_design == pytest.approx(514.0178, rel=1e-5)
    picks = select_fenders(load_fender_catalogue(FIXTURE), required_energy_knm=e_design)
    assert picks[0].fender.fender_id == "GEN-PN-4"
    assert math.isclose(picks[0].utilisation, e_design / 700.0)
