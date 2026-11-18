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
    FenderRating,
    added_mass_factor_ueda,
    added_mass_factor_ukc,
    added_mass_factor_vasco_costa,
    berthing_energy,
    block_coefficient,
    contact_geometry,
    eccentricity_factor,
    energy_sensitivity,
    fender_size_sensitivity,
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
    # a caller-supplied design factor (1.25, assumed), then select a fender.
    cb = block_coefficient(DISPLACEMENT_T, LBP_M, BEAM_M, DRAFT_M)  # 0.846883
    k = radius_of_gyration(cb, LBP_M)  # (0.19 * 0.846883 + 0.11) * 180 = 48.763
    ce = eccentricity_factor(k, LBP_M / 4)
    cm = added_mass_factor_ukc(DRAFT_M, 13.8)  # UKC 1.8 = 0.15 D -> 1.7625
    e_normal = berthing_energy(DISPLACEMENT_T, 0.12, ce=ce, cm=cm)
    e_design = e_normal * 1.25
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


# --------------------------------------------------------------------------
# Sensitivity tables: one input at a time, and fender sizes against a base
# --------------------------------------------------------------------------

BASE_INPUTS = dict(
    displacement_t=DISPLACEMENT_T,
    velocity_m_s=0.12,
    ce=0.5,
    cm=1.8,
    cs=1.0,
    cc=1.0,
    abnormal_factor=1.25,
)


def test_energy_sensitivity_one_at_a_time():
    # base design energy: 0.5 * 60000 * 0.0144 * 0.5 * 1.8 * 1.25 = 486.0 kNm
    rows = energy_sensitivity(
        BASE_INPUTS,
        {
            "velocity_m_s": [(0.20, "moderate"), (0.08, "favourable")],
            "displacement_t": [(45_000.0, "part load")],
            "abnormal_factor": [(1.75, "smallest vessel")],
            "cc": [(0.9, "solid quay")],
        },
    )
    by = {(r.parameter, r.value): r for r in rows}
    assert by[("velocity_m_s", 0.20)].base_design_energy_knm == pytest.approx(486.0)
    # (0.20 / 0.12)^2 = 2.777778 -> +177.7778 %
    assert by[("velocity_m_s", 0.20)].change_pct == pytest.approx(177.7778, rel=1e-6)
    # (0.08 / 0.12)^2 = 0.444444 -> -55.5556 %
    assert by[("velocity_m_s", 0.08)].change_pct == pytest.approx(-55.5556, rel=1e-5)
    assert by[("displacement_t", 45_000.0)].change_pct == pytest.approx(-25.0)
    assert by[("abnormal_factor", 1.75)].change_pct == pytest.approx(40.0)
    assert by[("cc", 0.9)].design_energy_knm == pytest.approx(437.4)
    assert by[("cc", 0.9)].source == "solid quay"
    # the base row comes first at 0 %, then rows keep the order given
    assert rows[0].parameter == "base"
    assert rows[0].change_pct == 0.0
    assert rows[0].design_energy_knm == pytest.approx(486.0)
    assert [r.parameter for r in rows][1:3] == ["velocity_m_s", "velocity_m_s"]


def test_energy_sensitivity_rejects_unknown_input():
    with pytest.raises(KeyError):
        energy_sensitivity(BASE_INPUTS, {"tide": [(1.0, "x")]})
    with pytest.raises(KeyError):
        energy_sensitivity({"displacement_t": 1.0}, {})


def test_fender_size_sensitivity_against_base():
    # base GEN-PN-3: 400 kNm, 1100 kN, 135 kPa, 60 % of 2200 mm = 1320 mm
    # design energy 388.8 kNm -> base margin 400 / 388.8 - 1 = 2.880658 %
    cat = load_fender_catalogue(FIXTURE)
    rows = fender_size_sensitivity(
        cat,
        base_id="GEN-PN-3",
        design_energy_knm=388.8,
        fender_ids=["GEN-PN-3", "GEN-PN-4"],
    )
    base, big = rows
    assert base.is_base and not big.is_base
    assert base.energy_margin_pct == pytest.approx(2.880658, rel=1e-5)
    assert base.change_pct["rated_energy"] == pytest.approx(0.0)
    assert base.deflection_mm == pytest.approx(1320.0)
    # GEN-PN-4: 700 kNm, 1450 kN, 210 kPa, 60 % of 2600 = 1560 mm
    assert big.change_pct["rated_energy"] == pytest.approx(75.0)
    assert big.change_pct["reaction"] == pytest.approx(31.81818, rel=1e-6)
    assert big.change_pct["hull_pressure"] == pytest.approx(55.55556, rel=1e-6)
    assert big.change_pct["deflection"] == pytest.approx(18.18182, rel=1e-6)
    # margin 700 / 388.8 - 1 = 80.04115 %; change in percentage points 77.16049
    assert big.energy_margin_pct == pytest.approx(80.04115, rel=1e-6)
    assert big.change_pct["energy_margin_pp"] == pytest.approx(77.16049, rel=1e-6)


def test_fender_size_sensitivity_unknown_base():
    with pytest.raises(KeyError):
        fender_size_sensitivity(load_fender_catalogue(FIXTURE), "NOPE", 100.0)


# --------------------------------------------------------------------------
# Review fixes: input validation, unknown pressure, reaction tolerance,
# signed contact offset, sensitivity domain, base rows
# --------------------------------------------------------------------------


def _rating(**over):
    vals = dict(
        fender_id="T-1",
        manufacturer="generic",
        fender_type="pneumatic",
        diameter_mm=2200.0,
        length_mm=4000.0,
        internal_pressure_kpa=50.0,
        rated_energy_knm=400.0,
        reaction_kn=1100.0,
        hull_pressure_kpa=135.0,
        rated_deflection_pct=60.0,
        source="fictional test row",
    )
    vals.update(over)
    return FenderRating(**vals)


@pytest.mark.parametrize(
    "bad",
    [
        dict(displacement_t=math.nan),
        dict(displacement_t=math.inf),
        dict(velocity_m_s=math.nan),
        dict(velocity_m_s=math.inf),
        dict(ce=math.nan),
        dict(cm=math.inf),
        dict(cs=-0.9),
        dict(cc=math.nan),
    ],
)
def test_energy_rejects_non_finite_inputs(bad):
    args = dict(displacement_t=DISPLACEMENT_T, velocity_m_s=0.12, ce=0.5, cm=1.5)
    args.update(bad)
    with pytest.raises(ValueError):
        berthing_energy(**args)


@pytest.mark.parametrize(
    "over",
    [
        dict(rated_energy_knm=math.nan),
        dict(rated_energy_knm=-400.0),
        dict(reaction_kn=math.nan),
        dict(reaction_kn=-1.0),
        dict(hull_pressure_kpa=math.inf),
        dict(hull_pressure_kpa=-5.0),
        dict(diameter_mm=math.nan),
        dict(rated_deflection_pct=0.0),
    ],
)
def test_rating_rejects_non_finite_or_wrong_sign(over):
    with pytest.raises(ValueError):
        _rating(**over)


@pytest.mark.parametrize("cell", ["nan", "inf", "-400"])
def test_catalogue_rejects_bad_energy_cell(tmp_path, cell):
    lines = FIXTURE.read_text(encoding="utf-8").splitlines()
    head = lines[0]
    row = lines[3].split(",")
    row[FENDER_CATALOGUE_COLUMNS.index("rated_energy_knm")] = cell
    bad = tmp_path / "bad.csv"
    bad.write_text(head + "\n" + ",".join(row) + "\n", encoding="utf-8")
    with pytest.raises(ValueError, match="GEN-PN-3"):
        load_fender_catalogue(bad)


@pytest.mark.parametrize(
    "kwargs",
    [
        dict(required_energy_knm=math.nan),
        dict(required_energy_knm=math.inf),
        dict(required_energy_knm=-1.0),
        dict(required_energy_knm=500.0, fenders_in_contact=1.5),
        dict(required_energy_knm=500.0, fenders_in_contact=0),
        dict(required_energy_knm=500.0, fenders_in_contact=True),
        dict(required_energy_knm=500.0, energy_tolerance=math.nan),
        dict(required_energy_knm=500.0, angular_factor=math.nan),
        dict(required_energy_knm=500.0, reaction_tolerance=-0.1),
        dict(required_energy_knm=500.0, reaction_tolerance=math.inf),
        dict(required_energy_knm=500.0, max_reaction_kn=math.nan),
        dict(required_energy_knm=500.0, max_reaction_kn=-1.0),
        dict(required_energy_knm=500.0, max_hull_pressure_kpa=math.nan),
    ],
)
def test_select_rejects_invalid_arguments(kwargs):
    with pytest.raises(ValueError):
        select_fenders(load_fender_catalogue(FIXTURE), **kwargs)


def test_unknown_hull_pressure_excluded_when_limit_set():
    row = _rating(hull_pressure_kpa=None)
    # no pressure limit: the row is screened on energy alone
    assert len(select_fenders([row], 388.8)) == 1
    # a pressure limit cannot be shown to be met, so the row is excluded
    assert select_fenders([row], 388.8, max_hull_pressure_kpa=1000.0) == []


def test_reaction_tolerance_applies_to_check_and_returned_values():
    row = _rating()
    # demand 350; capacity 400 * 0.9 = 360 -> energy passes
    # base reaction 1100 <= 1150 passes without a reaction tolerance
    assert (
        len(select_fenders([row], 350.0, energy_tolerance=0.10, max_reaction_kn=1150.0))
        == 1
    )
    # +10 %: 1100 * 1.10 = 1210 kN > 1150 -> excluded
    assert (
        select_fenders(
            [row],
            350.0,
            energy_tolerance=0.10,
            reaction_tolerance=0.10,
            max_reaction_kn=1150.0,
        )
        == []
    )
    # returned values carry the tolerance: 1210 kN and 135 * 1.10 = 148.5 kPa
    pick = select_fenders([row], 350.0, reaction_tolerance=0.10)[0]
    assert pick.reaction_kn == pytest.approx(1210.0)
    assert pick.hull_pressure_kpa == pytest.approx(148.5)
    assert pick.fender.reaction_kn == pytest.approx(1100.0)
    # the pressure check uses the raised pressure: 148.5 > 140 -> excluded
    assert (
        select_fenders(
            [row], 350.0, reaction_tolerance=0.10, max_hull_pressure_kpa=140.0
        )
        == []
    )


@pytest.mark.parametrize(
    "x_from_bow, angle, expected_ce",
    [
        # forward contact (45 m fwd of midships), bow in: see the hand check above
        (45.0, 6.0, 0.588025),
        # aft contact, 45 m aft of midships, same orientation:
        # R cos(phi) = -45 sin 6 + 16 cos 6 = -4.703764 + 15.912350 = 11.208586
        # Ce = (2224.0656 + 125.6324) / 4505.0656 = 0.521568
        (135.0, 6.0, 0.521568),
        # the reverse angle swaps the two
        (135.0, -6.0, 0.588025),
        (45.0, -6.0, 0.521568),
    ],
)
def test_contact_geometry_keeps_sign_of_offset(x_from_bow, angle, expected_ce):
    r, phi = contact_geometry(
        LBP_M, BEAM_M, x_from_bow_m=x_from_bow, berthing_angle_deg=angle
    )
    assert r == pytest.approx(47.759816, rel=1e-6)
    assert eccentricity_factor(47.16, r, phi) == pytest.approx(expected_ce, rel=1e-5)


def test_energy_sensitivity_rejects_zero_base():
    base = dict(BASE_INPUTS, velocity_m_s=0.0)
    with pytest.raises(ValueError, match="zero"):
        energy_sensitivity(base, {"cc": [(0.9, "x")]})


@pytest.mark.parametrize("factor", [-1.0, 0.0, math.nan, math.inf])
def test_energy_sensitivity_rejects_bad_design_factor(factor):
    with pytest.raises(ValueError):
        energy_sensitivity(BASE_INPUTS, {"abnormal_factor": [(factor, "x")]})
    with pytest.raises(ValueError):
        energy_sensitivity(dict(BASE_INPUTS, abnormal_factor=factor), {})


def test_energy_sensitivity_base_row_only():
    rows = energy_sensitivity(BASE_INPUTS, {})
    assert len(rows) == 1
    assert rows[0].parameter == "base"
    assert rows[0].change_pct == 0.0
    assert rows[0].source == "base case"


def test_fender_size_sensitivity_always_includes_base_first():
    cat = load_fender_catalogue(FIXTURE)
    rows = fender_size_sensitivity(cat, "GEN-PN-3", 388.8, fender_ids=["GEN-PN-4"])
    assert [r.fender.fender_id for r in rows] == ["GEN-PN-3", "GEN-PN-4"]
    assert rows[0].is_base


def test_select_skips_capacity_that_underflows_to_zero():
    # Finite positive factors can underflow the adjusted capacity to 0.0;
    # with zero demand that row must be skipped, not divided by.
    row = load_fender_catalogue(FIXTURE)[2]
    picks = select_fenders(
        [row],
        0.0,
        energy_tolerance=math.nextafter(1.0, 0.0),
        angular_factor=math.ulp(0.0),
    )
    assert picks == []
