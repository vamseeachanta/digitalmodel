"""Tests for converting a roll damping target into what each solver accepts.

AQWA reports percentage critical damping and accepts only an absolute damping
matrix. OrcaWave accepts both. Matching the two therefore requires the
conversion, and the conversion has two traps: the critical damping depends on
the frequency-dependent added inertia, and the two solvers use different unit
systems for the same matrix.

The reference values are taken from the AQWA ship RAO example shipped in this
repository, `docs/domains/aqwa/examples/03_dat/001_ship_raos/`. Its `.LIS` was
produced by ANSYS in Workbench 2022 R2, independently of this code, and prints
both an undamped natural period and an approximate percentage critical damping
against wave frequency. Reproducing those two columns is what establishes that
the formula implemented here is the one AQWA uses.
"""

import math
import re

import pytest

from digitalmodel.hydrodynamics.diffraction.roll_damping import (
    ORCAWAVE_SI_PER_NEWTON_METRE_SECOND,
    CriticalDampingBasis,
    RollDampingError,
    critical_damping,
    damping_to_percent_critical,
    fidp_row_card,
    percent_critical_to_damping,
    representable_value,
    resonant_basis,
    to_orcawave_si,
)

# --- reference values, 001_SHIP_RAOS ----------------------------------------
# Roll hydrostatic stiffness, from the LIS "TOTAL HYDROSTATIC STIFFNESS" block,
# with respect to the structure centre of gravity. N m/rad.
C44 = 3.21575e08
# Roll inertia about the centre of gravity, from the deck's PMAS record. kg m2.
I44 = 8.35728e09

# omega rad/s, A44 kg m2, B44 N m s/rad: the roll diagonal of the added mass
# and radiation damping blocks at each wave frequency.
COEFFICIENTS = [
    (0.1000, 2.9322e09, 4.2551e02),
    (0.4074, 3.0364e09, 7.6063e06),
    (0.7149, 3.1401e09, 2.4752e08),
    (1.0223, 2.8880e09, 6.4600e08),
    (1.3298, 2.5444e09, 8.4688e08),
    (1.6372, 2.3539e09, 7.8489e08),
    (1.9446, 2.2933e09, 5.9743e08),
    (2.2521, 2.2895e09, 4.2702e08),
]
# The LIS "APPROXIMATE PERCENTAGE CRITICAL DAMPING" roll column, one decimal.
AQWA_PERCENT_CRITICAL = [0.0, 0.2, 6.4, 17.0, 22.6, 21.1, 16.1, 11.5]
# The LIS "UNDAMPED NATURAL PERIOD" roll column, seconds, two decimals.
AQWA_NATURAL_PERIOD = [37.23, 37.40, 37.57, 37.16, 36.58, 36.26, 36.16, 36.15]


class TestAgainstTheShippedAqwaResults:
    """The external check: reproduce what ANSYS printed, from the same inputs."""

    @pytest.mark.parametrize(
        "index", range(len(COEFFICIENTS)), ids=[f"w{c[0]}" for c in COEFFICIENTS]
    )
    def test_percent_critical_matches_the_lis_column(self, index):
        _, added, damping = COEFFICIENTS[index]
        percent = damping_to_percent_critical(damping, C44, I44, added)
        assert percent == pytest.approx(AQWA_PERCENT_CRITICAL[index], abs=0.05)

    @pytest.mark.parametrize(
        "index", range(len(COEFFICIENTS)), ids=[f"w{c[0]}" for c in COEFFICIENTS]
    )
    def test_natural_period_matches_the_lis_column(self, index):
        _, added, _ = COEFFICIENTS[index]
        basis = CriticalDampingBasis(C44, I44, added)
        assert basis.natural_period == pytest.approx(
            AQWA_NATURAL_PERIOD[index], abs=0.01
        )

    def test_the_column_is_not_reproduced_by_ignoring_added_inertia(self):
        """Guards the tests above against passing for the wrong reason.

        Dropping A44 leaves the structural inertia only, which is the mistake
        the conversion exists to prevent. The added inertia is roughly 30% of
        the total here, so the error it introduces is about 14% on the critical
        damping -- small enough that a loose tolerance would hide it, which is
        why the tolerance above is 0.05 and this margin is checked explicitly.
        """
        _, _, damping = COEFFICIENTS[4]
        wrong = damping_to_percent_critical(damping, C44, I44, 0.0)
        margin = abs(wrong - AQWA_PERCENT_CRITICAL[4])
        assert margin > 20 * 0.05, (
            f"dropping the added inertia moves the answer by only {margin:.3f} "
            "points, so the tolerance of 0.05 is not discriminating"
        )


class TestCriticalDamping:
    def test_equals_twice_the_root_of_stiffness_times_total_inertia(self):
        assert critical_damping(C44, I44, 2.9322e09) == pytest.approx(
            2.0 * math.sqrt(C44 * (I44 + 2.9322e09))
        )

    def test_percent_and_damping_round_trip(self):
        damping = percent_critical_to_damping(5.0, C44, I44, 2.9322e09)
        assert damping_to_percent_critical(
            damping, C44, I44, 2.9322e09
        ) == pytest.approx(5.0)

    def test_zero_damping_is_zero_percent(self):
        assert damping_to_percent_critical(0.0, C44, I44, 2.9322e09) == 0.0

    def test_scales_linearly_with_the_target(self):
        one = percent_critical_to_damping(1.0, C44, I44, 2.9322e09)
        ten = percent_critical_to_damping(10.0, C44, I44, 2.9322e09)
        assert ten == pytest.approx(10.0 * one)

    @pytest.mark.parametrize(
        "stiffness,inertia,added",
        [
            (0.0, I44, 1.0e09),
            (-1.0, I44, 1.0e09),
            (C44, 0.0, 0.0),
            (C44, -1.0, 0.0),
            (C44, I44, -2.0e10),
        ],
    )
    def test_rejects_a_basis_that_is_not_physical(self, stiffness, inertia, added):
        with pytest.raises(RollDampingError):
            critical_damping(stiffness, inertia, added)

    def test_rejects_a_negative_target(self):
        with pytest.raises(RollDampingError):
            percent_critical_to_damping(-1.0, C44, I44, 1.0e09)

    def test_rejects_a_negative_damping(self):
        with pytest.raises(RollDampingError):
            damping_to_percent_critical(-1.0, C44, I44, 1.0e09)

    def test_basis_reports_natural_frequency_consistently(self):
        basis = CriticalDampingBasis(C44, I44, 2.9322e09)
        assert basis.natural_frequency == pytest.approx(
            2.0 * math.pi / basis.natural_period
        )
        assert basis.critical_damping == pytest.approx(
            2.0 * (I44 + 2.9322e09) * basis.natural_frequency
        )


class TestResonantBasis:
    """Pinning the target at roll resonance, where the added inertia is implicit."""

    def test_solves_the_implicit_natural_frequency(self):
        table = [(w, a) for w, a, _ in COEFFICIENTS]
        basis = resonant_basis(C44, I44, table)
        # The relation omega_n^2 = C / (I + A(omega_n)) must hold at the answer.
        assert basis.natural_frequency**2 == pytest.approx(
            C44 / (I44 + basis.added_inertia), rel=1e-6
        )

    def test_resonance_for_this_hull_falls_just_inside_the_solved_range(self):
        """Roll resonance is near 0.17 rad/s.

        That is above the lowest solved frequency of 0.100 rad/s, so the added
        inertia at resonance is interpolated from solver output rather than
        held at an endpoint. It sits between the first two tabulated values,
        and the natural period agrees with the 36.2 to 37.6 s the LIS reports.
        """
        table = [(w, a) for w, a, _ in COEFFICIENTS]
        basis = resonant_basis(C44, I44, table)
        assert table[0][0] < basis.natural_frequency < table[1][0]
        assert basis.extrapolated is False
        assert min(table[0][1], table[1][1]) <= basis.added_inertia <= max(
            table[0][1], table[1][1]
        )
        assert 36.0 < basis.natural_period < 38.0

    def test_resonance_below_the_table_is_held_and_flagged(self):
        """Outside the solved range the added inertia is held, not extrapolated.

        Holding is the defensible choice because a linear extrapolation of
        added inertia has no physical warrant, but the caller must be told that
        the value at resonance was not computed by the solver.
        """
        table = [(0.50, 1.0e09), (1.00, 1.2e09)]
        basis = resonant_basis(8.0e07, 1.0e09, table)
        assert basis.natural_frequency < table[0][0]
        assert basis.extrapolated is True
        assert basis.added_inertia == pytest.approx(table[0][1])

    def test_resonance_inside_the_range_interpolates(self):
        table = [(0.10, 1.0e09), (0.50, 2.0e09), (1.00, 3.0e09)]
        # Chosen so the fixed point lands at 0.3 rad/s, strictly between
        # tabulated frequencies rather than on one of them.
        basis = resonant_basis(2.25e08, 1.0e09, table)
        assert basis.natural_frequency == pytest.approx(0.3, rel=1e-6)
        assert basis.added_inertia == pytest.approx(1.5e09, rel=1e-6)
        assert table[0][0] < basis.natural_frequency < table[-1][0]
        assert basis.extrapolated is False

    def test_rejects_a_table_with_fewer_than_two_points(self):
        with pytest.raises(RollDampingError):
            resonant_basis(C44, I44, [(0.1, 2.9e09)])

    def test_rejects_an_unsorted_or_duplicated_table(self):
        with pytest.raises(RollDampingError):
            resonant_basis(C44, I44, [(0.5, 1.0e09), (0.1, 2.0e09)])
        with pytest.raises(RollDampingError):
            resonant_basis(C44, I44, [(0.1, 1.0e09), (0.1, 2.0e09)])


class TestUnits:
    """The 1000x trap: AQWA takes N m s/rad, the OrcaWave SI case takes kN m s/rad."""

    def test_conversion_factor_is_one_thousandth(self):
        assert ORCAWAVE_SI_PER_NEWTON_METRE_SECOND == pytest.approx(1.0e-03)

    def test_converts_a_critical_fraction_to_orcawave_units(self):
        damping = percent_critical_to_damping(5.0, C44, I44, 2.9322e09)
        assert to_orcawave_si(damping) == pytest.approx(damping / 1000.0)

    def test_the_two_solvers_receive_numerically_different_values(self):
        damping = percent_critical_to_damping(5.0, C44, I44, 2.9322e09)
        assert to_orcawave_si(damping) != pytest.approx(damping)


class TestFidpCard:
    def test_card_is_fixed_column_and_carries_the_row_index(self):
        card = fidp_row_card(4, [0.0, 0.0, 0.0, 1.9054e08, 0.0, 0.0])
        assert card.startswith("      FIDP")
        assert len(card) == 80
        assert card[10:20].strip() == "4"
        # Six ten-column value fields follow the index field.
        assert card[20:80].rstrip() == (
            " 0.000e+00 0.000e+00 0.000e+00 1.905e+08 0.000e+00 0.000e+00"
        )

    def test_a_negative_value_fills_its_field_exactly(self):
        """A real deck carries entries like -9.332e+10, which fill the field.

        The value then abuts the one before it, so a whitespace split sees one
        token where there are two. Decks written by AQWA itself do this -- see
        `F_FST1_L00_DAMPAD.dat` row 5 -- so a reader must parse the numbers
        rather than the spaces between them. That is what is asserted here.
        """
        card = fidp_row_card(5, [0.0, 0.0, 0.0, 0.0, -9.332e10, 0.0])
        assert len(card) == 80
        assert "0.000e+00-9.332e+10" in card, "the fields do not abut as expected"
        assert len(card.split()) == 7, "a naive whitespace split must lose one"
        numbers = re.findall(r"[-+]?\d*\.?\d+(?:[eEdD][-+]?\d+)?", card[10:])
        assert [float(n) for n in numbers] == [
            5.0, 0.0, 0.0, 0.0, 0.0, -9.332e10, 0.0
        ]

    def test_a_value_is_rounded_to_what_the_card_can_hold(self):
        """A FIDP field carries four significant figures, and no more."""
        assert representable_value(1.9054e08) == pytest.approx(1.905e08, rel=1e-12)
        assert representable_value(-9.332e10) == pytest.approx(-9.332e10, rel=1e-12)
        card = fidp_row_card(4, [0.0, 0.0, 0.0, 1.9054e08, 0.0, 0.0])
        assert "1.905e+08" in card

    def test_six_values_are_required(self):
        with pytest.raises(RollDampingError):
            fidp_row_card(4, [0.0, 0.0, 0.0])

    @pytest.mark.parametrize("row", [0, 7, -1])
    def test_row_must_be_a_mode_index(self, row):
        with pytest.raises(RollDampingError):
            fidp_row_card(row, [0.0] * 6)

    def test_card_round_trips_through_the_deck_basis_reader(self, tmp_path):
        """A generated card must be read back as the value that was written."""
        from digitalmodel.hydrodynamics.diffraction.aqwa_deck_basis import (
            read_aqwa_basis,
        )

        # A minimal single-structure deck, built here so the test depends on no
        # particular checkout location.
        text = "\n".join([
            "* Hydrodynamic Solver Unit System : Metric: kg, m [N]",
            "JOB AQWA  LINE",
            "OPTIONS REST END",
            "          COOR",
            f"{1:>6d}{98000:>5d}" + " " * 9
            + f"{0.0:>10.4f}{0.0:>10.4f}{-4.0:>10.4f}",
            " END",
            "          MATE",
            "     1         98000  45000000",
            " END",
            "          GEOM",
            "     1PMAS     98000 8.358e+09 0.000e+00 0.000e+00"
            " 1.130e+11 0.000e+00 1.222e+11",
            " END",
            "          WFS1",
            " END",
        ]) + "\n"
        target = 1.9054e08
        rows = [
            fidp_row_card(
                mode, [target if mode == 4 and c == 3 else 0.0 for c in range(6)]
            )
            for mode in range(1, 7)
        ]
        stamped = text.replace(
            "          WFS1\n END",
            "          WFS1\n" + "\n".join(rows) + "\n END",
            1,
        )
        assert stamped != text, "the empty Deck 7 was not found in the source deck"
        deck = tmp_path / "damped.dat"
        deck.write_text(stamped, encoding="ascii", errors="ignore")

        basis = read_aqwa_basis(str(deck))
        assert basis.has_additional_damping is True
        # The card holds four significant figures, so the value that comes back
        # is the representable one, exactly.
        assert basis.additional_damping[3][3] == pytest.approx(
            representable_value(target), rel=1e-12
        )
        assert basis.additional_damping[3][3] != pytest.approx(target, rel=1e-12)
