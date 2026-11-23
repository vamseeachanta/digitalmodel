"""Characterisation tests for the legacy BS 7910 module (#2160).

Pins the behaviour of the pure helper methods in
``asset_integrity.common.BS7910_critical_flaw_limits.BS7910_2013`` and
``fracture_mechanics_components`` around the pandas-2 / ``exit()`` / a-c
range fixes, so the modernisation is behaviour-preserving.  Methods are
exercised unbound on stubs carrying only the attributes each one reads; the
class constructor needs external solution tables and is not run here.

Expected values are derived by hand from the algorithm as written, not by
calling the code.
"""

from __future__ import annotations

import logging
from types import SimpleNamespace

import pandas as pd
import pytest

from digitalmodel.asset_integrity.common.BS7910_critical_flaw_limits import (
    BS7910_2013,
)

COLS = ["a_over_B", "a_over_c", "B_over_ri", "theta", "theta_location", "Mm", "Mb"]


def _solution_table():
    return pd.DataFrame(
        [
            [0.2, 0.5, 0.1, 90, "deepest", 1.0, 0.5],
            [0.4, 0.5, 0.1, 90, "deepest", 1.6, 1.1],
            [0.2, 0.5, 0.1, 0, "surface", 9.0, 9.0],  # other theta, ignored
        ],
        columns=COLS,
    )


class TestManualInterpolationUsingFilters:
    """The `DataFrame.append` site (pandas < 2 only) -> `pd.concat`."""

    def test_interpolates_between_bracketing_rows(self):
        # Query a/B = 0.3 sits midway between the 0.2 and 0.4 rows.
        # a/B factor = (0.3 - 0.2) / (0.4 - 0.2) = 0.5; a/c and B/ri are
        # exact table values so their factors are 0; the method averages the
        # three -> 1/6.  Mm = 1.0 + (1.6 - 1.0)/6 = 1.1; Mb = 0.5 + 0.6/6 = 0.6.
        M_m, M_b = BS7910_2013.dataframe_manual_interpolation_using_filters(
            None, 0.1, 0.3, 0.5, _solution_table(), 90, "deepest"
        )
        assert M_m == pytest.approx(1.1, abs=1e-12)
        assert M_b == pytest.approx(0.6, abs=1e-12)

    def test_exact_table_point_returns_table_values(self):
        M_m, M_b = BS7910_2013.dataframe_manual_interpolation_using_filters(
            None, 0.1, 0.2, 0.5, _solution_table(), 90, "deepest"
        )
        assert M_m == pytest.approx(1.0)
        assert M_b == pytest.approx(0.5)

    def test_out_of_range_clamps_to_edge_row(self):
        # a/B = 0.9 is above the table: low = high = 0.4 -> factor 0 -> edge row.
        M_m, M_b = BS7910_2013.dataframe_manual_interpolation_using_filters(
            None, 0.1, 0.9, 0.5, _solution_table(), 90, "deepest"
        )
        assert M_m == pytest.approx(1.6)
        assert M_b == pytest.approx(1.1)

    def test_input_table_not_mutated(self):
        df = _solution_table()
        before = df.copy()
        BS7910_2013.dataframe_manual_interpolation_using_filters(
            None, 0.1, 0.3, 0.5, df, 90, "deepest"
        )
        pd.testing.assert_frame_equal(df, before)


class TestReferenceAnnexes:
    """The two `exit()` calls -> raised exceptions."""

    @staticmethod
    def _stub(orientation, location):
        return SimpleNamespace(
            flaw=SimpleNamespace(
                geometry="thin_pipe", orientation=orientation, location=location
            )
        )

    @pytest.mark.parametrize(
        "orientation, location, si, rs",
        [
            ("axial", "internal_surface", "Annex_M_7_2_2", "Annex_P_9_2"),
            ("axial", "external_surface", "Annex_M_7_2_4", "Annex_P_9_4"),
            ("axial", "embedded", "Annex_M_7_3_6", "Annex_P_10_6"),
            ("circumferential", "internal_surface", "Annex_M_7_3_2", "Annex_P_10_2"),
            ("circumferential", "external_surface", "Annex_M_7_3_4", "Annex_P_10_4"),
            ("circumferential", "embedded", "Annex_M_7_3_6", "Annex_P_10_6"),
        ],
    )
    def test_known_combinations_resolve(self, orientation, location, si, rs):
        stub = self._stub(orientation, location)
        BS7910_2013.get_reference_annexes(stub)
        assert stub.stress_intensity_annex == si
        assert stub.reference_stress_annex == rs

    @pytest.mark.parametrize("orientation", ["axial", "circumferential"])
    def test_unknown_location_raises_instead_of_exiting(self, orientation):
        stub = self._stub(orientation, "not_a_location")
        with pytest.raises(ValueError, match="not_a_location"):
            BS7910_2013.get_reference_annexes(stub)


class TestFlawDimensionAcceptanceCheck:
    """The a/c range check must test both bounds (was `range[0]` twice)."""

    @staticmethod
    def _stub(a, c, B=10.0, ri=50.0, W=1000.0, annex="Annex_M_7_2_2"):
        return SimpleNamespace(
            stress_intensity_annex=annex,
            a=a,
            c=c,
            B=B,
            ri=ri,
            W=W,
            flaw=SimpleNamespace(location="internal_surface"),
        )

    def test_a_over_c_inside_range_is_acceptable(self, caplog):
        # a/c = 2/4 = 0.5 is inside the Annex M.7.2.2 range [0.05, 1.0]
        with caplog.at_level(logging.INFO):
            BS7910_2013.perform_flaw_dimension_acceptance_check(self._stub(2.0, 4.0))
        assert "a/c check acceptable" in caplog.text
        assert "flaw a/c" not in caplog.text

    def test_a_over_c_above_range_is_rejected(self, caplog):
        # a/c = 6/4 = 1.5 exceeds the upper bound 1.0
        with caplog.at_level(logging.INFO):
            BS7910_2013.perform_flaw_dimension_acceptance_check(self._stub(6.0, 4.0))
        assert "flaw a/c, 1.5 beyond code limits" in caplog.text

    def test_a_over_c_below_range_is_rejected(self, caplog):
        # a/c = 0.1/4 = 0.025 is below the lower bound 0.05
        with caplog.at_level(logging.INFO):
            BS7910_2013.perform_flaw_dimension_acceptance_check(self._stub(0.1, 4.0))
        assert "flaw a/c, 0.025 beyond code limits" in caplog.text


class TestMakeInitialFlawMonotonous:
    """The `df.ix` site (removed in pandas 1.0) -> `df.loc`."""

    def test_depth_is_clipped_to_running_minimum_after_sorting(self):
        from digitalmodel.asset_integrity.common.fracture_mechanics_components import (
            FractureMechanicsComponents,
        )

        df = pd.DataFrame(
            {"final_flaw_length": [3.0, 1.0, 2.0], "final_flaw_depth": [4.0, 7.0, 6.0]}
        )
        # sorted by length: (1, 7), (2, 6), (3, 4) -> depth already
        # non-increasing, unchanged.
        out = FractureMechanicsComponents.make_initial_flaw_monotonous(None, df)
        assert list(out["final_flaw_length"]) == [1.0, 2.0, 3.0]
        assert list(out["final_flaw_depth"]) == [7.0, 6.0, 4.0]

        df = pd.DataFrame(
            {"final_flaw_length": [1.0, 2.0, 3.0], "final_flaw_depth": [5.0, 7.0, 6.0]}
        )
        # row 1: 7 > 5 -> 5; row 2: 6 > 5 -> 5
        out = FractureMechanicsComponents.make_initial_flaw_monotonous(None, df)
        assert list(out["final_flaw_depth"]) == [5.0, 5.0, 5.0]
