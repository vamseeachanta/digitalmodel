"""
Tests for asset_integrity.common.fad.FAD (Failure Assessment Diagram).

Tests cover:
- FAD initialisation from config
- Plastic collapse load ratio limit (Lr_max) calculation per BS7910 cl.7.3.2
- BS7910_2013_option_1 DataFrame structure and boundary conditions
- Fr_value at Lr=0 is 1.0 (by definition)
- FAD boundary: Fr_value at Lr_max is 0.0
- get_BS7910_2013_FAD returns a dict with option keys
"""

import math
import pytest
import pandas as pd


# ---------------------------------------------------------------------------
# Helper — minimal config for FAD
# ---------------------------------------------------------------------------


def _make_fad_cfg(smys: float = 448.0e6, smus: float = 531.0e6, e: float = 200e9):
    """Return a minimal config dict for FAD initialisation."""
    return {
        "Outer_Pipe": {
            "Geometry": {
                "Nominal_OD": 0.3239,
                "Nominal_ID": 0.2953,
                "Design_WT": 0.01430,
            },
            "Material": {
                "Material": "steel",
                "Material_Grade": "X65",
            },
        },
        "Inner_Pipe": None,
        "Material": {
            "steel": {
                "E": e,
                "Rho": 7850.0,
                "Poissionsratio": 0.3,
                "Grades": {
                    "X65": {
                        "SMYS": smys,
                        "SMUS": smus,
                    }
                },
            }
        },
    }


def _make_fad(smys=448.0e6, smus=531.0e6, e=200e9):
    from digitalmodel.asset_integrity.common.fad import FAD
    cfg = _make_fad_cfg(smys=smys, smus=smus, e=e)
    return FAD(cfg)


# ---------------------------------------------------------------------------
# Plastic collapse load ratio limit
# ---------------------------------------------------------------------------


class TestPlasticCollapseLoadRatioLimit:
    """Tests for Lr_max per BS7910:2013 clause 7.3.2."""

    def test_lr_max_formula(self):
        # Lr_max = (SMYS + SMUS) / (2 * SMYS)
        smys, smus = 448.0e6, 531.0e6
        fad = _make_fad(smys=smys, smus=smus)
        expected = (smys + smus) / (2 * smys)
        actual = fad.get_plastic_collapse_load_ratio_limit()
        assert actual == pytest.approx(expected, rel=1e-8)

    def test_lr_max_above_one_for_smus_gt_smys(self):
        # For any real steel SMUS > SMYS, Lr_max > 1
        fad = _make_fad(smys=450e6, smus=550e6)
        lr_max = fad.get_plastic_collapse_load_ratio_limit()
        assert lr_max > 1.0

    def test_lr_max_equal_one_when_smys_equals_smus(self):
        # Edge case: perfectly elastic-plastic material
        fad = _make_fad(smys=400e6, smus=400e6)
        lr_max = fad.get_plastic_collapse_load_ratio_limit()
        assert lr_max == pytest.approx(1.0)

    def test_lr_max_increases_with_larger_smus(self):
        fad_low = _make_fad(smys=450e6, smus=500e6)
        fad_high = _make_fad(smys=450e6, smus=600e6)
        assert fad_high.get_plastic_collapse_load_ratio_limit() > \
               fad_low.get_plastic_collapse_load_ratio_limit()

    def test_lr_max_decreases_with_larger_smys_fixed_smus(self):
        fad_low_smys = _make_fad(smys=300e6, smus=550e6)
        fad_high_smys = _make_fad(smys=500e6, smus=550e6)
        # Higher SMYS → lower Lr_max (denominator grows faster than numerator)
        assert fad_high_smys.get_plastic_collapse_load_ratio_limit() < \
               fad_low_smys.get_plastic_collapse_load_ratio_limit()

    def test_lr_max_positive(self):
        fad = _make_fad()
        assert fad.get_plastic_collapse_load_ratio_limit() > 0


# ---------------------------------------------------------------------------
# FAD dict structure
# ---------------------------------------------------------------------------


class TestGetBS7910_2013FAD:
    """Tests for the top-level FAD builder method."""

    def _run(self):
        fad = _make_fad()
        return fad.get_BS7910_2013_FAD()

    def test_returns_dict(self):
        result = self._run()
        assert isinstance(result, dict)

    def test_option_1_key_present(self):
        result = self._run()
        assert "option_1" in result

    def test_option_2_key_present(self):
        result = self._run()
        assert "option_2" in result

    def test_option_3_key_present(self):
        result = self._run()
        assert "option_3" in result

    def test_option_2_is_none(self):
        result = self._run()
        assert result["option_2"] is None

    def test_option_3_is_none(self):
        result = self._run()
        assert result["option_3"] is None

    def test_option_1_is_dataframe(self):
        result = self._run()
        assert isinstance(result["option_1"], pd.DataFrame)


# ---------------------------------------------------------------------------
# BS7910_2013_option_1 DataFrame
# ---------------------------------------------------------------------------


class TestBS7910Option1:
    """Tests for the BS7910:2013 option 1 FAD curve DataFrame."""

    def _get_df(self, smys=448.0e6, smus=531.0e6, e=200e9):
        fad = _make_fad(smys=smys, smus=smus, e=e)
        fad.get_BS7910_2013_FAD()
        return fad.FAD["option_1"]

    def test_dataframe_has_lr_column(self):
        df = self._get_df()
        assert "L_r" in df.columns

    def test_dataframe_has_kr_column(self):
        df = self._get_df()
        assert "K_r" in df.columns

    def test_dataframe_has_rows(self):
        df = self._get_df()
        assert len(df) > 0

    def test_lr_starts_at_zero(self):
        df = self._get_df()
        assert df["L_r"].iloc[0] == pytest.approx(0.0)

    def test_kr_at_lr_zero_is_one(self):
        # At Lr = 0: Fr = (1 + 0)^(-0.5) * (0.3 + 0.7*exp(0)) = 1 * 1 = 1
        df = self._get_df()
        kr_at_zero = df[df["L_r"] < 1e-9]["K_r"].values
        assert len(kr_at_zero) > 0
        assert kr_at_zero[0] == pytest.approx(1.0, rel=1e-4)

    def test_kr_at_lr_max_is_zero(self):
        df = self._get_df()
        last_kr = df["K_r"].iloc[-1]
        assert last_kr == pytest.approx(0.0)

    def test_lr_ends_at_lr_max(self):
        smys, smus = 448.0e6, 531.0e6
        lr_max_expected = (smys + smus) / (2 * smys)
        df = self._get_df(smys=smys, smus=smus)
        last_lr = df["L_r"].iloc[-1]
        assert last_lr == pytest.approx(lr_max_expected, rel=1e-6)

    def test_kr_values_non_negative(self):
        df = self._get_df()
        assert (df["K_r"] >= 0).all()

    def test_lr_values_non_negative(self):
        df = self._get_df()
        assert (df["L_r"] >= 0).all()

    def test_kr_generally_decreasing_with_lr(self):
        # FAD curve is broadly decreasing from Lr=0 to Lr_max.
        # Check that the last K_r value is less than the first.
        df = self._get_df()
        assert df["K_r"].iloc[-1] < df["K_r"].iloc[0]

    def test_dataframe_types_are_numeric(self):
        df = self._get_df()
        assert df["L_r"].dtype.kind in ("f", "i")
        assert df["K_r"].dtype.kind in ("f", "i")

    def test_high_yield_strength_shifts_lr_max(self):
        # Higher SMYS (close to SMUS) → Lr_max closer to 1.0
        df_low = self._get_df(smys=350e6, smus=550e6)
        df_high = self._get_df(smys=520e6, smus=550e6)
        lr_max_low = df_low["L_r"].iloc[-1]
        lr_max_high = df_high["L_r"].iloc[-1]
        assert lr_max_high < lr_max_low
