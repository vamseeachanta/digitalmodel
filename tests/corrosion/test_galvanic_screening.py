# ABOUTME: Tests for galvanic compatibility screening (MIL-STD-889 anodic-index
# ABOUTME: method): symmetry, known couples, monotonicity, matrix, env ordering.
"""Tests for :mod:`digitalmodel.corrosion.galvanic_screening`."""

import itertools

import pytest

from digitalmodel.corrosion.galvanic_screening import (
    ANODIC_INDEX_V,
    ENVIRONMENTS,
    compatibility_matrix,
    screen_couple,
    verdict_rank,
)

METALS = list(ANODIC_INDEX_V)
ENVS = list(ENVIRONMENTS)


class TestAnodicIndexTable:
    def test_published_anchor_values(self):
        # Anchor points of the published MIL-STD-889 anodic-index table.
        assert ANODIC_INDEX_V["gold"] == 0.00
        assert ANODIC_INDEX_V["silver"] == 0.15
        assert ANODIC_INDEX_V["carbon_steel"] == 0.85
        assert ANODIC_INDEX_V["zinc"] == 1.25
        assert ANODIC_INDEX_V["magnesium"] == 1.75

    def test_thresholds_published_rule(self):
        assert ENVIRONMENTS["harsh_marine"] == 0.15
        assert ENVIRONMENTS["normal_industrial"] == 0.25
        assert ENVIRONMENTS["controlled_indoor"] == 0.50


class TestScreenCouple:
    @pytest.mark.parametrize("env", ENVS)
    def test_symmetry(self, env):
        for a, b in itertools.combinations(METALS, 2):
            r_ab = screen_couple(a, b, env)
            r_ba = screen_couple(b, a, env)
            assert r_ab.verdict == r_ba.verdict
            assert r_ab.delta_v == r_ba.delta_v
            assert r_ab.anodic == r_ba.anodic
            assert r_ab.cathodic == r_ba.cathodic

    @pytest.mark.parametrize("env", ENVS)
    @pytest.mark.parametrize("metal", METALS)
    def test_same_metal_zero_delta_ok(self, metal, env):
        r = screen_couple(metal, metal, env)
        assert r.delta_v == 0.0
        assert r.verdict == "OK"
        assert r.anodic is None and r.cathodic is None

    def test_zinc_carbon_steel_harsh_marine(self):
        # Known result: galvanized steel — zinc sacrificial to steel,
        # dV = 1.25 - 0.85 = 0.40 V > 0.15 V allowable in harsh marine.
        r = screen_couple("zinc", "carbon_steel", "harsh_marine")
        assert r.delta_v == pytest.approx(0.40)
        assert r.verdict in ("PROTECT", "MARGINAL")
        assert r.verdict == "PROTECT"  # 0.40 > 0.15 + 0.05
        assert r.anodic == "zinc"
        assert r.cathodic == "carbon_steel"

    def test_stainless_copper_ok_in_harsh_marine(self):
        # dV = 0.50 - 0.35 = 0.15 V — exactly at the harsh-marine allowable.
        r = screen_couple("stainless_316_passive", "copper", "harsh_marine")
        assert r.delta_v == pytest.approx(0.15)
        assert r.verdict == "OK"

    def test_marginal_band(self):
        # dV = 0.20 V: 0.05 V over the harsh-marine allowable -> MARGINAL,
        # but OK in normal_industrial (0.25 V allowable).
        r = screen_couple("stainless_316_passive", "nickel", "harsh_marine")
        assert r.delta_v == pytest.approx(0.20)
        assert r.verdict == "MARGINAL"
        assert (
            screen_couple("stainless_316_passive", "nickel",
                          "normal_industrial").verdict == "OK"
        )

    @pytest.mark.parametrize("env", ENVS)
    def test_monotone_larger_delta_never_improves(self, env):
        # Sort couples by delta_v; verdict rank must be non-decreasing.
        results = [screen_couple(a, b, env)
                   for a, b in itertools.combinations_with_replacement(METALS, 2)]
        results.sort(key=lambda r: r.delta_v)
        ranks = [verdict_rank(r.verdict) for r in results]
        assert ranks == sorted(ranks)

    def test_anodic_member_has_larger_index(self):
        for a, b in itertools.combinations(METALS, 2):
            r = screen_couple(a, b, "harsh_marine")
            if r.anodic is not None:
                assert ANODIC_INDEX_V[r.anodic] > ANODIC_INDEX_V[r.cathodic]

    def test_area_ratio_note_present_for_dissimilar(self):
        r = screen_couple("aluminum_alloy", "stainless_316_passive",
                          "harsh_marine")
        assert any("area ratio" in n or "cathode/anode" in n for n in r.notes)

    def test_unknown_metal_raises(self):
        with pytest.raises(KeyError, match="Unknown metal"):
            screen_couple("unobtainium", "zinc", "harsh_marine")

    def test_unknown_environment_raises(self):
        with pytest.raises(KeyError, match="Unknown environment"):
            screen_couple("zinc", "copper", "outer_space")

    def test_custom_thresholds_parameterizable(self):
        strict = {"lab": 0.01}
        r = screen_couple("copper", "cupronickel", "lab", environments=strict)
        assert r.allowable_delta_v == 0.01
        assert r.verdict == "OK"  # same group, delta 0.0


class TestCompatibilityMatrix:
    @pytest.mark.parametrize("env", ENVS)
    def test_shape_and_diagonal(self, env):
        m = compatibility_matrix(env)
        n = len(METALS)
        assert m.shape == (n, n)
        assert list(m.index) == METALS
        assert list(m.columns) == METALS
        for metal in METALS:
            assert m.loc[metal, metal] == "OK"

    def test_symmetric(self):
        m = compatibility_matrix("harsh_marine")
        assert m.equals(m.T)

    def test_matches_screen_couple(self):
        m = compatibility_matrix("normal_industrial")
        for a, b in itertools.combinations(METALS, 2):
            assert m.loc[a, b] == screen_couple(a, b, "normal_industrial").verdict

    def test_metal_subset(self):
        subset = ["zinc", "carbon_steel", "copper"]
        m = compatibility_matrix("harsh_marine", metals=subset)
        assert m.shape == (3, 3)
        assert m.loc["zinc", "carbon_steel"] == "PROTECT"


class TestEnvironmentOrdering:
    def test_harsh_strictest_controlled_most_permissive(self):
        # For every couple, moving harsh -> normal -> controlled must never
        # worsen the verdict.
        order = ["harsh_marine", "normal_industrial", "controlled_indoor"]
        for a, b in itertools.combinations_with_replacement(METALS, 2):
            ranks = [verdict_rank(screen_couple(a, b, e).verdict)
                     for e in order]
            assert ranks == sorted(ranks, reverse=True)

    def test_thresholds_strictly_increasing(self):
        assert (ENVIRONMENTS["harsh_marine"]
                < ENVIRONMENTS["normal_industrial"]
                < ENVIRONMENTS["controlled_indoor"])
