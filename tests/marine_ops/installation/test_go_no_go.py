"""Tests for Go/No-Go integration into the pipeline."""
import sys
import importlib.util
from pathlib import Path
import pytest

# Load modules directly (avoid namespace issues)
def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    mod.__module__ = name
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod

_INSTALLATION_DIR = Path(__file__).resolve().parents[3] / "src/digitalmodel/marine_ops/installation"
gng = load_module("go_no_go", str(_INSTALLATION_DIR / "go_no_go.py"))
jl = load_module("jumper_lift", str(_INSTALLATION_DIR / "jumper_lift.py"))


class TestGoNoGoDecision:
    """Test overall Go/No-Go decision logic."""

    def setup_method(self):
        cfg = jl.KNOWN_JUMPER_CONFIGS["ballymore_mf_plet"]
        self.results = jl.run_jumper_analysis(cfg)
        self.decision = gng.evaluate_go_no_go(
            "Ballymore Manifold-to-PLET", self.results
        )

    def test_returns_decision(self):
        assert isinstance(self.decision, gng.GoNoGoDecision)

    def test_has_criteria(self):
        assert len(self.decision.criteria) > 0

    def test_jumper_name_set(self):
        assert self.decision.jumper_name == "Ballymore Manifold-to-PLET"

    def test_criteria_have_states(self):
        for c in self.decision.criteria:
            assert c.state in [
                gng.CriterionState.PASS,
                gng.CriterionState.WARNING,
                gng.CriterionState.FAIL,
            ]

    def test_criteria_have_values(self):
        for c in self.decision.criteria:
            assert isinstance(c.value, (int, float))

    def test_criteria_have_limits(self):
        for c in self.decision.criteria:
            assert isinstance(c.limit, (int, float))

    def test_criteria_have_descriptions(self):
        for c in self.decision.criteria:
            assert len(c.description) > 0

    def test_criteria_have_references(self):
        for c in self.decision.criteria:
            assert len(c.reference) > 0

    def test_summary_generated(self):
        assert len(self.decision.summary) > 50

    def test_no_failed_criteria(self):
        """Ballymore jumper should have no FAIL criteria."""
        fail_states = [c for c in self.decision.criteria if c.state == gng.CriterionState.FAIL]
        assert len(fail_states) == 0, f"Unexpected FAIL: {[c.name for c in fail_states]}"

    def test_overall_state_is_marginal_or_go(self):
        """Ballymore jumper should be GO or MARGINAL."""
        assert self.decision.overall_state in [
            gng.DecisionState.GO,
            gng.DecisionState.MARGINAL,
        ]


class TestCriterionChecks:
    """Test individual criterion evaluation."""

    def test_check_above_is_safe_pass(self):
        result = gng._check_criterion(
            "Test", value=10.0, limit=5.0, unit="x", above_is_safe=True
        )
        assert result.state == gng.CriterionState.PASS
        assert result.margin > 0

    def test_check_above_is_safe_fail(self):
        result = gng._check_criterion(
            "Test", value=3.0, limit=5.0, unit="x", above_is_safe=True
        )
        assert result.state == gng.CriterionState.FAIL
        assert result.margin < 0

    def test_check_below_is_safe_pass(self):
        result = gng._check_criterion(
            "Test", value=0.3, limit=0.7, unit="x", above_is_safe=False
        )
        assert result.state == gng.CriterionState.PASS

    def test_check_below_is_safe_fail(self):
        result = gng._check_criterion(
            "Test", value=0.9, limit=0.7, unit="x", above_is_safe=False
        )
        assert result.state == gng.CriterionState.FAIL

    def test_check_warning_zone(self):
        """Value close to limit should trigger WARNING."""
        result = gng._check_criterion(
            "Test", value=0.68, limit=0.7, unit="x", above_is_safe=False,
            warning_factor=0.9
        )
        assert result.state == gng.CriterionState.WARNING

    def test_warning_factor_default(self):
        result = gng._check_criterion(
            "Test", value=0.92, limit=1.0, unit="x", above_is_safe=False
        )
        # 0.92 is between 0.9*1.0 and 1.0 -> WARNING
        assert result.state == gng.CriterionState.WARNING


class TestCraneCriteria:
    """Test crane-specific criteria."""

    def setup_method(self):
        cfg = jl.KNOWN_JUMPER_CONFIGS["ballymore_mf_plet"]
        self.results = jl.run_jumper_analysis(cfg)

    def test_sz_crane_utilisation_passes(self):
        decision = gng.evaluate_go_no_go("test", self.results)
        sz_crit = [c for c in decision.criteria if "SZ" in c.name and "SWL" in c.name]
        assert len(sz_crit) == 1
        assert sz_crit[0].state == gng.CriterionState.PASS
        assert sz_crit[0].value < 0.7  # Well under limit

    def test_dz_crane_utilisation_passes(self):
        decision = gng.evaluate_go_no_go("test", self.results)
        dz_crit = [c for c in decision.criteria if "DZ" in c.name and "SWL" in c.name]
        assert len(dz_crit) == 1
        assert dz_crit[0].state == gng.CriterionState.PASS


class TestPrintDecision:
    """Test decision formatting."""

    def test_print_decision_returns_string(self):
        decision = gng.GoNoGoDecision(
            jumper_name="Test",
            overall_state=gng.DecisionState.GO,
            criteria=[
                gng.CriterionResult(
                    name="Test criterion",
                    state=gng.CriterionState.PASS,
                    value=1.0, limit=2.0, unit="x", margin=1.0
                )
            ],
            summary="Test summary"
        )
        output = gng.print_decision(decision)
        assert isinstance(output, str)
        assert "GO" in output
        assert "Test" in output

    def test_no_go_format(self):
        decision = gng.GoNoGoDecision(
            jumper_name="Bad Jumper",
            overall_state=gng.DecisionState.NO_GO,
            criteria=[
                gng.CriterionResult(
                    name="Failed criterion",
                    state=gng.CriterionState.FAIL,
                    value=1.0, limit=0.5, unit="x", margin=-0.5
                )
            ],
            summary="Test failure"
        )
        output = gng.print_decision(decision)
        assert "NO_GO" in output
        assert "FAIL" in output


class TestIssue472Criteria:
    """Issue #472: DNV weather/limit Go/No-Go threshold logic.

    Exercises the splash-zone Hs (DNV-ST-N001, < 1.0 m) and bend-radius
    (R/OD) criteria added for #472, plus governing-constraint reporting,
    boundary convention, and multiple-violation handling.
    """

    def setup_method(self):
        cfg = jl.KNOWN_JUMPER_CONFIGS["ballymore_mf_plet"]
        self.results = jl.run_jumper_analysis(cfg)

    # ----- helpers --------------------------------------------------------
    def _criterion(self, decision, name):
        match = [c for c in decision.criteria if c.name == name]
        assert len(match) == 1, f"expected exactly one '{name}'"
        return match[0]

    # ----- GO case (all within limits) ------------------------------------
    def test_go_all_within_limits(self):
        """Calm weather window (Hs 0.5 m) -> splash-zone PASS, no FAILs."""
        decision = gng.evaluate_go_no_go(
            "calm-window", self.results, conditions={"hs_m": 0.5}
        )
        splash = self._criterion(decision, "Splash zone Hs")
        assert splash.state == gng.CriterionState.PASS
        fails = [c for c in decision.criteria if c.state == gng.CriterionState.FAIL]
        assert fails == []
        assert decision.overall_state in (
            gng.DecisionState.GO, gng.DecisionState.MARGINAL
        )

    # ----- NO-GO case (one limit exceeded, governing reported) ------------
    def test_no_go_splash_zone_exceeded_is_governing(self):
        """Hs 1.5 m > 1.0 m limit -> NO_GO with splash-zone the governing FAIL."""
        decision = gng.evaluate_go_no_go(
            "rough-window", self.results, conditions={"hs_m": 1.5}
        )
        assert decision.overall_state == gng.DecisionState.NO_GO
        fails = [c for c in decision.criteria if c.state == gng.CriterionState.FAIL]
        assert len(fails) == 1
        assert fails[0].name == "Splash zone Hs"
        assert fails[0].value == 1.5
        assert fails[0].limit == 1.0
        assert fails[0].margin < 0  # negative margin = over the limit

    # ----- boundary case (exactly at limit) -------------------------------
    def test_boundary_exactly_at_limit_is_inclusive(self):
        """Hs == 1.0 m (exactly at limit) is allowed (inclusive: <= limit).

        Convention: a value EQUAL to a not-to-exceed limit is not a FAIL
        (value <= limit). Being exactly at the limit lands in the WARNING
        band (>= warning_factor * limit) rather than PASS.
        """
        decision = gng.evaluate_go_no_go(
            "boundary-window", self.results, conditions={"hs_m": 1.0}
        )
        splash = self._criterion(decision, "Splash zone Hs")
        assert splash.state != gng.CriterionState.FAIL
        assert splash.state == gng.CriterionState.WARNING
        assert splash.margin == 0.0  # limit - value == 0 at the boundary

    # ----- multiple violations: ordering / count -------------------------
    def test_multiple_violations_reported_in_criteria_order(self):
        """Two limits breached (Hs and R/OD) -> both reported, source order."""
        decision = gng.evaluate_go_no_go(
            "multi-violation", self.results,
            conditions={"hs_m": 2.0},
            min_bend_radius_factor=50.0,  # rigid jumper is only ~4.65x OD -> FAIL
        )
        assert decision.overall_state == gng.DecisionState.NO_GO
        fail_names = [
            c.name for c in decision.criteria
            if c.state == gng.CriterionState.FAIL
        ]
        assert "Minimum bend radius factor (R/OD)" in fail_names
        assert "Splash zone Hs" in fail_names
        # R/OD criterion is appended before the splash-zone criterion
        assert fail_names.index("Minimum bend radius factor (R/OD)") < \
            fail_names.index("Splash zone Hs")

    def test_default_conditions_backward_compatible(self):
        """No conditions dict -> splash-zone defaults to benign 0.0 m PASS."""
        decision = gng.evaluate_go_no_go("legacy-call", self.results)
        splash = self._criterion(decision, "Splash zone Hs")
        assert splash.value == 0.0
        assert splash.state == gng.CriterionState.PASS
