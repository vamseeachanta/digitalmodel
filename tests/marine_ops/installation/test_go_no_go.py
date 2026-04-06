"""Tests for Go/No-Go decision logic."""
import sys
import importlib.util
import pytest

# Load go_no_go module directly
spec = importlib.util.spec_from_file_location(
    "go_no_go",
    "/mnt/local-analysis/workspace-hub/digitalmodel/src/digitalmodel/marine_ops/installation/go_no_go.py"
)
gng = importlib.util.module_from_spec(spec)
gng.__module__ = "go_no_go"
sys.modules["go_no_go"] = gng
spec.loader.exec_module(gng)

# Load jumper_lift for test data
spec2 = importlib.util.spec_from_file_location(
    "jumper_lift",
    "/mnt/local-analysis/workspace-hub/digitalmodel/src/digitalmodel/marine_ops/installation/jumper_lift.py"
)
jl = importlib.util.module_from_spec(spec2)
jl.__module__ = "jumper_lift"
sys.modules["jumper_lift"] = jl
spec2.loader.exec_module(jl)


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
