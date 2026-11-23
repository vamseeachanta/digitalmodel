"""Tests for commissioning validator — FAT/SAT/IST test sequence generator.

TDD tests: 26 total covering data models, generator, validator, and export.
"""

import csv
import os
import tempfile

import pytest

from digitalmodel.power.commissioning import (
    CommissioningPhase,
    CommissioningSequenceGenerator,
    PunchItem,
    TestResult,
    TestResultsValidator,
    TestStep,
)


# ---------------------------------------------------------------------------
# Data model tests (5)
# ---------------------------------------------------------------------------


class TestCommissioningPhase:
    """Tests 1, 4: CommissioningPhase enum."""

    def test_phase_enum_has_three_members(self):
        """Test 1: CommissioningPhase enum has 3 members."""
        assert len(CommissioningPhase) == 3
        assert CommissioningPhase.FAT.value == "fat"
        assert CommissioningPhase.SAT.value == "sat"
        assert CommissioningPhase.IST.value == "ist"

    def test_invalid_phase_string_raises(self):
        """Test 4: Invalid phase string raises ValueError."""
        with pytest.raises(ValueError):
            CommissioningPhase("invalid_phase")


class TestTestStep:
    """Tests 2, 3: TestStep dataclass."""

    def test_teststep_fields_accessible(self):
        """Test 2: TestStep dataclass has all 7 fields."""
        step = TestStep(
            step_id="FAT-001",
            phase=CommissioningPhase.FAT,
            description="Insulation resistance test",
            preconditions=["power_off"],
            action="Apply 1kV DC for 60s",
            acceptance_criteria="R >= 100 MOhm",
            dependencies=[],
        )
        assert step.step_id == "FAT-001"
        assert step.phase == CommissioningPhase.FAT
        assert step.description == "Insulation resistance test"
        assert step.preconditions == ["power_off"]
        assert step.action == "Apply 1kV DC for 60s"
        assert step.acceptance_criteria == "R >= 100 MOhm"
        assert step.dependencies == []

    def test_teststep_empty_dependencies(self):
        """Test 3: TestStep with empty dependencies is valid."""
        step = TestStep(
            step_id="FAT-001",
            phase=CommissioningPhase.FAT,
            description="First test",
            preconditions=[],
            action="Do something",
            acceptance_criteria="Pass",
            dependencies=[],
        )
        assert step.dependencies == []


class TestPunchItem:
    """Test 5: PunchItem severity validation."""

    def test_punch_item_severity_values(self):
        """Test 5: PunchItem accepts only critical/major/minor severity."""
        for severity in ("critical", "major", "minor"):
            item = PunchItem(
                step_id="FAT-001",
                description="Failed test",
                phase=CommissioningPhase.FAT,
                severity=severity,
                action_required="Retest",
            )
            assert item.severity == severity

    def test_punch_item_invalid_severity_raises(self):
        """Test 5b: Invalid severity raises ValueError."""
        with pytest.raises(ValueError):
            PunchItem(
                step_id="FAT-001",
                description="Failed test",
                phase=CommissioningPhase.FAT,
                severity="trivial",
                action_required="Retest",
            )


# ---------------------------------------------------------------------------
# CommissioningSequenceGenerator tests (10)
# ---------------------------------------------------------------------------


class TestCommissioningSequenceGenerator:
    """Tests 6–15: Generator tests."""

    @pytest.fixture()
    def default_config(self):
        return {
            "system_name": "Power Distribution",
            "subsystems": ["switchgear", "transformer"],
            "voltage_kv": 33.0,
            "phases": [CommissioningPhase.FAT, CommissioningPhase.SAT, CommissioningPhase.IST],
        }

    @pytest.fixture()
    def generator(self, default_config):
        return CommissioningSequenceGenerator(default_config)

    def test_generate_fat_returns_at_least_5_steps(self, generator):
        """Test 6: generate_sequence(FAT) returns >= 5 steps."""
        steps = generator.generate_sequence(CommissioningPhase.FAT)
        assert len(steps) >= 5
        assert all(s.phase == CommissioningPhase.FAT for s in steps)

    def test_generate_sat_returns_at_least_5_steps(self, generator):
        """Test 7: generate_sequence(SAT) returns >= 5 steps."""
        steps = generator.generate_sequence(CommissioningPhase.SAT)
        assert len(steps) >= 5
        assert all(s.phase == CommissioningPhase.SAT for s in steps)

    def test_generate_ist_returns_at_least_5_steps(self, generator):
        """Test 8: generate_sequence(IST) returns >= 5 steps."""
        steps = generator.generate_sequence(CommissioningPhase.IST)
        assert len(steps) >= 5
        assert all(s.phase == CommissioningPhase.IST for s in steps)

    def test_generate_all_phases_returns_3_keys(self, generator):
        """Test 9: generate_all_phases returns dict with 3 phase keys."""
        result = generator.generate_all_phases()
        assert len(result) == 3
        assert set(result.keys()) == {
            CommissioningPhase.FAT,
            CommissioningPhase.SAT,
            CommissioningPhase.IST,
        }

    def test_step_ids_unique_within_phase(self, generator):
        """Test 10: Step IDs are unique within each phase."""
        for phase in CommissioningPhase:
            steps = generator.generate_sequence(phase)
            ids = [s.step_id for s in steps]
            assert len(ids) == len(set(ids)), f"Duplicate step_ids in {phase.value}"

    def test_dependencies_reference_valid_step_ids(self, generator):
        """Test 11: All dependencies reference valid step_ids within phase."""
        for phase in CommissioningPhase:
            steps = generator.generate_sequence(phase)
            valid_ids = {s.step_id for s in steps}
            for step in steps:
                for dep in step.dependencies:
                    assert dep in valid_ids, f"{step.step_id} depends on unknown {dep}"

    def test_validate_dependencies_catches_bad_ref(self, generator):
        """Test 12: validate_dependencies catches invalid dependency refs."""
        bad_steps = [
            TestStep(
                step_id="X-001",
                phase=CommissioningPhase.FAT,
                description="Test",
                preconditions=[],
                action="Do",
                acceptance_criteria="Pass",
                dependencies=["NONEXISTENT-999"],
            ),
        ]
        errors = generator.validate_dependencies(bad_steps)
        assert len(errors) > 0
        assert "NONEXISTENT-999" in errors[0]

    def test_empty_config_gives_minimal_steps(self):
        """Test 13: Empty system_config still produces at least 1 step per phase."""
        gen = CommissioningSequenceGenerator({})
        for phase in CommissioningPhase:
            steps = gen.generate_sequence(phase)
            assert len(steps) >= 1

    def test_custom_subsystems_add_extra_steps(self, default_config):
        """Test 14: More subsystems produce more steps."""
        gen_default = CommissioningSequenceGenerator(default_config)
        extended_config = {**default_config, "subsystems": ["switchgear", "transformer", "cable", "relay"]}
        gen_extended = CommissioningSequenceGenerator(extended_config)
        default_count = len(gen_default.generate_sequence(CommissioningPhase.FAT))
        extended_count = len(gen_extended.generate_sequence(CommissioningPhase.FAT))
        assert extended_count > default_count

    def test_steps_ordered_respecting_dependencies(self, generator):
        """Test 15: Steps with dependencies appear after their dependencies."""
        for phase in CommissioningPhase:
            steps = generator.generate_sequence(phase)
            seen = set()
            for step in steps:
                for dep in step.dependencies:
                    assert dep in seen, f"{step.step_id} appears before dependency {dep}"
                seen.add(step.step_id)


# ---------------------------------------------------------------------------
# TestResultsValidator tests (8)
# ---------------------------------------------------------------------------


class TestTestResultsValidator:
    """Tests 16–23: Validator tests."""

    @pytest.fixture()
    def fat_steps(self):
        config = {
            "system_name": "Test System",
            "subsystems": ["switchgear"],
            "voltage_kv": 11.0,
            "phases": [CommissioningPhase.FAT],
        }
        gen = CommissioningSequenceGenerator(config)
        return gen.generate_sequence(CommissioningPhase.FAT)

    @pytest.fixture()
    def validator(self, fat_steps):
        return TestResultsValidator(fat_steps)

    def _write_csv(self, filepath, rows, header=None):
        """Helper: write CSV with optional header."""
        if header is None:
            header = ["step_id", "status", "actual_value", "notes", "timestamp"]
        with open(filepath, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(header)
            for row in rows:
                writer.writerow(row)

    def test_load_results_csv_valid(self, validator, fat_steps):
        """Test 16: load_results_csv parses valid CSV."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            filepath = f.name
        try:
            rows = [
                [s.step_id, "pass", "OK", "", "2026-03-13T10:00:00"]
                for s in fat_steps
            ]
            self._write_csv(filepath, rows)
            results = validator.load_results_csv(filepath)
            assert len(results) == len(fat_steps)
            assert all(isinstance(r, TestResult) for r in results)
        finally:
            os.unlink(filepath)

    def test_load_results_csv_empty_file(self, validator):
        """Test 17: load_results_csv with header-only file returns empty list."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            filepath = f.name
        try:
            self._write_csv(filepath, [])
            results = validator.load_results_csv(filepath)
            assert results == []
        finally:
            os.unlink(filepath)

    def test_load_results_csv_missing_columns(self, validator):
        """Test 18: load_results_csv with missing columns raises ValueError."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            filepath = f.name
        try:
            self._write_csv(filepath, [["FAT-001", "pass"]], header=["step_id", "status"])
            with pytest.raises(ValueError, match="missing columns"):
                validator.load_results_csv(filepath)
        finally:
            os.unlink(filepath)

    def test_validate_all_pass(self, validator, fat_steps):
        """Test 19: validate with all passing results."""
        results = [
            TestResult(
                step_id=s.step_id,
                status="pass",
                actual_value="OK",
                notes="",
                timestamp="2026-03-13T10:00:00",
            )
            for s in fat_steps
        ]
        summary = validator.validate(results)
        assert summary["passed"] == len(fat_steps)
        assert summary["failed"] == 0
        assert summary["not_run"] == 0
        assert summary["errors"] == []

    def test_validate_with_failures(self, validator, fat_steps):
        """Test 20: validate counts failures correctly."""
        results = []
        for i, s in enumerate(fat_steps):
            status = "fail" if i == 0 else "pass"
            results.append(
                TestResult(
                    step_id=s.step_id,
                    status=status,
                    actual_value="NG" if i == 0 else "OK",
                    notes="",
                    timestamp="2026-03-13T10:00:00",
                )
            )
        summary = validator.validate(results)
        assert summary["failed"] == 1
        assert summary["passed"] == len(fat_steps) - 1

    def test_validate_unknown_step_id(self, validator):
        """Test 21: validate flags unknown step_id in errors."""
        results = [
            TestResult(
                step_id="UNKNOWN-999",
                status="pass",
                actual_value="OK",
                notes="",
                timestamp="2026-03-13T10:00:00",
            )
        ]
        summary = validator.validate(results)
        assert len(summary["errors"]) > 0
        assert "UNKNOWN-999" in summary["errors"][0]

    def test_generate_punch_list_from_failures(self, validator, fat_steps):
        """Test 22: generate_punch_list creates PunchItem per failure."""
        results = [
            TestResult(
                step_id=fat_steps[0].step_id,
                status="fail",
                actual_value="Low reading",
                notes="Below threshold",
                timestamp="2026-03-13T10:00:00",
            )
        ]
        punch = validator.generate_punch_list(results)
        assert len(punch) == 1
        assert isinstance(punch[0], PunchItem)
        assert punch[0].step_id == fat_steps[0].step_id

    def test_punch_list_empty_when_all_pass(self, validator, fat_steps):
        """Test 23: Punch list is empty when all results pass."""
        results = [
            TestResult(
                step_id=s.step_id,
                status="pass",
                actual_value="OK",
                notes="",
                timestamp="2026-03-13T10:00:00",
            )
            for s in fat_steps
        ]
        punch = validator.generate_punch_list(results)
        assert punch == []


# ---------------------------------------------------------------------------
# Export tests (3)
# ---------------------------------------------------------------------------


class TestExport:
    """Tests 24–26: CSV and markdown export."""

    @pytest.fixture()
    def sample_punch_items(self):
        return [
            PunchItem(
                step_id="FAT-001",
                description="Insulation below threshold",
                phase=CommissioningPhase.FAT,
                severity="critical",
                action_required="Replace cable and retest",
            ),
            PunchItem(
                step_id="FAT-003",
                description="Minor label mismatch",
                phase=CommissioningPhase.FAT,
                severity="minor",
                action_required="Relabel panel",
            ),
        ]

    @pytest.fixture()
    def validator(self):
        step = TestStep(
            step_id="FAT-001",
            phase=CommissioningPhase.FAT,
            description="Test",
            preconditions=[],
            action="Do",
            acceptance_criteria="Pass",
            dependencies=[],
        )
        return TestResultsValidator([step])

    def test_export_punch_list_csv(self, validator, sample_punch_items):
        """Test 24: export_punch_list_csv writes valid CSV."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            filepath = f.name
        try:
            validator.export_punch_list_csv(sample_punch_items, filepath)
            with open(filepath) as f:
                reader = csv.reader(f)
                rows = list(reader)
            assert len(rows) == 3  # header + 2 items
            assert "step_id" in rows[0]
            assert rows[1][0] == "FAT-001"
        finally:
            os.unlink(filepath)

    def test_export_punch_list_markdown(self, validator, sample_punch_items):
        """Test 25: export_punch_list_markdown returns markdown table."""
        md = validator.export_punch_list_markdown(sample_punch_items)
        assert "| step_id" in md or "| Step ID" in md
        assert "FAT-001" in md
        assert "critical" in md
        assert "minor" in md

    def test_export_punch_list_csv_empty(self, validator):
        """Test 26: export_punch_list_csv with empty list writes header only."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            filepath = f.name
        try:
            validator.export_punch_list_csv([], filepath)
            with open(filepath) as f:
                reader = csv.reader(f)
                rows = list(reader)
            assert len(rows) == 1  # header only
            assert "step_id" in rows[0]
        finally:
            os.unlink(filepath)
