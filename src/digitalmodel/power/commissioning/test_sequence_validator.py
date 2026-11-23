"""Commissioning validator — FAT/SAT/IST test sequence generator.

Generates structured test sequences for power system commissioning phases
(Factory Acceptance Test, Site Acceptance Test, Integrated Systems Test),
validates results against expected sequences, and produces punch lists.
"""

import csv
from dataclasses import dataclass
from enum import Enum


class CommissioningPhase(str, Enum):
    """Commissioning test phases."""

    FAT = "fat"  # Factory Acceptance Test
    SAT = "sat"  # Site Acceptance Test
    IST = "ist"  # Integrated Systems Test


@dataclass
class TestStep:
    """A single test step in a commissioning sequence."""

    step_id: str
    phase: CommissioningPhase
    description: str
    preconditions: list[str]
    action: str
    acceptance_criteria: str
    dependencies: list[str]  # step_ids that must pass first


@dataclass
class TestResult:
    """Result of executing a test step."""

    step_id: str
    status: str  # "pass" | "fail" | "not_run"
    actual_value: str
    notes: str
    timestamp: str


_VALID_SEVERITIES = {"critical", "major", "minor"}


@dataclass
class PunchItem:
    """An outstanding item from failed or incomplete tests."""

    step_id: str
    description: str
    phase: CommissioningPhase
    severity: str  # "critical" | "major" | "minor"
    action_required: str

    def __post_init__(self):
        if self.severity not in _VALID_SEVERITIES:
            raise ValueError(
                f"severity must be one of {_VALID_SEVERITIES}, got '{self.severity}'"
            )


# Phase-specific step templates
_FAT_TEMPLATES = [
    ("insulation_resistance", "Insulation resistance test", ["power_off"], "Apply 1kV DC for 60s", "R >= 100 MOhm"),
    ("hi_pot", "High potential test", ["insulation_resistance_pass"], "Apply test voltage per IEC 62271", "No breakdown"),
    ("relay_settings", "Protection relay settings verification", ["power_off"], "Inject test currents", "Trip times within tolerance"),
    ("breaker_trip", "Circuit breaker trip test", ["relay_settings_verified"], "Trigger trip signal", "Breaker opens within spec"),
    ("control_logic", "Control logic functional test", ["breaker_trip_pass"], "Exercise all control sequences", "All sequences execute correctly"),
]

_SAT_TEMPLATES = [
    ("cable_megger", "Cable megger test", ["cables_terminated"], "Apply 1kV DC megger", "R >= 1 MOhm per km"),
    ("grounding_continuity", "Grounding continuity test", ["grounding_installed"], "Measure ground resistance", "R <= 1 Ohm"),
    ("phase_rotation", "Phase rotation verification", ["cables_connected"], "Measure phase sequence", "ABC sequence confirmed"),
    ("protection_coordination", "Protection coordination test", ["relay_settings_verified"], "Inject fault currents", "Correct trip sequence"),
    ("functional_interlock", "Functional interlock test", ["protection_coordination_pass"], "Exercise all interlocks", "All interlocks function"),
]

_IST_TEMPLATES = [
    ("load_test", "Load test", ["all_systems_energized"], "Apply rated load for 4h", "No overheating or trips"),
    ("failover", "Failover test", ["load_test_pass"], "Simulate primary failure", "Backup assumes load within spec"),
    ("scada_integration", "SCADA integration test", ["communications_verified"], "Monitor all points", "All SCADA points respond"),
    ("emergency_shutdown", "Emergency shutdown test", ["scada_integration_pass"], "Trigger ESD", "System shuts down safely"),
    ("performance_verification", "Performance verification", ["load_test_pass", "failover_pass"], "Run 24h performance test", "KPIs within targets"),
]

_PHASE_TEMPLATES = {
    CommissioningPhase.FAT: _FAT_TEMPLATES,
    CommissioningPhase.SAT: _SAT_TEMPLATES,
    CommissioningPhase.IST: _IST_TEMPLATES,
}


class CommissioningSequenceGenerator:
    """Generates structured test sequences for commissioning phases."""

    def __init__(self, system_config: dict):
        self._config = system_config
        self._subsystems = system_config.get("subsystems", [])

    def generate_sequence(self, phase: CommissioningPhase) -> list[TestStep]:
        """Generate ordered test steps for a commissioning phase."""
        templates = _PHASE_TEMPLATES[phase]
        prefix = phase.value.upper()
        steps = []
        id_map: dict[str, str] = {}  # template_name -> step_id

        for i, (name, desc, preconditions, action, criteria) in enumerate(templates, 1):
            step_id = f"{prefix}-{i:03d}"
            id_map[name] = step_id

        # Build steps with resolved dependencies
        for i, (name, desc, preconditions, action, criteria) in enumerate(templates, 1):
            step_id = f"{prefix}-{i:03d}"
            deps = []
            # Link to previous step if index > 0
            if i > 1:
                prev_name = templates[i - 2][0]
                deps.append(id_map[prev_name])

            steps.append(
                TestStep(
                    step_id=step_id,
                    phase=phase,
                    description=desc,
                    preconditions=preconditions,
                    action=action,
                    acceptance_criteria=criteria,
                    dependencies=deps,
                )
            )

        # Add per-subsystem steps
        base_idx = len(templates) + 1
        for j, subsystem in enumerate(self._subsystems):
            step_id = f"{prefix}-{base_idx + j:03d}"
            steps.append(
                TestStep(
                    step_id=step_id,
                    phase=phase,
                    description=f"{subsystem} commissioning check",
                    preconditions=[],
                    action=f"Verify {subsystem} per specifications",
                    acceptance_criteria=f"{subsystem} meets acceptance criteria",
                    dependencies=[steps[-1].step_id] if steps else [],
                )
            )

        return steps

    def generate_all_phases(self) -> dict[CommissioningPhase, list[TestStep]]:
        """Generate test sequences for all commissioning phases."""
        return {phase: self.generate_sequence(phase) for phase in CommissioningPhase}

    def validate_dependencies(self, steps: list[TestStep]) -> list[str]:
        """Validate that all dependency references are valid. Returns error strings."""
        valid_ids = {s.step_id for s in steps}
        errors = []
        for step in steps:
            for dep in step.dependencies:
                if dep not in valid_ids:
                    errors.append(
                        f"Step {step.step_id} references unknown dependency '{dep}'"
                    )
        return errors


_REQUIRED_CSV_COLUMNS = {"step_id", "status", "actual_value", "notes", "timestamp"}


class TestResultsValidator:
    """Validates test results against an expected test sequence."""

    def __init__(self, sequence: list[TestStep]):
        self._sequence = sequence
        self._step_map = {s.step_id: s for s in sequence}

    def load_results_csv(self, filepath: str) -> list[TestResult]:
        """Load test results from a CSV file."""
        with open(filepath, newline="") as f:
            reader = csv.DictReader(f)
            columns = set(reader.fieldnames or [])
            missing = _REQUIRED_CSV_COLUMNS - columns
            if missing:
                raise ValueError(f"CSV missing columns: {sorted(missing)}")
            return [
                TestResult(
                    step_id=row["step_id"],
                    status=row["status"],
                    actual_value=row["actual_value"],
                    notes=row["notes"],
                    timestamp=row["timestamp"],
                )
                for row in reader
            ]

    def validate(self, results: list[TestResult]) -> dict:
        """Validate results against expected sequence.

        Returns dict with passed, failed, not_run counts and error list.
        """
        passed = 0
        failed = 0
        not_run = 0
        errors = []

        seen_ids = set()
        for r in results:
            seen_ids.add(r.step_id)
            if r.step_id not in self._step_map:
                errors.append(f"Unknown step_id '{r.step_id}'")
                continue
            if r.status == "pass":
                passed += 1
            elif r.status == "fail":
                failed += 1
            elif r.status == "not_run":
                not_run += 1

        # Count steps with no result as not_run
        for step_id in self._step_map:
            if step_id not in seen_ids:
                not_run += 1

        return {
            "passed": passed,
            "failed": failed,
            "not_run": not_run,
            "errors": errors,
        }

    def generate_punch_list(self, results: list[TestResult]) -> list[PunchItem]:
        """Generate punch list from failed test results."""
        punch_items = []
        for r in results:
            if r.status != "fail":
                continue
            step = self._step_map.get(r.step_id)
            if step is None:
                continue
            punch_items.append(
                PunchItem(
                    step_id=r.step_id,
                    description=f"{step.description}: {r.actual_value}",
                    phase=step.phase,
                    severity="critical",
                    action_required=f"Investigate and retest: {r.notes}" if r.notes else "Investigate and retest",
                )
            )
        return punch_items

    def export_punch_list_csv(self, punch_items: list[PunchItem], filepath: str) -> None:
        """Export punch list to CSV file."""
        fieldnames = ["step_id", "description", "phase", "severity", "action_required"]
        with open(filepath, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            for item in punch_items:
                writer.writerow({
                    "step_id": item.step_id,
                    "description": item.description,
                    "phase": item.phase.value,
                    "severity": item.severity,
                    "action_required": item.action_required,
                })

    def export_punch_list_markdown(self, punch_items: list[PunchItem]) -> str:
        """Export punch list as a markdown table string."""
        lines = [
            "| Step ID | Description | Phase | Severity | Action Required |",
            "|---------|-------------|-------|----------|-----------------|",
        ]
        for item in punch_items:
            lines.append(
                f"| {item.step_id} | {item.description} | {item.phase.value} | "
                f"{item.severity} | {item.action_required} |"
            )
        return "\n".join(lines)
