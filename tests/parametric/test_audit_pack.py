"""Section 15: gate rows identify the output actually asserted."""
from pathlib import Path


def test_gate_rows_name_asserted_package_outputs():
    text = (Path(__file__).parents[2] / "src/digitalmodel/parametric/docs/agent-pack.md").read_text(encoding="utf-8")
    required = {
        "P0": ["Atlas.predict", "1e-9"],
        "P1": ["validation", "0 failed", "count"],
        "P1b": ["query.router", "0 failed", "count"],
        "P2": ["Atlas.predict", 'RESPONSE_FUNCS["mooring_fatigue"]', "1e-6"],
        "P5a": ["refresh.refresh_status", "0 stale", "count"],
        "P5b": ["query._handle_value", "provenance", "0 absent", "count"],
        "P5c": ["refresh.main", "2", "count"],
        "P5d": ["Atlas.predict", "0", "count"],
    }
    for check, needles in required.items():
        row = next(line for line in text.splitlines() if f"| {check} |" in line)
        assert all(needle in row for needle in needles), row
    assert "issue pending (4)" not in text
