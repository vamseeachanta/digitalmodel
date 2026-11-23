from __future__ import annotations

import json

from digitalmodel.solvers.openfoam.convergence_audit import audit_one, main, md_table

from .test_force_cycle_average import write_force


def test_audit_verdicts_for_settled_short_and_two_extrema(tmp_path):
    settled = write_force(tmp_path / "settled.dat", 4_000)
    short = write_force(tmp_path / "short.dat", 600)
    two_extrema = write_force(tmp_path / "two.dat", 1_800)
    assert audit_one("settled", settled, 500, 1.0, 1.0)["verdict"] in {
        "settled",
        "extrapolable",
    }
    assert audit_one("short", short, 0, 1.0, 1.0)["verdict"] == "short"
    row = audit_one("two", two_extrema, 500, 1.0, 1.0)
    assert row["n_extrema"] == 2
    assert row["verdict"] == "transient"


def test_md_table_has_one_row_per_input_and_consistent_scaling():
    rows_n = [
        {"label": "a", "rows": 10, "n_extrema": 0, "half_period": None,
         "viscous": -20.0, "cycle_change_pct": None, "aitken_total": None,
         "fit_total": None, "fit_period": None, "fit_tau": None,
         "amp_ok_at": None, "verdict": "short"}
    ]
    table_n = md_table(rows_n, 1.0)
    assert "viscous N" in table_n
    assert "-20.00" in table_n

    rows_kn = [dict(rows_n[0], label="b", viscous=-20_000.0)]
    table_kn = md_table(rows_kn, 1.0)
    assert "viscous kN" in table_kn
    assert "-20.0" in table_kn
    assert len([line for line in table_kn.splitlines()[2:] if line.startswith("|")]) == 1


def test_cli_accepts_labels_and_writes_json_and_markdown(tmp_path, capsys):
    force = write_force(tmp_path / "run.dat", 4_000)
    json_path, md_path = tmp_path / "audit.json", tmp_path / "audit.md"
    assert main([
        f"demo={force}", "--start", "500", "--json", str(json_path),
        "--md", str(md_path),
    ]) == 0
    assert json.loads(json_path.read_text())[0]["label"] == "demo"
    assert "| demo |" in md_path.read_text()
    assert "| demo |" in capsys.readouterr().out
