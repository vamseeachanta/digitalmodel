"""Cross-machine baseline report tests (#2300). Receipts are synthetic."""

from __future__ import annotations

import ast
import importlib.util
import json
import re
import sys
from pathlib import Path

import pytest

from digitalmodel.solvers.benchmark import report, runner

REPO = Path(__file__).resolve().parents[3]
ENV = {"os": "Linux 6.8.0", "cpu_model": "Example CPU 9000", "logical_cores": 32,
       "physical_cores": 16, "ram_gb": 128.0, "python": "3.12.3"}


def _entry(case="motorbike", variant=8, median=100.0, fp=None, *, sv="v2312",
           n_ok=3, eligible=True, consistent=True, errors=(), rel=1e-6, ab=0.0,
           busy=False, solver="openfoam", sha="abc", label="ranks"):
    fp = {"cells": 350000, "cd": 0.41, "iterations": 100} if fp is None else fp
    done = n_ok > 0
    stats = {"median": median, "min": median * 0.9, "max": median * 1.1}
    return {"case": case, "solver": solver, "variant": variant,
            "variant_label": label, "warmup": 1, "solver_version": sv,
            "rel_tol": rel, "abs_tol": ab, "load_before": [], "busy_allowed": busy,
            "n_ok": n_ok, "repeats": [], "errors": list(errors),
            "wall_s": stats if done else None, "solve_s": stats if done else None,
            "threads_observed": [variant] if done else [],
            "fingerprint": fp if done else None,
            "input_sha256": sha if done else None,
            "timing_basis": "solver" if done else None,
            "fingerprint_consistent": consistent and done,
            "complete": n_ok == 3 and not errors,
            "baseline_eligible": eligible}


def _receipt(label, results, started="2026-10-01T00:00:00+00:00", version="1"):
    return {"pack_version": version, "machine_label": label, "started_utc": started,
            "finished_utc": started, "environment": dict(ENV), "repeats": 3,
            "results": results, "ok": all(r["baseline_eligible"] for r in results)}


def _variant(model, case="motorbike", variant=8, pack=0):
    (found,) = [v for c in model["packs"][pack]["cases"] if c["case"] == case
                for v in c["variants"] if v["variant"] == variant]
    return found


def _both(receipts):
    model = report.build_report(receipts)
    return model, report.render_html(model)


# ------------------------------------------------------- cross-machine agreement


def test_two_machines_agreeing_within_tolerance():
    model, html = _both([
        _receipt("fleet-a", [_entry(median=100.0)]),
        _receipt("fleet-b", [_entry(median=60.0, fp={"cells": 350000,
                                                      "cd": 0.41 + 1e-9,
                                                      "iterations": 100})])])
    agreement = _variant(model)["agreement"]
    assert agreement["status"] == report.AGREE
    assert agreement["machines"] == ["fleet-a", "fleet-b"]
    assert "agree within tolerance" in html
    assert "fleet-a" in html and "fleet-b" in html


def test_two_machines_disagreeing_beyond_tolerance():
    model, html = _both([
        _receipt("fleet-a", [_entry()]),
        _receipt("fleet-b", [_entry(fp={"cells": 350000, "cd": 0.47,
                                        "iterations": 100})])])
    agreement = _variant(model)["agreement"]
    assert agreement["status"] == report.DIFFER
    assert agreement["differing_keys"] == ["cd"]
    assert "differ beyond tolerance" in html
    assert "agree within tolerance across" not in html


def test_differing_solver_version_is_reported_not_counted_as_a_difference():
    model, html = _both([
        _receipt("fleet-a", [_entry(sv="v2312")]),
        _receipt("fleet-b", [_entry(sv="v2406", fp={"cells": 350000, "cd": 0.47,
                                                    "iterations": 100})])])
    assert _variant(model)["agreement"]["status"] == report.VERSION
    assert "different solver version" in html
    assert "v2312" in html and "v2406" in html
    assert model["packs"][0]["summary"][report.DIFFER] == 0
    assert model["packs"][0]["summary"][report.VERSION] == 1


def test_changed_input_is_not_compared():
    model, html = _both([_receipt("fleet-a", [_entry(sha="abc")]),
                         _receipt("fleet-b", [_entry(sha="def")])])
    assert _variant(model)["agreement"]["status"] == report.INPUT
    assert "input files differ" in html


def test_list_fingerprints_use_the_case_tolerance():
    base = {"a33_te_at_samples": [1.0e6, 2.0e6], "heave_rao_0deg_at_samples": [0.9, 0.1]}
    near = {"a33_te_at_samples": [1.0e6 * (1 + 5e-4), 2.0e6],
            "heave_rao_0deg_at_samples": [0.9, 0.1]}
    kw = dict(case="box-barge", variant=4, solver="orcawave", label="threads")
    loose = report.build_report([
        _receipt("fleet-a", [_entry(fp=base, rel=1e-3, **kw)]),
        _receipt("fleet-b", [_entry(fp=near, rel=1e-3, **kw)])])
    tight = report.build_report([
        _receipt("fleet-a", [_entry(fp=base, rel=1e-6, **kw)]),
        _receipt("fleet-b", [_entry(fp=near, rel=1e-6, **kw)])])
    assert _variant(loose, "box-barge", 4)["agreement"]["status"] == report.AGREE
    assert _variant(tight, "box-barge", 4)["agreement"]["status"] == report.DIFFER


def test_ineligible_result_is_marked_and_excluded_from_agreement():
    wild = {"cells": 1, "cd": 9.9, "iterations": 1}
    model, html = _both([
        _receipt("fleet-a", [_entry()]),
        _receipt("fleet-b", [_entry()]),
        _receipt("fleet-c", [_entry(fp=wild, eligible=False, busy=True)])])
    variant = _variant(model)
    assert variant["agreement"]["status"] == report.AGREE
    assert variant["agreement"]["machines"] == ["fleet-a", "fleet-b"]
    assert variant["agreement"]["excluded"] == ["fleet-c"]
    (row,) = [r for r in variant["rows"] if r["machine"] == "fleet-c"]
    assert row["eligible"] is False
    assert any("busy" in reason for reason in row["reasons"])
    assert "not baseline-eligible" in html.lower()
    assert "excluded" in html.lower()


def test_two_machines_with_one_eligible_result_are_not_compared():
    model, html = _both([
        _receipt("fleet-a", [_entry()]),
        _receipt("fleet-b", [_entry(eligible=False, consistent=False)])])
    variant = _variant(model)
    assert variant["agreement"]["status"] == report.NOT_COMPARED
    (row,) = [r for r in variant["rows"] if r["machine"] == "fleet-b"]
    assert any("between repeats" in reason for reason in row["reasons"])
    assert "agree within tolerance across" not in html


def test_single_machine_case_says_so():
    model, html = _both([_receipt("fleet-a", [_entry()])])
    assert _variant(model)["agreement"]["status"] == report.ONE_MACHINE
    assert "one machine only; no cross-machine comparison" in html


def test_failed_variant_with_null_solve_time_renders():
    failed = _entry(variant=32, n_ok=0, eligible=False,
                    errors=["RuntimeError: decomposePar failed"])
    model, html = _both([_receipt("fleet-a", [failed, _entry(variant=8)])])
    (row,) = _variant(model, variant=32)["rows"]
    assert row["solve_s"] is None and row["eligible"] is False
    assert "0 of 3" in html
    assert "decomposePar failed" in html
    assert "None" not in html


# ------------------------------------------------------------ receipt selection


def test_most_recent_receipt_per_machine_is_used_and_count_is_stated():
    old = _receipt("fleet-a", [_entry(median=500.0)], started="2026-09-01T00:00:00+00:00")
    new = _receipt("fleet-a", [_entry(median=90.0, eligible=False, consistent=False)],
                   started="2026-10-02T00:00:00+00:00")
    for order in ([old, new], [new, old]):
        model, html = _both(order)
        (row,) = _variant(model)["rows"]
        assert row["solve_s"]["median"] == 90.0
        assert row["supplied"] == 2
        assert row["older_eligible"] == 1
        assert "most recent of 2" in html


def test_mixed_pack_versions_are_reported_separately_and_never_compared():
    model, html = _both([_receipt("fleet-a", [_entry()], version="1"),
                         _receipt("fleet-b", [_entry()], version="2")])
    assert [p["pack_version"] for p in model["packs"]] == ["1", "2"]
    for pack in (0, 1):
        assert _variant(model, pack=pack)["agreement"]["status"] == report.ONE_MACHINE
    assert "Pack version 1" in html and "Pack version 2" in html
    assert "different pack versions" in html


def test_a_file_that_is_not_a_receipt_is_refused(tmp_path):
    bad = tmp_path / "bad.json"
    bad.write_text(json.dumps({"hello": 1}))
    with pytest.raises(ValueError, match="not a benchmark receipt"):
        report.load_receipts([bad])
    with pytest.raises(ValueError, match="no receipts"):
        report.build_report([])


# ------------------------------------------------------------- privacy, wording


def test_no_hostname_path_or_licence_server_from_errors_reaches_html(monkeypatch):
    monkeypatch.setattr(runner.platform, "node", lambda: "buildhost77")
    error = ("BUILDHOST77 cannot reach 1055@lic01 reading "
             "C:\\Users\\bob\\cases\\motorBike.zip and /home/alice/run/log.simpleFoam")
    entry = _entry(n_ok=0, eligible=False, errors=[error])
    entry["skipped"] = "host busy on buildhost77 under /home/alice/run/x"
    html = report.render_html(report.build_report([_receipt("fleet-a", [entry])]))
    lowered = html.lower()
    for secret in ("buildhost77", "lic01", "bob", "alice", "\\users", "/home/"):
        assert secret not in lowered
    assert "motorBike.zip" in html and "log.simpleFoam" in html
    assert "&lt;host&gt;" in html and "&lt;licence-server&gt;" in html


def test_only_label_and_environment_block_are_shown():
    receipt = _receipt("fleet-a", [_entry()])
    receipt["hostname"] = "secret-host"
    receipt["environment"]["user"] = "carol"
    html = report.render_html(report.build_report([receipt]))
    assert "secret-host" not in html and "carol" not in html
    for shown in ("Linux 6.8.0", "Example CPU 9000", "128"):
        assert shown in html


def test_receipt_text_is_html_escaped():
    entry = _entry(case="<script>alert(1)</script>", sv="<b>v1</b>")
    html = report.render_html(report.build_report([_receipt("<i>m</i>", [entry])]))
    assert "<script>" not in html and "<b>v1</b>" not in html and "<i>m</i>" not in html
    assert "&lt;script&gt;" in html


def test_wording_does_not_overstate():
    _, html = _both([_receipt("fleet-a", [_entry()]), _receipt("fleet-b", [_entry()])])
    assert "validated" not in html.lower()


# ----------------------------------------------------------------------- chart


def test_chart_per_case_with_fixed_colours_and_legend_for_several_variants():
    receipts = [
        _receipt("fleet-a", [_entry(variant=8), _entry(variant=16, median=60.0),
                             _entry(case="box-barge", variant=4, solver="aqwa")]),
        _receipt("fleet-b", [_entry(variant=8), _entry(variant=16, median=50.0)])]
    model, html = _both(receipts)
    assert html.count("<svg") == 2
    motorbike, barge = (report.render_chart(c) for c in sorted(
        model["packs"][0]["cases"], key=lambda c: c["case"], reverse=True))
    assert motorbike.index("#2a78d6") < motorbike.index("#eb6834")
    assert "#1baf7a" not in motorbike
    assert 'class="legend"' in motorbike
    assert "#2a78d6" in barge and 'class="legend"' not in barge
    # text is never drawn in a series colour
    for text in re.findall(r"<text[^>]*>", html):
        assert not any(c in text for c in report.SERIES_COLOURS)
    # the numbers are also in a table
    assert "<table" in html and "60.0" in html and "50.0" in html


def test_chart_marks_ineligible_bars_and_skips_missing_timings():
    receipts = [_receipt("fleet-a", [
        _entry(variant=8, eligible=False, consistent=False),
        _entry(variant=32, n_ok=0, eligible=False, errors=["boom"])])]
    (case,) = report.build_report(receipts)["packs"][0]["cases"]
    svg = report.render_chart(case)
    assert svg.count('class="bar') == 1
    assert "bar ineligible" in svg
    assert "no completed repeat" in svg


def test_case_without_any_timing_has_no_chart():
    receipts = [_receipt("fleet-a", [_entry(n_ok=0, eligible=False, errors=["boom"])])]
    html = report.render_html(report.build_report(receipts))
    assert "<svg" not in html
    assert "No completed solve times" in html


# ------------------------------------------------------------ module and script


def test_report_module_imports_only_the_standard_library():
    tree = ast.parse(Path(report.__file__).read_text(encoding="utf-8"))
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            names = [alias.name.split(".")[0] for alias in node.names]
        elif isinstance(node, ast.ImportFrom) and node.level == 0:
            names = [node.module.split(".")[0]]
        else:
            continue
        for name in names:
            assert name in sys.stdlib_module_names, name


def test_report_subcommand_writes_one_self_contained_file(tmp_path, capsys):
    spec = importlib.util.spec_from_file_location(
        "solver_benchmark_script", REPO / "scripts" / "solver_benchmark.py")
    script = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(script)
    paths = []
    for label in ("fleet-a", "fleet-b"):
        path = tmp_path / f"{label}.json"
        path.write_text(json.dumps(_receipt(label, [_entry()])))
        paths.append(str(path))
    out = tmp_path / "nested" / "fleet.html"
    assert script.main(["report", *paths, "--out", str(out)]) == 0
    html = out.read_text(encoding="utf-8")
    assert html.lstrip().lower().startswith("<!doctype html>")
    assert "agree within tolerance" in html
    assert str(tmp_path) not in html and tmp_path.name not in html
    assert "fleet-a.json" not in html
    for external in ("<script", "<link", "<img", "http://", "https://"):
        assert external not in html
    assert "report:" in capsys.readouterr().out
