"""Runner, environment, busy-guard, privacy and compare tests (#2300)."""

from __future__ import annotations

import json
import math
import platform

import pytest

from digitalmodel.solvers.benchmark import compare, env, runner

IDLE = lambda: {"avg_pct": 1.0, "max_core_pct": 5.0}  # noqa: E731


def _case(name, outcomes, solver_version="1.0"):
    """A fake case whose run() returns the queued outcomes in order."""
    queue = list(outcomes)

    def run(work_dir, variant):
        assert work_dir.is_dir()
        return queue.pop(0)

    return runner.Case(name=name, solver=name, variants=[1], run=run,
                       version=lambda: solver_version, rel_tol=1e-6, abs_tol=0.0)


def _ok(wall, value):
    return {"ok": True, "wall_s": wall, "solve_s": wall - 1,
            "fingerprint": {"x": value}, "threads_observed": 1}


def _run(cases, tmp_path, **kw):
    kw.setdefault("load_sampler", IDLE)
    kw.setdefault("warmup", 0)
    return runner.run_pack(cases, tmp_path, machine_label="m1", **kw)


def test_run_pack_aggregates_repeats(tmp_path):
    case = _case("fake", [_ok(10, 1.0), _ok(12, 1.0), _ok(11, 1.0)])
    receipt = _run([case], tmp_path, repeats=3)
    (entry,) = receipt["results"]
    assert entry["case"] == "fake" and entry["variant"] == 1
    assert entry["n_ok"] == 3
    assert entry["wall_s"] == {"median": 11, "min": 10, "max": 12}
    assert entry["solve_s"] == {"median": 10, "min": 9, "max": 11}
    assert entry["fingerprint"] == {"x": 1.0}
    assert entry["fingerprint_consistent"] is True
    assert entry["threads_observed"] == [1]
    assert entry["solver_version"] == "1.0"
    assert entry["baseline_eligible"] is True
    assert receipt["ok"] is True
    assert receipt["machine_label"] == "m1"
    assert receipt["pack_version"] == runner.PACK_VERSION


def test_warmup_runs_are_discarded(tmp_path):
    case = _case("fake", [_ok(99, 1.0), _ok(10, 1.0)])
    entry = _run([case], tmp_path, repeats=1, warmup=1)["results"][0]
    assert entry["n_ok"] == 1
    assert entry["wall_s"]["median"] == 10
    assert entry["warmup"] == 1


def test_inconsistent_fingerprint_is_flagged(tmp_path):
    case = _case("fake", [_ok(10, 1.0), _ok(10, 2.0)])
    receipt = _run([case], tmp_path, repeats=2)
    assert receipt["results"][0]["fingerprint_consistent"] is False
    assert receipt["ok"] is False


@pytest.mark.parametrize("fingerprint", [{}, {"x": math.nan}, {"x": math.inf}, None])
def test_missing_or_nonfinite_fingerprint_fails_the_repeat(tmp_path, fingerprint):
    outcome = {"ok": True, "wall_s": 1, "solve_s": 1, "fingerprint": fingerprint}
    entry = _run([_case("fake", [outcome])], tmp_path, repeats=1)["results"][0]
    assert entry["n_ok"] == 0
    assert "fingerprint" in entry["errors"][0]


def test_failed_repeat_fails_receipt_and_keeps_error(tmp_path):
    case = _case("fake", [_ok(10, 1.0), {"ok": False, "error": "licence"}])
    receipt = _run([case], tmp_path, repeats=2)
    entry = receipt["results"][0]
    assert entry["n_ok"] == 1
    assert entry["errors"] == ["licence"]
    assert receipt["ok"] is False


def test_exception_in_case_is_recorded_not_raised(tmp_path):
    def boom(work_dir, variant):
        raise RuntimeError("no solver")

    case = runner.Case("fake", "fake", [1], boom, lambda: None)
    receipt = _run([case], tmp_path, repeats=1)
    assert "RuntimeError: no solver" in receipt["results"][0]["errors"][0]


@pytest.mark.parametrize("load", [
    {"avg_pct": 55.0, "max_core_pct": 90.0},   # whole host busy
    {"avg_pct": 2.0, "max_core_pct": 99.0},    # one saturated core, diluted average
])
def test_busy_host_is_refused(tmp_path, load):
    case = _case("fake", [_ok(10, 1.0)])
    receipt = _run([case], tmp_path, repeats=1, load_sampler=lambda: load)
    entry = receipt["results"][0]
    assert entry["n_ok"] == 0
    assert entry["skipped"].startswith("host busy")
    assert receipt["ok"] is False


def test_busy_check_runs_before_every_repeat(tmp_path):
    loads = iter([{"avg_pct": 1.0, "max_core_pct": 5.0},
                  {"avg_pct": 60.0, "max_core_pct": 95.0}])
    case = _case("fake", [_ok(10, 1.0), _ok(10, 1.0)])
    entry = _run([case], tmp_path, repeats=2, load_sampler=lambda: next(loads))
    entry = entry["results"][0]
    assert entry["n_ok"] == 1
    assert entry["skipped"].startswith("host busy")


def test_busy_host_allowed_is_not_baseline_eligible(tmp_path):
    busy = {"avg_pct": 55.0, "max_core_pct": 90.0}
    case = _case("fake", [_ok(10, 1.0)])
    receipt = _run([case], tmp_path, repeats=1, load_sampler=lambda: busy,
                   allow_busy=True)
    entry = receipt["results"][0]
    assert entry["n_ok"] == 1
    assert entry["load_before"] == [busy]
    assert entry["baseline_eligible"] is False


def test_concurrent_pack_runs_are_locked_out(tmp_path):
    lock = tmp_path / "bench.lock"
    with runner.pack_lock(lock):
        with pytest.raises(RuntimeError, match="another benchmark"):
            with runner.pack_lock(lock):
                pass
    with runner.pack_lock(lock):  # released after exit
        pass


def test_receipt_is_json_serialisable_and_has_no_hostname(tmp_path):
    case = _case("fake", [_ok(10, 1.0)])
    receipt = _run([case], tmp_path, repeats=1)
    text = json.dumps(receipt)
    assert "host" not in receipt
    assert platform.node() not in text


def test_errors_are_sanitised(tmp_path):
    node = platform.node()
    case = _case("fake", [{"ok": False,
                           "error": f"{node} cannot reach 1055@lic01 in C:\\Users\\bob\\x"}])
    entry = _run([case], tmp_path, repeats=1)["results"][0]
    error = entry["errors"][0]
    assert node not in error and "lic01" not in error
    assert "\\Users\\bob" not in error
    assert error.startswith("<host>") and "<licence-server>" in error


def test_only_allowlisted_detail_keys_survive(tmp_path):
    outcome = _ok(10, 1.0)
    outcome.update(stdout="secret", licence_env={"X": "1055@lic"}, input_sha256="ab")
    entry = _run([_case("fake", [outcome])], tmp_path, repeats=1)["results"][0]
    text = json.dumps(entry)
    assert "secret" not in text and "1055@lic" not in text
    assert entry["input_sha256"] == "ab"


def test_environment_fields():
    info = env.machine_environment()
    for key in ("os", "cpu_model", "logical_cores", "physical_cores", "ram_gb",
                "python"):
        assert key in info
    assert info["logical_cores"] >= 1
    assert platform.node() not in json.dumps(info)


def test_cpu_load_sample_shape():
    sample = env.sample_cpu_load(interval=0.2)
    if sample is not None:
        assert 0.0 <= sample["avg_pct"] <= 100.0
        assert sample["avg_pct"] <= sample["max_core_pct"] + 1e-9


# ---------------------------------------------------------------- compare


def _receipt(results, version="1"):
    return {"pack_version": version, "machine_label": "m", "results": results}


def _entry(case, wall, fp, sv="1.0", rel=1e-6, ab=0.0, variant=1, n_ok=1,
           eligible=True):
    return {"case": case, "variant": variant, "solver_version": sv,
            "rel_tol": rel, "abs_tol": ab, "n_ok": n_ok,
            "solve_s": {"median": wall}, "fingerprint": fp,
            "fingerprint_consistent": True, "baseline_eligible": eligible}


def test_compare_ok_slower_and_mismatch():
    base = _receipt([_entry("a", 10, {"x": 1.0}), _entry("b", 10, {"x": 1.0}),
                     _entry("c", 10, {"x": 1.0})])
    new = _receipt([_entry("a", 10.5, {"x": 1.0 + 1e-9}),
                    _entry("b", 13, {"x": 1.0}),
                    _entry("c", 10, {"x": 1.1})])
    rows = {r["case"]: r for r in compare.compare(new, base)["rows"]}
    assert rows["a"]["fingerprint"] == "MATCH" and rows["a"]["timing"] == "OK"
    assert rows["b"]["timing"] == "SLOWER"
    assert rows["b"]["time_ratio"] == pytest.approx(1.3)
    assert rows["c"]["fingerprint"] == "MISMATCH"


def test_compare_abs_tol_covers_near_zero_values():
    base = _receipt([_entry("a", 10, {"x": 0.0}, ab=1e-9)])
    new = _receipt([_entry("a", 10, {"x": 5e-10}, ab=1e-9)])
    assert compare.compare(new, base)["rows"][0]["fingerprint"] == "MATCH"


def test_compare_version_change_is_reported_not_failed():
    base = _receipt([_entry("a", 10, {"x": 1.0}, sv="1.0")])
    new = _receipt([_entry("a", 10, {"x": 1.1}, sv="2.0")])
    result = compare.compare(new, base)
    assert result["rows"][0]["fingerprint"] == "VERSION_CHANGED"
    assert result["ok"] is True


def test_compare_version_change_does_not_waive_failure():
    base = _receipt([_entry("a", 10, {"x": 1.0}, sv="1.0")])
    new = _receipt([_entry("a", 10, {}, sv="2.0", n_ok=0)])
    result = compare.compare(new, base)
    assert result["rows"][0]["fingerprint"] == "FAILED"
    assert result["ok"] is False


def test_compare_refuses_different_pack_versions():
    with pytest.raises(ValueError, match="pack version"):
        compare.compare(_receipt([], "1"), _receipt([], "2"))


def test_compare_refuses_ineligible_baseline():
    base = _receipt([_entry("a", 10, {"x": 1.0}, eligible=False)])
    with pytest.raises(ValueError, match="baseline"):
        compare.compare(_receipt([_entry("a", 10, {"x": 1.0})]), base)


def test_compare_missing_case_is_listed():
    base = _receipt([_entry("a", 10, {"x": 1.0})])
    result = compare.compare(_receipt([]), base)
    assert result["rows"][0]["fingerprint"] == "MISSING"
    assert result["ok"] is False
