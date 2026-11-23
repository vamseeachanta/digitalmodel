"""Issue 2027: synthetic histories exercise the criterion and watcher loop."""
import contextlib
import io
import json
import os
from pathlib import Path
import re
import subprocess
import sys

import pytest

WATCH = Path(__file__).resolve().parents[2] / "scripts/cfd/ittc_watch.sh"
ARMED_NS = 1_700_000_000_000_000_000
CONVERGED = "# Time Cd\n1 1\n2 1\n3 1\n4 1\n"

# Only clock capture, polling and the process probe are replaced. Discovery,
# file freshness, pass accounting and trigger creation use the shipped code.
WATCHER_DRIVER = r'''
poll=0
python3() {
  if [ "$1" = "-c" ]; then
    printf '%s\n' "$TEST_ARMED_NS" >> "$TEST_CLOCK_CALLS"
    printf '%s\n' "$TEST_ARMED_NS"
  else
    "$TEST_PYTHON" "$@"
  fi
}
sleep() {
  poll=$((poll + 1))
  "$TEST_PYTHON" -c "$TEST_POLL_DRIVER" "$poll"
}
pgrep() {
  [ "$1" = "-x" ] && [ "$2" = "$DM_CFD_SOLVER" ] &&
    [ "$poll" -le "$TEST_POLLS" ]
}
watch=$1
shift
source "$watch" "$@"
'''

POLL_DRIVER = r'''
import json
import os
from pathlib import Path
import sys

poll = int(sys.argv[1])
case = Path(os.environ["TEST_CASE"])
# Every poll must start without a trigger, including the final passing poll.
assert not (case / "ITTC_CONVERGED").exists(), "premature convergence trigger"
log = case / "ittc_watch.log"
(case / f"before-poll-{poll}.log").write_text(
    log.read_text(encoding="utf-8"), encoding="utf-8"
)
steps = json.loads(os.environ["TEST_STEPS"])
if poll <= len(steps):
    for name, contents in steps[poll - 1]:
        path = case / "postProcessing" / "coefficients" / "0" / name
        path.write_text(contents, encoding="utf-8")
        timestamp = int(os.environ["TEST_ARMED_NS"]) + poll * 1_000_000_000
        os.utime(path, ns=(timestamp, timestamp))
'''


def run_watcher(tmp_path, contents, steps):
    case = tmp_path / "cases" / "restart"
    coefficients = case / "postProcessing" / "coefficients" / "0"
    coefficients.mkdir(parents=True)
    for name, data, offset in (
        ("coefficient.dat", CONVERGED, -1_000_000_000),
        ("coefficient_4.dat", contents, 1_000_000_000),
    ):
        path = coefficients / name
        path.write_text(data, encoding="utf-8")
        os.utime(path, ns=(ARMED_NS + offset, ARMED_NS + offset))
    env = {
        key: value for key, value in os.environ.items()
        if not key.startswith(("GIT_", "DM_CFD_", "CFD_"))
        and key not in ("BASH_ENV", "ENV")
    }
    env.update({
        "DM_CFD_ROOT": str(tmp_path), "DM_CFD_CASES_DIR": "cases",
        "DM_CFD_SOLVER": "synthetic-solver", "DM_CFD_POLL_SECONDS": "0",
        "MIN_ITER": "4", "WINDOW": "2", "WINDOWS": "2", "HOLD": "2",
        "SPREAD_PCT": "0.60", "DRIFT_PCT": "0.35",
        "TEST_PYTHON": sys.executable, "TEST_ARMED_NS": str(ARMED_NS),
        "TEST_CLOCK_CALLS": str(case / "clock-calls.txt"),
        "TEST_CASE": str(case), "TEST_POLL_DRIVER": POLL_DRIVER,
        "TEST_STEPS": json.dumps(steps), "TEST_POLLS": str(len(steps)),
    })
    result = subprocess.run(
        ["bash", "-c", WATCHER_DRIVER, "watcher-test", str(WATCH), "restart"],
        cwd=tmp_path, env=env, capture_output=True, text=True, timeout=15,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert (case / "clock-calls.txt").read_text().splitlines() == [str(ARMED_NS)]
    assert (case / "ittc_watch.marker").read_text().startswith("OK ")
    return case, (case / "ittc_watch.log").read_text(encoding="utf-8")


def criterion(path, armed_ns, monkeypatch):
    source = WATCH.read_text(encoding="utf-8").split("<<'PY'\n", 1)[1]
    source = source.split("\nPY\n", 1)[0]
    # Synthetic inputs, not engineering limits: constant Cd over two windows.
    monkeypatch.setattr(sys, "argv", [
        "criterion", str(path), "4", "2", "2", "0.60", "0.35", str(armed_ns)
    ])
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        try:
            exec(compile(source, str(WATCH), "exec"), {})
        except SystemExit as exc:
            assert exc.code in (None, 0)
    return output.getvalue().strip()


@pytest.fixture
def history(tmp_path):
    path = tmp_path / "coefficient.dat"
    path.write_text("# Time Cd\n1 1\n2 1\n3 1\n4 1\n", encoding="utf-8")
    return path


@pytest.mark.parametrize("offset", [0, 1_000_000_000])
def test_old_or_equal_mtime_cannot_pass(history, monkeypatch, offset):
    # Issue 2027 requires modification strictly after arming, including equality.
    armed = history.stat().st_mtime_ns + offset
    assert criterion(history, armed, monkeypatch) == "WAIT stale-coefficient"


def test_fresh_history_passes(history, monkeypatch):
    armed = history.stat().st_mtime_ns - 1_000_000_000
    assert criterion(history, armed, monkeypatch).startswith("PASS ")


@pytest.mark.parametrize("contents", ["", "# Time Cd\n"])
def test_restart_empty_file_does_not_use_old_history(tmp_path, contents):
    # Both histories coexist for three checks: an erroneous fallback must never
    # reach HOLD, even though the old history contains converged coefficients.
    case, log = run_watcher(tmp_path, contents, [[], [], []])
    assert log.count("no-rows") == 3
    assert "PASS " not in log
    assert "solver no longer running; watcher exiting without a stop" in log
    assert not (case / "ITTC_CONVERGED").exists()


@pytest.mark.parametrize("contents", ["", "# Time Cd\n"])
def test_watcher_resets_passes_before_fresh_convergence(tmp_path, contents):
    steps = [
        [],
        [("coefficient_4.dat", CONVERGED)],
        [("coefficient_4.dat", contents)],
        [("coefficient_4.dat", CONVERGED)],
        [("coefficient_5.dat", CONVERGED.replace(" 1\n", " 2\n"))],
        [],
    ]
    case, log = run_watcher(tmp_path, contents, steps)
    assert re.findall(r"no-rows|PASS \d/\d|reset after \d+ pass\(es\)", log) == [
        "no-rows", "PASS 1/2", "reset after 1 pass(es)", "no-rows",
        "PASS 1/2", "PASS 1/2", "PASS 2/2",
    ]
    # The changed mean establishes that the newly discovered version was read.
    assert "PASS 1/2  iter=4 mean=2.000000e+00" in log
    assert "PASS 2/2" not in (case / "before-poll-6.log").read_text()
    assert (case / "ITTC_CONVERGED").is_file()
    assert "ITTC criterion met; trigger created" in log
    assert not (case / "before-poll-7.log").exists()


def test_missing_file_waits(history, monkeypatch):
    missing = history.with_name("coefficient_missing.dat")
    assert criterion(missing, history.stat().st_mtime_ns, monkeypatch) == "WAIT coefficient-unavailable"
