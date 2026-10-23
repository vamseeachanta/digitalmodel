"""Issue 2027: synthetic histories exercise the shipped Python criterion."""
import contextlib
import io
from pathlib import Path
import sys

import pytest

WATCH = Path(__file__).resolve().parents[2] / "scripts/cfd/ittc_watch.sh"


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
def test_restart_empty_file_does_not_use_old_history(history, monkeypatch, contents):
    armed = history.stat().st_mtime_ns
    newest = history.with_name("coefficient_4.dat")
    newest.write_text(contents, encoding="utf-8")
    assert criterion(history, armed, monkeypatch) == "WAIT stale-coefficient"
    fresh_boundary = newest.stat().st_mtime_ns - 1
    assert criterion(newest, fresh_boundary, monkeypatch) == "WAIT no-rows"


def test_missing_file_waits(history, monkeypatch):
    missing = history.with_name("coefficient_missing.dat")
    assert criterion(missing, history.stat().st_mtime_ns, monkeypatch) == "WAIT coefficient-unavailable"


def test_shell_pass_state_is_reset_on_file_change():
    source = WATCH.read_text(encoding="utf-8")
    assert '"$DRIFT_PCT" "$WATCH_STARTED_NS"' in source
    assert 'if [ "$COEF" != "$last_coef" ]; then pass=0; fi' in source
    assert 'pass=0; cfd_say "WAIT no coefficient file yet"' in source
