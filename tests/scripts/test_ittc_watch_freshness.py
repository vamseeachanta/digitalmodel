"""In-memory boundary tests; issue 2027 supplies the strict freshness rule."""
import contextlib
import io
import os
from pathlib import Path
import sys
from types import SimpleNamespace

import pytest


@pytest.mark.parametrize(
    "mtime,contents,expected",
    [
        (9, "1 1\n2 1\n3 1\n4 1\n", "WAIT stale-coefficient"),
        (10, "1 1\n2 1\n3 1\n4 1\n", "WAIT stale-coefficient"),
        (11, "", "WAIT no-rows"),
        (11, "# Time Cd\n", "WAIT no-rows"),
        (11, "1 1\n2 1\n3 1\n4 1\n", "PASS "),
        (11, "1 1\n2 1\n", "WAIT iter=2<4"),
        (11, "1 1\n2 1\n3 2\n4 2\n", "WAIT iter=4"),
    ],
)
def test_criterion_boundary(monkeypatch, mtime, contents, expected):
    # All numbers are synthetic inputs: arming at 10 ns, two two-row windows.
    # Constant means have zero spread/drift; means 1 and 2 fail both limits.
    watch = Path(__file__).resolve().parents[2] / "scripts/cfd/ittc_watch.sh"
    source = watch.read_text(encoding="utf-8").split("<<'PY'\n", 1)[1]
    source = source.split("\nPY\n", 1)[0]
    history = io.StringIO(contents)
    history.fileno = lambda: 0
    monkeypatch.setattr(os, "fstat", lambda fd: SimpleNamespace(st_mtime_ns=mtime))
    monkeypatch.setattr(sys, "argv", [
        "criterion", "coefficient.dat", "4", "2", "2", "0.60", "0.35", "10"
    ])
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        try:
            exec(compile(source, str(watch), "exec"), {"open": lambda *a, **kw: history})
        except SystemExit as exc:
            assert exc.code in (None, 0)
    assert output.getvalue().startswith(expected)
    assert history.closed
