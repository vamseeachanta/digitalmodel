# ABOUTME: Shared golden helpers for the workflow-API adoption tests (workspace-hub#3285).
"""Golden-comparison helpers for the digitalmodel workflow-API tests.

#3283's ``golden_workflow_test`` template is not yet merged into assetutilities, so
these tests carry a small self-contained golden helper. Two determinism signals:

* ``reproducible is True`` -- a double-run (``verify_reproducible=True``) in two
  separate tempdir roots yields an identical content hash (in-env determinism).
* a committed golden -- the result payload is pinned. For ``kind: files`` and the
  fully-rounded mooring payload the exact ``result_hash`` is asserted; for the FFS
  in_memory payload (which carries unrounded floats whose last ULP can shift across
  numpy builds) the VALUE is compared with tolerance instead of the exact hash.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

GOLDENS = Path(__file__).resolve().parent / "goldens"

# Wiki base for DNV-OS-E301 citation resolution in the mooring route. The committed
# fixture tree is the only copy of the page in this public repo (the real wiki is the
# private llm-wiki). The mooring example fixture points repo_root here too.
FIXTURE_WIKI_ROOT = Path(__file__).resolve().parents[1] / "citations" / "fixtures"


def load_golden(name: str) -> dict:
    return json.loads((GOLDENS / name).read_text())


def deep_approx(actual, expected, *, rel: float = 1e-9, abs_: float = 1e-12) -> None:
    """Assert ``actual`` matches ``expected`` with float tolerance on numbers."""
    if isinstance(expected, dict):
        assert isinstance(actual, dict), f"expected dict, got {type(actual)}"
        assert set(actual) == set(expected), (
            f"key mismatch: {set(actual) ^ set(expected)}"
        )
        for key in expected:
            deep_approx(actual[key], expected[key], rel=rel, abs_=abs_)
    elif isinstance(expected, list):
        assert isinstance(actual, list) and len(actual) == len(expected)
        for a, e in zip(actual, expected):
            deep_approx(a, e, rel=rel, abs_=abs_)
    elif isinstance(expected, bool):
        assert actual is expected or actual == expected
    elif isinstance(expected, (int, float)) and not isinstance(expected, bool):
        assert actual == pytest.approx(expected, rel=rel, abs=abs_)
    else:
        assert actual == expected
