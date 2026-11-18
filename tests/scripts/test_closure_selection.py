"""Minimum shard coverage for a single changed path on a pull request."""

from pathlib import Path

import pytest

from scripts.ci import detect_touched_domains as detector


DOMAINS = detector.parse_domains(Path(__file__).resolve().parents[2] / "tests/DOMAINS.md")
FULL = {domain.name for domain in DOMAINS}


@pytest.mark.parametrize(("path", "closure"), [
    pytest.param("src/digitalmodel/citations/schema.py", {"citations"}, id="citations"),
    pytest.param("src/digitalmodel/citations/schema.py", {"subsea"},
                 id="dependent", marks=pytest.mark.xfail(strict=True, raises=AssertionError,
                 reason="Subsea stability tests import the schema, but the citation route omits subsea")),
    pytest.param("src/digitalmodel/process_piping/line_sizing.py", {"asset-integrity"}, id="family"),
    pytest.param("src/digitalmodel/engine.py", FULL, id="engine"),
    pytest.param("uv.lock", FULL, id="configuration"),
    pytest.param("src/digitalmodel/new_unrouted_package/model.py", FULL, id="unknown-route"),
])
def test_pr_closure(path, closure):
    selected = detector.touched_domains([path], DOMAINS)
    assert closure <= {domain.name for domain in selected}
