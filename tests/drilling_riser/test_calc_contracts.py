"""Unit tests for the wiki-side calc-contract loader (#1453).

All fixture values are SYNTHETIC (public-safe) — the real contracts live on
the private llm-wiki and are exercised by ``test_assembly_golden.py``.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.drilling_riser.calc_contracts import (
    REGISTRY_REL,
    SOURCES_REL,
    CalcContractError,
    find_contract,
    load_calc_contracts,
    resolve_wiki_root,
)

_REGISTRY_PAGE = """# Registry

```yaml
calc_contracts:
  - rsu_id: RSU-9001
    calc: 16q-min-top-tension
    n_wires: 8
    n_fail: 1
    min_top_tension_t: 123.4
```
"""

_SOURCE_PAGE = """# Source page

Some prose, then an UNRELATED fenced YAML block:

```yaml
joint_library:
  - class: BUOY-EXAMPLE
    length_ft: 75
```

and the contract block:

```yaml
calc_contracts:
  - rsu_id: RSU-9002
    calc: 16q-min-top-tension
    n_tensioners: 4
    n_fail: 1
    tsr_min_kips: 100.0
```
"""


@pytest.fixture()
def wiki_root(tmp_path: Path) -> Path:
    registry = tmp_path / REGISTRY_REL
    registry.parent.mkdir(parents=True)
    registry.write_text(_REGISTRY_PAGE)
    sources = tmp_path / SOURCES_REL
    sources.mkdir(parents=True)
    (sources / "example-source.md").write_text(_SOURCE_PAGE)
    (sources / "no-contracts.md").write_text("# prose only\n")
    return tmp_path


def test_loads_registry_and_source_page_contracts(wiki_root: Path):
    contracts = load_calc_contracts(wiki_root)
    by_id = {c["rsu_id"]: c for c in contracts}
    assert set(by_id) == {"RSU-9001", "RSU-9002"}
    assert by_id["RSU-9001"]["source_page"] == "stackup-registry.md"
    assert by_id["RSU-9002"]["source_page"] == "example-source.md"
    # Unrelated fenced YAML blocks are ignored, not treated as contracts.
    assert by_id["RSU-9002"]["tsr_min_kips"] == 100.0


def test_find_contract_hit_and_miss(wiki_root: Path):
    hit = find_contract("RSU-9002", "16q-min-top-tension", wiki_root)
    assert hit is not None and hit["n_tensioners"] == 4
    assert find_contract("RSU-9002", "other-calc", wiki_root) is None
    assert find_contract("RSU-9999", "16q-min-top-tension", wiki_root) is None


def test_duplicate_contract_fails_closed(wiki_root: Path):
    dup = wiki_root / SOURCES_REL / "zz-duplicate.md"
    dup.write_text(_SOURCE_PAGE)
    with pytest.raises(CalcContractError, match="duplicate contract"):
        load_calc_contracts(wiki_root)


def test_malformed_contract_block_fails_closed(wiki_root: Path):
    bad = wiki_root / SOURCES_REL / "bad.md"
    bad.write_text("```yaml\ncalc_contracts:\n  - rsu_id: [unclosed\n```\n")
    with pytest.raises(CalcContractError, match="malformed"):
        load_calc_contracts(wiki_root)


def test_other_contract_dialects_are_skipped(wiki_root: Path):
    """The wiki also carries looser result-list contract dialects (``id:`` +
    ``expected:`` entries, no ``calc`` key) — not consumed by dm, not an
    error."""
    other = wiki_root / SOURCES_REL / "other-dialect.md"
    other.write_text(
        "```yaml\ncalc_contracts:\n"
        "  - id: RSU-9003-mode-a\n"
        "    expected:\n"
        "      - {name: example-metric, value: 1.0, units: kN}\n"
        "```\n"
    )
    contracts = load_calc_contracts(wiki_root)
    assert {c["rsu_id"] for c in contracts} == {"RSU-9001", "RSU-9002"}


def test_resolve_wiki_root_honors_env(wiki_root: Path, monkeypatch):
    monkeypatch.setenv("LLM_WIKI_PATH", str(wiki_root))
    assert resolve_wiki_root() == wiki_root


def test_resolve_wiki_root_rejects_rootless_env(tmp_path: Path, monkeypatch):
    # Env points somewhere without the registry page -> fall through (and
    # None when no known clone carries it either, unless a real clone does).
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    resolved = resolve_wiki_root()
    assert resolved != tmp_path
