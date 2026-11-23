"""Wiki-side calc-contract loader (#1280 / #1453, epic #1279).

The private llm-wiki riser registry carries machine-readable ``calc_contracts``
fenced-YAML blocks — the golden-value data contracts consumed by
digitalmodel's in-context golden tests and by the registry batch-run example.
dm carries only the KEY names and this loader; the VALUES live wiki-side and
resolve at test/run time (never committed here).

Contract locations (llm-wiki#826): the registry page
(``datasets/stackup-registry.md``, e.g. RSU-0007) AND per-dataset SOURCE pages
(``sources/*.md``, e.g. RSU-0010/0012/0014/0023). This loader scans both.

Discipline mirrors ``riser_database.loader``: bounded reads (size caps,
``safe_load``), fail-closed on a malformed contract block or a duplicate
``(rsu_id, calc)`` key, and no fuzzy matching.
"""

from __future__ import annotations

import os
import re
from pathlib import Path
from typing import Iterator, Optional

import yaml

__all__ = [
    "CalcContractError",
    "REGISTRY_REL",
    "SOURCES_REL",
    "contract_referenced_rsu_ids",
    "find_contract",
    "load_calc_contracts",
    "resolve_wiki_root",
]

REGISTRY_REL = "wikis/riser-projects/wiki/datasets/stackup-registry.md"
SOURCES_REL = "wikis/riser-projects/wiki/sources"
_KNOWN_WIKI_CLONES = (
    Path("/mnt/local-analysis/llm-wiki"),  # abs-path-allowed
    Path.home() / "workspace-hub" / "llm-wiki",
)
#: Bounded-read cap per wiki page (these are curated markdown pages).
MAX_PAGE_BYTES = 5 * 1024 * 1024
_FENCED_YAML = re.compile(r"```yaml\n(.*?)```", re.DOTALL)


class CalcContractError(RuntimeError):
    """Fail-closed contract-scan error (malformed block, duplicate key)."""


def resolve_wiki_root() -> Optional[Path]:
    """First llm-wiki clone whose registry page exists: ``LLM_WIKI_PATH``
    env, then the known clone locations. ``None`` when unavailable
    (standalone mode — callers skip loudly or report missing-inputs)."""
    candidates: list[Path] = []
    env = os.environ.get("LLM_WIKI_PATH")
    if env:
        candidates.append(Path(env))
    candidates.extend(_KNOWN_WIKI_CLONES)
    for base in candidates:
        if (base / REGISTRY_REL).is_file():
            return base
    return None


def _contract_pages(wiki_root: Path) -> Iterator[Path]:
    yield wiki_root / REGISTRY_REL
    sources_dir = wiki_root / SOURCES_REL
    if sources_dir.is_dir():
        yield from sorted(sources_dir.glob("*.md"))


def _blocks(page: Path) -> Iterator[dict]:
    if page.stat().st_size > MAX_PAGE_BYTES:
        raise CalcContractError(
            f"{page.name}: size exceeds bounded-read cap {MAX_PAGE_BYTES}"
        )
    text = page.read_text(encoding="utf-8")
    for block in _FENCED_YAML.findall(text):
        try:
            doc = yaml.safe_load(block)
        except yaml.YAMLError as exc:
            if "calc_contracts" in block:
                raise CalcContractError(
                    f"{page.name}: malformed calc_contracts YAML block"
                ) from exc
            continue  # unrelated fenced YAML (not a contract block)
        if isinstance(doc, dict) and "calc_contracts" in doc:
            yield doc


def load_calc_contracts(wiki_root: Path) -> tuple[dict, ...]:
    """All calc contracts on the registry page + source pages, fail-closed.

    Each contract is the raw wiki-side mapping plus a ``source_page`` key
    (page filename, provenance only).

    Only the dm#1280 golden dialect is collected: entries keyed by BOTH
    ``rsu_id`` and ``calc``. The wiki also carries looser result-list
    contract dialects (e.g. ``id:`` + ``expected:`` entries) — those are a
    wiki-side pattern dm does not consume yet and are skipped, not errors.
    A duplicate ``(rsu_id, calc)`` raises :class:`CalcContractError`.
    """
    contracts: dict[tuple[str, str], dict] = {}
    for page in _contract_pages(wiki_root):
        for doc in _blocks(page):
            entries = doc["calc_contracts"]
            if not isinstance(entries, list):
                raise CalcContractError(f"{page.name}: calc_contracts is not a list")
            for contract in entries:
                if not isinstance(contract, dict):
                    raise CalcContractError(
                        f"{page.name}: non-mapping calc contract entry"
                    )
                key = (contract.get("rsu_id"), contract.get("calc"))
                if not all(key):
                    continue  # non-golden contract dialect (not consumed)
                if key in contracts:
                    raise CalcContractError(
                        f"duplicate contract {key} on {page.name} and "
                        f"{contracts[key]['source_page']}"
                    )
                contracts[key] = {**contract, "source_page": page.name}
    return tuple(contracts.values())


_RSU_ID = re.compile(r"RSU-\d{4}")


def contract_referenced_rsu_ids(wiki_root: Path) -> frozenset[str]:
    """RSU ids referenced by ANY contract dialect (golden ``rsu_id`` entries
    AND the looser ``id:``-keyed result-list entries) — lets a batch run
    distinguish 'result contracts exist, no golden 16Q chain yet' from 'no
    contract at all' when reporting missing-inputs reasons."""
    ids: set[str] = set()
    for page in _contract_pages(wiki_root):
        for doc in _blocks(page):
            entries = doc["calc_contracts"]
            if not isinstance(entries, list):
                continue
            for contract in entries:
                if not isinstance(contract, dict):
                    continue
                ref = str(contract.get("rsu_id") or contract.get("id") or "")
                match = _RSU_ID.match(ref)
                if match:
                    ids.add(match.group())
    return frozenset(ids)


def find_contract(rsu_id: str, calc: str, wiki_root: Path) -> Optional[dict]:
    """The unique contract for ``(rsu_id, calc)``, or ``None`` if the wiki
    carries no such contract (the caller decides fail vs missing-inputs)."""
    for contract in load_calc_contracts(wiki_root):
        if contract["rsu_id"] == rsu_id and contract["calc"] == calc:
            return contract
    return None
