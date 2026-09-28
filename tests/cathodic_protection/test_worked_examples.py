"""Worked-example contract for docs/domains/cathodic_protection/examples (#2214).

Every ```python block in ``examples/calc-0xx*.md`` and ``examples/example-0*.md`` is
extracted, executed in a fresh namespace with the repo ``src/`` importable, and must
raise nothing. A block whose first non-blank line is ``# not-runnable: <reason>`` is
skipped with that reason. When a block leaves a ``cfg`` dict behind, its
``inputs.calculation_type`` must be one of the keys ``CathodicProtection.router``
accepts; the accepted set is read from the router source so the two cannot drift.
"""

from __future__ import annotations

import contextlib
import io
import re
import warnings
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "src"
EXAMPLES = ROOT / "docs" / "domains" / "cathodic_protection" / "examples"
ROUTER_SOURCE = (
    SRC / "digitalmodel" / "infrastructure" / "base_solvers" / "hydrodynamics"
    / "cathodic_protection.py"
)

FENCE = re.compile(r"```python[ \t]*\n(.*?)```", re.S)
NOT_RUNNABLE = re.compile(r"^#\s*not-runnable:\s*(?P<reason>.+?)\s*$")
ROUTER_KEY = re.compile(r'cfg\["inputs"\]\["calculation_type"\]\s*==\s*"([^"]+)"')

EXPECTED_ROUTER_KEYS = {
    "ABS_gn_ships_2018",
    "DNV_RP_F103_2010",
    "ABS_gn_offshore_2018",
    "DNV_RP_B401_offshore",
}


def router_keys_from_source() -> set[str]:
    text = ROUTER_SOURCE.read_text(encoding="utf-8")
    router_body = text.split("def router(", 1)[1].split("\n    def ", 1)[0]
    return set(ROUTER_KEY.findall(router_body))


def example_files() -> list[Path]:
    files = sorted(EXAMPLES.glob("calc-0*.md")) + sorted(EXAMPLES.glob("example-0*.md"))
    return files


def python_blocks() -> list[pytest.ParameterSet]:
    params = []
    for path in example_files():
        text = path.read_text(encoding="utf-8")
        for index, match in enumerate(FENCE.finditer(text), start=1):
            params.append(pytest.param(path, index, match.group(1), id=f"{path.name}#{index}"))
    return params


def not_runnable_reason(code: str) -> str | None:
    for line in code.splitlines():
        if not line.strip():
            continue
        tag = NOT_RUNNABLE.match(line.strip())
        return tag.group("reason") if tag else None
    return None


def test_router_accepts_exactly_the_documented_keys() -> None:
    assert router_keys_from_source() == EXPECTED_ROUTER_KEYS


def test_examples_are_collected() -> None:
    files = example_files()
    assert len(files) >= 14, [p.name for p in files]
    assert sum(1 for _ in python_blocks()) >= len(files)


@pytest.mark.parametrize("path,index,code", python_blocks())
def test_worked_example_block_executes(path: Path, index: int, code: str, monkeypatch) -> None:
    reason = not_runnable_reason(code)
    if reason:
        pytest.skip(f"{path.name}#{index} tagged not-runnable: {reason}")

    monkeypatch.syspath_prepend(str(SRC))
    namespace: dict = {"__name__": f"cp_worked_example_{path.stem}_{index}"}
    with contextlib.redirect_stdout(io.StringIO()), warnings.catch_warnings():
        warnings.simplefilter("ignore")
        exec(compile(code, f"{path.name}#{index}", "exec"), namespace)

    cfg = namespace.get("cfg")
    if isinstance(cfg, dict):
        key = cfg.get("inputs", {}).get("calculation_type")
        if key is not None:
            assert key in router_keys_from_source(), (
                f"{path.name}#{index} passes calculation_type={key!r}; "
                f"the router accepts {sorted(router_keys_from_source())}"
            )
