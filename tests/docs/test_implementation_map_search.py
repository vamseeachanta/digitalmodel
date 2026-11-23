# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check optimized searches against the recorded regular expressions."""

import importlib.util
import re
from pathlib import Path

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "implementation_map_search", ROOT / "scripts/implementation_map_search.py"
)
SEARCH = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(SEARCH)

FORCE = r"(?:0\.5|1\s*/\s*2).*(?:rho|density).*(?:cd|drag|area)"
STRESS = r"(?:stress|stress_range|delta_sigma).*\*\*.*\bm\b"
LOG = r"log10.*(?:cycles|stress)"
INTERNAL = r"internal_pressure.*(?:area|A_i)"
EXTERNAL = r"external_pressure.*(?:area|A_o)"
CORPUS = (
    "", "0.5 rho cd", "0.5 density area", "1 / 2 rho drag", "1\n/\n2 rho cd",
    "0.5\n1 /\n2 rho cd", "0.5 rho\ncd", "0.5 cd rho", "0.5 rhocd",
    "stress ** m", "stress_range ** m", "delta_sigma ** m", "stress ** mm",
    "stress ** m_", "stress ** _m", "stress ** αm", "stress ** mα",
    "stress **\nm", "stress_range ** m\nnext", "log10(stress)", "log10 cycles",
    "log10\nstress", "internal_pressure * A_i", "external_pressure * A_o",
    "area internal_pressure", "internal_pressure\narea", "AREA INTERNAL_PRESSURE",
    "0.5 rho 0.5 density area", "1 \n / \n 2 density cd", "1 / 20.5 rho cd",
    "stress ** mm stress ** m", "x|y", "[a|b]", "foo[bar]|tail",
)


@pytest.mark.parametrize("pattern", [FORCE, STRESS, LOG, INTERNAL, EXTERNAL])
@pytest.mark.parametrize("flags", [0, re.I, re.M, re.A, re.I | re.A])
def test_ordered_branches_preserve_search(pattern: str, flags: int) -> None:
    """Compare each known branch over positive and negative ordered examples."""
    compiled = re.compile(pattern, flags)
    for text in CORPUS:
        assert SEARCH.matches(compiled, text) == bool(compiled.search(text)), (pattern, text)


def test_recorded_searches_preserve_existence() -> None:
    """Compare every recorded normalized pattern over a bounded corpus."""
    document = yaml.safe_load((ROOT / "docs/registry/implementation-map.yaml").read_text())
    patterns = {
        signal["pattern"].replace("\\\\", "\\")
        for family in document["families"]
        for signal in family["search_signals"]
    }
    assert len(patterns) >= 30
    for pattern in sorted(patterns):
        compiled = re.compile(pattern, re.I)
        for text in CORPUS:
            assert SEARCH.matches(compiled, text) == bool(compiled.search(text)), (pattern, text)


@pytest.mark.parametrize("other", [r"x\|y", r"[a|b]", r"[]|]", r"[()|]", r"(?:foo|bar)", r"foo[\[]bar]"])
def test_top_level_split_preserves_nested_and_escaped_pipes(other: str) -> None:
    """Keep nested alternatives, literal pipes and character classes intact."""
    compiled = re.compile(other + "|" + FORCE)
    for text in CORPUS + ("|", "]", "(", "foo", "bar", "foo[bar]"):
        assert SEARCH.matches(compiled, text) == bool(compiled.search(text)), (other, text)


@pytest.mark.parametrize("flags", [re.S, re.X, re.S | re.I])
def test_unsupported_flags_use_original_search(flags: int) -> None:
    """Preserve newline and whitespace semantics outside the optimized contract."""
    compiled = re.compile(FORCE, flags)
    for text in CORPUS:
        assert SEARCH.matches(compiled, text) == bool(compiled.search(text)), text


def test_backreference_uses_original_search() -> None:
    """Retain capturing-group numbering across alternative branches."""
    compiled = re.compile(r"(x)\1|" + FORCE + r"|\1")
    for text in CORPUS + ("xx", "x"):
        assert SEARCH.matches(compiled, text) == bool(compiled.search(text)), text


def test_repeated_prefixes_are_searched_once_per_physical_line(monkeypatch: pytest.MonkeyPatch) -> None:
    """Avoid repeated suffix scans after many prefixes on one physical line."""
    calls = []
    original = SEARCH._ordered_suffix

    def counted(tokens: tuple, text: str, start: int, end: int) -> bool:
        calls.append((start, end))
        return original(tokens, text, start, end)

    monkeypatch.setattr(SEARCH, "_ordered_suffix", counted)
    assert not SEARCH.matches(re.compile(FORCE), "0.5 " * 20000)
    assert len(calls) == 1
