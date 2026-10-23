"""Regression checks for the remediated security-lint boundaries.

Source: Bandit 1.7.5 B105 documents comparisons against variables named
``token`` as candidates and warns of false positives:
https://bandit.readthedocs.io/en/1.7.5/plugins/b105_hardcoded_password_string.html
No engineering coefficients or acceptance limits are introduced.
The custom loader boundary follows Bandit 1.7.5 B506:
https://bandit.readthedocs.io/en/1.7.5/plugins/b506_yaml_load.html
"""
from pathlib import Path

import pytest

# Bandit is a dev extra, not a runtime/test-extra dependency. Report missing
# lint tooling as a skip in minimal environments rather than breaking collection.
BanditConfig = pytest.importorskip(
    "bandit.core.config", reason="Security-lint contracts require Bandit and pbr",
).BanditConfig
BanditManager = pytest.importorskip(
    "bandit.core.manager", reason="Security-lint contracts require Bandit and pbr",
).BanditManager

ROOT = Path(__file__).resolve().parents[2]
PARSERS = (
    "src/digitalmodel/ansys/cylinder_expression.py",
    "src/digitalmodel/solvers/openfoam/warm_start/admissibility.py",
)


def rule_findings(path, rule):
    """Run the pinned hook's rule without printing matched literals."""
    manager = BanditManager(BanditConfig(), "file", profile={"include": [rule]})
    manager.discover_files([str(path)])
    manager.run_tests()
    assert not manager.get_skipped(), "Bandit could not read the input"
    return [issue.test_id for issue in manager.results]


@pytest.mark.parametrize("relative_path", PARSERS)
def test_parser_has_no_password_literal_findings(relative_path):
    """Expected absence follows the non-authentication role of both parsers."""
    assert rule_findings(ROOT / relative_path, "B105") == []


@pytest.mark.parametrize("rule,source_text", [
    ("B105", 'if token == "(":\n    pass\n'),
    ("B506", 'import yaml\nyaml.load("{}", Loader=CustomLoader)\n'),
])
def test_rule_is_active(tmp_path, rule, source_text):
    """Documented rule patterns must trigger the positive controls."""
    source = tmp_path / "grammar.py"
    source.write_text(source_text, encoding="utf-8")
    assert rule_findings(source, rule) == [rule]


def test_catalogue_loader_has_no_unsafe_yaml_findings():
    assert rule_findings(ROOT / "src/digitalmodel/bha_database/loader.py", "B506") == []
