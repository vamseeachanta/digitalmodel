"""The test extra supplies security-lint tooling without the dev extra."""
import tomllib
from pathlib import Path


def test_security_lint_tooling_is_available_in_test_extra():
    """The pinned scanner and its import helper must be declared for tests."""
    root = Path(__file__).resolve().parents[2]
    project = tomllib.loads((root / "pyproject.toml").read_text(encoding="utf-8"))
    requirements = project["project"]["optional-dependencies"]["test"]
    required = {"bandit==1.7.5", "pbr>=2.0.0"}
    missing = required - set(requirements)
    assert not missing, f"Security-lint test dependencies missing: {sorted(missing)}"
