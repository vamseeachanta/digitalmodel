"""Pytest parametrize compatibility demo for YAML test vector fixtures.

Proves that fixture YAML files can drive parametrized tests.
Uses naval_architecture/en400_fundamentals.yaml as the reference domain.
"""

import pathlib

import pytest
import yaml

FIXTURES_DIR = pathlib.Path(__file__).parent


def load_worked_examples(fixture_path: pathlib.Path) -> list[dict]:
    """Load worked_examples from a YAML fixture file."""
    data = yaml.safe_load(fixture_path.read_text())
    return data.get("worked_examples", [])


EN400_FUNDAMENTALS = load_worked_examples(
    FIXTURES_DIR / "naval_architecture" / "en400_fundamentals.yaml"
)


@pytest.mark.parametrize(
    "example",
    EN400_FUNDAMENTALS,
    ids=[ex["description"][:60] for ex in EN400_FUNDAMENTALS],
)
def test_en400_fundamentals_vector(example):
    """Verify each worked example has required fields and valid values."""
    assert "inputs" in example
    assert "outputs" in example
    assert example.get("use_as_test") is True

    for key, value in example["outputs"].items():
        assert isinstance(value, (int, float, str)), (
            f"Output '{key}' must be a number or string, got {type(value)}"
        )
