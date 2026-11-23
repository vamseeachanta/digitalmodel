# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Packaging and setup contracts for the real authentication request path."""

import tomllib
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]


@pytest.mark.parametrize(
    ("distribution", "version"),
    [("flask-httpauth", "4.8.1"), ("flask-restful", "0.3.10")],
)
def test_auth_request_dependencies_are_pinned(distribution, version):
    project = tomllib.loads((ROOT / "pyproject.toml").read_text())
    assert f"{distribution}=={version}" in project["project"]["dependencies"]
    locked = tomllib.loads((ROOT / "uv.lock").read_text())
    packages = {package["name"]: package for package in locked["package"]}
    assert packages[distribution]["version"] == version
    model = packages["digitalmodel"]
    assert {"name": distribution} in model["dependencies"]
    assert {"name": distribution, "specifier": f"=={version}"} in (
        model["metadata"]["requires-dist"]
    )


def test_template_auth_environment_has_no_example_values():
    entries = {}
    for line in (ROOT / ".env.template").read_text().splitlines():
        name, separator, value = line.partition("=")
        if separator and not name.lstrip().startswith("#"):
            entries[name] = value
    for suffix in ("USER", "PASSWORD"):
        name = f"TEMPLATE_CODE_BASIC_AUTH_{suffix}"
        if name not in entries or entries[name].strip():
            pytest.fail("Independent template authentication requires empty setup entries")
