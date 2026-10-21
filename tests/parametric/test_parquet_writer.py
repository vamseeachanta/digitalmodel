# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Match the declared writer versions to the committed byte-identity fixture."""
import json
import tomllib

from packaging.requirements import Requirement
import pyarrow.parquet as parquet
import pytest

from digitalmodel.drilling_riser import operability_atlas as oa


@pytest.mark.parametrize("package", ["pandas", "pyarrow"])
def test_parquet_writer_versions_match_committed_artifact(package):
    base = oa.REPO_ROOT / "atlases" / oa.BASENAME
    atlas_id = (base / "default.txt").read_text().strip()
    metadata = parquet.read_metadata(base / atlas_id / "grid.parquet")
    writer = json.loads(metadata.metadata[b"pandas"])
    versions = {"pandas": writer["pandas_version"],
                writer["creator"]["library"]: writer["creator"]["version"]}
    config = tomllib.loads((oa.REPO_ROOT / "pyproject.toml").read_text())
    requirements = [Requirement(value) for value in config["project"]["dependencies"]]
    requirement = next(item for item in requirements if item.name == package)
    assert str(requirement.specifier) == f"=={versions[package]}", (
        f"{package} must match the committed writer for the byte-identity gate"
    )
