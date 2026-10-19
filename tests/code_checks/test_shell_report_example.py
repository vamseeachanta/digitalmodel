# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""The rendered examples are reproducible views of declared synthetic inputs."""

import hashlib
import importlib.util
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def load_example():
    spec = importlib.util.spec_from_file_location(
        "shell_example",
        ROOT / "examples/code_checks/shell_thickness_reports.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_compact_examples_match_committed_pages_and_manifests(tmp_path):
    module = load_example()
    outputs = module.generate(tmp_path, compact=True)
    expected = ROOT / "docs/domains/storage-tanks"
    assert len(outputs) == 3
    for output in outputs:
        assert output.is_file()
        assert output.read_bytes() == (expected / output.name).read_bytes()
    manifest = json.loads((tmp_path / "shell-thickness-examples.json").read_text())
    for filename, digest in manifest["sha256"].items():
        assert hashlib.sha256((tmp_path / filename).read_bytes()).hexdigest() == digest
    tank = (tmp_path / "one-foot-worked-example.html").read_text()
    assert "18.846" in tank and "5.616" in tank
    assert "3 significant figures" in tank
    assert "assumed, based on previous experience" in tank
    assert "NOT_EVALUATED" in tank
    assert "Plotly.newPlot" in tank
    assert "cdn.plot.ly/plotly-" in tank
    assert len(tank.encode()) < 1_000_000


def test_default_example_delivery_is_self_contained(tmp_path):
    outputs = load_example().generate(tmp_path)
    for path in outputs:
        if path.suffix == ".html":
            content = path.read_text()
            assert "<script src=" not in content
            assert "Plotly.newPlot" in content
