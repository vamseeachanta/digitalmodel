# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent, solver-free catalogue reference and master-count checks."""

import importlib.util
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "src/digitalmodel/solvers/orcaflex/model_catalogue.py"


def load_module():
    """Load the generator by path without entering the solver initializer."""
    spec = importlib.util.spec_from_file_location("catalogue_references", SOURCE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("separator", ["/", "\\"])
def test_same_named_unrelated_folders_are_not_references(tmp_path, separator):
    """Only the model's parent-and-name path receives reference credit."""
    module = load_module()
    owner = "docs/domains/orcaflex/library/templates/geometry"
    model = tmp_path / owner / "model.yml"
    model.parent.mkdir(parents=True)
    model.write_text("General: {}\n", encoding="utf-8")
    texts = {
        "reference.py": "templates/geometry/model.yml",
        "source.py": "src/package/geometry/engine.py",
        "test.py": "tests/geometry/test_model.py",
        "benchmark.py": "docs/benchmarks/geometry/qualification.yml",
        "parent_prefix.py": "other_templates/geometry/model.yml",
        "name_suffix.py": "templates/geometry_extra/model.yml",
    }
    tests = tmp_path / "tests"
    tests.mkdir()
    for filename, path in texts.items():
        (tmp_path / path).parent.mkdir(parents=True, exist_ok=True)
        (tests / filename).write_text(path.replace("/", separator), encoding="utf-8")
    usage = module.find_usage(tmp_path, [(model.parent, [model])])
    assert usage[owner] == ["tests/reference.py"]


@pytest.fixture(scope="module")
def catalogue_data():
    """Read every card and its reference list once through the generator."""
    module = load_module()
    groups = module.source_groups(ROOT / "docs/domains/orcaflex")
    omissions = []
    entries = list(module.catalogue_entries(ROOT, groups, omissions))
    return module, entries, module.find_usage(ROOT, groups)


@pytest.mark.parametrize(
    ("name", "unrelated"),
    [
        (
            "drilling_riser",
            [
                "tests/DOMAINS.md",
                "tests/contracts/test_force_units_boundary_contract.py",
            ],
        ),
        ("mooring_buoy", ["tests/solvers/smoke/test_native_model_harness.py"]),
    ],
)
def test_reviewed_cards_exclude_unrelated_references(catalogue_data, name, unrelated):
    """The reviewed cards exclude known mentions of other owners."""
    module, entries, usage = catalogue_data
    path = f"docs/domains/orcaflex/library/templates/{name}/spec.yml"
    assert path in [entry[0] for entry in entries]
    owner = module.model_path(path)
    references = usage[owner]
    for reference in unrelated:
        assert reference not in references


def test_real_references_have_path_context(catalogue_data):
    """Each card's references independently contain its parent's model component."""
    module, entries, usage = catalogue_data
    for path, _, _ in entries:
        owner = module.model_path(path)
        pair = owner.split("/")[-2:]
        for reference in usage[owner]:
            text = (ROOT / reference).read_text(encoding="utf-8-sig", errors="replace")
            text = text.replace("\\", "/")
            tokens = "".join(
                c if c.isalnum() or c in "_-./" else " " for c in text
            ).split()
            components = [token.strip("/").split("/") for token in tokens]
            assert any(
                parts[i : i + len(pair)] == pair
                for parts in components
                for i in range(len(parts))
            ), (owner, reference)


def test_master_failure_count_includes_non_modular_sources():
    """Unreadable master files count wherever they occur; other files do not."""
    page = load_module().render_catalogue(
        [
            ("synthetic/first/modular/master.yml", "General: [", "generic"),
            ("synthetic/second/includes/master.yml", "General: [", "generic"),
            ("synthetic/third/master.yml", "General: {}", "generic"),
            ("synthetic/fourth/model.yml", "General: [", "generic"),
        ]
    )
    assert "master-file form: 2" in page
    assert "all master.yml files" in page
