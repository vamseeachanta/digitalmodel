# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Catalogue coverage regressions using disposable, synthetic source trees."""

import importlib.util
from pathlib import Path


SOURCE = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/solvers/orcaflex/model_catalogue.py"
)


def load_module():
    """Load by path so that no licensed solver package is initialized."""
    spec = importlib.util.spec_from_file_location("catalogue_coverage", SOURCE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def make_roots(root, module):
    """Create the three roots named by the catalogue's source contract."""
    domain = root / "docs/domains/orcaflex"
    for relative in module.ROOTS:
        (domain / relative).mkdir(parents=True)
    return domain


def test_empty_template_in_populated_category_is_reported(tmp_path):
    """Two template folders: one card plus one omission accounts for both."""
    module = load_module()
    domain = make_roots(tmp_path, module)
    category = domain / "templates/risers"
    populated = category / "synthetic_present"
    empty = category / "synthetic_empty"
    populated.mkdir(parents=True)
    empty.mkdir()
    (populated / "spec.yml").write_text("General: {}\n", encoding="utf-8")
    omissions = []
    groups = module.source_groups(domain)
    entries = list(module.catalogue_entries(tmp_path, groups, omissions))
    assert len(entries) == 1
    assert groups == sorted(groups)
    assert (
        "docs/domains/orcaflex/templates/risers/synthetic_empty: no YAML files"
        in omissions
    )
    assert not any(
        item.endswith("templates/risers: no YAML files") for item in omissions
    )


def test_missing_roots_are_listed_instead_of_aborting(tmp_path):
    """A partial source tree retains its model and accounts for absent roots."""
    module = load_module()
    domain = tmp_path / "docs/domains/orcaflex"
    model = domain / "library/model_library/synthetic_present"
    model.mkdir(parents=True)
    (model / "spec.yml").write_text("General: {}\n", encoding="utf-8")
    omissions = []
    entries = list(
        module.catalogue_entries(tmp_path, module.source_groups(domain), omissions)
    )
    assert len(entries) == 1
    assert omissions == [
        "docs/domains/orcaflex/library/templates: catalogue root not present",
        "docs/domains/orcaflex/templates: catalogue root not present",
    ]


def test_structural_folders_are_not_empty_template_models(tmp_path):
    """Includes/inputs belong to the template, not to the template inventory."""
    module = load_module()
    domain = make_roots(tmp_path, module)
    template = domain / "templates/synthetic_present"
    (template / "includes").mkdir(parents=True)
    (template / "inputs").mkdir()
    (template / "spec.yml").write_text("General: {}\n", encoding="utf-8")
    groups = module.source_groups(domain)
    assert [group for group, _ in groups] == [template]


def test_partial_tree_build_is_deterministic_and_accounts_for_every_model(tmp_path):
    """Hand count: three YAML models produce three cards and two missing roots.

    Synthetic coordinates A=(2,3,4), B=(5,7,11) project directly to
    plan (2,3)->(5,7) and elevation (2,4)->(5,11); no solver shape is claimed.
    These are selected test inputs, not measured data or acceptance limits.
    """
    module = load_module()
    domain = tmp_path / "docs/domains/orcaflex"
    models = domain / "library/model_library"
    texts = {
        "synthetic_chord": (
            "Lines: [{EndAConnection: Fixed, EndBConnection: Free, "
            "EndAPosition: [2, 3, 4], EndBPosition: [5, 7, 11]}]\n"
        ),
        "synthetic_spec": "metadata: {structure: riser}\n",
        "synthetic_include": "IncludeFile: [synthetic.yml]\n",
    }
    for name, text in texts.items():
        model = models / name
        model.mkdir(parents=True)
        (model / "spec.yml").write_text(text, encoding="utf-8")
    (domain / "MODEL_CLAIM_REGISTRY.yaml").write_text("models: []\n", encoding="utf-8")
    (domain / "MODEL_CLAIM_INVENTORY.yaml").write_text(
        "inventory: []\n", encoding="utf-8"
    )
    first = module.build(tmp_path)
    assert first == module.build(tmp_path)
    assert first.count("<article>") == 3
    assert first.count("catalogue root not present") == 2
    assert "Water depth: not stated" in first
    assert "IncludeFile: 1" in first
    assert 'x1="2.000" y1="3.000" x2="5.000" y2="7.000"' in first
    assert 'x1="2.000" y1="4.000" x2="5.000" y2="11.000"' in first
    assert "synthetic.yml" not in first
    assert str(tmp_path) not in first
