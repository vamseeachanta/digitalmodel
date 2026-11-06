# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inventory regressions for the supplemental engineering families.

Expected symbols are read independently from the named source definitions,
not discovered by the inventory generator. No solver or calculator is imported.
"""

import copy
import hashlib
import importlib.util
from pathlib import Path

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "family_map_generator", ROOT / "scripts/generate_implementation_map.py"
)
GENERATOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(GENERATOR)

MEMBERS = {
    "second-order-slow-drift-response": (
        ("orcawave/drift_forces.py", "compute_mean_drift_force"),
        ("orcawave/drift_forces.py", "newman_approximation"),
        ("orcawave/drift_forces.py", "full_qtf_slowly_varying"),
        ("hydrodynamics/bemrosetta/models/qtf_data.py", "QTFComponent.get_mean_drift"),
    ),
    "extreme-statistics": (
        ("orcaflex/postprocessor.py", "fit_weibull"),
        ("orcaflex/postprocessor.py", "fit_gumbel"),
        ("hydrodynamics/river_currents/extremes.py", "WeibullFit.return_level"),
        ("hydrodynamics/river_currents/extremes.py", "GumbelFit.return_level"),
        ("subsea/pipeline/free_span/weibull_current.py", "fit_weibull_current"),
    ),
    "design-waves": (
        ("solvers/openfoam/wave_models.py", "RegularWaveBC.to_dict"),
        ("solvers/orcaflex/modular_generator/builders/environment_builder.py",
         "EnvironmentBuilder._build_wave_trains"),
        ("hydrodynamics/planing_hull/solver.py", "PlaningMotionSolver._wave_surface"),
    ),
    "sloshing": (
        ("hydrodynamics/sloshing.py", "rectangular_tank_periods"),
        ("hydrodynamics/sloshing.py", "cylindrical_tank_periods"),
        ("hydrodynamics/sloshing.py", "screen_tank"),
        ("hydrodynamics/sloshing.py", "router"),
    ),
    "geotechnics": (
        ("geotechnical/piles.py", "skin_friction_clay"),
        ("geotechnical/pile_capacity.py", "alpha_method_capacity"),
        ("geotechnical/anchors.py", "suction_anchor_capacity"),
        ("geotechnical/mudmat.py", "mudmat_bearing_capacity"),
        ("geotechnical/liquefaction.py", "assess_liquefaction"),
    ),
    "scour": (
        ("geotechnical/scour.py", "critical_sediment_motion"),
        ("geotechnical/scour.py", "pipeline_scour_depth"),
        ("geotechnical/scour.py", "monopile_scour_depth"),
        ("geotechnical/scour.py", "rock_armour_thickness"),
        ("geotechnical/cfg_workflows.py", "ScourWorkflow.router"),
    ),
}


@pytest.fixture(scope="module")
def document():
    return yaml.safe_load((ROOT / "docs/registry/implementation-map.yaml").read_text())


@pytest.mark.parametrize("family_id", MEMBERS)
def test_supplemental_family_is_required(document, family_id):
    doc = copy.deepcopy(document)
    doc["families"] = [f for f in doc["families"] if f["id"] != family_id]
    assert f"missing or empty family: {family_id}" in GENERATOR.check_map(doc, ROOT)


@pytest.mark.parametrize("family_id,path,symbol", [
    (family, path, symbol)
    for family, members in MEMBERS.items() for path, symbol in members
])
def test_reviewed_member_is_mapped(document, family_id, path, symbol):
    family = next(f for f in document["families"] if f["id"] == family_id)
    assert any(e["path"] == "src/digitalmodel/" + path and e["symbol"] == symbol
               for e in family["implementations"])


@pytest.mark.parametrize("family_id", MEMBERS)
def test_supplement_has_current_evidence_and_bounded_claims(document, family_id):
    family = next(f for f in document["families"] if f["id"] == family_id)
    assert family["coverage"] == "partial"
    assert family["inventory_scope"]["revision"]
    assert family["inventory_scope"]["method_note"] == (
        "docs/registry/implementation-map-families.md"
    )
    assert family["limitations"]
    identities = [(e["path"], e["symbol"]) for e in family["implementations"]]
    assert len(identities) == len(set(identities))
    for entry in family["implementations"]:
        source = (ROOT / entry["path"]).read_bytes()
        node = GENERATOR.definitions(source.decode())[entry["symbol"]]
        assert entry["line"] == GENERATOR.start_line(node)
        assert entry["file_sha256"] == hashlib.sha256(source).hexdigest()
        assert entry["body_sha256"] == GENERATOR.body_digest(node)
        assert entry["what"] and entry["relation"]


def test_scour_lookup_worked_example(document):
    """Hand count from scour.py and cfg_workflows.py: four calculations
    plus one configuration delegate gives five callable members. Result
    records are data models and do not add independent calculations.
    """
    family = next(f for f in document["families"] if f["id"] == "scour")
    calculations = {e["symbol"] for e in family["implementations"]
                    if e["relation"] == "independent"}
    delegates = {e["symbol"] for e in family["implementations"]
                 if e["relation"] == "delegate"}
    assert calculations == {
        "critical_sediment_motion", "pipeline_scour_depth",
        "monopile_scour_depth", "rock_armour_thickness",
    }
    assert delegates == {"ScourWorkflow.router"}


def test_related_defects_have_inspection_evidence(document):
    by_issue = {row["issue"]: row for row in document["other_defects"]}
    for number in (2617, 2618):
        assert by_issue[number]["status"] == "not_checked"
        assert by_issue[number]["evidence"]
        assert by_issue[number]["what"]


def test_scoped_reconciliation_does_not_expand_inventory(tmp_path):
    """Only one of two hand-written matching definitions is in scope.

    The reviewed file has one unresolved name; the other file must not
    contribute either a search hit or an unresolved candidate.
    """
    texts = {"reviewed.py": "def scour_depth(): pass\n",
             "unreviewed.py": "def scour_depth(): pass\n"}
    for path, text in texts.items():
        (tmp_path / path).write_text(text)
    family = {
        "id": "scour", "implementations": [],
        "inventory_scope": {"source_paths": ["reviewed.py"]},
        "search_signals": [{"search": "algorithm", "pattern": "scour"}],
    }
    doc = {"families": [family], "rejected_candidates": []}
    GENERATOR.reconcile(doc, texts, GENERATOR.Sources(tmp_path))
    assert family["search_audit"] == [{"search": "algorithm", "matching_files": 1}]
    assert family["reconciliation"] == {
        "matched_definitions": 1, "unresolved_definitions": 1,
    }
    assert [row["path"] for row in doc["rejected_candidates"]] == ["reviewed.py"]


def test_reconciled_scoped_names_still_have_partial_coverage(tmp_path):
    """Accounting for a known name cannot prove coverage beyond its file."""
    (tmp_path / "reviewed.py").write_text("def scour_depth(): pass\n")
    family = {
        "id": "scour", "implementations": [
            {"path": "reviewed.py", "symbol": "scour_depth"}],
        "inventory_scope": {"source_paths": ["reviewed.py"]},
        "search_signals": [{"search": "algorithm", "pattern": "scour"}],
    }
    doc = {"families": [family], "rejected_candidates": []}
    GENERATOR.reconcile(doc, {"reviewed.py": "def scour_depth(): pass\n"},
                        GENERATOR.Sources(tmp_path))
    assert family["reconciliation"]["unresolved_definitions"] == 0
    assert family["coverage"] == "partial"


def test_regeneration_refreshes_scope_revision_without_mutating_input(tmp_path):
    """A regenerated supplement describes the new revision, retaining its files."""
    (tmp_path / "reviewed.py").write_text("def scour_depth(): pass\n")
    family = {
        "id": "scour", "implementations": [
            {"path": "reviewed.py", "symbol": "scour_depth", "line": 1}],
        "inventory_scope": {"source_paths": ["reviewed.py"], "revision": "a" * 40},
        "search_signals": [{"search": "algorithm", "pattern": "scour"}],
    }
    doc = {"families": [family], "rejected_candidates": [], "searched": {},
           "seed_verification": []}
    refreshed = GENERATOR.regenerate(doc, tmp_path, tracked_paths=["reviewed.py"],
                                     revision="b" * 40)
    scope = refreshed["families"][0]["inventory_scope"]
    assert scope == {"source_paths": ["reviewed.py"], "revision": "b" * 40}
    assert family["inventory_scope"]["revision"] == "a" * 40
    assert refreshed["families"][0]["coverage"] == "partial"
