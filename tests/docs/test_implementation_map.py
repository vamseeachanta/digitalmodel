# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate live references and test reproducible snapshot diagnostics."""

import ast
import copy
import hashlib
import importlib.util
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
MAP_PATH = ROOT / "docs/registry/implementation-map.yaml"
_SPEC = importlib.util.spec_from_file_location(
    "implementation_map_generator", ROOT / "scripts/generate_implementation_map.py")
_MODULE = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_MODULE)
FAMILY_IDS = _MODULE.FAMILY_IDS
REQUIRED_KEYS = _MODULE.REQUIRED_KEYS
check_map = _MODULE.check_map


@pytest.fixture(scope="module")
def map_document():
    return yaml.safe_load(MAP_PATH.read_text(encoding="utf-8"))


def test_implementation_map_references(map_document):
    assert check_map(map_document, ROOT) == []
    stale = _MODULE.map_staleness(map_document, ROOT)
    if stale:
        import logging
        logging.getLogger(__name__).warning(
            "Implementation map snapshot has %s staleness diagnostics; run the generator to inspect drift.", len(stale))


def test_missing_path_is_reported(map_document):
    doc = copy.deepcopy(map_document)
    missing = "src/implementation_map_missing_probe.py"
    doc["families"][0]["implementations"][0]["path"] = missing
    problems = check_map(doc, ROOT)
    assert any(missing in problem for problem in problems), problems


# Mutation fixtures keep checkout-wide validation out of the negative tests.


@pytest.fixture
def sample(tmp_path):
    source = tmp_path / "sample.py"
    source.write_text("class First:\n    def calculate(self):\n        return 1\n\nclass Second:\n    def calculate(self):\n        return 2\n")
    entry = {"path": "sample.py", "symbol": "First.calculate", "line": 2,
             "file_sha256": hashlib.sha256(source.read_bytes()).hexdigest()}
    doc = {key: [] for key in REQUIRED_KEYS}
    doc.update(schema_version=1, repository="sample", revision="a" * 40,
               searched={}, notes=[], families=[
                   {"id": name, "implementations": [copy.deepcopy(entry)],
                    "search_signals": [{"search": "algorithm", "pattern": "calculate"}]}
                   for name in dict.fromkeys((*FAMILY_IDS, "linear-wave-kinematics"))
               ], known_defects=[{"number": n} for n in (2050, 2107, 2543)])
    return doc, tmp_path


def generator():
    return _MODULE


def test_shifted_line_is_staleness_not_reference_failure(sample):
    doc, root = sample
    doc["families"][0]["implementations"][0]["line"] = 999
    assert check_map(doc, root) == []
    assert any("line drift" in p for p in generator().map_staleness(doc, root))


def test_same_named_method_does_not_satisfy_wrong_qualified_symbol(sample):
    doc, root = sample
    doc["families"][0]["implementations"][0]["symbol"] = "Missing.calculate"
    assert any("symbol" in p for p in check_map(doc, root))


def test_interior_line_is_reported_as_staleness(sample):
    doc, root = sample
    doc["families"][0]["implementations"][0]["line"] = 3
    assert any("line drift" in p for p in generator().map_staleness(doc, root))


def test_missing_linear_wave_family_is_reported(sample):
    doc, root = sample
    doc["families"] = [f for f in doc["families"] if f["id"] != "linear-wave-kinematics"]
    assert any("linear-wave-kinematics" in p for p in check_map(doc, root))


def test_missing_known_defect_is_reported(sample):
    doc, root = sample
    doc["known_defects"] = doc["known_defects"][1:]
    assert any("2050" in p for p in check_map(doc, root))


def test_file_and_group_digest_drift_is_reported(sample):
    doc, root = sample
    doc["families"][0]["implementations"][0]["file_sha256"] = "0" * 64
    doc["byte_identical_groups"] = [{"id": "copy", "sha256": "0" * 64,
                                     "paths": ["sample.py"]}]
    doc["identical_body_groups"] = [{"id": "body", "sha256": "0" * 64,
                                    "members": [doc["families"][0]["implementations"][0]]}]
    assert check_map(doc, root) == []
    stale = generator().map_staleness(doc, root)
    assert all(any(kind in p for p in stale) for kind in
               ("file digest drift", "byte group drift", "body group drift"))


def test_generator_reconciles_named_definitions_not_whole_file_rejections(sample):
    doc, root = sample
    doc["rejected_candidates"] = [{"path": "sample.py", "symbol": None,
                                     "families": [FAMILY_IDS[0]], "reason": "prose"}]
    module = generator()
    result = module.regenerate(doc, root, tracked_paths=["sample.py"], revision="b" * 40)
    family = result["families"][0]
    assert family["coverage"] == "partial"
    rejected = [r for r in result["rejected_candidates"] if r.get("symbol") == "Second.calculate"]
    assert rejected and FAMILY_IDS[0] in rejected[0]["families"]
    assert rejected[0]["disposition"] == "needs_review"
    assert family["reconciliation"]["unresolved_definitions"] == 1
    assert result["revision"] == "b" * 40
    assert result["searched"]["tracked_files"] == 1
    assert result["searched"]["text_files_searched"] == 1
    assert result == module.regenerate(result, root, tracked_paths=["sample.py"], revision="b" * 40)


@pytest.mark.parametrize("family,path,symbol", [
    ("dnv-rp-e305-on-bottom-stability", "subsea/on_bottom_stability/dnv_rp_f109.py", "generalized_stability_check"),
    ("wind-and-current-force-routes", "subsea/on_bottom_stability/dnv_rp_f109.py", "hydrodynamic_force_per_meter"),
    ("dnv-rp-e305-on-bottom-stability", "subsea/on_bottom_stability/simplified.py", "submerged_weight"),
    ("sn-curves-and-miner-damage", "structural/fatigue/sn_curves.py", "BilinearSNCurve.get_stress_range"),
    ("sn-curves-and-miner-damage", "subsea/pipeline/free_span/_bilinear_sn.py", "BilinearSNCurve.get_allowable_cycles"),
    ("catenary-mooring-line-statics", "subsea/catenary_riser/simple_catenary.py", "SimpleCatenaryAnalyzer.calculate_tension_along_riser"),
    ("catenary-mooring-line-statics", "marine_ops/marine_analysis/catenary/simplified.py", "SimplifiedCatenarySolver.calculate_forces"),
    ("wave-spectra", "fatigue/fatg.py", "fatg_significant_stress_range"),
    ("linear-wave-kinematics", "hydrodynamics/planing_hull/solver.py", "PlaningMotionSolver._wave_number"),
    ("linear-wave-kinematics", "solvers/openfoam/hull_free_surface.py", "deep_water_wavelength"),
    ("linear-wave-kinematics", "solvers/openfoam/validation/wave_excited_body_rao.py", "group_velocity"),
])
def test_reviewed_family_members_are_mapped(family, path, symbol, map_document):
    doc = map_document
    entries = next(f for f in doc["families"] if f["id"] == family)["implementations"]
    assert any(e["path"] == "src/digitalmodel/" + path and e["symbol"] == symbol for e in entries)


def test_generator_normalizes_recorded_escaped_patterns(sample):
    doc, root = sample
    doc["families"][0]["search_signals"][0]["pattern"] = r"\\bcalculate\\b"
    result = generator().regenerate(doc, root, tracked_paths=["sample.py"], revision="b" * 40)
    assert result["families"][0]["reconciliation"]["matched_definitions"] == 2


def test_generator_preserves_unchanged_yaml_bytes():
    text = "schema_version: 1\nnotes: ['a note']\nfamilies: []\n"
    assert generator().render_map(text, yaml.safe_load(text)) == text


def test_path_escape_is_rejected(sample):
    doc, root = sample
    doc["families"][0]["implementations"][0]["path"] = "../sample.py"
    assert any("path" in p for p in check_map(doc, root))


def test_symlink_escape_is_rejected(sample, tmp_path_factory):
    doc, root = sample
    outside = tmp_path_factory.mktemp("outside") / "source.py"
    outside.write_text("class First:\n    def calculate(self):\n        return 1\n")
    (root / "escaped.py").symlink_to(outside)
    doc["families"][0]["implementations"][0]["path"] = "escaped.py"
    assert any("path" in p for p in check_map(doc, root))


def test_regeneration_does_not_reuse_changed_scalar_anchors():
    text = "first: &ref old\nsecond: *ref\nthird: [*ref]\n"
    changed = {"first": "new", "second": "another", "third": ["old"]}
    assert yaml.safe_load(generator().render_map(text, changed)) == changed


def test_regeneration_does_not_mutate_other_alias_consumers():
    text = "first: &rows [one, two]\nsecond: *rows\n"
    changed = {"first": ["one", "changed"], "second": ["one", "two"]}
    assert yaml.safe_load(generator().render_map(text, changed)) == changed


def test_decorated_definition_accepts_definition_or_first_decorator(sample):
    doc, root = sample
    (root / "sample.py").write_text(
        "class First:\n    @staticmethod\n    def calculate():\n        return 1\n")
    for line in (2, 3):
        for family in doc["families"]:
            family["implementations"][0]["line"] = line
        assert check_map(doc, root) == []
        assert not any("line drift" in p for p in generator().map_staleness(doc, root))


def test_diverged_copy_warns_without_losing_symbol_validity(sample):
    doc, root = sample
    (root / "copy.py").write_bytes((root / "sample.py").read_bytes())
    entry = copy.deepcopy(doc["families"][0]["implementations"][0])
    entry["path"] = "copy.py"
    doc["families"][0]["implementations"].append(entry)
    result = generator().regenerate(doc, root, tracked_paths=["sample.py", "copy.py"], revision="b" * 40)
    assert generator().map_staleness(result, root) == []
    (root / "copy.py").write_text((root / "copy.py").read_text().replace("return 1", "return 9"))
    assert check_map(result, root) == []
    stale = generator().map_staleness(result, root)
    assert any("byte group drift" in p for p in stale)
    assert any("body group drift" in p for p in stale)


def test_reference_check_runs_in_ci_harness():
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    job = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in job
    assert any("tests/docs/test_implementation_map.py" in step.get("run", "") for step in job["steps"])
    assert any("tests/docs/test_implementation_map_search.py" in step.get("run", "") for step in job["steps"])


def test_git_reads_ignore_inherited_repository_bindings(tmp_path, monkeypatch):
    import os

    module = generator()
    expected = module.git_output(ROOT, "rev-parse", "HEAD")
    invalid = tmp_path / "invalid-repository"
    invalid.mkdir()
    names = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR")
    for name in names:
        monkeypatch.setenv(name, str(invalid))
    assert module.git_output(ROOT, "rev-parse", "HEAD") == expected
    assert all(os.environ[name] == str(invalid) for name in names)


def test_empty_seed_directory_is_reported(sample):
    doc, root = sample
    (root / "empty").mkdir()
    doc["seed_verification"] = [{"path": "empty", "exists": "yes"}]
    assert any("empty seed" in p for p in check_map(doc, root))


def test_moved_seed_requires_its_qualified_destination_symbol(sample):
    doc, root = sample
    doc["seed_verification"] = [{"path": "old.py", "exists": "no", "status": "moved",
                                 "moved_to": "sample.py", "symbol": "Absent.calculate"}]
    assert any("symbol" in p for p in check_map(doc, root))


def test_long_numeric_reports_have_bounded_search_cost(sample, map_document):
    import json
    import subprocess
    import sys

    doc, root = sample
    signal = next(f for f in map_document["families"] if f["id"] == "wind-and-current-force-routes")["search_signals"][0]
    doc["families"][0]["search_signals"] = [signal]
    (root / "numeric-report.txt").write_text("0.5 " * 200000)
    (root / "document.json").write_text(json.dumps(doc))
    program = """
import importlib.util, json, pathlib, sys
spec = importlib.util.spec_from_file_location('map_generator', sys.argv[1])
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
root = pathlib.Path(sys.argv[2])
doc = json.loads((root / 'document.json').read_text())
result = module.regenerate(doc, root, tracked_paths=['sample.py', 'numeric-report.txt'], revision='b' * 40)
assert result['searched']['text_files_searched'] == 2
assert result['families'][0]['search_audit'][0]['matching_files'] == 0
"""
    subprocess.run([sys.executable, "-c", program,
                    str(ROOT / "scripts/generate_implementation_map.py"), str(root)],
                   check=True, capture_output=True, text=True, timeout=30)


@pytest.mark.parametrize("path,symbol", [
    ("removed.py", "First.calculate"), ("sample.py", "Removed.calculate"),
])
def test_regeneration_drops_missing_curated_rejections(sample, path, symbol):
    doc, root = sample
    doc["rejected_candidates"] = [{
        "path": path, "symbol": symbol, "families": [FAMILY_IDS[0]],
        "reason": "Previously inspected candidate",
    }]
    result = generator().regenerate(doc, root, tracked_paths=["sample.py"], revision="b" * 40)
    assert check_map(result, root) == []
    assert not any(r["path"] == path and r.get("symbol") == symbol
                   for r in result["rejected_candidates"])


def test_mapped_definition_cannot_be_rejected_in_same_family(sample):
    doc, root = sample
    doc["rejected_candidates"] = [{
        "path": "sample.py", "symbol": "First.calculate",
        "families": [FAMILY_IDS[0]], "reason": "Presentation only",
    }]
    assert any("mapped and rejected" in p for p in check_map(doc, root))
    result = generator().regenerate(doc, root, tracked_paths=["sample.py"], revision="b" * 40)
    assert check_map(result, root) == []
    assert not any(r.get("symbol") == "First.calculate"
                   for r in result["rejected_candidates"])


def test_rejection_in_another_family_is_retained(sample):
    doc, root = sample
    doc["families"][1]["implementations"][0]["symbol"] = "Second.calculate"
    doc["rejected_candidates"] = [{
        "path": "sample.py", "symbol": "First.calculate",
        "families": [FAMILY_IDS[0], FAMILY_IDS[1]], "reason": "Presentation only",
    }]
    result = generator().regenerate(doc, root, tracked_paths=["sample.py"], revision="b" * 40)
    rejected = [r for r in result["rejected_candidates"]
                if r.get("symbol") == "First.calculate"]
    assert len(rejected) == 1
    assert rejected[0]["families"] == [FAMILY_IDS[1]]
    assert check_map(result, root) == []


def test_ci_harness_prints_staleness_diagnostics():
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    steps = workflow["jobs"]["ci-harness-tests"]["steps"]
    commands = [step.get("run", "") for step in steps]
    assert any("python scripts/generate_implementation_map.py" in command
               and "--write" not in command for command in commands)


def test_inventory_excludes_external_links_without_reading_them(sample, tmp_path_factory):
    doc, root = sample
    outside = tmp_path_factory.mktemp("external_inventory") / "source.py"
    outside.write_text("external source must not be searched")
    (root / "external.py").symlink_to(outside)
    texts, _, exclusions = generator().inventory(root, ["sample.py", "external.py"])
    assert set(texts) == {"sample.py"}
    assert exclusions["external_links"] == 1
    doc["families"][0]["implementations"][0]["path"] = "external.py"
    assert any("escaping path" in p for p in check_map(doc, root))


def test_registry_generator_has_a_reasoned_non_html_classification():
    import sys
    sys.path.insert(0, str(ROOT / "scripts"))
    try:
        import check_generated_html as checker
        path = "scripts/generate_implementation_map.py"
        assert path in checker.discover_candidate_generators(ROOT)
        assert checker.DISCOVERY_FALSE_POSITIVES.get(path, "").strip()
        assert not any(path in problem for problem in checker.validate_registry(ROOT))
    finally:
        sys.path.remove(str(ROOT / "scripts"))


@pytest.fixture(scope="module")
def reel_lay_family(map_document):
    """Select the scoped reel inventory required by the reconciliation issue."""
    family = next((f for f in map_document["families"]
                   if f["id"] == "reel-lay-residual-curvature"), None)
    assert family is not None, "Missing reel-lay residual-curvature family"
    return family


def test_reel_lay_inventory_has_exact_scoped_members(reel_lay_family):
    """Expected names come from inspection of the workflow's reel branch."""
    path = "src/digitalmodel/pipelay/workflow.py"
    identities = [(row["path"], row["symbol"])
                  for row in reel_lay_family["implementations"]]
    assert sorted(identities) == sorted([
        (path, "run_reel_lay"), (path, "_reel_result"),
    ])


@pytest.mark.parametrize("symbol,relation", [
    ("run_reel_lay", "caller"), ("_reel_result", "independent"),
])
def test_reel_lay_recorded_symbols_resolve(reel_lay_family, symbol, relation):
    """Resolve exact lexical definitions with AST, without importing a solver."""
    row = next(r for r in reel_lay_family["implementations"] if r["symbol"] == symbol)
    assert row["relation"] == relation
    for reference, _ in _MODULE.records(row):
        definitions = _MODULE.definitions((ROOT / reference["path"]).read_text())
        assert isinstance(definitions[reference["symbol"]], ast.FunctionDef)


def test_reel_lay_delegation_matches_source(reel_lay_family):
    """The reviewed wrapper directly calls the separately recorded calculation."""
    rows = {row["symbol"]: row for row in reel_lay_family["implementations"]}
    caller, calculation = rows["run_reel_lay"], rows["_reel_result"]
    assert caller["delegates_to"] == [{
        "path": calculation["path"], "symbol": calculation["symbol"],
    }]
    definitions = _MODULE.definitions((ROOT / caller["path"]).read_text())
    calls = {node.func.id for node in ast.walk(definitions[caller["symbol"]])
             if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)}
    assert calculation["symbol"] in calls
    assert not calculation.get("delegates_to")


def test_reel_lay_inventory_does_not_claim_qualification(reel_lay_family):
    """Classification cannot substitute for numerical or native qualification."""
    assert reel_lay_family["classification_scope"] == "reel_workflow_only"
    assert reel_lay_family["qualification"] == "classification_only"
    assert reel_lay_family["numerical_qualification"] == "NOT_EVALUATED"
    assert reel_lay_family["native_solver_qualification"] == "NOT_EVALUATED"
    assert reel_lay_family["source_revision"]
    for deferred in ("operability", "stinger", "J-lay"):
        assert deferred in reel_lay_family["classification_limits"]


@pytest.mark.parametrize("disposition", ["missing", "empty"])
def test_missing_or_empty_reel_lay_family_is_reported(sample, disposition):
    doc, root = sample
    family_id = "reel-lay-residual-curvature"
    doc["families"] = [f for f in doc["families"] if f["id"] != family_id]
    if disposition == "empty":
        doc["families"].append({"id": family_id, "implementations": []})
    assert any(family_id in problem for problem in check_map(doc, root))


def test_reel_lay_classification_limits_survive_regeneration(sample, reel_lay_family):
    """Name reconciliation shall retain the inspected classification boundary."""
    doc, _ = sample
    doc["families"] = [copy.deepcopy(reel_lay_family)]
    path = "src/digitalmodel/pipelay/workflow.py"
    result = generator().regenerate(doc, ROOT, tracked_paths=[path], revision="b" * 40)
    family = result["families"][0]
    assert family["coverage"] == "reconciled_names"
    assert family["reconciliation"] == {
        "matched_definitions": 2, "unresolved_definitions": 0,
    }
    assert family["classification_limits"] == reel_lay_family["classification_limits"]
    assert family["source_revision"] == reel_lay_family["source_revision"]
    test_reel_lay_inventory_does_not_claim_qualification(family)
