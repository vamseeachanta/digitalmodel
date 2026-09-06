import json
from pathlib import Path

import yaml

from digitalmodel.solvers.openfoam.warm_start.decision import decide
from digitalmodel.solvers.openfoam.warm_start.admissibility import evaluate
from digitalmodel.solvers.openfoam.warm_start.cli import parser
from digitalmodel.solvers.openfoam.warm_start.fields import clean_restart, rewrite_speed_fields
from digitalmodel.solvers.openfoam.warm_start.record import RecordStore


FIELD = """FoamFile { format ascii; class volVectorField; object U; }
internalField uniform (-1 0 0);
boundaryField
{
 inlet { type fixedValue; value uniform (-1 0 0); }
 outlet { type outletPhaseMeanVelocity; Umean 1; value uniform (-1 0 0); }
}
"""


def test_beta_prior_refuses_first_geometry_hop_and_calibration_allows_it():
    cold = decide("geometry", 5000, 400, [], margin_fraction=.1)
    assert cold.decision == "COLD_BY_EV" and cold.ev == 200
    warm = decide("geometry", 5000, 400, [], calibrate=True)
    assert warm.decision == "WARM_CALIBRATION"


def test_copy_cleanup_and_ascii_boundary_rewrite(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    for name in ("alpha.water", "U", "p_rgh", "k", "omega", "nut", "phi", "p"):
        (source / "20").mkdir(parents=True, exist_ok=True)
        (source / "20" / name).write_text(FIELD)
    (source / "20" / "uniform").mkdir()
    (target / "0.orig").mkdir(parents=True)
    for name in ("alpha.water", "U", "p_rgh", "k", "omega", "nut"):
        (target / "0.orig" / name).write_text(FIELD.replace("-1", "-2").replace("Umean 1", "Umean 2"))
    clean_restart(source / "20", target)
    rewrite_speed_fields(target, dry_run=True)
    assert "uniform (-2 0 0)" in (target / "0" / "U").read_text()
    assert "Umean 2" in (target / "0" / "U").read_text()
    assert not (target / "0" / "phi").exists()
    assert not (target / "0" / "uniform").exists()


def test_record_update(tmp_path: Path):
    store = RecordStore(tmp_path, "speed", "L1", 5000)
    store.append({"id": "x", "decision": "WARM", "outcome": "WARM_OK", "iterations": 2000})
    data = yaml.safe_load(store.path.read_text())
    assert data["summary"]["successes"] == 1
    assert data["summary"]["p_posterior"] == .6


def _check(verdict, identifier):
    return next(check for check in verdict.checks if check.identifier == identifier)


def _owner(case: Path, cells: int) -> None:
    mesh = case / "constant" / "polyMesh"
    mesh.mkdir(parents=True, exist_ok=True)
    (mesh / "owner").write_text(f'note "nCells:{cells}";\n')


def test_a7_infers_legacy_level_from_finest_cells_per_wavelength(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    (source / "case_provenance.json").write_text(json.dumps({
        "refinement": {"levels": [20, 40, 80]}, "speed": 1,
    }))
    (target / "case_provenance.json").write_text(json.dumps({"speed": 1}))
    verdict = evaluate(source, target, "speed", level="r3")
    check = _check(verdict, "A7")
    assert check.passed
    assert "80-class" in check.detail


def test_a7_infers_matching_class_from_mesh_cell_count(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    (source / "case_provenance.json").write_text('{"speed": 1}')
    (target / "case_provenance.json").write_text('{"speed": 1, "mesh_level": "40"}')
    _owner(source, 950); _owner(target, 1000)
    check = _check(evaluate(source, target, "speed", level="40"), "A7")
    assert check.passed
    assert "within 10%" in check.detail


def test_source_mesh_level_override_is_accepted_and_controls_a7(tmp_path: Path):
    args = parser().parse_args([
        "plan", "--target", str(tmp_path / "target"), "--from", "case",
        "--hop", "speed", "--source", str(tmp_path / "source"),
        "--mesh-level", "r3", "--source-mesh-level", "80",
    ])
    assert args.source_mesh_level == "80"


def test_dry_run_can_mark_missing_target_mesh_a6_pending(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir(); _owner(source, 1000)
    (source / "case_provenance.json").write_text('{"speed": 1, "mesh_level": "40"}')
    (target / "case_provenance.json").write_text('{"speed": 1, "mesh_level": "40"}')
    check = _check(evaluate(source, target, "speed", level="40", allow_pending_mesh=True), "A6")
    assert check.passed is None
    assert "target mesh is not staged" in check.detail
    assert "A6 PENDING" in evaluate(source, target, "speed", level="40", allow_pending_mesh=True).render()
