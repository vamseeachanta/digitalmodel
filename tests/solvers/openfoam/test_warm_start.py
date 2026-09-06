import json
from pathlib import Path

import yaml

from digitalmodel.solvers.openfoam.warm_start.decision import decide
from digitalmodel.solvers.openfoam.warm_start.admissibility import GateCheck, GateVerdict, evaluate
from digitalmodel.solvers.openfoam.warm_start.cli import main, parser
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


def test_a3_ignores_foam_header_comments_and_formatting(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    for case in (source, target):
        (case / "system").mkdir(parents=True)
    source_text = """FoamFile { version 2.0; object fvSchemes; } // generated
ddtSchemes { default Euler; } /* note */
gradSchemes { default Gauss linear; }
"""
    target_text = """FoamFile
{
 version 2.1;
 object fvSchemes;
}
ddtSchemes{default Euler;} gradSchemes { default Gauss linear; }
"""
    for name in ("fvSchemes", "fvSolution"):
        (source / "system" / name).write_text(source_text)
        (target / "system" / name).write_text(target_text)
    check = _check(evaluate(source, target, "speed"), "A3")
    assert check.passed, check.detail


def test_a3_reports_first_differing_entry(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    for case, solver in ((source, "smoothSolver"), (target, "PBiCGStab")):
        (case / "system").mkdir(parents=True)
        (case / "system" / "fvSchemes").write_text("ddtSchemes { default Euler; }\n")
        (case / "system" / "fvSolution").write_text(
            f"solvers {{ p_rgh {{ solver {solver}; tolerance 1e-7; }} }}\n"
        )
    check = _check(evaluate(source, target, "speed"), "A3")
    assert not check.passed
    assert "p_rgh.solver" in check.detail


def test_a1_accepts_fit_and_latest_cycle_mean_within_two_percent(tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    (source / "log.solver").write_text("End\n")
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.warm_start.admissibility.audit_one",
        lambda *args: {"verdict": "oscillating", "fit_total": 101.0, "cycle_total": 100.0},
    )
    check = _check(evaluate(source, target, "speed"), "A1")
    assert check.passed
    assert "fit-vs-cycle" in check.detail


def test_source_settled_override_is_explicit_in_plan_marker_and_ledger(tmp_path: Path, monkeypatch, capsys):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    record = tmp_path / "records"; record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    ledger = tmp_path / "warm_start.tsv"
    passing = GateVerdict(tuple(GateCheck(identifier, True, "ok") for identifier in
                                ("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A9")))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate", lambda *a, **k: passing)
    source_time = source / "20"; source_time.mkdir()
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.clean_restart", lambda *a: None)
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.rewrite_speed_fields", lambda *a: None)
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.reset_control", lambda *a: None)
    rc = main([
        "prepare", "--target", str(target), "--from", "case", "--hop", "speed",
        "--source", str(source), "--record", str(record), "--ledger", str(ledger),
        "--source-settled-override", "operator reviewed force history", "--calibrate",
    ])
    assert rc == 0
    output = capsys.readouterr().out
    assert "A1 OVERRIDDEN" in output
    assert "operator reviewed force history" in output
    assert "A1 OVERRIDDEN" in (target / "WARM_PLANNED").read_text()
    assert "operator reviewed force history" in ledger.read_text()


def test_source_settled_override_requires_calibration_to_proceed(tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    record = tmp_path / "records"; record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    passing = GateVerdict(tuple(GateCheck(identifier, True, "ok") for identifier in
                                ("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A9")))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate", lambda *a, **k: passing)
    rc = main([
        "prepare", "--target", str(target), "--from", "case", "--hop", "speed",
        "--source", str(source), "--record", str(record),
        "--source-settled-override", "manual review",
    ])
    assert rc != 0
    assert not (target / "WARM_PLANNED").exists()
