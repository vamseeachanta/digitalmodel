import json
import shutil
import subprocess
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.openfoam.warm_start.decision import decide
from digitalmodel.solvers.openfoam.warm_start.admissibility import GateCheck, GateVerdict, evaluate
from digitalmodel.solvers.openfoam.warm_start.cli import _latest, main, parser
from digitalmodel.solvers.openfoam.warm_start.checks import stop_and_fallback
from digitalmodel.solvers.openfoam.warm_start.fields import (
    ascii_write_format,
    clean_restart,
    restore_cold_restart,
    rewrite_speed_fields,
    verify_warm_fields,
)
from digitalmodel.solvers.openfoam.warm_start.record import RecordStore
from .test_force_cycle_average import write_force


FIELD = """FoamFile { format ascii; class volVectorField; object U; }
internalField uniform (-1 0 0);
boundaryField
{
 inlet { type fixedValue; value uniform (-1 0 0); }
 outlet { type outletPhaseMeanVelocity; Umean 1; value uniform (-1 0 0); }
}
"""

WARM_FIELD = """FoamFile { format ascii; class volVectorField; object U; }
internalField nonuniform List<vector>
2
(
(-1 0 0)
(-1.1 0 0)
);
boundaryField
{
 inlet { type fixedValue; value uniform (-1 0 0); }
 outlet { type zeroGradient; }
}
"""


def _literal_speed_templates(case: Path) -> None:
    zero = case / "0.orig"
    zero.mkdir(parents=True)
    (zero / "U").write_text("""FoamFile { version 2.0; format ascii; class volVectorField; object U; }
Umean 2.5;
mUmean -$Umean;
internalField uniform ($mUmean 0 0);
boundaryField
{
    inlet { type fixedValue; value $internalField; }
    outlet { type outletPhaseMeanVelocity; Umean $Umean; value $internalField; }
    vent { type inletOutlet; inletValue uniform (0 0 0); value $internalField; }
}
""")
    for name, value in (("k", "0.0375"), ("omega", "12.25")):
        (zero / name).write_text(f"""FoamFile {{ version 2.0; format ascii; class volScalarField; object {name}; }}
inletLevel {value};
internalField uniform $inletLevel;
boundaryField
{{
    inlet {{ type fixedValue; value uniform $inletLevel; }}
    outlet {{ type inletOutlet; inletValue uniform $inletLevel; value $internalField; }}
}}
""")


def test_change_dictionary_is_headered_literal_and_preserves_internal_field(tmp_path: Path):
    target = tmp_path / "target"
    _literal_speed_templates(target)
    (target / "0").mkdir()
    for name in ("U", "k", "omega"):
        shutil.copy2(target / "0.orig" / name, target / "0" / name)
    (target / "0" / "U").write_text(
        (target / "0" / "U").read_text().replace(
            "internalField uniform ($mUmean 0 0);", "internalField uniform (-1.75 0 0);"
        )
    )

    rewrite_speed_fields(target, dry_run=True)

    dictionary = (target / "system" / "changeDictionaryDict").read_text()
    assert "class dictionary;" in dictionary
    assert "object changeDictionaryDict;" in dictionary
    assert "dictionaryReplacement" not in dictionary
    assert "$internalField" not in dictionary
    assert "#includeEtc" not in dictionary
    assert "value uniform (-2.5 0 0);" in dictionary
    assert "Umean 2.5;" in dictionary
    assert "value uniform 0.0375;" in dictionary
    assert "value uniform 12.25;" in dictionary
    assert "inletValue uniform (0 0 0);" in dictionary
    assert "inletValue uniform 0.0375;" in dictionary
    assert "internalField" not in dictionary


def test_change_dictionary_v2312_applies_literals_and_preserves_warm_internal(tmp_path: Path):
    if not all(shutil.which(command) for command in
               ("changeDictionary", "foamDictionary", "blockMesh")):
        pytest.skip("OpenFOAM changeDictionary/foamDictionary/blockMesh are not on PATH")
    target = tmp_path / "target"
    _literal_speed_templates(target)
    (target / "0").mkdir()
    for name in ("U", "k", "omega"):
        shutil.copy2(target / "0.orig" / name, target / "0" / name)
    warm_internal = "internalField nonuniform List<vector> 1 ((-1.75 0 0));"
    (target / "0" / "U").write_text(
        (target / "0" / "U").read_text().replace(
            "internalField uniform ($mUmean 0 0);", warm_internal
        )
    )
    for name in ("k", "omega"):
        path = target / "0" / name
        path.write_text(path.read_text().replace(
            "internalField uniform $inletLevel;",
            "internalField nonuniform List<scalar> 1 (0.1);",
        ))
    for name in ("alpha.water", "p_rgh", "nut"):
        (target / "0" / name).write_text(
            WARM_FIELD.replace("object U", f"object {name}")
        )
    (target / "system").mkdir()
    (target / "system" / "controlDict").write_text("""FoamFile
{ version 2.0; format ascii; class dictionary; object controlDict; }
application interFoam;
startFrom startTime;
startTime 0;
stopAt endTime;
endTime 1;
deltaT 1;
writeControl timeStep;
writeInterval 1;
""")
    (target / "constant").mkdir()
    (target / "system" / "blockMeshDict").write_text("""FoamFile
{ version 2.0; format ascii; class dictionary; object blockMeshDict; }
scale 1;
vertices ((0 0 0) (1 0 0) (1 1 0) (0 1 0) (0 0 1) (1 0 1) (1 1 1) (0 1 1));
blocks (hex (0 1 2 3 4 5 6 7) (1 1 1) simpleGrading (1 1 1));
edges ();
boundary
(
 inlet { type patch; faces ((0 4 7 3)); }
 outlet { type patch; faces ((1 2 6 5)); }
 walls { type wall; faces ((0 1 5 4) (3 7 6 2) (0 3 2 1) (4 5 6 7)); }
);
""")
    (target / "system" / "fvSchemes").write_text("""FoamFile
{ version 2.0; format ascii; class dictionary; object fvSchemes; }
ddtSchemes { default Euler; }
gradSchemes { default Gauss linear; }
divSchemes { default none; }
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes { default corrected; }
""")
    (target / "system" / "fvSolution").write_text("""FoamFile
{ version 2.0; format ascii; class dictionary; object fvSolution; }
solvers {}
""")
    subprocess.run(["blockMesh"], cwd=target, check=True, capture_output=True, text=True)

    rewrite_speed_fields(target)

    checks = {
        ("U", "internalField"): "nonuniform List<vector> 1((-1.75 0 0))",
        ("U", "boundaryField.inlet.value"): "uniform (-2.5 0 0)",
        ("U", "boundaryField.outlet.Umean"): "2.5",
        ("U", "boundaryField.outlet.value"): "uniform (-2.5 0 0)",
        ("k", "boundaryField.inlet.value"): "uniform 0.0375",
        ("omega", "boundaryField.inlet.value"): "uniform 12.25",
    }
    for (field, entry), expected in checks.items():
        result = subprocess.run(
            ["foamDictionary", f"0/{field}", "-entry", entry, "-value"],
            cwd=target, check=True, capture_output=True, text=True,
        )
        actual = " ".join(result.stdout.split()).replace("( ", "(").replace(" )", ")")
        assert actual == expected


def test_beta_prior_refuses_first_geometry_hop_and_calibration_allows_it():
    cold = decide("geometry", 5000, 400, [], margin_fraction=.1)
    assert cold.decision == "COLD_BY_EV" and cold.ev == 200
    warm = decide("geometry", 5000, 400, [], calibrate=True)
    assert warm.decision == "WARM_CALIBRATION"


def test_calibration_guard_and_posterior_ignore_invalid_unattempted_entries():
    ignored = [
        {"decision": "WARM_CALIBRATION_INVALID_COPY", "outcome": None, "iterations": None},
        {"decision": "WARM_CALIBRATION", "outcome": "NOT_ATTEMPTED_TOOL", "iterations": None},
    ]
    result = decide("speed", 5000, 400, ignored, calibrate=True)
    assert result.decision == "WARM_CALIBRATION"
    assert result.successes == result.failures == 0
    with pytest.raises(ValueError, match="calibration already used"):
        decide("speed", 5000, 400, [
            {"decision": "WARM_CALIBRATION", "outcome": "WARM_ABORTED", "iterations": 400}
        ], calibrate=True)


def test_latest_reconstructs_newer_processor_time_and_never_implicitly_uses_zero(
        tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    (source / "0").mkdir(parents=True)
    (source / "processor0" / "5046").mkdir(parents=True)
    target.mkdir()
    calls = []
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.run_foam",
                        lambda command, cwd: calls.append((command, cwd)))

    assert _latest(source, target) == source / "5046"
    assert calls[0][0] == ["reconstructPar", "-time", "5046", "-fields",
                           "(alpha.water U p_rgh k omega nut)"]

    empty = tmp_path / "zero-only"
    (empty / "0").mkdir(parents=True)
    with pytest.raises(ValueError, match="--source-time 0"):
        _latest(empty, target)


def test_a5_checks_the_chosen_time_not_any_complete_serial_time(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    for field in ("alpha.water", "U", "p_rgh", "k", "omega", "nut"):
        path = source / "0" / field
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(FIELD)
    for rank in range(2):
        (source / f"processor{rank}" / "20").mkdir(parents=True)
    target.mkdir()

    latest = _check(evaluate(source, target, "speed", source_time="latestTime"), "A5")
    assert not latest.passed
    assert "time=20" in latest.detail and "reconstructed=False" in latest.detail
    explicit = _check(evaluate(source, target, "speed", source_time="0"), "A5")
    assert explicit.passed
    assert "time=0" in explicit.detail


def test_ascii_write_format_is_temporary_for_binary_control_dict(tmp_path: Path):
    control = tmp_path / "system" / "controlDict"
    control.parent.mkdir()
    original = "application interFoam;\nwriteFormat binary;\n"
    control.write_text(original)
    with ascii_write_format(tmp_path):
        assert "writeFormat ascii;" in control.read_text()
        assert "writeFormat binary;" not in control.read_text()
    assert control.read_text() == original


def test_restore_cold_restart_clears_stale_write_now(tmp_path: Path):
    (tmp_path / "0.cold").mkdir()
    (tmp_path / "0.cold" / "state").write_text("cold\n")
    (tmp_path / "0").mkdir()
    (tmp_path / "system").mkdir()
    (tmp_path / "system" / "controlDict").write_text("stopAt writeNow;\n")
    restore_cold_restart(tmp_path)
    assert (tmp_path / "0" / "state").read_text() == "cold\n"
    assert "stopAt endTime;" in (tmp_path / "system" / "controlDict").read_text()


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
    dictionary = (target / "system" / "changeDictionaryDict").read_text()
    assert "uniform (-2 0 0)" in dictionary
    assert "Umean 2" in dictionary
    assert not (target / "0" / "phi").exists()
    assert not (target / "0" / "uniform").exists()


def test_same_decomposition_copies_and_verifies_all_fields_per_rank(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    for rank in range(2):
        source_time = source / f"processor{rank}" / "20"
        target_zero = target / f"processor{rank}" / "0"
        source_time.mkdir(parents=True)
        target_zero.mkdir(parents=True)
        _owner(source / f"processor{rank}", 10 + rank)
        _owner(target / f"processor{rank}", 10 + rank)
        for name in ("alpha.water", "U", "p_rgh", "k", "omega", "nut"):
            (source_time / name).write_text(WARM_FIELD.replace("object U", f"object {name}"))
            (target_zero / name).write_text(FIELD.replace("object U", f"object {name}"))
    (source / "20").mkdir()
    (source / "20" / "p").write_text(FIELD)
    (target / "0").mkdir(parents=True)
    (target / "0" / "sentinel").write_text("serial zero must be untouched\n")

    layout = clean_restart(source / "20", target)

    assert layout == (target / "processor0" / "0", target / "processor1" / "0")
    assert (target / "0" / "sentinel").exists()
    verify_warm_fields(layout)
    for zero in layout:
        assert set(path.name for path in zero.iterdir()) == set(
            ("alpha.water", "U", "p_rgh", "k", "omega", "nut")
        )
        assert all("nonuniform List" in (zero / field).read_text()
                   for field in ("alpha.water", "U", "p_rgh", "k", "omega", "nut"))


def test_missing_warm_field_writes_cold_fallback_and_restores_cold(tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    source_time = source / "20"
    source_time.mkdir(parents=True)
    target.mkdir()
    (target / "0").mkdir()
    (target / "0" / "sentinel").write_text("cold\n")
    for name in ("alpha.water", "p_rgh", "k", "omega", "nut"):
        (source_time / name).write_text(WARM_FIELD.replace("object U", f"object {name}"))
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    passing = GateVerdict(tuple(GateCheck(identifier, True, "ok") for identifier in
                                ("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A9")))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate", lambda *a, **k: passing)
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.fields.run", lambda *a, **k: None)

    rc = main([
        "prepare", "--target", str(target), "--from", "case", "--hop", "speed",
        "--source", str(source), "--record", str(record), "--calibrate",
    ])

    assert rc == 3
    assert (target / "0" / "sentinel").read_text() == "cold\n"
    marker = (target / "COLD_FALLBACK").read_text()
    assert "required warm field missing" in marker
    assert "U" in marker


def test_run_refuses_resetting_chain_before_copy(tmp_path: Path, monkeypatch, capsys):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    unsafe = target / "solve_chain.sh"
    unsafe.write_text("#!/bin/sh\nrm -rf 0\ncp -a 0.orig 0\n")
    passing = GateVerdict(tuple(GateCheck(identifier, True, "ok") for identifier in
                                ("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A9")))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate", lambda *a, **k: passing)

    rc = main(["run", "--target", str(target), "--from", "case", "--hop", "speed",
               "--source", str(source), "--record", str(record), "--calibrate"])

    assert rc == 3
    message = "relaunch script resets 0/ from 0.orig; use a warm-aware chain"
    assert message in capsys.readouterr().err
    assert message in (target / "COLD_FALLBACK").read_text()
    assert not (target / "WARM_FIELDS").exists()


def test_successful_run_writes_field_hash_marker_before_launch(tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    for name in ("alpha.water", "U", "p_rgh", "k", "omega", "nut"):
        path = source / "20" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(WARM_FIELD.replace("object U", f"object {name}"))
        template = target / "0.orig" / name
        template.parent.mkdir(parents=True, exist_ok=True)
        template.write_text(FIELD.replace("object U", f"object {name}"))
    (target / "system").mkdir()
    (target / "system" / "controlDict").write_text("stopAt endTime;\n")
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    warm_chain = target / "warm_chain.sh"
    warm_chain.write_text("#!/bin/sh\n# preserve WARM_FIELDS\n")
    passing = GateVerdict(tuple(GateCheck(identifier, True, "ok") for identifier in
                                ("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A9")))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate", lambda *a, **k: passing)
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.rewrite_speed_fields", lambda *a: None)
    launches = []
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.subprocess.Popen",
                        lambda command, **kwargs: launches.append(command))

    rc = main(["run", "--target", str(target), "--from", "case", "--hop", "speed",
               "--source", str(source), "--record", str(record), "--calibrate",
               "--relaunch", str(warm_chain)])

    assert rc == 0 and launches == [str(warm_chain)]
    marker = yaml.safe_load((target / "WARM_FIELDS").read_text())
    assert marker["source"] == str(source.resolve())
    assert marker["source_time"] == "20"
    assert set(marker["sha256"]) == {f"0/{name}" for name in
                                      ("alpha.water", "U", "p_rgh", "k", "omega", "nut")}


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


def test_a3_compares_scalar_and_vector_entries_numerically(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    for case in (source, target):
        (case / "system").mkdir(parents=True)
        (case / "system" / "fvSchemes").write_text("ddtSchemes { default Euler; }\n")
    (source / "system" / "fvSolution").write_text(
        "solvers { p { tolerance 1e-07; direction (1e-07 2 3.0); } }\n"
    )
    (target / "system" / "fvSolution").write_text(
        "solvers { p { tolerance 1e-7; direction (0.0000001 2.0 3); } }\n"
    )
    assert _check(evaluate(source, target, "speed"), "A3").passed

    (target / "system" / "fvSolution").write_text(
        "solvers { p { tolerance 1e-6; direction (0.0000001 2.0 3); } }\n"
    )
    check = _check(evaluate(source, target, "speed"), "A3")
    assert not check.passed
    assert "p.tolerance" in check.detail


def test_calibration_prepare_exception_restores_cold_without_relaunch(
        tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    source_time = source / "20"
    source_time.mkdir(parents=True)
    target.mkdir()
    (target / "0").mkdir()
    (target / "0" / "sentinel").write_text("cold\n")
    for name in ("alpha.water", "U", "p_rgh", "k", "omega", "nut"):
        (source_time / name).write_text(FIELD)
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    warm_chain = target / "warm_chain.sh"
    warm_chain.write_text("#!/bin/sh\n# preserve WARM_FIELDS\n")
    passing = GateVerdict(tuple(GateCheck(identifier, True, "ok") for identifier in
                                ("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A9")))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate", lambda *a, **k: passing)
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.warm_start.cli.rewrite_speed_fields",
        lambda *a, **k: (_ for _ in ()).throw(RuntimeError("forced dictionary failure")),
    )
    launches = []
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.warm_start.cli.subprocess.Popen",
        lambda command, **kwargs: launches.append((command, kwargs)),
    )

    rc = main([
        "run", "--target", str(target), "--from", "case", "--hop", "speed",
        "--source", str(source), "--record", str(record), "--calibrate",
        "--relaunch", str(warm_chain),
    ])

    assert rc != 0
    assert (target / "0" / "sentinel").read_text() == "cold\n"
    assert "forced dictionary failure" in (target / "COLD_FALLBACK").read_text()
    hops = yaml.safe_load((record / "record_speed_default.yml").read_text())["hops"]
    assert hops[-1]["outcome"] == "WARM_ABORTED"
    assert "forced dictionary failure" in hops[-1]["reason"]
    assert launches == []


def test_refused_run_with_relaunch_starts_cold_and_records_not_attempted(
        tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    refused = GateVerdict((GateCheck("A1", False, "source unsettled"),))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate",
                        lambda *args, **kwargs: refused)
    launches = []
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.subprocess.Popen",
                        lambda command, **kwargs: launches.append((command, kwargs)))

    rc = main(["run", "--target", str(target), "--from", "case", "--hop", "speed",
               "--source", str(source), "--record", str(record), "--calibrate",
               "--relaunch", "cold-stub --marker SAME"])

    assert rc == 3
    assert launches == []
    assert "COLD_BY_GATE A1" in (target / "COLD_FALLBACK").read_text()
    hop = yaml.safe_load((record / "record_speed_default.yml").read_text())["hops"][-1]
    assert hop["decision"] == "COLD_BY_GATE"
    assert hop["outcome"] == "NOT_ATTEMPTED"


def test_production_refusal_relaunches_cold_but_returns_rc_2(tmp_path: Path, monkeypatch):
    source, target = tmp_path / "source", tmp_path / "target"
    source.mkdir(); target.mkdir()
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate",
                        lambda *args, **kwargs: GateVerdict((GateCheck("A1", False, "unsettled"),)))
    launches = []
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.subprocess.Popen",
                        lambda command, **kwargs: launches.append(command))

    rc = main(["run", "--target", str(target), "--from", "case", "--hop", "speed",
               "--source", str(source), "--record", str(record),
               "--relaunch", "cold-command"])

    assert rc == 2
    assert launches == ["cold-command"]
    assert "COLD_BY_GATE A1" in (target / "COLD_FALLBACK").read_text()


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
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_default.yml").write_text("n_cold: 5000\n")
    ledger = tmp_path / "warm_start.tsv"
    passing = GateVerdict(tuple(GateCheck(identifier, True, "ok") for identifier in
                                ("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A9")))
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.evaluate", lambda *a, **k: passing)
    source_time = source / "20"; source_time.mkdir()
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.clean_restart", lambda *a: None)
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.rewrite_speed_fields", lambda *a: None)
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli.verify_warm_fields", lambda *a: None)
    monkeypatch.setattr("digitalmodel.solvers.openfoam.warm_start.cli._write_warm_fields_marker", lambda *a: None)
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
    record = tmp_path / "records"
    record.mkdir()
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


def _write_checkpoint_history(case: Path, pressure: list[float], viscous=-100.0) -> None:
    path = case / "postProcessing" / "forces_hull" / "0" / "force.dat"
    path.parent.mkdir(parents=True)
    with path.open("w") as stream:
        for iteration, value in enumerate(pressure):
            total = value + viscous
            stream.write(
                f"{iteration} {total} 0 0 {value} 0 0 {viscous} 0 0\n"
            )


@pytest.mark.parametrize(
    ("pressure", "expected", "rc"),
    [
        ([0.0] * 799, "CONTINUE", 0),
        ([0.0, 60.0] * 400, "ABORT", 3),
        ([0.0] * 800, "OK", 0),
    ],
)
def test_check_prints_one_verdict_line_for_synthetic_histories(
        tmp_path: Path, capsys, pressure, expected, rc):
    target = tmp_path / expected.lower()
    _write_checkpoint_history(target, pressure)
    record = tmp_path / "records"
    record.mkdir(exist_ok=True)
    (record / "level_r3.yml").write_text(
        "n_cold: 5000\n"
        "first_cycle_amplitude_pressure: 100\n"
        "settled_viscous: -100\n"
        "cold_settling_iteration: 2000\n"
    )

    actual_rc = main([
        "check", "--target", str(target), "--n-cold", "5000",
        "--mesh-level", "r3", "--record", str(record),
    ])

    assert actual_rc == rc
    output = capsys.readouterr().out.splitlines()
    assert len(output) == 1
    assert output[0].startswith(f"{expected} ")


def test_check_missing_cold_reference_is_one_line_rc_2(tmp_path: Path, capsys):
    target = tmp_path / "target"
    _write_checkpoint_history(target, [0.0] * 20)

    rc = main([
        "check", "--target", str(target), "--n-cold", "5000",
        "--mesh-level", "r3", "--record", str(tmp_path / "empty"),
    ])

    captured = capsys.readouterr()
    assert rc == 2
    assert captured.out == ""
    assert captured.err.count("\n") == 1
    assert "--cold-ref" in captured.err


def test_check_cold_reference_precedence_explicit_then_source_then_record(
        tmp_path: Path, monkeypatch, capsys):
    target = tmp_path / "cases" / "target"
    source = tmp_path / "cases" / "source"
    explicit = tmp_path / "explicit.dat"
    _write_checkpoint_history(target, [0.0] * 20)
    write_force(source / "postProcessing" / "forces_hull" / "0" / "force.dat")
    write_force(explicit)
    (target / "WARM_PLANNED").write_text(f"source_path={source.resolve()}\n")
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_r3.yml").write_text(
        "n_cold: 5000\nfirst_cycle_amplitude_pressure: 3\n"
        "settled_viscous: -100\ncold_settling_iteration: 9\n"
    )
    seen = []

    def fake_reduce(path):
        seen.append(Path(path))
        return {"first_cycle_amplitude_pressure": 100.0,
                "settled_viscous": -100.0, "cold_settling_iteration": 2000}

    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.warm_start.cli.cold_reference_statistics",
        fake_reduce,
    )
    base = ["check", "--target", str(target), "--n-cold", "5000",
            "--mesh-level", "r3", "--record", str(record)]
    assert main([*base, "--cold-ref", str(explicit)]) == 0
    assert seen.pop() == explicit
    assert "different condition at the same speed accepted" in capsys.readouterr().out
    assert main(base) == 0
    assert seen.pop() == source.resolve()


def test_record_add_cold_stores_per_level_statistics(tmp_path: Path, capsys):
    case = tmp_path / "case"
    write_force(case / "postProcessing" / "forces_hull" / "0" / "force.dat")
    record = tmp_path / "records"

    rc = main([
        "record", "add-cold", "--case", str(case), "--mesh-level", "r3",
        "--record", str(record),
    ])

    assert rc == 0
    data = yaml.safe_load((record / "level_r3.yml").read_text())
    assert data["first_cycle_amplitude_pressure"] > 0
    assert data["settled_viscous"] == pytest.approx(-174_000.0)
    assert data["cold_settling_iteration"] > 0
    assert capsys.readouterr().out.count("\n") == 1


def test_check_act_invokes_abort_fallback_only_when_requested(
        tmp_path: Path, monkeypatch, capsys):
    target = tmp_path / "target"
    _write_checkpoint_history(target, [0.0, 60.0] * 400)
    record = tmp_path / "records"
    record.mkdir()
    (record / "level_r3.yml").write_text(
        "n_cold: 5000\nfirst_cycle_amplitude_pressure: 100\n"
        "settled_viscous: -100\ncold_settling_iteration: 2000\n"
    )
    actions = []
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.warm_start.cli.stop_and_fallback",
        lambda *args: actions.append(args),
    )
    command = ["check", "--target", str(target), "--mesh-level", "r3",
               "--record", str(record), "--n-cold", "5000"]

    assert main(command) == 3
    assert actions == []
    assert not (target / "WARM_ABORTED").exists()
    capsys.readouterr()
    assert main([*command, "--act", "--relaunch", "cold-command"]) == 3
    assert actions and actions[-1][2] == "cold-command"


def test_abort_action_stops_archives_restores_and_relaunches(
        tmp_path: Path, monkeypatch):
    target = tmp_path / "target"
    (target / "system").mkdir(parents=True)
    (target / "0").mkdir()
    (target / "0" / "state").write_text("warm\n")
    (target / "0.cold").mkdir()
    (target / "0.cold" / "state").write_text("cold\n")
    (target / "log.solver").write_text("warm log\n")
    (target / "postProcessing" / "forces" / "0").mkdir(parents=True)
    (target / "postProcessing" / "forces" / "0" / "force.dat").write_text("warm forces\n")
    calls = []
    launches = []
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.warm_start.checks.subprocess.run",
        lambda *args, **kwargs: calls.append((args, kwargs)),
    )
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.warm_start.checks.subprocess.Popen",
        lambda *args, **kwargs: launches.append((args, kwargs)),
    )

    archive = stop_and_fallback(target, "pressure_excursion", "cold-command")

    assert calls[0][0][0][-2:] == ["-set", "writeNow"]
    assert (target / "WARM_ABORTED").read_text() == "pressure_excursion\n"
    assert (archive / "log.solver").exists()
    assert (archive / "postProcessing" / "forces" / "0" / "force.dat").exists()
    assert (target / "0" / "state").read_text() == "cold\n"
    assert launches[0][0][0] == "cold-command"


def test_reference_warm_aware_chain_keeps_verified_zero(tmp_path: Path):
    case = tmp_path / "case"
    (case / "0").mkdir(parents=True)
    (case / "0" / "U").write_text(WARM_FIELD)
    (case / "0" / "state").write_text("warm\n")
    (case / "0.orig").mkdir()
    (case / "0.orig" / "state").write_text("cold\n")
    (case / "WARM_FIELDS").write_text("source: fixture\n")
    script = Path(__file__).parents[3] / "scripts" / "cfd" / "solve_chain_warm_aware.sh"

    subprocess.run([str(script), str(case), "true"], check=True)

    assert (case / "WARM_FIELDS_KEPT").exists()
    assert (case / "0" / "state").read_text() == "warm\n"
