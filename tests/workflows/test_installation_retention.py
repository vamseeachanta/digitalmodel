import json
import hashlib
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.workflows.installation_retention import audit_run, create_archive


def fixture_run(tmp_path):
    run = tmp_path / "source" / "data" / "study" / "runs" / "case_000"
    for folder in ["extracted", "installation_traces", "results"]:
        (run / folder).mkdir(parents=True)
    np.savez(run / "installation_traces/traces.npz", time=[0., 1., 2.], load=[10., 11., 12.])
    meta = dict(trace_sha256=hashlib.sha256((run / "installation_traces/traces.npz").read_bytes()).hexdigest(), channels={"load": dict(object="Sling", variable="Effective tension", position="End A", units="kN")})
    (run / "installation_traces/metadata.json").write_text(json.dumps(meta))
    item = dict(label="Sling / Effective tension / End A", units="kN", time=[0., 1., 2.], values=[10., 11., 12.], statistics={"max": 12})
    graph = dict(label="Sling / Effective tension", units="kN", X=[0., 3.], Min=[1., 2.], Max=[12., 13.], Mean=[7., 8.])
    extraction = dict(time_histories=[item], range_graphs=[graph], period=[0, 2])
    (run / "extracted/extraction.json").write_text(json.dumps(extraction))
    (run / "run.json").write_text(json.dumps(dict(run_id="case_000", extraction=extraction, warnings=["review"])))
    (run / "extracted/time_000.csv").write_text("time_s,Sling / Effective tension / End A [kN]\n0,10\n1,11\n2,12\n")
    (run / "extracted/range_000.csv").write_text("arc_length_m,Min [kN],Max [kN],Mean [kN]\n0,1,12,7\n3,2,13,8\n")
    (run / "results/batch_summary.json").write_text('{"status":"complete"}')
    (run / "results/cases.csv").write_text("case,status\n0,complete\n")
    return run


def test_audit_covers_csv_and_nested_json_without_data_loss(tmp_path):
    run = fixture_run(tmp_path)
    result = audit_run(run)
    assert result["time_csv_count"] == 1 and result["range_csv_count"] == 1
    assert result["json_time_series_count"] == 2 and result["json_range_series_count"] == 2
    assert result["precision"] == "exact float64 equality"
    compact = result["compact_records"]
    assert compact["run.json"]["warnings"] == ["review"]
    assert compact["run.json"]["extraction"]["time_histories"][0]["retained_channel"] == "load"


@pytest.mark.parametrize("defect", ["value", "time", "units", "identity", "range", "unknown_csv"])
def test_data_or_identity_gap_rejects_retention(tmp_path, defect):
    run = fixture_run(tmp_path)
    path = run / "extracted/time_000.csv"
    text = path.read_text()
    if defect == "value": text = text.replace("1,11", "1,11.0000001")
    if defect == "time": text = text.replace("1,11", "1.1,11")
    if defect == "units": text = text.replace("[kN]", "[N]")
    if defect == "identity": text = text.replace("End A", "End B")
    path.write_text(text)
    if defect == "range":
        path = run / "extracted/range_000.csv"
        path.write_text(path.read_text().replace("0,1,12,7", "0,1,99,7"))
    if defect == "unknown_csv": (run / "unique.csv").write_text("x\n1\n")
    with pytest.raises(ValueError): audit_run(run)


def test_archive_copies_readback_verified_files_and_refuses_overwrite(tmp_path):
    run = fixture_run(tmp_path)
    root = tmp_path / "source"
    target = tmp_path / "archive" / "lean-r1"
    result = create_archive(root, [{"path": run.relative_to(root).as_posix(), "dataset_id": "study", "group": "test"}], target, tmp_path / "archive")
    assert result["run_count"] == 1 and result["backup_status"] == "NOT_VERIFIED"
    assert (run / "installation_traces/traces.npz").exists()
    assert (target / "manifest.json").exists()
    with pytest.raises(FileExistsError): create_archive(root, [], target, tmp_path / "archive")


def test_archive_rejects_escaped_run_path_before_creating_target(tmp_path):
    fixture_run(tmp_path)
    target = tmp_path / "archive" / "lean-r1"
    with pytest.raises(ValueError):
        create_archive(tmp_path / "source", [{"path": "../elsewhere", "dataset_id": "study", "group": "test"}], target, tmp_path / "archive")
    assert not target.exists()


def test_archive_rejects_absolute_entry_before_creating_target(tmp_path):
    run = fixture_run(tmp_path)
    target = tmp_path / "archive" / "lean-r1"
    with pytest.raises(ValueError):
        create_archive(tmp_path / "source", [{"path": str(run)}], target, tmp_path / "archive")
    assert not target.exists()


def test_trace_hash_mismatch_rejects_audit(tmp_path):
    run = fixture_run(tmp_path)
    path = run / "installation_traces/metadata.json"
    record = json.loads(path.read_bytes())
    record["trace_sha256"] = "0" * 64
    path.write_text(json.dumps(record))
    with pytest.raises(ValueError, match="digest"):
        audit_run(run)


def test_nested_json_mismatch_rejects_audit(tmp_path):
    run = fixture_run(tmp_path)
    path = run / "run.json"
    record = json.loads(path.read_bytes())
    record["extraction"]["time_histories"][0]["values"][0] = 99
    path.write_text(json.dumps(record))
    with pytest.raises(ValueError):
        audit_run(run)


def test_compact_records_restore_original_json_semantics(tmp_path):
    run = fixture_run(tmp_path)
    compact = audit_run(run)["compact_records"]
    item = compact["run.json"]["extraction"]["time_histories"][0]
    with np.load(run / item.pop("retained_file")) as arrays:
        item["time"] = arrays[item.pop("retained_time_channel")].tolist()
        item["values"] = arrays[item.pop("retained_channel")].tolist()
    graph = compact["run.json"]["extraction"]["range_graphs"][0]
    values = np.loadtxt(run / graph.pop("retained_file"), delimiter=",", skiprows=1)
    graph.update({key: values[:, column].tolist() for column, key in enumerate(["X", "Min", "Max", "Mean"])})
    assert compact["run.json"] == json.loads((run / "run.json").read_bytes())


def test_archive_preserves_source_run_id_instead_of_directory_alias(tmp_path):
    run = fixture_run(tmp_path)
    path = run / "run.json"
    record = json.loads(path.read_bytes())
    record["run_id"] = "original-run-identity"
    path.write_text(json.dumps(record))
    root = tmp_path / "source"
    result = create_archive(root, [{"path": run.relative_to(root).as_posix()}], tmp_path / "archive/v1", tmp_path / "archive")
    assert result["runs"][0]["run_id"] == "original-run-identity"


@pytest.mark.parametrize("defect", ["missing_record", "missing_id", "empty_id"])
def test_missing_run_identity_fails_before_target_creation(tmp_path, defect):
    run = fixture_run(tmp_path)
    path = run / "run.json"
    if defect == "missing_record":
        path.unlink()
    else:
        record = json.loads(path.read_bytes())
        if defect == "missing_id": record.pop("run_id")
        else: record["run_id"] = ""
        path.write_text(json.dumps(record))
    root = tmp_path / "source"
    target = tmp_path / "archive/v1"
    with pytest.raises(ValueError, match="run identity"):
        create_archive(root, [{"path": run.relative_to(root).as_posix()}], target, target.parent)
    assert not target.exists()


@pytest.mark.parametrize("alias", ["dot", "case"])
def test_duplicate_path_alias_fails_before_target_creation(tmp_path, alias):
    run = fixture_run(tmp_path)
    root = tmp_path / "source"
    relative = run.relative_to(root).as_posix()
    second = relative.replace("/runs/", "/./runs/") if alias == "dot" else relative.upper()
    if not (root / second).exists(): pytest.skip("Case alias does not exist on this filesystem")
    target = tmp_path / "archive/v1"
    with pytest.raises(ValueError, match="duplicat"):
        create_archive(root, [{"path": relative}, {"path": second}], target, target.parent)
    assert not target.exists()


def test_inner_symlink_escape_rejects_audit(tmp_path):
    run = fixture_run(tmp_path)
    external = tmp_path / "outside.json"
    external.write_text('{}')
    link = run / "external.json"
    try: link.symlink_to(external)
    except OSError: pytest.skip("OS does not grant symlink creation")
    with pytest.raises(ValueError, match="escapes"):
        audit_run(run)
