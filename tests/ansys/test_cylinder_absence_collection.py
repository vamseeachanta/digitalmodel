"""RED contract tests for the pressure-absence-only process collector."""
from copy import deepcopy
import importlib
import json
import subprocess

import pytest


def module():
    return importlib.import_module("digitalmodel.ansys.cylinder_absence_collection")


def initial(pid, parent, name, executable=None, created="100.0"):
    return {
        "pid": pid,
        "parent_pid": parent,
        "name": name,
        "executable_path": executable,
        "creation_time": created,
    }


def cim_row(pid, parent, name, created="19700101000000.000000+000"):
    return {
        "ProcessId": pid,
        "ParentProcessId": parent,
        "Name": name,
        "CreationDate": created,
        "ExecutablePath": None,
    }


def resolution(rows):
    return {
        "performed": True,
        "query": "SELECT bounded identity fields FROM Win32_Process",
        "argv": [r"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe"],
        "stdin_base64": "ODIw",
        "script_sha256": "a" * 64,
        "powershell_sha256": "b" * 64,
        "elapsed_ns": 1,
        "return_code": 0,
        "stdout_base64": "e30=",
        "stderr_base64": "",
        "raw_rows": rows,
        "dependency_pins": {"fixture": "synthetic"},
        "provider_metadata": {"provider": "Win32_Process fixture"},
    }


def install_observation(monkeypatch, rows, resolved=None, final_map=None):
    observed = deepcopy(rows)
    final = deepcopy(observed)
    if final_map is not None:
        final = [{**item, "parent_pid": final_map[item["pid"]]}
                 for item in observed if item["pid"] in final_map]
        for pid, parent in final_map.items():
            if pid not in {item["pid"] for item in final}:
                final.append(initial(pid, parent, "python.exe", r"C:\Python\python.exe"))
    monkeypatch.setattr(
        module(), "_collect_initial_populations",
        lambda: ([deepcopy(observed), deepcopy(observed)],
                 [{item["pid"]: item["parent_pid"] for item in observed}] * 2),
    )
    monkeypatch.setattr(
        module(), "_collect_final_population",
        lambda: (deepcopy(final), {item["pid"]: item["parent_pid"] for item in final}),
    )
    if resolved is not None:
        monkeypatch.setattr(module(), "_query_blank_names", lambda pids: deepcopy(resolved))
    monkeypatch.setattr(module().socket, "gethostname", lambda: "TEST-HOST")
    monkeypatch.setattr(module().time, "time_ns", lambda: 100_000_000_000)


def test_retains_originals_and_resolves_blank_name_without_pid_allowance(monkeypatch):
    rows = [initial(4, 0, "System", "", "0"), initial(820, 4, "", "", "0")]
    evidence = resolution([cim_row(820, 4, "Secure System")])
    install_observation(monkeypatch, rows, evidence)

    value = module().collect_absence_snapshot()

    assert value["schema"] == "process-snapshot-1"
    assert value["original_initial_inventory"] == rows
    assert value["observed_inventories"] == [value["initial_inventory"]] * 3
    assert value["initial_inventory"] == [
        initial(4, 0, "System", None, "0"),
        initial(820, 4, "Secure System", None, "0"),
    ]
    assert value["name_resolution"] == evidence
    assert value["rows"] == []
    assert value["population_check"]["events"] == []
    assert value["coverage"] == {
        "selector": "ansys-mpi-lineage-v1",
        "enumerated_count": 2,
        "selected_count": 0,
        "excluded_count": 2,
        "selected_details_complete": True,
        "limitation": "Nonselected protected image paths may be unavailable.",
    }


def test_no_blank_name_skips_cim_but_still_records_nonquery(monkeypatch):
    rows = [initial(4, 0, "System", "")]
    install_observation(monkeypatch, rows)
    monkeypatch.setattr(
        module(), "_query_blank_names",
        lambda pids: pytest.fail("CIM must not run without blank names"),
    )

    value = module().collect_absence_snapshot()

    assert value["name_resolution"]["performed"] is False
    assert value["name_resolution"]["query"] is None
    assert value["name_resolution"]["raw_rows"] == []
    assert value["initial_inventory"][0]["executable_path"] is None


def test_observed_at_uses_bounded_decimal_for_nonintegral_nanoseconds(monkeypatch):
    rows = [initial(4, 0, "System", "")]
    install_observation(monkeypatch, rows)
    monkeypatch.setattr(module().time, "time_ns", lambda: 100_000_000_001)
    assert module().collect_absence_snapshot()["observed_at"] == "100.000000001"


@pytest.mark.parametrize(
    "raw_rows",
    [
        [],
        [cim_row(820, 4, "")],
        [cim_row(820, 4, "Secure System"), cim_row(820, 4, "Secure System")],
        [cim_row(820, 5, "Secure System")],
        [cim_row(820, 4, "Secure System", "19700101000000.000002+000")],
    ],
)
def test_cim_identity_mismatch_or_unresolved_name_refuses(monkeypatch, raw_rows):
    rows = [initial(820, 4, "", None, "0")]
    install_observation(monkeypatch, rows, resolution(raw_rows))
    with pytest.raises(ValueError):
        module().collect_absence_snapshot()


def test_fractional_epoch_within_one_microsecond_is_accepted(monkeypatch):
    rows = [initial(820, 4, "", None, "0.0000009")]
    install_observation(
        monkeypatch, rows,
        resolution([cim_row(820, 4, "Secure System", "19700101000000.000000+000")]),
    )
    assert module().collect_absence_snapshot()["initial_inventory"][0]["name"] == "Secure System"


@pytest.mark.parametrize(
    "created,dmtf",
    [
        ("0.0000009", "19691231190000.000000-300"),
        ("0.0000009", "19700101050000.000000+300"),
    ],
)
def test_dmtf_utc_offsets_are_converted_to_the_same_instant(monkeypatch, created, dmtf):
    rows = [initial(820, 4, "", None, created)]
    install_observation(monkeypatch, rows, resolution([cim_row(820, 4, "Secure System", dmtf)]))
    assert module().collect_absence_snapshot()["initial_inventory"][0]["name"] == "Secure System"


def test_ignoring_dmtf_offset_would_refuse(monkeypatch):
    rows = [initial(820, 4, "", None, "0")]
    install_observation(
        monkeypatch, rows,
        resolution([cim_row(820, 4, "Secure System", "19700101000000.000000-300")]),
    )
    with pytest.raises(ValueError, match="creation"):
        module().collect_absence_snapshot()


@pytest.mark.parametrize("final_map", [{820: 5}, {820: 4, 900: 4}])
def test_relevant_parent_change_or_unrelated_addition_is_classified(monkeypatch, final_map):
    rows = [initial(820, 4, "", None, "0")]
    install_observation(
        monkeypatch, rows, resolution([cim_row(820, 4, "Secure System")]), final_map,
    )
    if 900 in final_map:
        value = module().collect_absence_snapshot()
        assert value["coverage"]["enumerated_count"] == 2
        assert value["population_check"]["events"][-1]["disposition"] == "RETAINED_UNRELATED"
    else:
        with pytest.raises(ValueError, match="complete|parent"):
            module().collect_absence_snapshot()


def test_existing_seed_and_descendant_are_selected_with_existing_details(monkeypatch):
    rows = [
        initial(40, 4, "ANSYS261.exe", r"C:\ANSYS\bin\ANSYS261.exe"),
        initial(41, 40, "worker.exe", r"C:\ANSYS\bin\worker.exe"),
        initial(90, 4, "python.exe", r"C:\Python\python.exe"),
    ]
    install_observation(monkeypatch, rows)
    seen = []

    def details(row, cache):
        seen.append((deepcopy(row), cache))
        return {**row, "executable_sha256": "c" * 64, "argv": [], "script_sources": []}

    monkeypatch.setattr(module(), "_details", details)
    value = module().collect_absence_snapshot()
    assert [row["pid"] for row in value["rows"]] == [40, 41]
    assert [row[0]["pid"] for row in seen] == [40, 41]
    assert seen[0][1] is seen[1][1]


def test_blank_name_resolved_to_seed_refuses_normalized_selected_identity(monkeypatch):
    rows = [initial(820, 4, "", "", "0")]
    install_observation(monkeypatch, rows, resolution([cim_row(820, 4, "ANSYS261.exe")]))
    monkeypatch.setattr(
        module(), "_details",
        lambda row, cache: (_ for _ in ()).throw(ValueError("selected process changed")),
    )
    with pytest.raises(module().CollectionError, match="selected process changed") as caught:
        module().collect_absence_snapshot()
    assert caught.value.evidence["original_initial_inventory"] == rows
    assert caught.value.evidence["name_resolution"]["raw_rows"][0]["Name"] == "ANSYS261.exe"


def test_targeted_powershell_runner_retains_exact_evidence(monkeypatch):
    captured = {}

    class Completed:
        returncode = 0
        stdout = json.dumps([cim_row(820, 4, "Secure System")]).encode()
        stderr = b""

    def run(argv, **kwargs):
        captured["argv"] = argv
        captured.update(kwargs)
        return Completed()

    monkeypatch.setattr(module(), "_powershell_identity", lambda: (r"C:\Windows\powershell.exe", "d" * 64))
    monkeypatch.setattr(module().subprocess, "run", run)
    monkeypatch.setattr(module().time, "perf_counter_ns", lambda: 10)
    value = module()._query_blank_names([820])
    assert captured["timeout"] <= 2
    assert captured["shell"] is False
    assert captured["capture_output"] is True
    assert captured["input"] == b"820"
    assert "-NoProfile" in captured["argv"]
    assert "-NonInteractive" in captured["argv"]
    assert "-EncodedCommand" in captured["argv"]
    assert value["performed"] is True
    assert value["argv"] == captured["argv"]
    assert value["stdin_base64"] == "ODIw"
    assert value["script_sha256"] == module().hashlib.sha256(module().QUERY_SCRIPT.encode()).hexdigest()
    assert value["powershell_sha256"] == "d" * 64
    assert value["return_code"] == 0
    assert value["stdout_base64"]
    assert value["stderr_base64"] == ""
    assert value["raw_rows"] == [cim_row(820, 4, "Secure System")]
    assert value["dependency_pins"] == {
        "powershell_path": r"C:\Windows\powershell.exe",
        "powershell_sha256": "d" * 64,
        "script_sha256": value["script_sha256"],
    }
    assert value["provider_metadata"] == {"provider": "Win32_Process via Get-WmiObject"}
    assert captured["creationflags"] == subprocess.CREATE_NO_WINDOW


def test_targeted_powershell_timeout_refuses(monkeypatch):
    monkeypatch.setattr(module(), "_powershell_identity", lambda: (r"C:\Windows\powershell.exe", "d" * 64))

    def timeout(argv, **kwargs):
        raise subprocess.TimeoutExpired(argv, kwargs["timeout"], output=b"partial", stderr=b"")

    monkeypatch.setattr(module().subprocess, "run", timeout)
    with pytest.raises(module().CollectionError, match="timed out") as caught:
        module()._query_blank_names([820])
    assert caught.value.evidence["return_code"] is None
    assert caught.value.evidence["stdout_base64"] == "cGFydGlhbA=="
    assert caught.value.evidence["stderr_base64"] == ""


def test_collection_timeout_retains_original_table_and_partial_query(monkeypatch):
    rows = [initial(820, 4, "", None, "0")]
    monkeypatch.setattr(
        module(), "_collect_initial_populations",
        lambda: ([deepcopy(rows), deepcopy(rows)], [{820: 4}, {820: 4}]),
    )
    partial = {"performed": True, "stdout_base64": "cGFydGlhbA==", "raw_rows": []}

    def timeout(pids):
        raise module().CollectionError("Windows process query timed out", partial)

    monkeypatch.setattr(module(), "_query_blank_names", timeout)
    with pytest.raises(module().CollectionError) as caught:
        module().collect_absence_snapshot()
    assert caught.value.evidence["original_initial_inventory"] == rows
    assert caught.value.evidence["original_observed_inventories"] == [rows, rows]
    assert caught.value.evidence["observed_inventories"] == []
    assert caught.value.evidence["name_resolution"] == partial
    assert [item["stage"] for item in caught.value.evidence["parent_maps"]] == ["A", "B"]


@pytest.mark.parametrize(
    "returncode,stdout,stderr,match",
    [
        (3, b"[]", b"", "failed"),
        (0, b"[]", b"warning", "error output"),
        (0, b"not-json", b"", "JSON"),
    ],
)
def test_targeted_powershell_output_failures_retain_evidence(
        monkeypatch, returncode, stdout, stderr, match):
    class Completed:
        pass
    completed = Completed()
    completed.returncode, completed.stdout, completed.stderr = returncode, stdout, stderr
    monkeypatch.setattr(module(), "_powershell_identity", lambda: (r"C:\Windows\powershell.exe", "d" * 64))
    monkeypatch.setattr(module().subprocess, "run", lambda *args, **kwargs: completed)
    monkeypatch.setattr(module().time, "perf_counter_ns", lambda: 10)
    with pytest.raises(module().CollectionError, match=match) as caught:
        module()._query_blank_names([820])
    assert caught.value.evidence["stdout_base64"]
    assert caught.value.evidence["return_code"] == returncode


@pytest.mark.parametrize("pids", [[True], [-1], [2**32], [1, 1], list(range(65))])
def test_targeted_query_requires_unique_bounded_integer_pids(monkeypatch, pids):
    monkeypatch.setattr(
        module().subprocess, "run", lambda *args, **kwargs: pytest.fail("must not run"),
    )
    with pytest.raises(ValueError, match="PID"):
        module()._query_blank_names(pids)


def test_collection_issues_at_most_one_name_query(monkeypatch):
    rows = [initial(820, 4, "", None, "0"), initial(821, 4, "", None, "0")]
    calls = []
    result = resolution([cim_row(820, 4, "Secure System"), cim_row(821, 4, "Registry")])
    install_observation(monkeypatch, rows, result)

    def query(pids):
        calls.append(pids)
        return deepcopy(result)

    monkeypatch.setattr(module(), "_query_blank_names", query)
    module().collect_absence_snapshot()
    assert calls == [[820, 821]]


