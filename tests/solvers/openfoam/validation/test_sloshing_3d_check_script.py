"""Case-matrix and execution contracts for the rotational comparison driver."""

import importlib.util
import re
import sys
from pathlib import Path

import pytest


@pytest.fixture()
def driver(monkeypatch):
    root = Path(__file__).resolve().parents[4] / "scripts/cfd"
    monkeypatch.syspath_prepend(str(root))
    spec = importlib.util.spec_from_file_location("run_sloshing_3d_check", root / "run_sloshing_3d_check.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_matrix_builds_requested_meshes_and_period_basis(tmp_path, driver):
    specs = driver.case_specs(t1_roll=1.12, t1_pitch=1.49, ranks=4)
    assert len(specs) == 27
    assert sum(s["dimension"] == 2 for s in specs) == 7
    assert sum(s["dimension"] == 3 for s in specs) == 20
    assert all(s["ranks"] == (1 if s["dimension"] == 2 else 4) for s in specs)
    for spec in specs:
        case = driver.build_case(tmp_path, spec)
        text = (case / "system/blockMeshDict").read_text()
        assert f'({spec["nx"]} {spec["ny"]} {spec["nz"]})' in text
        if spec["kind"] == "forced":
            assert spec["period"] == pytest.approx(spec["ratio"] * (1.12 if spec["direction"] == "roll" else 1.49))
        else:
            assert re.search(r"adjustTimeStep\s+no;", (case / "system/controlDict").read_text())


def test_pitch_reduction_and_digest_bound_resume(tmp_path, driver, monkeypatch):
    import math
    spec = next(s for s in driver.case_specs(ranks=1, smoke=True)
                if s["group"] == "coarse-3d" and s["direction"] == "pitch")
    case = driver.build_case(tmp_path, spec)
    calls = []

    def launch(case, argv):
        calls.append(argv)
        log = case / f"log.{argv[-1] if argv[0] == 'mpirun' else argv[0]}"
        if argv[0] == "blockMesh":
            mesh = case / "constant/polyMesh"
            mesh.mkdir()
            (mesh / "boundary").write_text("synthetic test boundary\n")
        if argv[0] == "interFoam":
            log.write_text("Time = 0.03\nEnd\n")
            for i in range(1, 7):
                folder = case / f"postProcessing/interfaceHeight{i}/0"
                folder.mkdir(parents=True)
                folder.joinpath("height.dat").write_text("\n".join(
                    f"{j * .0005} {0.45 + i * .001 * math.sin(2 * math.pi * j * .0005 / spec['period'])} 0"
                    for j in range(61)) + "\n")
            folder = case / "postProcessing/tankRollMoment/0"
            folder.mkdir(parents=True)
            folder.joinpath("moment.dat").write_text("\n".join(
                f"{j * .0005} ({3 * math.sin(2 * math.pi * j * .0005 / spec['period'])} 0 0) (0 0 0) (0 0 0)"
                for j in range(61)) + "\n")
        else:
            log.write_text("End\n")
        return 0.1

    monkeypatch.setattr(driver, "_launch", launch)
    monkeypatch.setattr(driver, "_available_memory_gb", lambda: 7)
    row = driver.run_case(case)
    assert row["status"] == "smoke-completed"
    assert row["moment_amplitude_nm"] == pytest.approx(3, rel=1e-6)
    assert row["moment_per_m_nm"] == pytest.approx(3 / .9, rel=1e-6)
    assert row["wall_harmonic_amp_m"] == pytest.approx(.001, rel=1e-6)
    assert driver.run_case(case) == row
    assert len(calls) == 3
    (case / "postProcessing/interfaceHeight1/0/height.dat").write_text("altered\n")
    with pytest.raises(ValueError, match="output digest"):
        driver.run_case(case)
    assert len(calls) == 3


def test_incomplete_and_nonfinite_output_cannot_complete(tmp_path, driver, monkeypatch):
    spec = next(s for s in driver.case_specs(ranks=1, smoke=True) if s["group"] == "reproduction-2d")
    case = driver.build_case(tmp_path, spec)
    folder = case / "postProcessing/interfaceHeight1/0"
    folder.mkdir(parents=True)
    folder.joinpath("height.dat").write_text("0.01 .45 0\n0.015 nan 0\n0.02 .45 0\n0.03 .45 0\n")
    with pytest.raises(ValueError, match="nonfinite"):
        driver.reduce_case(case, spec)
    folder.joinpath("height.dat").write_text("0.01 .45 0\n0.015 .45 0\n0.02 .45 0\n0.03 .45 0\n")
    with pytest.raises(ValueError, match="configured duration"):
        driver.reduce_case(case, spec)
    monkeypatch.setattr(driver, "_launch", lambda c, argv: (case / "log.interFoam").write_text("Time = 0.01\nEnd\n") if argv[0] == "interFoam" else 0.1)
    with pytest.raises(RuntimeError, match="end time"):
        driver.run_case(case)
    import json
    assert json.loads((case / "_result.json").read_text())["status"] == "failed"


def test_input_mutation_blocks_resume_without_launch(tmp_path, driver, monkeypatch):
    import json
    spec = driver.case_specs(ranks=1, smoke=True)[0]
    case = driver.build_case(tmp_path, spec)
    (case / "_state.json").write_text(json.dumps({"input_sha256": driver.input_digest(case), "stages": [], "failures": 0}))
    control = case / "system/controlDict"
    control.write_text(control.read_text() + "// changed numerical input\n")
    monkeypatch.setattr(driver, "_launch", lambda *_: pytest.fail("mutated case launched"))
    with pytest.raises(ValueError, match="input digest"):
        driver.run_case(case)


def test_missing_sampling_interval_blocks_reduction(tmp_path, driver):
    spec = next(s for s in driver.case_specs(ranks=1, smoke=True) if s["group"] == "reproduction-2d")
    case = driver.build_case(tmp_path, spec)
    folder = case / "postProcessing/interfaceHeight1/0"
    folder.mkdir(parents=True)
    folder.joinpath("height.dat").write_text("0 .45 0\n0.005 .45 0\n0.025 .45 0\n0.03 .45 0\n")
    with pytest.raises(ValueError, match="missing sampling interval"):
        driver.reduce_case(case, spec)


def test_case_generation_is_byte_identical_twice(tmp_path, driver):
    import hashlib
    def hashes(root):
        return {p.relative_to(root).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                for p in sorted(root.rglob("*")) if p.is_file()}
    for spec in driver.case_specs(t1_roll=1.12, t1_pitch=1.49):
        driver.build_case(tmp_path / "one", spec)
        driver.build_case(tmp_path / "two", spec)
    assert hashes(tmp_path / "one") == hashes(tmp_path / "two")


def test_restart_histories_are_numeric_and_duplicate_checked(tmp_path, driver):
    for start, text in (("2", "2 .45 0\n3 .46 0\n"), ("10", "10 .47 0\n11 .48 0\n")):
        folder = tmp_path / f"postProcessing/interfaceHeight1/{start}"
        folder.mkdir(parents=True)
        folder.joinpath("height.dat").write_text(text)
    rows = driver._merged_history(tmp_path, "interfaceHeight1", "height.dat")
    assert [r[0] for r in rows] == [2, 3, 10, 11]
    folder.joinpath("height.dat").write_text("2 .47 0\n10 .47 0\n11 .48 0\n")
    with pytest.raises(ValueError, match="contradictory duplicate"):
        driver._merged_history(tmp_path, "interfaceHeight1", "height.dat")


def test_memory_and_rank_limits_prevent_launch(tmp_path, driver, monkeypatch):
    with pytest.raises(ValueError, match="ranks"):
        driver.case_specs(ranks=8)
    monkeypatch.setattr(driver, "_available_memory_gb", lambda: 5)
    monkeypatch.setattr(driver.subprocess, "run", lambda *_args, **_kwargs: pytest.fail("launch under memory limit"))
    with pytest.raises(RuntimeError, match="below 6 GB"):
        driver._launch(tmp_path, ["interFoam"])


def test_restart_requires_complete_equal_rank_checkpoints(tmp_path, driver):
    spec = next(s for s in driver.case_specs(ranks=2) if s["dimension"] == 3 and s["kind"] == "forced")
    for i in range(2):
        checkpoint = tmp_path / f"processor{i}/0.05"
        checkpoint.mkdir(parents=True)
        for field in ("U", "p_rgh", "alpha.water"):
            (checkpoint / field).write_text("synthetic checkpoint\n")
    with pytest.raises(ValueError, match="incomplete restart"):
        driver._restart_fields(tmp_path, spec)
    for i in range(2):
        mesh = tmp_path / f"processor{i}/0.05/polyMesh"
        mesh.mkdir()
        mesh.joinpath("points").write_text("synthetic mesh checkpoint\n")
    driver._restart_fields(tmp_path, spec)


def test_prepared_state_tampering_blocks_solver(tmp_path, driver, monkeypatch):
    import json
    spec = driver.case_specs(ranks=1, smoke=True)[0]
    case = driver.build_case(tmp_path, spec)
    state = {"input_sha256": driver.input_digest(case), "stages": ["blockMesh", "setFields"], "failures": 0,
             "prepared_sha256": driver.prepared_digests(case, spec)}
    (case / "_state.json").write_text(json.dumps(state))
    (case / "0/U").write_text("changed initialized velocity\n")
    monkeypatch.setattr(driver, "_launch", lambda *_: pytest.fail("tampered state launched"))
    with pytest.raises(ValueError, match="prepared state digest"):
        driver.run_case(case)


def test_retry_inspects_only_current_invocation(tmp_path, driver, monkeypatch):
    from types import SimpleNamespace
    (tmp_path / "log.interFoam").write_text("FOAM FATAL ERROR\n")
    monkeypatch.setattr(driver, "_available_memory_gb", lambda: 7)
    def native(_argv, **kwargs):
        kwargs["stdout"].write("Time = 0.03\nEnd\n")
        return SimpleNamespace(returncode=0)
    monkeypatch.setattr(driver.subprocess, "run", native)
    assert driver._launch(tmp_path, ["interFoam"]) >= 0
    assert "FOAM FATAL ERROR" in (tmp_path / "log.interFoam").read_text()


def test_completion_uses_rendered_end_time(tmp_path, driver):
    spec = next(s for s in driver.case_specs(ranks=1) if s["group"] == "longitudinal-2d" and s["ratio"] == 1.15)
    case = driver.build_case(tmp_path, spec)
    control = (case / "system/controlDict").read_text()
    configured = float(re.search(r"endTime\s+([\d.e+-]+);", control)[1])
    assert configured < spec["end_time"]
    times, end = driver.completed_times(f"Time = {configured}\nEnd\n", control)
    assert times[-1] == end == configured


def test_restart_overlap_uses_later_native_segment(tmp_path, driver):
    for start, text in (("0", "0 .45 0\n.005 .45 0\n.01 .45 0\n.015 .45 0\n"),
                        ("0.01", ".01 .46 0\n.015 .46 0\n.02 .46 0\n.025 .46 0\n")):
        folder = tmp_path / f"postProcessing/interfaceHeight1/{start}"
        folder.mkdir(parents=True)
        folder.joinpath("height.dat").write_text(text)
    rows = driver._merged_history(tmp_path, "interfaceHeight1", "height.dat")
    assert [r[0] for r in rows] == [0, .005, .01, .015, .02, .025]
    assert [r[1][1] for r in rows] == [.45, .45, .46, .46, .46, .46]


def test_free_decay_restart_gaps_are_resampled_explicitly(tmp_path, driver):
    import math
    spec = next(s for s in driver.case_specs(ranks=1, smoke=True) if s["dimension"] == 2 and s["kind"] == "free-decay")
    spec["end_time"] = 1.6
    case = driver.build_case(tmp_path, spec)
    for start, indices in (("0", range(41)), ("0.8", range(41, 81))):
        folder = case / f"postProcessing/interfaceHeight1/{start}"
        folder.mkdir(parents=True)
        folder.joinpath("height.dat").write_text("\n".join(
            f"{j * .02} {0.45 + .01 * math.cos(2 * math.pi * j * .02 / spec['t1_s'])} 0"
            for j in indices) + "\n")
    row = driver.reduce_case(case, spec)
    assert row["sampling"]["mode"] == "interpolated-restart"
    assert row["sampling"]["interval_s"] == .02
    assert math.isfinite(row["natural_period_cfd_s"])


def test_longitudinal_cli_separates_decay_from_measured_period_forcing(tmp_path, driver):
    import json
    common = ["generate", "--work-dir", str(tmp_path), "--set", "longitudinal-2d"]
    assert driver.main(common + ["--kind", "free-decay"]) == 0
    original = next(tmp_path.glob("*/_spec.json"))
    first = original.read_bytes()
    assert driver.main(common + ["--kind", "forced", "--t1-pitch", "1.49"]) == 0
    assert original.read_bytes() == first
    specs = [json.loads(p.read_text()) for p in tmp_path.glob("*/_spec.json")]
    assert len(specs) == 6
    assert all(s["t1_s"] == 1.49 for s in specs if s["kind"] == "forced")


@pytest.mark.parametrize("name", ["blockMesh", "setFields", "decomposePar"])
@pytest.mark.parametrize("code,output,expected", [
    (0, "boundingBox: (0 0 0) (1 1 1)\nEnd\n", None),
    (0, "boundingBox: (0 0 0) (1 1 1)\nFOAM FATAL ERROR\n", "FOAM FATAL ERROR"),
    (0, "FOAM FATAL IO ERROR\n", "FOAM FATAL IO ERROR"),
    (2, "boundingBox: (0 0 0) (1 1 1)\n", "non-zero exit code 2"),
])
def test_utility_stage_preserves_fatal_and_exit_checks(name, code, output, expected):
    from digitalmodel.solvers.openfoam.runner import OpenFOAMRunner
    result = OpenFOAMRunner._detect_error(name, code, output, check_divergence=False)
    assert result is None if expected is None else expected in result
    assert "bounding" in OpenFOAMRunner._detect_error("interFoam", 0, "bounding alpha.water\n")


@pytest.mark.parametrize("marker", ["bounding", "divergence", "maximum number of iterations exceeded"])
def test_solver_divergence_checks_remain_required(marker):
    from digitalmodel.solvers.openfoam.runner import OpenFOAMRunner
    assert marker in OpenFOAMRunner._detect_error("interFoam", 0, marker)
    assert marker in OpenFOAMRunner._detect_error("interFoam", 0, marker, check_divergence=True)


@pytest.mark.parametrize("name", ["blockMesh", "setFields", "decomposePar"])
def test_native_utility_bounding_box_is_not_solver_divergence(tmp_path, driver, monkeypatch, name):
    from types import SimpleNamespace
    monkeypatch.setattr(driver, "_available_memory_gb", lambda: 7)
    def native(_argv, **kwargs):
        kwargs["stdout"].write("boundingBox: (0 0 0) (1 1 1)\nEnd\n")
        return SimpleNamespace(returncode=0)
    monkeypatch.setattr(driver.subprocess, "run", native)
    assert driver._launch(tmp_path, [name]) >= 0


@pytest.mark.parametrize("ranks", [2, 4])
def test_partition_faces_cross_vertical_probe_rays(tmp_path, driver, ranks):
    spec = next(s for s in driver.case_specs(ranks=ranks, smoke=True)
                if s["group"] == "coarse-3d" and s["direction"] == "pitch")
    case = driver.build_case(tmp_path, spec)
    text = (case / "system/decomposeParDict").read_text()
    # interfaceHeight sums per-rank line integrals. A vertical ray on a
    # longitudinal/transverse partition face can contribute twice.
    assert re.search(r"method\s+simple;", text)
    assert re.search(rf"n\s+\(1\s+{ranks}\s+1\);", text)


def test_partial_vertical_layer_partition_is_rejected(tmp_path, driver):
    spec = next(s for s in driver.case_specs(ranks=3, smoke=True)
                if s["group"] == "coarse-3d" and s["direction"] == "pitch")
    with pytest.raises(ValueError, match="vertical cell count must be divisible"):
        driver.build_case(tmp_path, spec)
    assert not (tmp_path / spec["name"]).exists()


@pytest.mark.parametrize("direction", ["roll", "pitch"])
@pytest.mark.parametrize("smoke", [False, True])
def test_time_step_refinement_preserves_physical_output_times(tmp_path, driver, direction, smoke):
    controls = []
    for group in ("medium-3d", "time-step-3d"):
        spec = next(s for s in driver.case_specs(smoke=smoke)
                    if s["group"] == group and s["direction"] == direction and s["ratio"] == 1.0)
        case = driver.build_case(tmp_path, spec)
        top, functions = (case / "system/controlDict").read_text().split("functions", 1)
        values = {key: float(re.search(rf"\b{key}\s+([\d.e+-]+);", top)[1])
                  for key in ("deltaT", "maxDeltaT", "maxCo", "writeInterval", "endTime")}
        assert re.search(r"writeControl\s+adjustableRunTime;", top)
        intervals = {"fields": values["writeInterval"]}
        for name, body in re.findall(r"(interfaceHeight\d+|tankRollMoment)\s*\{([^{}]+)\}", functions):
            assert re.search(r"writeControl\s+timeStep;", body)
            intervals[name] = int(re.search(r"writeInterval\s+(\d+);", body)[1]) * values["deltaT"]
        assert set(intervals) == {"fields", "tankRollMoment", *(f"interfaceHeight{i}" for i in range(1, 7))}
        controls.append((values, intervals))
    medium, refined = [c[0] for c in controls]
    assert [medium["deltaT"], refined["deltaT"]] == pytest.approx([0.001, 0.0005])
    assert [medium["maxDeltaT"], refined["maxDeltaT"]] == pytest.approx([0.001, 0.0005])
    assert len({medium["maxDeltaT"], refined["maxDeltaT"]}) == 2
    assert refined["deltaT"] == medium["deltaT"] / 2
    assert refined["maxDeltaT"] == medium["maxDeltaT"] / 2
    assert [medium["maxCo"], refined["maxCo"]] == [0.5, 0.5]
    assert medium["endTime"] == refined["endTime"]
    assert controls[0][1] == controls[1][1]
    for name, interval in controls[0][1].items():
        assert interval == pytest.approx(min(0.05, medium["endTime"]) if name == "fields"
                                         else 0.001 if name == "tankRollMoment" else 0.005)
        # At the capped step, both cases write at the same simulated times.
        count = int(medium["endTime"] / interval)
        assert [i * interval for i in range(1, count + 1)] == [
            i * controls[1][1][name] for i in range(1, count + 1)]


@pytest.mark.parametrize("direction", ["roll", "pitch"])
@pytest.mark.parametrize("missing", [False, True])
def test_refined_step_reduction_uses_preserved_probe_interval(tmp_path, driver, direction, missing):
    spec = next(s for s in driver.case_specs(smoke=True)
                if s["group"] == "time-step-3d" and s["direction"] == direction)
    spec["delta_t"] = 0.0005
    case = driver.build_case(tmp_path, spec)
    folder = case / "postProcessing/interfaceHeight1/0"
    folder.mkdir(parents=True)
    indices = [i for i in range(7) if not (missing and i == 3)]
    (folder / "height.dat").write_text("\n".join(f"{i * 0.005} 0.45 0" for i in indices) + "\n")
    if missing:
        with pytest.raises(ValueError, match="missing sampling interval"):
            driver._signal(case, "interfaceHeight1", "height.dat", spec)
    else:
        times, heights = driver._signal(case, "interfaceHeight1", "height.dat", spec)
        assert times == pytest.approx([i * 0.005 for i in indices])
        assert heights == pytest.approx([0.45] * len(indices))


@pytest.mark.parametrize("direction", ["roll", "pitch"])
@pytest.mark.parametrize("smoke", [False, True])
def test_time_step_study_has_three_rendered_levels(tmp_path, driver, direction, smoke):
    specs = [s for s in driver.case_specs(smoke=smoke)
             if s["group"] in ("medium-3d", "time-step-3d")
             and s["direction"] == direction and s["ratio"] == 1.0]
    specs.sort(key=lambda s: s["delta_t"], reverse=True)
    assert len(specs) == 3
    common = {k: v for k, v in specs[0].items() if k not in ("name", "group", "delta_t")}
    controls = []
    intervals = []
    normalized = []
    for spec in specs:
        assert {k: v for k, v in spec.items() if k not in ("name", "group", "delta_t")} == common
        case = driver.build_case(tmp_path, spec)
        top, functions = (case / "system/controlDict").read_text().split("functions", 1)
        values = {key: float(re.search(rf"\b{key}\s+([\d.e+-]+);", top)[1])
                  for key in ("deltaT", "maxDeltaT", "maxCo", "endTime", "writeInterval")}
        assert values["deltaT"] == values["maxDeltaT"]
        assert re.search(r"writeControl\s+adjustableRunTime;", top)
        physical = {"fields": values["writeInterval"]}
        for name, body in re.findall(r"(interfaceHeight\d+|tankRollMoment)\s*\{([^{}]+)\}", functions):
            assert re.search(r"writeControl\s+timeStep;", body)
            stride = int(re.search(r"writeInterval\s+(\d+);", body)[1])
            physical[name] = stride * values["deltaT"]
        assert set(physical) == {"fields", "tankRollMoment", *(f"interfaceHeight{i}" for i in range(1, 7))}
        controls.append(values)
        intervals.append(physical)
        # Apart from the step and dependent strides, the controls stay equal.
        top = re.sub(r"(\b(?:deltaT|maxDeltaT)\s+)[\d.e+-]+;", r"\1STEP;", top)
        scale = round(0.001 / values["deltaT"])
        functions = re.sub(r"(writeInterval\s+)(\d+);",
                           lambda match: f"{match[1]}{int(match[2]) // scale};", functions)
        normalized.append(top + "functions" + functions)
    for key in ("deltaT", "maxDeltaT"):
        steps = [values[key] for values in controls]
        assert len(set(steps)) == 3
        assert steps == pytest.approx([0.001, 0.0005, 0.00025])
        assert all(a / b == 2 for a, b in zip(steps, steps[1:]))
    assert {values["maxCo"] for values in controls} == {0.5}
    assert len({values["endTime"] for values in controls}) == 1
    assert normalized[0] == normalized[1] == normalized[2]
    assert intervals[0] == intervals[1] == intervals[2]
    for name, interval in intervals[0].items():
        expected = min(0.05, controls[0]["endTime"]) if name == "fields" else 0.001 if name == "tankRollMoment" else 0.005
        assert interval == pytest.approx(expected)
        count = int(controls[0]["endTime"] / interval)
        times = [[i * values[name] for i in range(1, count + 1)] for values in intervals]
        assert times[0] == times[1] == times[2]


@pytest.mark.parametrize("direction", ["roll", "pitch"])
@pytest.mark.parametrize("missing", [False, True])
def test_quarter_step_reduction_uses_preserved_probe_interval(tmp_path, driver, direction, missing):
    spec = next(s for s in driver.case_specs(smoke=True)
                if s["group"] == "time-step-3d" and s["direction"] == direction)
    # Exercise the reduction at the next step independently of enumeration.
    spec["delta_t"] = 0.00025
    case = driver.build_case(tmp_path, spec)
    folder = case / "postProcessing/interfaceHeight1/0"
    folder.mkdir(parents=True)
    indices = [i for i in range(7) if not (missing and i == 3)]
    (folder / "height.dat").write_text("\n".join(f"{i * 0.005} 0.45 0" for i in indices) + "\n")
    if missing:
        with pytest.raises(ValueError, match="missing sampling interval"):
            driver._signal(case, "interfaceHeight1", "height.dat", spec)
    else:
        times, heights = driver._signal(case, "interfaceHeight1", "height.dat", spec)
        assert times == pytest.approx([i * 0.005 for i in indices])
        assert heights == pytest.approx([0.45] * len(indices))
