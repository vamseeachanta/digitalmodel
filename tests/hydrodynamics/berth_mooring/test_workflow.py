"""Independent arithmetic checks for generic berth preparation.

All fixture numbers are synthetic test inputs, not recommended design values.
Expected values are derived below, independently of the implementation.
"""

import csv
from importlib.metadata import version
import math
from pathlib import Path
import platform

import numpy as np
import pytest
from scipy.integrate import simpson
import yaml

from digitalmodel.hydrodynamics.berth_mooring import (
    build_campaign,
    drag_load,
    evaluate_results,
    load_spec,
    input_digest,
    prepare_history,
    validate_spec,
)

INPUT = (
    Path(__file__).resolve().parents[3]
    / "examples/domains/mooring/berth_mooring/inputs.yml"
)


@pytest.fixture
def spec():
    return load_spec(INPUT)


def test_drag_and_moment():
    """NASA Drag Equation (public web edition, 2026 retrieval).

    F=0.5*1.2*1000*1*10**2=60000 N; at (10,-2),
    a force along +y gives Mz=10*60000=600000 N m.
    Reverse velocity gives the opposite load (signed drag convention).
    """
    load = dict(
        density_kg_m3=1.2,
        area_m2=1000,
        coefficient=1,
        velocity_m_s=[0, 10],
        application_m=[10, -2],
    )
    assert drag_load(load) == pytest.approx([0, 60000, 600000])
    load["velocity_m_s"] = [0, -10]
    assert drag_load(load) == pytest.approx([0, -60000, -600000])
    load["velocity_m_s"] = [0, 0]
    assert drag_load(load) == [0, 0, 0]


def test_time_mapping_and_superposition(spec):
    """Hand calculation: stagger=-200+2*t; times 0,100,200 give -200,0,200.

    Wind=0.5*1.2*1000*10**2=60000 N; current=0.5*1000*200=100000 N.
    The synthetic callback returns [stagger, 2*speed, 3*separation].
    The combined sway is 160004 N and yaw 1600300 N m.
    This tests transport and superposition, not passing hydrodynamics.
    """
    case = dict(
        name="arithmetic", speed_m_s=2, separation_m=100, wind=True, current=True
    )
    spec["sampling"]["times_s"] = [0, 100, 200]

    def forces(staggers, separation, speed):
        return [[x, 2 * speed, 3 * separation] for x in staggers]

    rows = prepare_history(spec, case, force_function=forces)
    requested = {r["time_s"]: r for r in rows}
    assert [requested[t]["stagger_m"] for t in (0, 100, 200)] == [-200, 0, 200]
    assert all(r["sway_n"] == pytest.approx(160004) for r in rows)
    assert all(r["yaw_nm"] == pytest.approx(1600300) for r in rows)


def test_history_resolves_requested_intervals(spec):
    """Every requested time is retained; no interval exceeds the analysis step."""
    spec["sampling"]["times_s"] = [0, 0.15, 0.31]
    case = build_campaign(spec)[4]
    rows = prepare_history(
        spec, case, force_function=lambda xs, *_: [[0, 0, 0] for _ in xs]
    )
    times = [r["time_s"] for r in rows]
    assert set(spec["sampling"]["times_s"]) <= set(times)
    assert all(0 < b - a <= 0.1 + 1e-12 for a, b in zip(times, times[1:]))


@pytest.mark.parametrize(
    "change",
    [
        lambda s: s["wind"].update(coefficient=float("nan")),
        lambda s: s["current"].update(area_m2=-1),
        lambda s: s["sampling"].update(times_s=[0, 0, 1]),
        lambda s: s["sampling"].update(times_s=[0, 2, 1]),
        lambda s: s["passages"][0].update(separation_m=20),
        lambda s: s["passages"][0].update(speed_m_s=0),
        lambda s: s.update(depth_m=5),
        lambda s: s["fenders"][0].update(length_force_kn=[[0, -1], [1, -2]]),
        lambda s: s["fenders"][0].update(length_force_kn=[[0, -1], [1, 0]]),
        lambda s: s["lines"][0].update(unstretched_length_m=0),
        lambda s: s["lines"][0].update(name="../escape"),
        lambda s: s["basis"].clear(),
    ],
)
def test_reject_invalid_inputs(spec, change):
    change(spec)
    with pytest.raises(ValueError):
        validate_spec(spec)


def test_missing_criteria_stays_unevaluated(spec):
    assert all(r["status"] == "NOT_EVALUATED" for r in evaluate_results(spec, None))


def test_stored_result_reduction(spec):
    """Hand calculation: max(20,80)/100=0.8; max(30,120)/100=1.2.

    Capacities here are synthetic required inputs, not standard acceptance limits.
    A peak exactly equal to its supplied allowable passes by definition.
    """
    names = [c["name"] for c in build_campaign(spec)]
    spec["criteria"] = {
        item["name"]: dict(allowable_kn=100, source="synthetic test criterion")
        for item in spec["lines"] + spec["fenders"]
    }
    data = dict(
        solver_version="synthetic-test", input_sha256=input_digest(spec), cases={}
    )
    for name in names:
        data["cases"][name] = {item["name"]: [20, 80] for item in spec["lines"]}
        data["cases"][name].update(
            {item["name"]: [30, 120] for item in spec["fenders"]}
        )
    results = evaluate_results(spec, data)
    assert results[0]["utilisation"] == pytest.approx(0.8)
    assert results[0]["status"] == "PASS"
    assert results[-1]["utilisation"] == pytest.approx(1.2)
    assert results[-1]["status"] == "FAIL"
    data["cases"][names[0]][spec["lines"][0]["name"]] = [100]
    assert evaluate_results(spec, data)[0]["status"] == "PASS"


@pytest.mark.parametrize(
    "mutate",
    [
        lambda d: d.update(solver_version=""),
        lambda d: d.update(input_sha256="incorrect"),
        lambda d: d["cases"].update(unexpected={}),
        lambda d: d["cases"].clear(),
        lambda d: next(iter(d["cases"].values())).clear(),
        lambda d: next(iter(d["cases"].values())).update(line_a=[float("inf")]),
        lambda d: next(iter(d["cases"].values())).update(line_a=[-1]),
    ],
)
def test_reject_incomplete_or_invalid_results(spec, mutate):
    data = dict(
        solver_version="synthetic-test",
        input_sha256=input_digest(spec),
        cases={
            c["name"]: {item["name"]: [1] for item in spec["lines"] + spec["fenders"]}
            for c in build_campaign(spec)
        },
    )
    mutate(data)
    with pytest.raises(ValueError):
        evaluate_results(spec, data)


def test_native_yaml_and_campaign(spec, tmp_path):
    """Three explicit passages x four environment combinations = twelve cases.

    Tether EA=1000 kN, l0=10 m, l=11 m gives T=1000*(11-10)/10=100 kN
    by Orcina Links: Data, TensionTether; stiffness must remain EA, not EA/l0.
    Fender table uses negative tension for compression, zero when disengaged.
    """
    from digitalmodel.hydrodynamics.berth_mooring import write_campaign
    from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

    spec["sampling"]["times_s"] = [0, 100, 200]
    output = write_campaign(spec, tmp_path)
    assert len(output["cases"]) == 12
    for case in output["cases"]:
        path = tmp_path / case["model"]
        result = OrcaFlexYAMLValidator().validate_file(path)
        assert result.valid, result.errors
        model = yaml.safe_load(path.read_text())
        assert f"# Case: {case['name']}" in path.read_text()
        assert f"# Force history: {case['history']}" in path.read_text()
        assert "Roll/pitch inertia: NOT_EVALUATED" in path.read_text()
        assert len(model["Links"]) == len(spec["lines"]) + len(spec["fenders"])
        assert model["Links"][0]["Stiffness"] == spec["lines"][0]["ea_kn"]
        assert model["Vessels"][0]["IncludeWindLoad"] == "No"
        assert case["coupled_response"] == "NOT_EVALUATED"
        assert (tmp_path / case["history"]).exists()


def test_unknown_roll_pitch_inertia_is_not_copied_from_yaw(spec):
    """An unprovided inertia is missing data, never the supplied yaw inertia."""
    from digitalmodel.hydrodynamics.berth_mooring import native_model

    draught = native_model(spec)["VesselTypes"][0]["Draughts"][0]
    tensor = draught[
        "MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ"
    ]
    assert tensor == [
        [None, 0, 0], [0, None, 0], [0, 0, spec["moored"]["yaw_inertia_t_m2"]]
    ]


def test_campaign_speed_squared_peaks(spec, tmp_path):
    """Unmodified campaign sampling must preserve the physical U squared scale.

    The first two passages differ only in speed; each passing-only component
    must have a fourfold sampled peak without manually aligning input times.
    """
    from digitalmodel.hydrodynamics.berth_mooring import write_campaign

    cases = write_campaign(spec, tmp_path)["cases"]
    slow, fast = cases[0], cases[4]
    for component in ("surge_n", "sway_n"):
        assert fast["peak_force_n"][component] == pytest.approx(
            4 * slow["peak_force_n"][component], rel=1e-10
        )
    assert fast["peak_yaw_nm"] == pytest.approx(4 * slow["peak_yaw_nm"], rel=1e-10)
    assert len({c["model"] for c in cases}) == 12


def test_manifest_records_observed_runtime(spec, tmp_path):
    """Byte reproduction is bounded by the observed numerical/output runtime."""
    from digitalmodel.hydrodynamics.berth_mooring import write_campaign

    spec["sampling"]["times_s"] = [0, 0.1]
    runtime = write_campaign(spec, tmp_path)["numerical_runtime"]
    assert runtime["python"] == platform.python_version()
    for package in ("numpy", "scipy", "pyyaml", "pydantic", "pandas", "plotly", "jinja2"):
        assert runtime[package] == version(package)


def test_actual_passing_symmetry_and_scaling(spec):
    """Independent invariants of the cited 1975 parabolic-hull equations.

    At zero stagger, symmetric hulls give zero surge/yaw (odd symmetry).
    Doubling speed multiplies each component by four through rho*U**2.
    No magnitude from the implementation is used as a reference value.
    """
    spec["sampling"]["times_s"] = [0, 100, 200]
    slow = dict(name="slow", speed_m_s=1, separation_m=100, wind=False, current=False)
    fast = dict(slow, name="fast", speed_m_s=2)
    # Compare the same staggers using half the elapsed times at double speed.
    a = prepare_history(spec, slow)
    spec["sampling"]["times_s"] = [0, 50, 100]
    b = prepare_history(spec, fast)
    for x, y in zip(a, b):
        for key in ("surge_n", "sway_n", "yaw_nm"):
            assert y[key] == pytest.approx(4 * x[key], abs=1e-7)
    assert a[-1]["surge_n"] == pytest.approx(0, abs=1e-7)
    assert a[-1]["yaw_nm"] == pytest.approx(0, abs=1e-7)


def test_asymmetric_stagger_matches_distributed_load_moment(spec):
    """Establish signed surge and right-handed yaw independently of primitives.

    Integrating the repository's area kernels by parts gives distributed loads
    qx=-rho U² S1 F'/(2 pi), qy=-rho U² eta S1 G'/pi. Direct numerical inner
    integrals use F'=integral S2' (2R²-eta_n²)/(eta_n²+R²)^(5/2),
    G'=integral 3 S2' R/(eta_n²+R²)^(5/2), R=x2-x1+stagger.
    Since x cross y = z, the yaw moment is integral x1*qy, with no sign flip.
    At +50 m stagger these independent signed surge and yaw integrals are positive.
    """
    spec["sampling"].update(times_s=[0, 250], time_step_s=250)
    moored, passing = spec["moored"], spec["passing"]
    x1 = np.linspace(-moored["length_m"] / 2, moored["length_m"] / 2, 401)
    x2 = np.linspace(-passing["length_m"] / 2, passing["length_m"] / 2, 401)
    area = moored["midship_area_m2"] * (1 - 4 * x1**2 / moored["length_m"]**2)
    slope = -8 * passing["midship_area_m2"] * x2 / passing["length_m"]**2
    r = x2[None, :] - x1[:, None] + 50
    qx, qy = np.zeros_like(x1), np.zeros_like(x1)
    scale = spec["water_density_kg_m3"] / math.pi  # U = 1 m/s.
    for image in range(-spec["numerics"]["image_pairs"], spec["numerics"]["image_pairs"] + 1):
        eta_squared = 100**2 + 4 * image**2 * spec["depth_m"]**2
        denominator = (eta_squared + r**2)**2.5
        fp = simpson(slope * (2 * r**2 - eta_squared) / denominator, x=x2, axis=1)
        gp = simpson(3 * slope * r / denominator, x=x2, axis=1)
        qx -= scale * area * fp / 2
        qy -= scale * 100 * area * gp
    expected = [simpson(qx, x=x1), simpson(qy, x=x1), simpson(x1 * qy, x=x1)]
    assert expected[0] > 0 and expected[2] > 0
    case = dict(speed_m_s=1, separation_m=100, wind=False, current=False)
    row = prepare_history(spec, case)[-1]
    assert [row[k] for k in ("surge_n", "sway_n", "yaw_nm")] == pytest.approx(
        expected, rel=1e-6
    )
    # A +y drag load at +x has the same positive yaw convention.
    case["wind"] = True
    assert prepare_history(spec, case)[-1]["yaw_nm"] == pytest.approx(
        expected[2] + 600000, rel=1e-6
    )


@pytest.mark.parametrize("times_s", [[0, 100, 200], [0, 100, 400]])
def test_native_interval_covers_force_history(spec, tmp_path, times_s):
    """Native start plus stage durations must cover every CSV sample.

    Orcina General data: Analysis defines the default start as minus the first
    stage duration. A custom ramp already built up before the start preserves
    the prepared load magnitudes throughout the sampled interval.
    """
    from digitalmodel.hydrodynamics.berth_mooring import write_campaign

    spec["sampling"]["times_s"] = times_s
    manifest = write_campaign(spec, tmp_path)
    for case in manifest["cases"]:
        general = yaml.safe_load((tmp_path / case["model"]).read_text())["General"]
        with (tmp_path / case["history"]).open(newline="") as stream:
            times = [float(row["time_s"]) for row in csv.DictReader(stream)]
        # Resolve the documented native default so the old model fails on its
        # actual interval, rather than merely on the absence of a YAML key.
        start = general.get("StartTime", -general["StageDuration"][0])
        finish = start + sum(general["StageDuration"])
        assert (start, finish) == (times[0], times[-1])
        assert general["FirstStage"] == 1
        assert general["RampStartTime"] < general["RampFinishTime"] < start
