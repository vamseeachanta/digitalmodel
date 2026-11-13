# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Licence-free checks of the generated IEA 15 MW / VolturnUS-S fowt model.

Comparators: published values recorded in basis.yml (Allen et al. 2020,
NREL/TP-5000-76773, Tables 6 and 11), the repository's quasi-static analysis
of the same platform (#2473, docs/reports/fowt-volturnus-mooring-analysis
manifest), and two existing repository catenary solvers.
"""
from __future__ import annotations

import copy
import hashlib
import json
import math
import os
import shutil
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.floating_wind.fowt_static_checks import (
    catenary_endpoint,
    model_from_spec,
    natural_periods,
    rigid_mass_matrix,
    run_checks,
    solve_catenary,
)
from digitalmodel.floating_wind.iea15_orcaflex_spec import (
    WAVE_CASES,
    build_spec,
    load_sources,
)
from digitalmodel.floating_wind.run_package_folder import (
    MANIFEST_NAME,
    prepare_case_folder,
    record_case_folder,
)

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "docs" / "reports" / "fowt-volturnus-mooring-analysis.manifest.json"


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def results(sources):
    basis, data = sources
    spec = build_spec(basis, data)
    turbine = data["turbine"]
    return run_checks(spec, data, basis["turbine"]["rated_rotor_thrust"]["value"],
                      [turbine["hub_overhang"]["value"], 0.0, turbine["hub_height"]["value"]],
                      turbine["shaft_tilt"]["value"])


@pytest.fixture(scope="module")
def previous():
    return json.loads(MANIFEST.read_text(encoding="utf-8"))["results"]


# ------------------------------------------------------------- catenary
def test_catenary_round_trip():
    x, z = catenary_endpoint(1.3e6, 2.0e6, 850.0, 5844.0, 3.27e9)
    h, v = solve_catenary(x, z, 850.0, 5844.0, 3.27e9)
    assert h == pytest.approx(1.3e6, rel=1e-8)
    assert v == pytest.approx(2.0e6, rel=1e-8)


def test_grounded_inextensible_limit_matches_repo_tool():
    from digitalmodel.marine_ops.fowt_quasi_static.mooring import grounded_horizontal_tension

    span, rise, length, w = 780.0, 186.0, 850.0, 5844.0
    h, _ = solve_catenary(span, rise, length, w, 1e15)
    assert h == pytest.approx(grounded_horizontal_tension(span, rise, length, w), rel=1e-6)


def _integrate(h: float, v: float, length: float, w: float, ea: float) -> np.ndarray:
    """Endpoint of an elastic catenary by direct integration from the anchor."""
    from scipy.integrate import solve_ivp

    def slope(s, _):
        tv = v - w * (length - s)
        t = math.hypot(h, tv)
        return [h / t * (1 + t / ea), tv / t * (1 + t / ea)]

    return solve_ivp(slope, [0, length], [0, 0], rtol=1e-11, atol=1e-9).y[:, -1]


def test_suspended_elastic_line_against_direct_integration():
    span, rise, length, w, ea = 550.0, 400.0, 690.0, 1000.0, 1e9
    h, v = solve_catenary(span, rise, length, w, ea, guess=(3e5, 6e5))
    assert v > w * length  # fully suspended
    assert _integrate(h, v, length, w, ea) == pytest.approx([span, rise], abs=1e-6)


def test_suspended_inextensible_line_matches_repo_catenary_solver():
    """Agreement in the inextensible limit.

    For EA = 1e9 N the repository CatenarySolver returns a horizontal tension
    6 % above the direct-integration value on this geometry; that difference
    is reported separately and is not a comparator here.
    """
    from digitalmodel.marine_ops.marine_analysis.catenary.solver import (
        CatenaryInput,
        CatenarySolver,
    )

    span, rise, length, w, ea = 550.0, 400.0, 690.0, 1000.0, 1e14
    h, v = solve_catenary(span, rise, length, w, ea, guess=(3e5, 6e5))
    other = CatenarySolver().solve(CatenaryInput(length=length, horizontal_span=span,
                                                 vertical_span=rise, weight_per_length=w,
                                                 ea_stiffness=ea))
    assert h == pytest.approx(other.horizontal_tension, rel=1e-5)
    assert v == pytest.approx(other.vertical_tension_fairlead, rel=1e-5)


def test_rigid_mass_matrix_symmetric(sources):
    model = model_from_spec(build_spec(*sources))
    mass = rigid_mass_matrix(model)
    assert np.allclose(mass, mass.T)
    assert mass[0, 4] == pytest.approx(model.mass * model.cog[2])


def test_flexible_representation_rejected_by_rigid_checks(sources):
    """A flexible hull holds the platform only; hull-only checks would drop tower and RNA."""
    spec = copy.deepcopy(build_spec(*sources))
    fowt = spec["fowt"]
    fowt["representation"] = "flexible"
    fowt["tower"] = {"base_elevation": 15.0, "sections": [
        {"length": 129.386, "outer_diameter": 8.0, "mass_per_length": 10.0,
         "bending_stiffness": 1.0e9, "axial_stiffness": 1.0e8, "torsional_stiffness": 1.0e9,
         "poisson_ratio": 0.3}]}  # synthetic fixture values, not reference-design data
    fowt["rna"] = {"mass": 945.77, "centre_of_mass_from_tower_top": [-7.32, 0.0, 4.6],
                   "inertia_about_centre_of_mass": [[4.0e5, 0, 0], [0, 2.7e5, 0], [0, 0, 2.9e5]]}
    with pytest.raises(NotImplementedError, match="flexible representation"):
        model_from_spec(spec)


# ---------------------------------------------------- still water (LC0)
def test_lc0_equilibrium_converged(results):
    assert np.max(np.abs(results["still_water"]["residual"])) < 1.0


def test_lc0_fairlead_tension_against_allen_table_6(results, sources):
    basis, _ = sources
    published = basis["mooring"]["published_pretension"]["value"]
    for line in results["still_water"]["lines"]:
        assert line["tension"] == pytest.approx(published, rel=0.02)


def test_lc0_net_buoyancy_carried_by_mooring(results):
    vertical = -results["still_water"]["mooring_load"][2]
    hydro_weight = results["net_buoyancy"]
    heave = results["still_water"]["position"][2]
    assert heave < 0  # the platform sinks until mooring weight balances buoyancy
    assert vertical > hydro_weight


# ------------------------------------------------------- natural periods
TOLERANCE = {"surge": 0.10, "heave": 0.05, "pitch": 0.05, "yaw": 0.10}
INDEX = {"surge": 0, "heave": 2, "pitch": 4, "yaw": 5}


@pytest.mark.parametrize("mode", ["surge", "heave", "pitch", "yaw"])
def test_natural_period_against_allen_table_11(results, sources, mode):
    basis, _ = sources
    published = basis["published"]["natural_periods"]["value"][INDEX[mode]]
    computed = results["natural"]["periods"][mode]
    assert computed == pytest.approx(published, rel=TOLERANCE[mode])


@pytest.mark.parametrize("mode", ["surge", "heave", "pitch", "yaw"])
def test_natural_period_against_repository_bem_analysis(results, previous, mode):
    computed = results["natural"]["periods"][mode]
    assert computed == pytest.approx(previous["periods"]["at_tower_top"][INDEX[mode]], rel=0.02)


def test_mooring_stiffness_against_repository_moorpy_model(results, previous):
    ours = np.diag(results["natural"]["mooring_stiffness"])
    theirs = np.diag(np.array(previous["mooring_stiffness"]))
    for i in (0, 1, 2, 5):
        assert ours[i] == pytest.approx(theirs[i], rel=0.02)
    for i in (3, 4):  # rotational terms depend on the trimmed equilibrium
        assert ours[i] == pytest.approx(theirs[i], rel=0.03)


# ------------------------------------------------------ rated thrust (LC4)
def test_surge_only_offset_against_repository_analysis(results, previous):
    """Rotations and draft held, horizontal thrust: the #2473 assumptions."""
    operating = next(c for c in previous["cases"] if c["sea"] == "operating"
                     and c["state"] == "intact" and c["heading"] == 0.0)
    assert results["loaded_surge_only"]["position"][0] == pytest.approx(
        operating["mean"][0], rel=0.05)


def test_free_offset_and_pitch_bounded(results):
    q = results["loaded_free"]["position"]
    assert np.max(np.abs(results["loaded_free"]["residual"])) < 1.0
    assert 15.0 < q[0] < 30.0  # inside the fowt_mooring atlas watch-circle axis
    assert 0.0 < math.degrees(q[4]) < 10.0
    upwind = results["loaded_free"]["lines"][0]
    assert upwind["utilisation"] < 1.0


# ------------------------------------------------- load cases and packages
EQ = {"surge": 0.387, "heave": -0.382, "pitch": -1.417}


def test_load_case_matrix(sources):
    from digitalmodel.floating_wind.iea15_orcaflex_spec import load_case_specs
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    basis, data = sources
    cases = load_case_specs(basis, data, EQ)
    assert list(cases) == ["LC0_statics", "LC1_surge_decay", "LC2_heave_decay",
                           "LC3_pitch_decay", "LC4a_rated_thrust_statics", "LC4_operating",
                           "LC5_parked_extreme"]
    for spec in cases.values():
        # Validate a copy: the Waves validator moves flat keys into 'trains' in place.
        ProjectInputSpec(**copy.deepcopy(spec))
    surge = cases["LC1_surge_decay"]["fowt"]["hull"]
    assert surge["included_in_statics"] == "None"
    assert surge["initial_position"] == pytest.approx([EQ["surge"] + 10.0, 0.0, EQ["heave"]])
    pitch = cases["LC3_pitch_decay"]["fowt"]["hull"]
    assert pitch["initial_orientation"][1] == pytest.approx(EQ["pitch"] + 5.0)
    heave = cases["LC2_heave_decay"]["fowt"]["hull"]
    assert heave["initial_position"][2] == pytest.approx(EQ["heave"] - 2.0)
    rated = cases["LC4a_rated_thrust_statics"]["fowt"]["rotor_thrust"]["thrust"]
    assert rated * 1000 == pytest.approx(basis["turbine"]["rated_rotor_thrust"]["value"])
    op = cases["LC4_operating"]
    assert op["environment"]["raw_properties"]["WaveTrains"][0]["WaveTp"] == pytest.approx(
        basis["environment"]["operating"]["peak_period"]["value"])
    assert op["environment"]["raw_properties"]["WaveTrains"][0]["WaveGamma"] == pytest.approx(
        basis["environment"]["operating"]["jonswap_gamma"]["value"])
    # The generated vessel type carries no load RAOs, so the generated model
    # keeps first-order wave loading off; run.md switches it on after the
    # licensed WAMIT import supplies the RAOs.
    assert WAVE_CASES == ("LC4_operating", "LC5_parked_extreme")
    for name in WAVE_CASES:
        assert cases[name]["fowt"]["hull"].get("include_wave_load_first_order", False) is False
    parked = cases["LC5_parked_extreme"]
    assert "rotor_thrust" not in parked["fowt"]
    assert parked["environment"]["waves"]["height"] == pytest.approx(10.7)


def test_sea_state_emits_peak_period_not_zero_crossing(sources, tmp_path):
    import yaml as _yaml

    from digitalmodel.floating_wind.iea15_orcaflex_spec import load_case_specs
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    spec = load_case_specs(*sources, EQ)["LC5_parked_extreme"]
    ModularModelGenerator.from_spec(ProjectInputSpec(**spec)).generate(tmp_path)
    env = _yaml.safe_load((tmp_path / "includes" / "03_environment.yml").read_text())
    train = env["Environment"]["WaveTrains"][0]
    assert train["WaveType"] == "JONSWAP"
    assert train["WaveTp"] == pytest.approx(14.2)
    assert "WaveTz" not in train
    assert train["WaveGamma"] == pytest.approx(2.75)
    assert train["WaveHs"] == pytest.approx(10.7)


def test_committed_results_reproduce(results):
    committed = json.loads((ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt"
                            / "iea15-volturnus-s" / "results.json").read_text(encoding="utf-8"))
    assert committed["natural_periods"]["heave"] == pytest.approx(
        results["natural"]["periods"]["heave"], rel=1e-6)
    assert committed["rated_thrust_free"]["surge"] == pytest.approx(
        results["loaded_free"]["position"][0], rel=1e-6)
    assert all(c["within_tolerance"] for c in committed["comparisons"])


def test_run_packages_written(tmp_path):
    import importlib.util

    script = ROOT / "scripts" / "fowt" / "write_iea15_run_packages.py"
    spec = importlib.util.spec_from_file_location("packages", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    assert module.main(["x", str(tmp_path)]) == 0
    index = json.loads((tmp_path / "index.json").read_text())
    assert len(index["cases"]) == 7
    for case in index["cases"]:
        folder = tmp_path / case["case"]
        assert (folder / "model" / "master.yml").exists()
        text = (folder / "run.md").read_text()
        assert "## Expected" in text or "## Limitations" in text
        assert "validated" in text  # the do-not-overclaim instruction is present


def test_wave_packages_include_wamit_import_output(tmp_path, monkeypatch):
    """The importer needs the first-order text output, with a selectable suffix.

    Placeholder bytes stand in for the public files here, so their digests are
    pinned for this test only; the real pins are tested separately below.
    """
    import importlib.util

    script = ROOT / "scripts" / "fowt" / "write_iea15_run_packages.py"
    spec = importlib.util.spec_from_file_location("packages", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    source = tmp_path / "source"
    source.mkdir()
    stem = Path(module.WAMIT[0]).stem
    output_name = f"{stem}_1stOrder.out.wamit"
    import_name = Path(output_name).with_suffix("").name
    files = {f"{stem}{suffix}": f"reference {suffix}\r\n".encode()
             for suffix in (".1", ".3", ".hst")}
    files[output_name] = b"first-order WAMIT output\r\n"
    for name, content in files.items():
        (source / name).write_bytes(content)
    monkeypatch.setattr(module, "WAMIT_SHA256", {
        name: hashlib.sha256(content).hexdigest() for name, content in files.items()})

    out = tmp_path / "packages"
    assert module.main(["x", str(out), str(source)]) == 0
    index = json.loads((out / "index.json").read_text())
    wave_cases = [case for case in index["cases"] if case["needs_wamit_import"]]
    assert len(wave_cases) == 2
    for case in index["cases"]:
        folder = out / case["case"]
        if not case["needs_wamit_import"]:
            assert not (folder / "hydro").exists()
            continue
        expected = {import_name if name == output_name else name: content
                    for name, content in files.items()}
        assert {p.name: p.read_bytes() for p in (folder / "hydro").iterdir()} == expected
        instructions = (folder / "run.md").read_text()
        assert f"`hydro/{import_name}`" in instructions
        assert "requested data" in instructions
        assert "centre of mass" in instructions
        assert "phase origins" in instructions
        assert "Wave load (1st order)" in instructions


def _packages_module():
    import importlib.util

    script = ROOT / "scripts" / "fowt" / "write_iea15_run_packages.py"
    spec = importlib.util.spec_from_file_location("packages", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_run_package_rerun_keeps_saved_results(tmp_path):
    """A rerun must never delete files the generator did not write (e.g. .sim results)."""
    module = _packages_module()
    assert module.main(["x", str(tmp_path)]) == 0
    first = (tmp_path / "LC0_statics" / "model" / "master.yml").read_bytes()
    # An untouched package regenerates in place.
    assert module.main(["x", str(tmp_path)]) == 0
    assert (tmp_path / "LC0_statics" / "model" / "master.yml").read_bytes() == first
    saved = tmp_path / "LC0_statics" / "model" / "LC0_statics.sim"
    saved.write_bytes(b"solver results")
    with pytest.raises(FileExistsError, match="LC0_statics.sim"):
        module.main(["x", str(tmp_path)])
    assert saved.read_bytes() == b"solver results"
    assert (tmp_path / "LC0_statics" / "model" / "master.yml").read_bytes() == first


def test_case_folder_refuses_unrecorded_or_edited_content(tmp_path):
    folder = tmp_path / "case"
    prepare_case_folder(folder)  # absent: created
    (folder / "sub").mkdir()
    (folder / "sub" / "a.yml").write_text("a", encoding="utf-8")
    record_case_folder(folder)
    assert (folder / MANIFEST_NAME).is_file()
    (folder / "sub" / "a.yml").write_text("edited", encoding="utf-8")
    with pytest.raises(FileExistsError, match="a.yml"):
        prepare_case_folder(folder)
    assert (folder / "sub" / "a.yml").read_text(encoding="utf-8") == "edited"
    (folder / "sub" / "a.yml").write_text("a", encoding="utf-8")
    prepare_case_folder(folder)  # unchanged generated content: cleared
    assert folder.is_dir() and not any(folder.iterdir())
    legacy = tmp_path / "legacy"
    legacy.mkdir()
    (legacy / "run.md").write_text("old", encoding="utf-8")
    with pytest.raises(FileExistsError, match="no generator manifest"):
        prepare_case_folder(legacy)  # written before manifests existed: never deleted
    assert (legacy / "run.md").read_text(encoding="utf-8") == "old"
    (folder / "x.txt").write_text("x", encoding="utf-8")
    record_case_folder(folder)
    (folder / MANIFEST_NAME).write_text("{not json", encoding="utf-8")
    with pytest.raises(FileExistsError, match="manifest"):
        prepare_case_folder(folder)
    assert (folder / "x.txt").exists()


# ------------------------------------- spec radiation used by the period check
def _spec_with_scaled_added_mass(sources, factor, entries=None):
    spec = copy.deepcopy(build_spec(*sources))
    radiation = spec["fowt"]["hull"]["radiation"]
    for k in range(len(radiation)) if entries is None else entries:
        radiation[k]["added_mass"] = [[factor * v for v in row]
                                      for row in radiation[k]["added_mass"]]
    return spec


@pytest.mark.parametrize("factor, entries", [
    (4.0, None),        # the review's probe: every spec added-mass matrix times four
    (1.01, [1]),        # one finite-frequency entry, 1 %
    (1.01, [0]),        # the infinite-frequency entry, 1 %
])
def test_natural_periods_reject_spec_radiation_not_matching_the_data(sources, factor, entries):
    """The periods use the data table, so the spec the generator emits must
    hold the same added mass; otherwise the check does not describe the
    generated model."""
    _, data = sources
    model = model_from_spec(_spec_with_scaled_added_mass(sources, factor, entries))
    with pytest.raises(ValueError, match="added mass differs"):
        natural_periods(model, data, np.zeros(6))


def test_natural_periods_reject_spec_missing_a_tabulated_period(sources):
    _, data = sources
    spec = copy.deepcopy(build_spec(*sources))
    del spec["fowt"]["hull"]["radiation"][5]
    with pytest.raises(ValueError, match="radiation periods"):
        natural_periods(model_from_spec(spec), data, np.zeros(6))


def test_run_checks_reject_perturbed_spec_radiation(sources):
    basis, data = sources
    turbine = data["turbine"]
    with pytest.raises(ValueError, match="added mass differs"):
        run_checks(_spec_with_scaled_added_mass(sources, 4.0), data,
                   basis["turbine"]["rated_rotor_thrust"]["value"],
                   [turbine["hub_overhang"]["value"], 0.0, turbine["hub_height"]["value"]],
                   turbine["shaft_tilt"]["value"])


# ------------------------------------------- packaged hydrodynamic source pins
def test_wamit_package_pins_cover_every_packaged_file():
    """Independent pins for all four packaged files; .1 and .hst equal the
    digests the extractor recorded for the same revision."""
    module = _packages_module()
    assert set(module.WAMIT_SHA256) == set(module.WAMIT)
    _, data = load_sources()
    recorded = {Path(k).name: v for k, v in data["source_sha256"].items()}
    stem = Path(module.WAMIT[0]).stem
    for name in (f"{stem}.1", f"{stem}.hst"):
        assert module.WAMIT_SHA256[name] == recorded[name]
    assert all(len(v) == 64 and int(v, 16) >= 0 for v in module.WAMIT_SHA256.values())


def _placeholder_wamit(module, folder):
    folder.mkdir()
    for name in module.WAMIT:
        (folder / name).write_bytes(b"not the pinned file\n")


def test_wave_packages_refuse_unpinned_wamit_bytes(tmp_path):
    """Stale or altered files are refused before any package is written."""
    module = _packages_module()
    source = tmp_path / "source"
    _placeholder_wamit(module, source)
    out = tmp_path / "packages"
    with pytest.raises(ValueError, match="WAMIT source digest mismatch"):
        module.main(["x", str(out), str(source)])
    assert not out.exists()


def test_unpinned_wamit_bytes_leave_existing_packages_unchanged(tmp_path):
    module = _packages_module()
    out = tmp_path / "packages"
    assert module.main(["x", str(out)]) == 0
    before = {p.relative_to(out): p.read_bytes() for p in out.rglob("*") if p.is_file()}
    source = tmp_path / "source"
    _placeholder_wamit(module, source)
    with pytest.raises(ValueError, match="WAMIT source digest mismatch"):
        module.main(["x", str(out), str(source)])
    after = {p.relative_to(out): p.read_bytes() for p in out.rglob("*") if p.is_file()}
    assert after == before


@pytest.mark.skipif(not os.environ.get("DIGITALMODEL_IEA15_RAW"),
                    reason="raw IEA files not present; set DIGITALMODEL_IEA15_RAW")
def test_wave_packages_accept_pinned_files_and_refuse_one_altered_byte(tmp_path):
    module = _packages_module()
    raw = Path(os.environ["DIGITALMODEL_IEA15_RAW"])
    source = tmp_path / "source"
    source.mkdir()
    for name in module.WAMIT:
        shutil.copyfile(raw / name, source / name)
    module.verify_wamit_sources(source)  # the pinned set is accepted
    radiation = source / f"{Path(module.WAMIT[0]).stem}.3"
    content = bytearray(radiation.read_bytes())
    content[-2] = ord("0") if content[-2] != ord("0") else ord("1")
    radiation.write_bytes(bytes(content))
    with pytest.raises(ValueError, match=f"WAMIT source digest mismatch: {radiation.name}"):
        module.verify_wamit_sources(source)


# ---------------------------------------------- inertia qualification carried
def test_committed_results_carry_the_inertia_qualification(sources):
    basis, _ = sources
    committed = json.loads((ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt"
                            / "iea15-volturnus-s" / "results.json").read_text(encoding="utf-8"))
    qualification = committed["qualification"]
    assert qualification["readiness"] == basis["qualification"]["readiness"]
    assert qualification["use_for_analysis"] == basis["qualification"]["use_for_analysis"]
    assert "RNA" in qualification["natural_periods"]
    assert "assumed" in qualification["natural_periods"]
    readme = (ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt"
              / "iea15-volturnus-s" / "README.md").read_text(encoding="utf-8")
    established = readme.split("Established without a licence")[1].split("Not established")[0]
    period_item = next(item for item in established.split("\n- ") if "natural periods" in item)
    assert "assumed" in period_item and "RNA" in period_item
