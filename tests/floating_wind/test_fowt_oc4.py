"""Second reference case: NREL 5 MW on the OC4-DeepCwind semi-submersible.

Comparators: Robertson et al. (2014) NREL/TP-5000-60601 Tables 2-2, 4-6 and
Eq. (5-14)/(5-15), recorded in basis.yml, and the OpenFAST linearisation held
in OpenFAST/r-test (periods recorded in basis.yml by the extraction script).
"""
from __future__ import annotations

import copy
import importlib.util
import json
import os
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.floating_wind.fowt_static_checks import (
    equilibrium,
    model_from_spec,
    natural_periods,
)
from digitalmodel.floating_wind.oc4_orcaflex_spec import FOLDER, build_oc4_spec, load_sources
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.routers.fowt_router import FowtRouter
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.fowt import FowtSpec

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt" / "oc4-deepcwind"
COMMIT = "dd5feaaaa500ba7283140107806300d551cff0a7"


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def spec(sources):
    return build_oc4_spec(*sources)


@pytest.fixture(scope="module")
def checks(spec, sources):
    _, data = sources
    model = model_from_spec(spec)
    still = equilibrium(model)
    return {"model": model, "still": still,
            "natural": natural_periods(model, data, still["position"])}


def _leaves(node, path=()):
    if isinstance(node, dict):
        if "value" in node and "source" in node:
            yield path, node
            return
        for key, child in node.items():
            yield from _leaves(child, path + (key,))


def test_every_value_sourced(sources):
    basis, data = sources
    leaves = list(_leaves(basis)) + list(_leaves(data))
    assert len(leaves) >= 25
    for path, leaf in leaves:
        assert leaf.get("unit"), path
        assert "NREL/TP-5000-60601" in leaf["source"] or COMMIT in leaf["source"] \
            or "aggregation" in leaf["source"] or "NREL/CP-5000-61154" in leaf["source"], path


def test_components_sum_to_system(sources):
    basis, _ = sources
    parts = basis["system"]["components"]
    assert sum(p["mass"] for p in parts) == pytest.approx(basis["system"]["mass"]["value"])
    assert all(p["source"] for p in parts)


def test_tower_integration_against_report_table_2_2(sources):
    basis, _ = sources
    tower = next(p for p in basis["system"]["components"] if p["name"] == "tower")
    assert tower["mass"] == pytest.approx(249718.0, rel=1e-4)
    assert tower["centre"][2] == pytest.approx(43.4, rel=0.01)


def test_spec_validates_generates_and_matches_committed(spec, tmp_path):
    ProjectInputSpec(**copy.deepcopy(spec))
    ModularModelGenerator.from_spec(ProjectInputSpec(**copy.deepcopy(spec))).generate(tmp_path)
    assert (tmp_path / "master.yml").exists()
    committed = yaml.safe_load((EXAMPLE / "spec.yml").read_text(encoding="utf-8"))
    assert committed == spec


def test_unpublished_breaking_load_left_empty(spec):
    routed = FowtRouter().route(FowtSpec(**spec["fowt"]))
    assert routed["line_types"][0]["properties"]["AllowableTension"] is None


def test_still_water_balanced(checks):
    q = checks["still"]["position"]
    assert abs(q[2]) < 0.01 and abs(np.degrees(q[4])) < 0.2


MOORING = [((0, 0), 7.08e4, 0.03), ((2, 2), 1.91e4, 0.03), ((4, 4), 8.73e7, 0.03),
           ((5, 5), 1.17e8, 0.03), ((0, 4), -1.08e5, 0.05)]


@pytest.mark.parametrize("index,published,tolerance", MOORING)
def test_mooring_stiffness_against_report_eq_5_15(checks, index, published, tolerance):
    k = checks["natural"]["mooring_stiffness"]
    assert k[index] == pytest.approx(published, rel=tolerance)


def test_mooring_vertical_preload_against_report_eq_5_14(checks):
    assert checks["still"]["mooring_load"][2] == pytest.approx(-1.839e6, rel=0.03)


@pytest.mark.parametrize("mode,tolerance", [("surge", 0.10), ("heave", 0.05),
                                            ("pitch", 0.05), ("yaw", 0.10)])
def test_periods_against_openfast_linearisation(checks, sources, mode, tolerance):
    basis, _ = sources
    lin = basis["comparators"]["openfast_linearisation_periods"]["value"][mode]
    assert checks["natural"]["periods"][mode] == pytest.approx(lin, rel=tolerance)


def test_surge_difference_explained_by_infinite_frequency_added_mass(checks, sources):
    """With A(infinity) the surge period moves to within 1 % of the linearisation."""
    basis, data = sources
    lin = basis["comparators"]["openfast_linearisation_periods"]["value"]["surge"]
    inf = natural_periods(checks["model"], data, checks["still"]["position"],
                          infinite_frequency_added_mass=True)["periods"]["surge"]
    assert inf == pytest.approx(lin, rel=0.01)
    assert checks["natural"]["periods"]["surge"] > lin * 1.05


def test_committed_results_reproduce(checks):
    committed = json.loads((EXAMPLE / "results.json").read_text(encoding="utf-8"))
    assert committed["natural_periods"]["pitch"] == pytest.approx(
        checks["natural"]["periods"]["pitch"], rel=1e-6)
    assert all(c["within_tolerance"] for c in committed["comparisons"])
    assert all(v["inside_band"] for v in committed["phase_ii_text_frequencies"].values())


@pytest.mark.parametrize("perturbation", [-1e-12, 1e-12])
def test_linearisation_precision_stable_under_roundoff(tmp_path, monkeypatch, perturbation):
    """Serialize derived modes at eight significant digits across eigensolver noise."""
    script = ROOT / "scripts" / "fowt" / "extract_oc4_generator_data.py"
    module_spec = importlib.util.spec_from_file_location("precision_extraction", script)
    module = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(module)
    periods = np.array([106.89211469, 106.97127140, 17.28332719,
                        25.32632935, 25.44747689, 80.22157269])
    damping = np.array([0.00916746364, 0.00919129334, 0.01136255032,
                        0.01098305263, 0.01105023581, 0.00509299695])
    omega = 2 * np.pi / periods
    matrix = np.block([[np.zeros((6, 6)), np.eye(6)],
                       [-np.diag(omega ** 2), -np.diag(2 * damping * omega)]])
    path = tmp_path / "synthetic.lin"
    path.write_text("A: 12\n" + "\n".join(" ".join(map(str, row)) for row in matrix),
                    encoding="utf-8")
    baseline = module.linearisation_periods(path)
    for mode, period, ratio in zip(baseline.values(), periods, damping):
        assert mode["period"] == float(f"{period:.8g}")
        assert mode["damping_ratio"] == float(f"{ratio:.8g}")
        assert mode["share"] == 1.0

    eig = np.linalg.eig

    def perturbed_eig(matrix):
        values, vectors = eig(matrix)
        return values[::-1] * (1 + perturbation), vectors[:, ::-1] * (1 - perturbation)

    monkeypatch.setattr(module.np.linalg, "eig", perturbed_eig)
    assert yaml.safe_dump(module.linearisation_periods(path)) == yaml.safe_dump(baseline)


@pytest.mark.skipif(not os.environ.get("DIGITALMODEL_OC4_RAW"),
                    reason="raw r-test files not present; set DIGITALMODEL_OC4_RAW")
def test_extraction_reproduces_committed_files(tmp_path):
    script = ROOT / "scripts" / "fowt" / "extract_oc4_generator_data.py"
    module_spec = importlib.util.spec_from_file_location("extract_oc4", script)
    module = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(module)
    module.main(["x", os.environ["DIGITALMODEL_OC4_RAW"], str(tmp_path)])
    for name in ("basis.yml", "generator_data.yml"):
        assert (tmp_path / name).read_bytes() == (FOLDER / name).read_bytes(), name


@pytest.mark.parametrize("script_name,case", [
    ("write_oc4_run_packages.py", "LC6_yaw_decay"),
    ("write_iea15_run_packages.py", "LC5_parked_extreme"),
])
def test_run_packages_retain_existing_results(tmp_path, script_name, case):
    """Refuse a nonempty output before changing any package or its index."""
    script = ROOT / "scripts" / "fowt" / script_name
    module_spec = importlib.util.spec_from_file_location("retention_packages", script)
    module = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(module)
    result = tmp_path / case / "model" / "saved.sim"
    result.parent.mkdir(parents=True)
    result.write_bytes(b"saved solver result")
    (tmp_path / "index.json").write_bytes(b"saved index")
    before = {p.relative_to(tmp_path): p.read_bytes()
              for p in tmp_path.rglob("*") if p.is_file()}

    with pytest.raises(FileExistsError, match="empty"):
        module.main(["x", str(tmp_path)])

    after = {p.relative_to(tmp_path): p.read_bytes()
             for p in tmp_path.rglob("*") if p.is_file()}
    assert after == before


def test_run_packages_written(tmp_path):
    """Licensed-run packages: statics plus four decays, each loadable spec and run.md."""
    script = ROOT / "scripts" / "fowt" / "write_oc4_run_packages.py"
    module_spec = importlib.util.spec_from_file_location("oc4_packages", script)
    module = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(module)
    assert module.main(["x", str(tmp_path)]) == 0
    index = json.loads((tmp_path / "index.json").read_text(encoding="utf-8"))
    names = [case["case"] for case in index["cases"]]
    assert names == ["LC0_statics", "LC1_surge_decay", "LC2_heave_decay", "LC3_pitch_decay",
                     "LC6_yaw_decay"]
    yaw = yaml.safe_load((tmp_path / "LC6_yaw_decay" / "spec.yml").read_text(encoding="utf-8"))
    assert yaw["fowt"]["hull"]["initial_orientation"][2] == 8.0
    assert yaw["fowt"]["hull"]["included_in_statics"] == "None"
    for name in names:
        FowtSpec(**yaml.safe_load((tmp_path / name / "spec.yml").read_text(encoding="utf-8"))["fowt"])
        assert (tmp_path / name / "model" / "master.yml").exists()
        text = (tmp_path / name / "run.md").read_text(encoding="utf-8")
        assert "validated" in text and "Expected" in text
    surge = (tmp_path / "LC1_surge_decay" / "run.md").read_text(encoding="utf-8")
    assert "106.89 s" in surge and "10 %" in surge
