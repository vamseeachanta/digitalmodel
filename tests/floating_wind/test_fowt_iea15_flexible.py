"""IEA 15 MW / VolturnUS-S flexible representation: finite-element tower and lumped RNA.

Comparators: the whole-system mass properties of basis.yml (aggregated from
the same public sources by the #2473 analysis), the tabulated floating-tower
station values, and the rigid representation of the same design.
"""
from __future__ import annotations

import copy
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.floating_wind.fowt_static_checks import (
    equilibrium,
    flexible_mass_properties,
    model_from_spec,
    natural_periods,
)
from digitalmodel.floating_wind.iea15_orcaflex_spec import (
    build_flexible_spec,
    build_spec,
    load_sources,
    rna_about_own_centre,
    tower_moduli,
    tower_sections,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.routers.fowt_router import FowtRouter
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.fowt import FowtSpec

ROOT = Path(__file__).resolve().parents[2]
SPEC_FILE = ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt" / "iea15-volturnus-s" \
    / "spec-flexible.yml"
GOLDEN = ROOT / "tests" / "solvers" / "orcaflex" / "modular_generator" / "goldens" \
    / "fowt_iea15_flexible_lc0"
COMMIT = "e4993d63de10f165389534461dd544006750fe60"


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def flexible(sources):
    return build_flexible_spec(*sources)


@pytest.fixture(scope="module")
def rigid(sources):
    return build_spec(*sources)


def test_floating_tower_table_sourced(sources):
    _, data = sources
    tower = data["floating_tower"]
    assert "Floating Tower Properties rows 2:21 columns B:K" in tower["source"]
    assert COMMIT in tower["source"]
    rows = tower["value"]
    assert len(rows) == 20 and all(len(row) == 10 for row in rows)
    assert rows[0][0] == 15.0 and rows[-1][0] == pytest.approx(144.386)


def test_tower_table_agrees_with_basis_columns(sources):
    """Columns B:G are also held in basis.yml (tower_table); both readings agree."""
    basis, data = sources
    for ours, theirs in zip(data["floating_tower"]["value"],
                            basis["system"]["tower_table"]["value"]):
        assert ours[:6] == pytest.approx(theirs, rel=1e-12)


def test_station_values_imply_one_steel(sources):
    _, data = sources
    moduli = tower_moduli(data)
    assert moduli["E_bending"] == pytest.approx([2.0e11] * 20, rel=1e-6)
    assert moduli["E_axial"] == pytest.approx([2.0e11] * 20, rel=1e-6)
    assert moduli["G"] == pytest.approx([7.93e10] * 20, rel=1e-6)


def test_tower_sections_length_and_mass(sources):
    basis, data = sources
    sections = tower_sections(data)
    assert len(sections) == 13
    assert sum(s["length"] for s in sections) == pytest.approx(144.386 - 15.0, abs=1e-9)
    mass = sum(s["length"] * s["mass_per_length"] for s in sections) * 1000.0
    assert mass == pytest.approx(basis["system"]["tower_mass"]["value"], rel=1e-4)


def test_constant_span_takes_station_values(sources):
    _, data = sources
    base = data["floating_tower"]["value"][0]
    first = tower_sections(data)[0]
    assert first["bending_stiffness"] == pytest.approx(base[6] / 1000.0)
    assert first["axial_stiffness"] == pytest.approx(base[9] / 1000.0)
    assert first["torsional_stiffness"] == pytest.approx(base[8] / 1000.0)
    assert first["mass_per_length"] == pytest.approx(base[3] / 1000.0)


def test_tapered_span_bounded_by_its_end_stations(sources):
    _, data = sources
    rows = data["floating_tower"]["value"]
    top = tower_sections(data)[-4:]
    for column, key in ((6, "bending_stiffness"), (3, "mass_per_length")):
        values = [s[key] * 1000.0 for s in top]
        assert values == sorted(values, reverse=True)
        assert rows[-1][column] < min(values) and max(values) < rows[-2][column]


def test_rna_inertia_round_trip(sources):
    """Moving the RNA's own-centre inertia back to the tower top gives the adopted tensor."""
    basis, _ = sources
    rna = rna_about_own_centre(basis)
    m = rna["mass"] * 1000.0
    r = np.array(rna["centre_of_mass_from_tower_top"])
    own = np.array(rna["inertia_about_centre_of_mass"]) * 1000.0
    at_top = own + m * (r @ r * np.eye(3) - np.outer(r, r))
    assert at_top == pytest.approx(np.array(basis["system"]["rna_inertia_cg"]["value"]), rel=1e-9)
    assert np.all(np.linalg.eigvalsh(own) > 0)


def test_components_reaggregate_to_system(flexible, sources):
    basis, _ = sources
    system = basis["system"]
    mass, centre, inertia = flexible_mass_properties(FowtSpec(**flexible["fowt"]))
    assert mass == pytest.approx(system["mass"]["value"], rel=1e-5)
    assert centre == pytest.approx(np.array(system["center_of_gravity"]["value"]), abs=1e-3)
    assert np.diag(inertia) == pytest.approx(np.diag(system["inertia_cg"]["value"]), rel=1e-4)


def test_dropped_rna_products_change_only_off_diagonals(flexible):
    fowt = FowtSpec(**flexible["fowt"])
    _, _, full = flexible_mass_properties(fowt)
    _, _, principal = flexible_mass_properties(fowt, principal_rna_inertia=True)
    assert np.diag(principal) == pytest.approx(np.diag(full), rel=1e-12)
    assert abs(principal[0, 2] - full[0, 2]) / full[0, 0] < 0.002


def test_rigid_equivalent_matches_rigid_representation(flexible, rigid, sources):
    _, data = sources
    model_f, model_r = model_from_spec(flexible), model_from_spec(rigid)
    eq_f, eq_r = equilibrium(model_f), equilibrium(model_r)
    # The README states 0.01 % for surge, heave and pitch; sway, roll and yaw are zero.
    for dof in (0, 2, 4):
        assert eq_f["position"][dof] == pytest.approx(eq_r["position"][dof], rel=1e-4), dof
    for dof in (1, 3, 5):
        assert abs(eq_f["position"][dof]) < 1e-9 and abs(eq_r["position"][dof]) < 1e-9, dof
    periods_f = natural_periods(model_f, data, eq_f["position"])["periods"]
    periods_r = natural_periods(model_r, data, eq_r["position"])["periods"]
    for mode in ("surge", "heave", "pitch", "yaw"):
        assert periods_f[mode] == pytest.approx(periods_r[mode], rel=1e-4), mode


def test_flexible_model_carries_spec_radiation(flexible, rigid):
    """The rigid-equivalent body keeps the validated spec radiation for the period check."""
    model_f, model_r = model_from_spec(flexible), model_from_spec(rigid)
    assert len(model_f.radiation_added_mass) == len(flexible["fowt"]["hull"]["radiation"]) > 0
    assert len(model_f.radiation_added_mass) == len(model_r.radiation_added_mass)
    for (period_f, added_f), (period_r, added_r) in zip(model_f.radiation_added_mass,
                                                        model_r.radiation_added_mass):
        assert period_f == period_r
        assert np.array_equal(added_f, added_r)


@pytest.mark.parametrize("infinite", [False, True])
def test_flexible_periods_refuse_spec_added_mass_unlike_the_data(flexible, sources, infinite):
    """Both period variants of the flexible path check the spec radiation against the data."""
    _, data = sources
    perturbed = copy.deepcopy(flexible)
    for entry in perturbed["fowt"]["hull"]["radiation"]:
        entry["added_mass"] = [[4.0 * v for v in row] for row in entry["added_mass"]]
    model = model_from_spec(perturbed)
    with pytest.raises(ValueError, match="added mass differs"):
        natural_periods(model, data, np.zeros(6), infinite_frequency_added_mass=infinite)


def test_hydrostatic_weight_term_uses_platform_only(flexible):
    """The VesselType carries the platform; tower and RNA weight act through their own objects."""
    fowt = FowtSpec(**flexible["fowt"])
    routed = FowtRouter().route(fowt)
    draught = routed["vessel_types"][0]["properties"]["Draughts"][0]
    stiffness = draught["HydrostaticStiffnessz, HydrostaticStiffnessRx, HydrostaticStiffnessRy"]
    hull = fowt.hull
    weight = hull.mass * fowt.gravity * (-hull.centre_of_mass[2])
    assert stiffness[2][2] == pytest.approx(
        hull.hydrostatic_stiffness_excluding_gravity[2][2] + weight)


def test_tower_line_connects_rna_and_hull(flexible):
    routed = FowtRouter().route(FowtSpec(**flexible["fowt"]))
    tower = next(line for line in routed["lines"] if line["name"] == "Tower")
    key = next(k for k in tower["properties"] if k.startswith("Connection, "))
    ends = tower["properties"][key]
    assert ends[0][0] == "RNA" and ends[1][0] == "VolturnUS-S" and ends[1][3] == 15.0
    rna = next(buoy for buoy in routed["buoys_6d"] if buoy["name"] == "RNA")
    assert rna["properties"]["InitialPosition"][2] == pytest.approx(144.386)


def test_example_spec_matches_builder_and_validates(flexible):
    committed = yaml.safe_load(SPEC_FILE.read_text(encoding="utf-8"))
    assert committed == flexible
    ProjectInputSpec(**copy.deepcopy(committed))


def test_golden_flexible_model(tmp_path):
    ModularModelGenerator(SPEC_FILE).generate(tmp_path)
    expected = sorted(p.relative_to(GOLDEN).as_posix() for p in GOLDEN.rglob("*.yml"))
    produced = sorted(p.relative_to(tmp_path).as_posix() for p in tmp_path.rglob("*.yml"))
    assert produced == expected
    for rel in expected:
        got = (tmp_path / rel).read_text(encoding="utf-8").replace("\r\n", "\n")
        want = (GOLDEN / rel).read_text(encoding="utf-8").replace("\r\n", "\n")
        assert got == want, rel


def test_tower_deflection_against_cantilever_formula(flexible):
    """Uniform tower, negligible own weight, RNA weight offset upwind: tip-moment cantilever."""
    from digitalmodel.floating_wind.fowt_static_checks import tower_top_deflection

    fowt = copy.deepcopy(flexible["fowt"])
    length, ei = 100.0, 1.0e9  # m, kN.m2
    fowt["tower"]["sections"] = [{"length": length, "outer_diameter": 8.0,
                                  "mass_per_length": 1.0e-9, "bending_stiffness": ei,
                                  "axial_stiffness": 1.0e8, "torsional_stiffness": 1.0e9,
                                  "poisson_ratio": 0.3}]  # synthetic fixture
    fowt["rna"]["centre_of_mass_from_tower_top"] = [-5.0, 0.0, 0.0]
    spec = FowtSpec(**fowt)
    moment = spec.rna.mass * 1000.0 * spec.gravity * 5.0  # N m, bends toward -x
    result = tower_top_deflection(spec, steps_per_section=400)
    assert result["top_displacement_x"] == pytest.approx(
        -moment * length ** 2 / (2.0 * ei * 1000.0), rel=1e-3)
    assert result["top_rotation_about_y"] == pytest.approx(-moment * length / (ei * 1000.0),
                                                           rel=1e-3)
    assert abs(result["top_displacement_y"]) < 1e-9


def test_tower_deflection_reference_values(flexible):
    """Values quoted in the report and the licensed package (LC0, hull at equilibrium)."""
    from digitalmodel.floating_wind.fowt_static_checks import tower_top_deflection

    spec = FowtSpec(**flexible["fowt"])
    upright = tower_top_deflection(spec)
    assert upright["top_displacement_x"] == pytest.approx(-0.1737, abs=5e-4)
    q = equilibrium(model_from_spec(flexible))["position"]
    pitched = tower_top_deflection(spec, (q[3], q[4], q[5]))
    assert pitched["top_displacement_x"] == pytest.approx(-0.2225, abs=5e-4)
    assert np.degrees(pitched["top_rotation_about_y"]) == pytest.approx(-0.389, abs=1e-3)


def test_tower_poisson_ratio_from_station_moduli(sources):
    """nu = E/(2G) - 1; for a thin tube J = 2I, so nu = EI/GJ - 1 at each station."""
    _, data = sources
    base = data["floating_tower"]["value"][0]
    sections = tower_sections(data)
    assert sections[0]["poisson_ratio"] == pytest.approx(base[6] / base[8] - 1.0, rel=1e-9)
    for section in sections:
        assert section["poisson_ratio"] == pytest.approx(200.0 / (2.0 * 79.3) - 1.0, rel=1e-3)


def test_generated_rna_reproduces_full_tensor(flexible, sources):
    """The emitted RNA pair keeps the source tensor's products of inertia."""
    basis, _ = sources
    from digitalmodel.floating_wind.iea15_orcaflex_spec import rna_about_own_centre

    fowt = FowtSpec(**flexible["fowt"])
    buoys = {b["name"]: b["properties"] for b in FowtRouter().route(fowt)["buoys_6d"]}
    host, child = buoys[fowt.rna.name], buoys[f"{fowt.rna.name} inertia"]
    assert host["MomentsOfInertia"] == [0, 0, 0]
    assert host["Mass"] == pytest.approx(fowt.rna.mass)
    a, b, c = np.radians(child["InitialAttitude"])
    rx = np.array([[1, 0, 0], [0, np.cos(a), -np.sin(a)], [0, np.sin(a), np.cos(a)]])
    ry = np.array([[np.cos(b), 0, np.sin(b)], [0, 1, 0], [-np.sin(b), 0, np.cos(b)]])
    rz = np.array([[np.cos(c), -np.sin(c), 0], [np.sin(c), np.cos(c), 0], [0, 0, 1]])
    r = rx @ ry @ rz
    rebuilt = r @ np.diag(child["MomentsOfInertia"]) @ r.T
    expected = np.array(rna_about_own_centre(basis)["inertia_about_centre_of_mass"])
    assert abs(expected[0, 2]) > 0.1 * expected[1, 1]  # the source tensor has products
    assert rebuilt == pytest.approx(expected, abs=1e-9 * expected.max())
    assert child["InitialAttitude"][1] == pytest.approx(12.75, abs=0.01)


def _rna_child(spec: dict) -> dict:
    fowt = FowtSpec(**spec["fowt"])
    buoys = {b["name"]: b["properties"] for b in FowtRouter().route(fowt)["buoys_6d"]}
    return buoys[f"{fowt.rna.name} inertia"]


def test_rna_child_written_at_stated_precision(flexible):
    """Principal moments at 11 significant figures, attitude at 1e-9 degree."""
    from digitalmodel.solvers.orcaflex.modular_generator.routers import fowt_router

    assert fowt_router.RNA_MOMENT_SIGNIFICANT_FIGURES == 11
    assert fowt_router.RNA_ATTITUDE_DECIMAL_PLACES == 9
    child = _rna_child(flexible)
    assert all(float(f"{m:.11g}") == m for m in child["MomentsOfInertia"])
    assert all(round(a, 9) == a for a in child["InitialAttitude"])


def test_rna_child_unchanged_by_eigensolver_roundoff(flexible, monkeypatch):
    """Eigensolver differences of the size seen between numpy builds (about 1e-14
    relative) do not change the written child buoy, so the golden is stable."""
    reference = _rna_child(flexible)
    exact = np.linalg.eigh

    def perturbed(matrix):
        values, vectors = exact(matrix)
        return values * (1.0 + 3e-14), vectors * (1.0 + 2e-14)

    monkeypatch.setattr(np.linalg, "eigh", perturbed)
    assert _rna_child(flexible) == reference
