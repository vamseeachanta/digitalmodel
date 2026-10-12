"""Offline spike checks; public-field derivation is recorded in the spike note."""
import importlib.util
import math
import shutil
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / "scripts/studies"))
SPEC = importlib.util.spec_from_file_location(
    "surface_crack_post", ROOT / "scripts/studies/surface_crack_post.py"
)
post = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(post)


def test_recovers_known_k_from_textbook_field():
    # Roylance, Introduction to Fracture Mechanics, p. 7, Eq. (4), MIT OCW.
    # sigma_y = K/sqrt(2*pi*r)*cos(theta/2)*
    #           [1+sin(theta/2)*sin(3*theta/2)]. On theta=0 this is
    # K/sqrt(2*pi*r). Convert K from MPa*sqrt(m) to MPa*sqrt(mm).
    # All dimensional inputs and the target K come from the repository wrapper.
    known_k = post.reference_k(90.0)
    n = post.LEVELS[-1]
    radii = [post.BENCHMARK["a_mm"] * (j / n)**2 for j in (1, 2, 3)]
    stresses = [known_k * math.sqrt(1000.0) / math.sqrt(2*math.pi*r)
                for r in radii]
    result = post.extrapolate_stress_k(radii, stresses)
    # Existing CalculiX tests use 1e-10; this is arithmetic, not FE accuracy.
    assert result["k_mpa_sqrt_m"] == pytest.approx(known_k, rel=1e-10)


def test_reference_wrapper_matches_package():
    spec = importlib.util.spec_from_file_location(
        "independent_reference", ROOT / "src/digitalmodel/asset_integrity/assessment/crack_fad.py"
    )
    reference = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = reference
    spec.loader.exec_module(reference)
    newman_raju_k = reference.newman_raju_k
    for phi in (0.0, 90.0):
        assert post.reference_k(phi) == newman_raju_k(
            **post.BENCHMARK, phi_deg=phi
        )


@pytest.mark.parametrize("radii, stresses", [
    ([0.0, 1.0], [1.0, 1.0]),
    ([1.0, 1.0], [1.0, 1.0]),
    ([1.0, 2.0], [1.0]),
    ([1.0, math.inf], [1.0, 1.0]),
])
def test_rejects_invalid_sampling(radii, stresses):
    with pytest.raises(ValueError):
        post.extrapolate_stress_k(radii, stresses)


def test_removes_linear_radial_correction():
    # A next-order r^(1/2) stress term makes apparent K linear in r.
    # The slope is a synthetic dimensioned fixture derived from benchmark K/a.
    k = post.reference_k(90.0)
    a = post.BENCHMARK["a_mm"]
    radii = [a*j/post.LEVELS[-1] for j in (1, 2, 3)]
    stress = [(k+k*r/a)*math.sqrt(1000.0/(2*math.pi*r)) for r in radii]
    assert post.extrapolate_stress_k(radii, stress)["k_mpa_sqrt_m"] == pytest.approx(k)


@pytest.mark.skipif(shutil.which("ccx") is None, reason="CalculiX binary absent")
def test_native_displacement_and_stress_readback(tmp_path):
    mesh_spec = importlib.util.spec_from_file_location(
        "surface_crack_mesh", ROOT / "scripts/studies/surface_crack_mesh.py"
    )
    mesh = importlib.util.module_from_spec(mesh_spec)
    mesh_spec.loader.exec_module(mesh)
    model = mesh.build_mesh(post.LEVELS[0])
    mesh.write_deck(tmp_path / "crack.inp", model)
    receipt = post.solve(tmp_path, shutil.which("ccx"))
    assert receipt["returncode"] == 0
    fields = post.read_fields(tmp_path)
    assert len(fields["stresses"]) == len(model["nodes"])
    assert len(fields["displacements"]) == len(model["nodes"])
    # Crack opens while intact symmetry nodes remain closed.
    probe = model["opening_node"]
    assert fields["displacements"][probe]["dy"] > 0
    intact = model["samples"][-1]["nodes"][0]
    assert abs(fields["displacements"][intact]["dy"]) < 1e-10
    assert fields["stresses"][intact]["syy"] > 0
    expected = (post.BENCHMARK["sigma_m_mpa"]*post.BENCHMARK["width_mm"]*
                post.BENCHMARK["t_mm"])
    assert post.reaction_y(tmp_path) == pytest.approx(-expected, rel=1e-6)


def test_mesh_preserves_rectangular_section_and_crack_geometry():
    from surface_crack_mesh import build_mesh, section
    for n in post.LEVELS:
        model = build_mesh(n)
        angles, points, rings, triangles, quads = section(n)
        areas = []
        for face in triangles+quads:
            polygon = [points[p] for p in face]
            cross = sum(x*z2-x2*z for (x,z),(x2,z2) in
                        zip(polygon, polygon[1:]+polygon[:1]))
            # x-z clockwise is positive extrusion orientation in the y direction.
            assert cross < 0
            areas.append(-cross/2)
        assert sum(areas) == pytest.approx(
            post.BENCHMARK["width_mm"]*post.BENCHMARK["t_mm"], rel=1e-10)
        for p in rings[n-1]:
            assert math.hypot(*points[p]) == pytest.approx(post.BENCHMARK["a_mm"])
        assert model["samples"][0]["phi_deg"] == 0
        assert model["samples"][-1]["phi_deg"] == pytest.approx(90)
