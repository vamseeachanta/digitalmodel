"""BRep controls: closed-form comparators and representation regression tests."""

import numpy as np
import pytest
import yaml

pytest.importorskip("hullprod")

from digitalmodel.hydrodynamics.hull_library.curvature_screen import (
    CurvatureSignature,
    representation_delta,
    screen_profile,
    screen_step,
)
from digitalmodel.hydrodynamics.hull_library.hull_surface_brep import (
    bspline_face_from_grid,
    export_step,
    flat_bottom_face,
    mirror_and_sew,
    profile_point_grid,
    profile_to_step,
)
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)


def _wigley_grid(nx=41, nz=11):
    x, z = np.meshgrid(
        np.linspace(-50, 50, nx), np.linspace(-6.25, 0, nz), indexing="ij"
    )
    y = 5 * (1 - (x / 50) ** 2) * (1 - (z / 6.25) ** 2)
    return np.stack([x, y, z], axis=-1)


def _wigley_integral():
    """Closed-form K of a graph, independently integrated over analytic area."""
    x = np.linspace(-50, 50, 2001)
    z = np.linspace(-6.25, 0, 1001)
    xx, zz = np.meshgrid(x, z, indexing="ij")
    fx = -xx / 250 * (1 - (zz / 6.25) ** 2)
    fz = -10 * zz / 6.25**2 * (1 - (xx / 50) ** 2)
    fxx = -(1 - (zz / 6.25) ** 2) / 250
    fzz = -10 / 6.25**2 * (1 - (xx / 50) ** 2)
    fxz = 2 * xx * zz / (250 * 6.25**2)
    area = np.sqrt(1 + fx**2 + fz**2)
    k = (fxx * fzz - fxz**2) / area**4
    trapz = getattr(np, "trapezoid", np.trapz)

    def integrate(v):
        return trapz(trapz(v, z, axis=1), x)

    return 100**2 * integrate(abs(k) * area) / integrate(area)


def test_wigley_closed_form(tmp_path):
    face = bspline_face_from_grid(_wigley_grid())
    half = screen_step(export_step(face, tmp_path / "half.step"), lref=100)
    full = screen_step(
        export_step(mirror_and_sew(face), tmp_path / "full.step"), lref=100
    )
    assert half.signature.I_D == pytest.approx(_wigley_integral(), rel=0.005)
    assert half.signature.a_elliptic == pytest.approx(0.626, abs=0.01)
    assert half.signature.a_saddle == pytest.approx(0.374, abs=0.01)
    assert full.signature.as_vector() == pytest.approx(
        half.signature.as_vector(), rel=1e-3
    )
    assert half.signature.status == "valid"


def test_exact_sphere_and_step_round_trip(tmp_path):
    from OCP.BRepPrimAPI import BRepPrimAPI_MakeSphere

    path = export_step(BRepPrimAPI_MakeSphere(1).Shape(), tmp_path / "sphere.step")
    result = screen_step(path, lref=2)
    assert result.signature.I_D == pytest.approx(4, abs=1e-8)
    assert result.signature.a_elliptic == pytest.approx(1)
    assert result.signature.representation == "brep"
    assert result.signature.reliability == "not_applicable"
    assert result.signature.source == "sphere.step"
    assert result.fields is None
    assert result.provenance["backend"] == "brep_native"
    assert "SI_UNIT(.METRE.)" in path.read_text().replace(
        " ", ""
    ) or "SI_UNIT($,.METRE.)" in path.read_text().replace(" ", "")
    # HullProd's imported geometry bounds must remain in metres.
    import hullprod
    from hullprod.types import ProducibilityConfig

    raw = hullprod.assess(
        path,
        lref=2000,
        config=ProducibilityConfig(brep_cache=False, brep_display_mesh=False),
    )
    assert raw.signature["I_D"] == pytest.approx(4, abs=1e-8)
    assert np.asarray(raw.metadata["bounds_min"]) / 1000 == pytest.approx(
        [-1, -1, -1], abs=1e-6
    )
    assert np.asarray(raw.metadata["bounds_max"]) / 1000 == pytest.approx(
        [1, 1, 1], abs=1e-6
    )


def test_exact_cylinder_side(tmp_path):
    from OCP.BRepPrimAPI import BRepPrimAPI_MakeCylinder

    face = BRepPrimAPI_MakeCylinder(1, 3).Face()
    sig = screen_step(export_step(face, tmp_path / "cylinder.step"), lref=2).signature
    assert sig.a_single == pytest.approx(1)
    assert sig.I_D == pytest.approx(0, abs=1e-10)


def test_profile_grid_matches_mesh_interpolation(ship_profile):
    grid = profile_point_grid(ship_profile)
    expected = HullMeshGenerator()._interpolate_hull_surface(
        ship_profile, grid[:, 0, 0], grid[0, :, 2]
    )
    assert grid.shape == (41, 11, 3)
    assert grid[:, :, 1] == pytest.approx(expected)
    assert grid[0, 0, 2] == -ship_profile.draft
    assert grid[-1, -1, 2] == 0
    assert grid[-1, -1, 0] == ship_profile.length_bp


def test_flat_bottom_and_profile_export(box_profile, tmp_path):
    from OCP.BRepGProp import BRepGProp
    from OCP.GProp import GProp_GProps

    grid = profile_point_grid(box_profile)
    props = GProp_GProps()
    BRepGProp.SurfaceProperties_s(flat_bottom_face(grid), props)
    assert props.Mass() == pytest.approx(1000)
    path = profile_to_step(box_profile, tmp_path / "box.step")
    sig = screen_step(path, lref=100).signature
    assert sig.a_flat == pytest.approx(1)
    assert sig.I_D == pytest.approx(0, abs=1e-8)
    assert flat_bottom_face(_wigley_grid()) is None


@pytest.mark.parametrize("target, count", [(2000, 1600), (8000, 7225)])
def test_fixture_ship_delta(ship_profile, target, count):
    cfg = MeshGeneratorConfig(target_panels=target)
    assert HullMeshGenerator().generate(ship_profile, cfg).n_panels == count
    result = screen_profile(ship_profile, cfg, representation="both")
    brep = CurvatureSignature.model_validate(result.provenance["brep_signature"])
    assert brep.status == "valid"
    assert brep.a_elliptic > brep.a_saddle
    delta = result.provenance["representation_delta"]
    assert delta == representation_delta(brep, result.signature)


def test_catalog_both_and_legacy_yaml(ship_profile):
    from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

    catalog = HullCatalog()
    entry = catalog.register_hull(ship_profile)
    sig = catalog.screen_hull(ship_profile.name, representation="both")
    assert sig is entry.curvature_signature
    assert sig.representation == "mesh"
    assert entry.curvature_signature_brep.representation == "brep"
    data = sig.model_dump()
    data.pop("representation")
    data.pop("source")
    legacy = CurvatureSignature.model_validate(yaml.safe_load(yaml.safe_dump(data)))
    assert legacy.representation == "mesh"
    assert legacy.source is None
    brep = catalog.screen_hull(ship_profile.name, representation="brep")
    assert brep is entry.curvature_signature_brep
    assert entry.curvature_signature is sig


def test_delta_zero_reference_and_reference_mismatch(tmp_path):
    from OCP.BRepPrimAPI import BRepPrimAPI_MakeSphere

    sig = screen_step(
        export_step(BRepPrimAPI_MakeSphere(1).Shape(), tmp_path / "s.step"), lref=2
    ).signature
    mesh = sig.model_copy(update={"representation": "mesh"})
    assert representation_delta(sig, mesh)["I_D"]["relative"] == 0
    mesh.a_flat = 0.1
    assert representation_delta(sig, mesh)["a_flat"] == {
        "absolute": 0.1,
        "relative": None,
    }
    mesh.lref = 3
    with pytest.raises(ValueError, match="reference length"):
        representation_delta(sig, mesh)


@pytest.mark.parametrize("representation", ["bad", "", None])
def test_invalid_representation(ship_profile, representation):
    with pytest.raises(ValueError, match="representation"):
        screen_profile(ship_profile, representation=representation)


@pytest.mark.parametrize("n_x,n_z", [(1, 11), (41, 1), (3.5, 11), (True, 11)])
def test_invalid_grid_counts(ship_profile, n_x, n_z):
    with pytest.raises(ValueError, match="grid"):
        profile_point_grid(ship_profile, n_x, n_z)


def test_nonfinite_grid_rejected():
    grid = _wigley_grid()
    grid[0, 0, 1] = np.nan
    with pytest.raises(ValueError, match="finite"):
        bspline_face_from_grid(grid)


@pytest.mark.parametrize("lref", [0, -1, float("nan"), float("inf")])
def test_invalid_lref(tmp_path, lref):
    with pytest.raises(ValueError, match="lref"):
        screen_step(tmp_path / "absent.step", lref=lref)


def test_pinched_bottom_rejected():
    grid = np.array(
        [[[0, 1, -1], [0, 1, 0]], [[1, 0, -1], [1, 1, 0]], [[2, 1, -1], [2, 1, 0]]],
        dtype=float,
    )
    with pytest.raises(ValueError, match="bottom"):
        flat_bottom_face(grid)


def test_export_restores_global_settings_and_is_quiet(tmp_path, capfd):
    from OCP.BRepPrimAPI import BRepPrimAPI_MakeSphere
    from OCP.Interface import Interface_Static
    from OCP.STEPControl import STEPControl_Controller

    STEPControl_Controller.Init_s()
    keys = ("write.step.unit", "xstep.cascade.unit")
    before = [Interface_Static.CVal_s(key) for key in keys]
    shape = BRepPrimAPI_MakeSphere(1).Shape()
    export_step(shape, tmp_path / "quiet.step")
    assert not capfd.readouterr().out
    assert [Interface_Static.CVal_s(key) for key in keys] == before
    with pytest.raises(RuntimeError, match="write"):
        export_step(shape, tmp_path / "missing" / "failure.step")
    assert [Interface_Static.CVal_s(key) for key in keys] == before


def test_step_source_does_not_store_directories(tmp_path):
    from OCP.BRepPrimAPI import BRepPrimAPI_MakeSphere

    path = export_step(BRepPrimAPI_MakeSphere(1).Shape(), tmp_path / "sphere.step")
    result = screen_step(path, lref=2)
    assert result.provenance["input_geometry"]["path"] == path.name
    assert result.provenance["brep_import"]["source_path"] == path.name


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="Measured I_D delta 33.7086% at 7225 panels; preserve the planned <15% expectation",
)
def test_fixture_ship_fine_mesh_delta_expectation(ship_profile):
    result = screen_profile(
        ship_profile, MeshGeneratorConfig(target_panels=8000), representation="both"
    )
    delta = result.provenance["representation_delta"]["I_D"]
    assert delta["relative"] < 0.15, f"mesh-vs-BRep I_D delta: {delta}"


def test_unconverged_brep_preserves_status(monkeypatch, tmp_path):
    from types import SimpleNamespace

    import hullprod

    raw = SimpleNamespace(
        signature={
            "I_D": None,
            "I_D_plus": None,
            "I_D_minus": None,
            "a_C": {"flat": 0, "single": 0, "elliptic": 0.4, "saddle": 0.6},
        },
        metadata={
            "metric_validity": {
                "developability_deviation": {"status": "quadrature_unconverged"}
            }
        },
    )
    monkeypatch.setattr(hullprod, "assess", lambda *a, **kw: raw)
    sig = screen_step(tmp_path / "unconverged.step", lref=100).signature
    assert sig.status == "quadrature_unconverged"
    assert np.isnan(sig.I_D)
    assert representation_delta(sig, sig)["I_D"] == {"absolute": None, "relative": None}
