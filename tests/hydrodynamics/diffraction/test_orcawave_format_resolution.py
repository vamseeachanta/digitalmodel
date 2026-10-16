"""Solver-free format regression checks; no native API or file generation."""
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
    _build_bodies_section, _build_damping_lid_section, _build_qtf_section,
)
from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser
from tests.hydrodynamics.diffraction.test_aqwa_half_deck import _format_spec


@pytest.mark.parametrize("key,native", [
    ("aqwa_dat", "Aqwa dat"), ("nemoh_dat", "Nemoh dat"),
])
def test_explicit_dat_dialect_roundtrip(key, native):
    spec = _format_spec("dat", key)
    assert _build_bodies_section(spec)["Bodies"][0]["BodyMeshFormat"] == native
    assert OrcaWaveInputParser._reverse_mesh_format(native) == key


@pytest.mark.parametrize("key,extension", [("gdf", "msh"), ("msh", "gdf"),
                                          ("obj", "dat")])
def test_body_rejects_conflicting_extension(key, extension):
    with pytest.raises(ValueError, match="conflict.*synthetic"):
        _build_bodies_section(_format_spec(extension, key))


@pytest.mark.parametrize("section", ["lid", "zone"])
@pytest.mark.parametrize("extension", ["msh", "obj"])
def test_auxiliary_formats_require_field_evidence(section, extension):
    build = {"lid": _build_damping_lid_section, "zone": _build_qtf_section}[section]
    with pytest.raises(ValueError, match=section):
        build(_format_spec(extension, "auto"))


@pytest.mark.parametrize("declared", ["auto", "Auto", "fdf", "FDF"])
def test_zone_resolves_fdf(declared):
    spec = _format_spec("gdf", "gdf")
    spec.free_surface_zone.mesh_file = "zone.FDF"
    spec.free_surface_zone.mesh_format = declared
    assert _build_qtf_section(spec)["FreeSurfacePanelledZoneMeshFormat"] == "Wamit fdf"


@pytest.mark.parametrize("declared", ["GDF", "Auto"])
def test_auxiliary_explicit_case(declared):
    spec = _format_spec("GDF", "auto")
    spec.damping_lid.mesh_format = declared
    spec.free_surface_zone.mesh_format = declared
    assert _build_damping_lid_section(spec)["DampingLidMeshFormat"] == "Wamit gdf"
    assert _build_qtf_section(spec)["FreeSurfacePanelledZoneMeshFormat"] == "Wamit gdf"


@pytest.mark.parametrize("declared", [None, "dat", "stl"])
def test_control_surface_rejects_ambiguous_or_unsupported(declared):
    from digitalmodel.hydrodynamics.diffraction.input_schemas import ControlSurfaceSpec
    spec = _format_spec("gdf", "gdf")
    spec.vessel.control_surface = ControlSurfaceSpec(
        type="mesh", mesh_file="surface.dat" if declared != "stl" else "surface.stl",
        mesh_format=declared,
    )
    with pytest.raises(ValueError, match="control surface.*synthetic.*surface"):
        _build_bodies_section(spec)


@pytest.mark.parametrize("name", ["L03_ship_benchmark", "L04_spar_benchmark"])
def test_aqwa_specs_emit_explicit_dialect(name):
    root = Path(__file__).resolve().parents[3]
    spec = DiffractionSpec.from_yaml(root / "docs/domains/orcawave" / name / "spec.yml")
    assert _build_bodies_section(spec)["Bodies"][0]["BodyMeshFormat"] == "Aqwa dat"


@pytest.mark.parametrize("relative", ["spec_fpso_turret.yml",
    "benchmarks/multibody_fpso_turret_v1/spec.yml"])
def test_synthetic_multibody_uses_gdf(relative):
    path = Path(__file__).parent / "fixtures" / relative
    spec = DiffractionSpec.from_yaml(path)
    assert all(body["BodyMeshFormat"] == "Wamit gdf"
               for body in _build_bodies_section(spec)["Bodies"])
