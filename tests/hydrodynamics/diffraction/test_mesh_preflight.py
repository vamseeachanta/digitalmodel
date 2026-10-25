"""Offline GDF controls; expected values are independent box arithmetic.

WAMIT User Manual v7.4, section 6.1: ISX/ISY are binary x/y-cut flags.
The synthetic box has length 4, beam 2, draft 1 in metres, hence V=8 m3.
Its positive-y half has V=4 m3, and positive-x/y quarter V=2 m3.
At the chosen test density 1000 kg/m3, floating mass is 8000 kg.
These are manufactured inputs, not physical acceptance recommendations.
"""

from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend


def box_spec(tmp_path, symmetry="none", flags="0 0", partial=False):
    from tests.hydrodynamics.diffraction.test_aqwa_half_deck import _box_panels

    panels = _box_panels()
    if partial:
        if flags.split()[0] == "1":
            panels = panels[(panels[:, :, 0] >= 0).all(axis=1)]
        if flags.split()[1] == "1":
            panels = panels[(panels[:, :, 1] >= 0).all(axis=1)]
    mesh = tmp_path / "box.gdf"
    mesh.write_text(
        "\n".join(
            ["Synthetic box", "1 1", flags, str(len(panels))]
            + [" ".join(map(str, point)) for panel in panels for point in panel]
        )
        + "\n"
    )
    return DiffractionSpec.model_validate(
        {
            "vessel": {
                "name": "synthetic",
                "type": "barge",
                "geometry": {"mesh_file": mesh.name, "symmetry": symmetry},
                "inertia": {
                    "mass": 8000,
                    "centre_of_gravity": [0, 0, -0.5],
                    "radii_of_gyration": [1, 1, 1],
                },
            },
            "environment": {"water_depth": 50, "water_density": 1000},
            "frequencies": {"values": [1]},
            "wave_headings": {"values": [0]},
        }
    )


def generate(backend, spec, tmp_path, mode="single"):
    spec._source_dir = tmp_path
    kwargs = {"spec_dir": tmp_path} if isinstance(backend, AQWABackend) else {}
    return getattr(backend, f"generate_{mode}")(spec, tmp_path / "output", **kwargs)


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
@pytest.mark.parametrize("mode", ["single", "modular"])
@pytest.mark.parametrize(
    "symmetry,flags", [("none", "0 1"), ("xz", "0 0"), ("xz", "1 0"), ("yz", "1 1")]
)
def test_mismatch_refuses_before_any_deck(
    tmp_path, backend_type, mode, symmetry, flags
):
    spec = box_spec(tmp_path, symmetry, flags, partial=True)
    with pytest.raises(ValueError, match=rf"ISX=.*ISY=.*spec symmetry.*{symmetry}"):
        generate(backend_type(), spec, tmp_path, mode)
    assert not list((tmp_path / "output").glob("*"))


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
@pytest.mark.parametrize(
    "symmetry,flags", [("none", "0 0"), ("xz", "0 1"), ("yz", "1 0"), ("xz+yz", "1 1")]
)
def test_matching_pairs_pass(tmp_path, backend_type, symmetry, flags):
    spec = box_spec(tmp_path, symmetry, flags, partial=True)
    backend = backend_type()
    result = generate(backend, spec, tmp_path)
    assert result.exists()
    if backend_type is OrcaWaveBackend:
        expected = {
            "none": "None",
            "xz": "xz plane",
            "yz": "yz plane",
            "xz+yz": "xz and yz planes",
        }[symmetry]
        assert (
            yaml.safe_load(result.read_text())["Bodies"][0]["BodyMeshSymmetry"]
            == expected
        )


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
@pytest.mark.parametrize("flags", ["2 0", "0 -1", "0.5 0", "0", "x y"])
def test_invalid_flags_fail_closed(tmp_path, backend_type, flags):
    spec = box_spec(tmp_path, flags=flags)
    with pytest.raises(ValueError, match="GDF symmetry"):
        generate(backend_type(), spec, tmp_path)


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
def test_full_coordinates_with_partial_header_refuse(tmp_path, backend_type):
    spec = box_spec(tmp_path, "xz", "0 1", partial=False)
    with pytest.raises(ValueError, match="both sides"):
        generate(backend_type(), spec, tmp_path)


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
@pytest.mark.parametrize(
    "symmetry,flags", [("none", "0 0"), ("xz", "0 1"), ("xz+yz", "1 1")]
)
def test_buoyancy_from_whole_body_volume(tmp_path, backend_type, symmetry, flags):
    """4*2*1=8 m3, rho*V=1000*8=8000 kg; residual is exactly zero."""
    spec = box_spec(tmp_path, symmetry, flags, partial=True)
    spec.vessel.geometry.buoyancy_relative_tolerance = 0
    backend = backend_type()
    generate(backend, spec, tmp_path)
    report = backend.preflight_reports[0]
    assert report["status"] == "PASS"
    assert report["volume_m3"] == pytest.approx(8)


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
@pytest.mark.parametrize("mode", ["single", "modular"])
def test_buoyancy_failure_writes_no_deck(tmp_path, backend_type, mode):
    """4000 kg versus 8000 kg displaced mass gives residual 0.5 > 0."""
    spec = box_spec(tmp_path, "xz", "0 1", partial=True)
    spec.vessel.inertia.mass = 4000
    spec.vessel.geometry.buoyancy_relative_tolerance = 0
    with pytest.raises(ValueError, match="DF2.*Mass"):
        generate(backend_type(), spec, tmp_path, mode)
    assert not list((tmp_path / "output").glob("*"))


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
def test_missing_tolerance_is_not_evaluated(tmp_path, backend_type):
    backend = backend_type()
    generate(backend, box_spec(tmp_path), tmp_path)
    assert backend.preflight_reports[0]["status"] == "NOT_EVALUATED"
    assert "tolerance" in backend.preflight_reports[0]["reason"]


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
@pytest.mark.parametrize("mass,accepted", [(8080, True), (8081, False)])
def test_caller_supplied_buoyancy_boundary(tmp_path, backend_type, mass, accepted):
    """At supplied 0.01, 8000*0.01=80 kg: 8080 passes, 8081 refuses.

    The inclusive displaced-mass denominator is defined by the repository's
    equilibrium_checks.check_mass_displacement, not an inferred criterion.
    """
    spec = box_spec(tmp_path)
    spec.vessel.geometry.buoyancy_relative_tolerance = 0.01
    spec.vessel.inertia.mass = mass
    if accepted:
        generate(backend_type(), spec, tmp_path)
    else:
        with pytest.raises(ValueError, match="DF2"):
            generate(backend_type(), spec, tmp_path)


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
def test_unreliable_geometry_cannot_pass_df2(tmp_path, backend_type):
    spec = box_spec(tmp_path)
    spec.vessel.geometry.buoyancy_relative_tolerance = 0
    mesh = tmp_path / "box.gdf"
    # Delete one wetted quad and adjust the count: an open box has no valid V.
    lines = mesh.read_text().splitlines()
    lines[3] = str(int(lines[3]) - 1)
    mesh.write_text("\n".join(lines[:-4]) + "\n")
    with pytest.raises(ValueError, match="DF2 requires a closed"):
        generate(backend_type(), spec, tmp_path)


def test_worked_example(tmp_path):
    """4*2*1=8 m3 for both native inputs; no solver execution is needed."""
    import runpy

    path = (
        Path(__file__).resolve().parents[3]
        / "examples/hydrodynamics/diffraction_symmetry.py"
    )
    example = runpy.run_path(str(path))
    reports = example["build_example"](tmp_path)
    assert set(reports) == {"aqwa", "orcawave"}
    assert (tmp_path / "orcawave/box.gdf").read_bytes() == (tmp_path / "box.gdf").read_bytes()
    assert all(value[0]["status"] == "PASS" for value in reports.values())
    assert all(value[0]["volume_m3"] == pytest.approx(8) for value in reports.values())


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
def test_solver_derived_mass_is_not_independent_buoyancy_evidence(
    tmp_path, backend_type
):
    spec = box_spec(tmp_path)
    spec.vessel.inertia.mode = "free_floating"
    spec.vessel.geometry.buoyancy_relative_tolerance = 0
    with pytest.raises(ValueError, match="DF2 requires an explicit mass"):
        generate(backend_type(), spec, tmp_path)


@pytest.mark.parametrize("backend_type", [AQWABackend, OrcaWaveBackend])
def test_later_body_mismatch_writes_no_deck(tmp_path, backend_type):
    spec = box_spec(tmp_path)
    data = spec.model_dump(mode="json", exclude_none=True)
    vessel = data.pop("vessel")
    data["bodies"] = [{"vessel": vessel}, {"vessel": vessel}]
    multi = DiffractionSpec.model_validate(data)
    multi.bodies[1].vessel.geometry.symmetry = "xz"
    with pytest.raises(ValueError, match="GDF symmetry.*spec symmetry"):
        generate(backend_type(), multi, tmp_path, "modular")
    assert not list((tmp_path / "output").glob("*"))


@pytest.mark.parametrize("tolerance", [-1, float("inf"), float("nan")])
def test_buoyancy_criterion_must_be_finite_and_nonnegative(tmp_path, tolerance):
    """A tolerance is a nonnegative finite fraction, not an engineering default."""
    spec = box_spec(tmp_path)
    data = spec.model_dump(mode="json", exclude_none=True)
    data["vessel"]["geometry"]["buoyancy_relative_tolerance"] = tolerance
    with pytest.raises(ValueError, match="buoyancy_relative_tolerance"):
        DiffractionSpec.model_validate(data)


def packed_diamond_spec(tmp_path, partial=False, width=12):
    """Every panel starts on y=0; later vertices reveal the stored sector."""
    spec = box_spec(tmp_path, "xz", "0 1")
    corners = [(2, 0), (0, 1), (-2, 0), (0, -1)]
    panels = []
    for first, second in zip(corners, corners[1:] + corners[:1]):
        if partial and (first[1] < 0 or second[1] < 0):
            continue
        if first[1] != 0:
            first, second = second, first
        panels.append([
            (*first, -1), (*second, -1), (*second, 0), (*first, 0)
        ])
    coordinates = [str(value) for panel in panels for point in panel for value in point]
    mesh = tmp_path / "box.gdf"
    mesh.write_text("\n".join(
        ["Synthetic diamond", "1 1", "0 1", str(len(panels))]
        + [" ".join(coordinates[i:i + width]) for i in range(0, len(coordinates), width)]
    ) + "\n")
    return spec


@pytest.mark.parametrize("mode", ["single", "modular"])
@pytest.mark.parametrize("width", [12, 24, 5, 1])
def test_packed_full_body_refuses_reflection(tmp_path, mode, width):
    spec = packed_diamond_spec(tmp_path, width=width)
    assert spec.vessel.geometry.buoyancy_relative_tolerance is None
    with pytest.raises(ValueError, match="both sides"):
        generate(OrcaWaveBackend(), spec, tmp_path, mode)
    assert not list((tmp_path / "output").glob("*"))


@pytest.mark.parametrize("mode", ["single", "modular"])
@pytest.mark.parametrize("width", [12, 24, 5, 1])
def test_packed_matching_sector_passes(tmp_path, mode, width):
    spec = packed_diamond_spec(tmp_path, partial=True, width=width)
    result = generate(OrcaWaveBackend(), spec, tmp_path, mode)
    assert yaml.safe_load(result.read_text())["Bodies"][0]["BodyMeshSymmetry"] == "xz plane"


@pytest.mark.parametrize("mode", ["single", "modular"])
@pytest.mark.parametrize("symmetry,flags", [("none", "0 0"), ("xz", "0 1")])
@pytest.mark.parametrize("defect", [
    "zero_count", "negative_count", "fractional_count", "invalid_count",
    "missing_count", "too_few", "too_many", "nan", "inf", "invalid_coordinate",
])
def test_gdf_coordinate_stream_must_be_complete_and_finite(
    tmp_path, mode, symmetry, flags, defect
):
    spec = box_spec(tmp_path, symmetry, flags, partial=True)
    mesh = tmp_path / "box.gdf"
    header = mesh.read_text().splitlines()[:4]
    coordinates = mesh.read_text().splitlines()[4:]
    tokens = " ".join(coordinates).split()
    counts = {"zero_count": "0", "negative_count": "-1", "fractional_count": "1.5",
              "invalid_count": "bad", "missing_count": ""}
    if defect in counts:
        header[3] = counts[defect]
    elif defect == "too_few":
        tokens.pop()
    elif defect == "too_many":
        tokens.append("0")
    else:
        # A later coordinate in a packed panel must not be discarded.
        tokens[5] = {"nan": "nan", "inf": "inf", "invalid_coordinate": "bad"}[defect]
    mesh.write_text("\n".join(header + [" ".join(tokens)]) + "\n")
    with pytest.raises(ValueError, match="GDF.*(panel count|coordinates)"):
        generate(OrcaWaveBackend(), spec, tmp_path, mode)
    assert not list((tmp_path / "output").glob("*"))


@pytest.mark.parametrize("mode", ["single", "modular"])
@pytest.mark.parametrize("annotation", ["end of panel 1", "end of panel 9"])
def test_panel_annotation_cannot_hide_missing_coordinates(tmp_path, mode, annotation):
    spec = packed_diamond_spec(tmp_path, partial=True)
    mesh = tmp_path / "box.gdf"
    lines = mesh.read_text().splitlines()
    lines[4] = "0 0 -1 " + annotation
    mesh.write_text("\n".join(lines) + "\n")
    with pytest.raises(ValueError, match="Invalid GDF coordinates"):
        generate(OrcaWaveBackend(), spec, tmp_path, mode)
    assert not list((tmp_path / "output").glob("*"))
