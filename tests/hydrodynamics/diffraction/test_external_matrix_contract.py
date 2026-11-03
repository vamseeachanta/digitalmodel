# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic input contracts; coefficients are chosen inputs, not design data."""

from pathlib import Path

import pytest
from pydantic import ValidationError

from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselSpec


def vessel_data():
    """Return arbitrary synthetic geometry and inertia, without engineering defaults."""
    return {
        "name": "synthetic",
        "geometry": {"mesh_file": "synthetic.gdf"},
        "inertia": {
            "mass": 1000,
            "centre_of_gravity": [0, 0, -2],
            "radii_of_gyration": [1, 1, 1],
        },
    }


def matrix():
    """Chosen symmetric coefficients: diagonal 1000..6000, coupling 200."""
    values = [[0.0] * 6 for _ in range(6)]
    for i in range(6):
        values[i][i] = (i + 1) * 1000.0
    values[0][3] = values[3][0] = 200.0
    return values


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
def test_matrix_requires_explicit_units_and_reference_point(kind):
    """Even a symmetric matrix is ambiguous without both declarations."""
    data = vessel_data()
    data[f"external_{kind}"] = matrix()
    with pytest.raises(ValidationError, match="units.*reference_point"):
        VesselSpec.model_validate(data)


def qualified_data(kind="damping", point="body_origin"):
    """Declare canonical N, m, s, rad and a named origin."""
    return dict(vessel_data(), **{
        f"external_{kind}": matrix(),
        f"external_{kind}_units": "SI",
        f"external_{kind}_reference_point": point,
    })


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("defect", ["ragged", "asymmetric", "nan", "inf"])
def test_invalid_matrix_is_rejected(kind, defect):
    """Exact symmetry is required; no numerical tolerance is silently assumed."""
    data = qualified_data(kind)
    values = data[f"external_{kind}"]
    if defect == "ragged":
        values[0].pop()
    elif defect == "asymmetric":
        values[0][3] = 201
    else:
        values[0][0] = float(defect)
    with pytest.raises(ValidationError):
        VesselSpec.model_validate(data)


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("field,value", [("units", "kN"),
                                          ("reference_point", "unknown")])
def test_unknown_metadata_is_rejected(kind, field, value):
    data = qualified_data(kind)
    data[f"external_{kind}_{field}"] = value
    with pytest.raises(ValidationError):
        VesselSpec.model_validate(data)


def spec_data(kind="damping", point="centre_of_mass"):
    """Minimal synthetic deck input; no solver or physical acceptance target."""
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

    return DiffractionSpec.model_validate({
        "vessel": qualified_data(kind, point),
        "environment": {"water_depth": 100},
        "frequencies": {"values": [1]},
        "wave_headings": {"values": [0]},
    })


@pytest.mark.parametrize("kind,keyword", [("stiffness", "SSTF"),
                                        ("damping", "FIDP")])
def test_cross_backend_matrix_conversion(tmp_path, kind, keyword):
    """Hand calculation: 1000 N = 1 kN, so diag 1000..6000 -> 1..6.

    The chosen mixed entries 200 N (with the relevant m/s/rad powers) ->
    0.2 kN in both transpose positions. AQWA remains N based. Source:
    OrcaWave WebHelp, Data: Model / Units and Data: Constraints, accessed 2026;
    AQWA Reference Manual 2025 R1 sections 11.6.1 and 11.7.1.
    """
    import yaml
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    spec = spec_data(kind)
    ow = yaml.safe_load(OrcaWaveBackend().generate_single(spec, tmp_path).read_text(encoding="utf-8"))
    body = ow["Bodies"][0]
    key = next(k for k in body if k.startswith(f"BodyExternal{kind.title()}Matrixx,"))
    assert body[key] == [
        [1, 0, 0, 0.2, 0, 0], [0, 2, 0, 0, 0, 0],
        [0, 0, 3, 0, 0, 0], [0.2, 0, 0, 4, 0, 0],
        [0, 0, 0, 0, 5, 0], [0, 0, 0, 0, 0, 6],
    ]
    assert body[f"BodyExternal{kind.title()}MatrixOriginType"] == "Centre of mass"
    rows = [line for line in AQWABackend().build_deck7(spec) if keyword in line]
    assert len(rows) == 6
    assert [[float(line[i:i + 10]) for i in range(20, 80, 10)]
            for line in rows] == matrix()


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
def test_aqwa_rejects_offset_body_origin(kind):
    """No parallel-axis transformation is assumed for an unsupported origin."""
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend

    with pytest.raises(ValueError, match="reference point"):
        AQWABackend().build_deck7(spec_data(kind, "body_origin"))


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("point", ["body_origin", "centre_of_mass"])
def test_orcawave_reverse_parse_normalizes_units_and_origin(tmp_path, kind, point):
    """Inverse hand calculation: native 0.2 kN -> canonical 200 N."""
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
    from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

    spec = spec_data(kind, point)
    path = OrcaWaveBackend().generate_single(spec, tmp_path)
    parsed = OrcaWaveInputParser().parse(path)
    assert getattr(parsed.vessel, f"external_{kind}") == matrix()
    assert getattr(parsed.vessel, f"external_{kind}_units") == "SI"
    assert getattr(parsed.vessel, f"external_{kind}_reference_point") == point


@pytest.mark.parametrize("backend", ["aqwa", "orcawave"])
@pytest.mark.parametrize("modular", [False, True])
def test_generated_matrix_provenance(tmp_path, backend, modular):
    """Read saved evidence back: chosen 200 N corresponds to 0.2 kN."""
    import json
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    spec = spec_data()
    adapter = AQWABackend() if backend == "aqwa" else OrcaWaveBackend()
    method = adapter.generate_modular if modular else adapter.generate_single
    method(spec, tmp_path)
    data = json.loads((tmp_path / f"external_matrices.{backend}.json").read_text(encoding="utf-8"))
    assert data["dof_order"] == ["surge", "sway", "heave", "roll", "pitch", "yaw"]
    row = data["matrices"][0]
    assert row["normalized_values"] == matrix()
    assert row["reference_point"] == "centre_of_mass"
    assert row["reference_point_body_coordinates_m"] == [0, 0, -2]
    assert row["native_values"][0][3] == (0.2 if backend == "orcawave" else 200)
    assert row["normalized_units"][0][3] == "N.s/rad"
    assert row["normalized_units"][3][0] == "N.m.s/m"


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("point", ["body_origin", "centre_of_mass"])
@pytest.mark.parametrize("modular", [False, True])
def test_free_floating_matrix_origin_provenance(tmp_path, kind, point, modular):
    """Solver-derived coordinates must not reuse ignored explicit-mode inputs."""
    import json
    import yaml
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    spec = spec_data(kind, point)
    spec.vessel.inertia.mode = "free_floating"
    # Chosen inputs: compatibility z=-2 m is ignored; cog_z=-5 m is emitted.
    spec.vessel.inertia.cog_z = -5
    adapter = OrcaWaveBackend()
    generate = adapter.generate_modular if modular else adapter.generate_single
    output = generate(spec, tmp_path)
    body = yaml.safe_load(output.read_text(encoding="utf-8"))["Bodies"][0]
    assert body["BodyCentreOfMassZRelativeToFreeSurface"] == -5
    assert "BodyCentreOfMass" not in body
    assert body[f"BodyExternal{kind.title()}MatrixOriginType"] == (
        "Body origin" if point == "body_origin" else "Centre of mass"
    )
    data = json.loads(
        (tmp_path / "external_matrices.orcawave.json").read_text(encoding="utf-8")
    )
    row = data["matrices"][0]
    assert row["reference_point"] == point
    if point == "centre_of_mass":
        assert row["reference_point_body_coordinates_m"] is None
        assert row["reference_point_body_coordinates_status"] == "unknown_solver_derived"
    else:
        assert row["reference_point_body_coordinates_m"] == [0, 0, 0]


@pytest.mark.parametrize("mutation", ["origin", "units", "shape"])
def test_reverse_parser_fails_closed(tmp_path, mutation):
    """A native deck may not silently supply the missing input contract."""
    import yaml
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
    from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

    path = OrcaWaveBackend().generate_single(spec_data(), tmp_path)
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    if mutation == "units":
        data["UnitsSystem"] = "US"
    elif mutation == "origin":
        del data["Bodies"][0]["BodyExternalDampingMatrixOriginType"]
    else:
        body = data["Bodies"][0]
        key = next(k for k in body if k.startswith("BodyExternalDampingMatrixx,"))
        body[key][0].pop()
    path.write_text(yaml.safe_dump(data), encoding="utf-8", newline="\n")
    with pytest.raises(ValueError):
        OrcaWaveInputParser().parse(path)


def test_worked_example_and_full_matrix_blocks(tmp_path):
    """All block types: chosen integer multiples of 1000 N -> integer kN.

    Each off-diagonal input is 1000*(1+min(i,j)), the expected native value
    is 1+min(i,j); diagonal input 10000*(1+i) gives 10*(1+i). This hand
    calculation uses SI kilo scaling, not the adapter's conversion helper.
    """
    import yaml
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    example = Path(__file__).resolve().parents[3] / "examples/hydrodynamics/diffraction/external_matrices.yml"
    spec = DiffractionSpec.from_yaml(example)
    for kind in ("stiffness", "damping"):
        assert getattr(spec.vessel, f"external_{kind}") == matrix()
        values = [[1000 * (10 * (1 + i) if i == j else 1 + min(i, j))
                   for j in range(6)] for i in range(6)]
        setattr(spec.vessel, f"external_{kind}", values)
    data = yaml.safe_load(OrcaWaveBackend().generate_single(spec, tmp_path).read_text(encoding="utf-8"))
    cards = AQWABackend().build_deck7(spec)
    for kind, keyword in (("stiffness", "SSTF"), ("damping", "FIDP")):
        key = next(k for k in data["Bodies"][0]
                   if k.startswith(f"BodyExternal{kind.title()}Matrixx,"))
        assert data["Bodies"][0][key] == [
            [10 * (1 + i) if i == j else 1 + min(i, j) for j in range(6)]
            for i in range(6)
        ]
        rows = [line for line in cards if keyword in line]
        assert [[float(line[j:j + 10]) for j in range(20, 80, 10)]
                for line in rows] == values


def test_aqwa_multibody_matrices_are_in_separate_blocks():
    """Distinct prescribed coefficients must stay attached to their own body."""
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend

    first, second = qualified_data(point="centre_of_mass"), qualified_data(point="centre_of_mass")
    second["external_damping"][3][3] = 8000
    spec = DiffractionSpec.model_validate({
        "bodies": [{"vessel": first}, {"vessel": second}],
        "environment": {"water_depth": 100},
        "frequencies": {"values": [1]}, "wave_headings": {"values": [0]},
    })
    cards = AQWABackend().build_deck7(spec)
    first_rows = [line for line in cards[:cards.index("          WFS2")] if "FIDP" in line]
    second_rows = [line for line in cards[cards.index("          WFS2"):] if "FIDP" in line]
    assert float(first_rows[3][50:60]) == 4000
    assert float(second_rows[3][50:60]) == 8000


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
def test_backend_revalidates_mutated_matrix(tmp_path, kind):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    spec = spec_data(kind)
    getattr(spec.vessel, f"external_{kind}")[0][3] = 201
    with pytest.raises(ValidationError):
        OrcaWaveBackend().generate_single(spec, tmp_path)


def test_reused_output_directory_has_current_provenance(tmp_path):
    """An absent matrix on a later generation must not retain old evidence."""
    import json
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    spec = spec_data()
    adapter = OrcaWaveBackend()
    adapter.generate_single(spec, tmp_path)
    spec.vessel.external_damping = None
    spec.vessel.external_damping_units = None
    spec.vessel.external_damping_reference_point = None
    adapter.generate_single(spec, tmp_path)
    data = json.loads((tmp_path / "external_matrices.orcawave.json").read_text(encoding="utf-8"))
    assert data["matrices"] == []


def test_aqwa_rejects_unrepresentable_card_value():
    """The reference manual specifies six E10 fields, not expanding fields."""
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend

    spec = spec_data()
    spec.vessel.external_damping[0][0] = -1e100  # Chosen overflow probe.
    with pytest.raises(ValueError, match="field width"):
        AQWABackend().build_deck7(spec)


def test_aqwa_rejects_rotated_matrix_axes():
    """A chosen nonzero attitude requires an unsupported full-matrix rotation."""
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend

    data = spec_data().model_dump()
    vessel = data.pop("vessel")
    data["bodies"] = [{"vessel": vessel, "attitude": [0, 0, 1]}]
    with pytest.raises(ValueError, match="attitude"):
        AQWABackend().build_deck7(DiffractionSpec.model_validate(data))


def test_aqwa_basis_reads_applied_structural_stiffness(tmp_path):
    """Readback must recognize the documented additional stiffness keyword."""
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
    from digitalmodel.hydrodynamics.diffraction.aqwa_deck_basis import read_aqwa_basis

    path = AQWABackend().generate_single(spec_data("stiffness"), tmp_path)
    basis = read_aqwa_basis(path)
    assert basis.additional_stiffness == tuple(tuple(row) for row in matrix())


def test_aqwa_basis_sums_repeated_sstf_records(tmp_path):
    """Hand calculation: two identical SSTF records add to twice each input.

    AQWA Reference Manual 2025 R1 section 11.7.1 specifies summation.
    """
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
    from digitalmodel.hydrodynamics.diffraction.aqwa_deck_basis import read_aqwa_basis

    path = AQWABackend().generate_single(spec_data("stiffness"), tmp_path)
    lines = path.read_text(encoding="utf-8").splitlines()
    path.write_text("\n".join(line + "\n" + line if "SSTF" in line else line
                              for line in lines) + "\n", encoding="utf-8", newline="\n")
    assert read_aqwa_basis(path).additional_stiffness == tuple(
        tuple(2 * value for value in row) for row in matrix()
    )


def test_aqwa_basis_refuses_cross_body_sstf(tmp_path):
    """A cross-body submatrix must not become a self-restoring matrix."""
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
    from digitalmodel.hydrodynamics.diffraction.aqwa_deck_basis import AmbiguousDeck, read_aqwa_basis

    path = AQWABackend().generate_single(spec_data("stiffness"), tmp_path)
    lines = path.read_text(encoding="utf-8").splitlines()
    path.write_text("\n".join(line[:10] + f"{2:5d}" + line[15:]
                              if "SSTF" in line else line for line in lines) + "\n",
                    encoding="utf-8", newline="\n")
    with pytest.raises(AmbiguousDeck, match="coupled SSTF"):
        read_aqwa_basis(path)
