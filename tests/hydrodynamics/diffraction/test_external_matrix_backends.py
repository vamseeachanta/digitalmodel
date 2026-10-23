# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Pure input-text tests: no native solver is loaded or started.

Synthetic entries are independently chosen. kN = 1000 N follows the SI kilo
prefix and OrcaWave help, Model / Units (accessed 2026-10-22).
"""

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
from tests.hydrodynamics.diffraction.test_external_matrix_contract import matrix_body


def synthetic_spec(**updates):
    return DiffractionSpec.model_validate({
        "vessel": matrix_body(**updates),
        "environment": {"water_depth": 100.0},
        "frequencies": {"values": [1.0]},
        "wave_headings": {"values": [0.0]},
    })


@pytest.mark.parametrize("point,native", [("body_origin", "Body origin"),
                                         ("centre_of_gravity", "Centre of mass")])
def test_orcawave_explicit_origin_and_all_four_blocks(tmp_path, point, native):
    """Outer product [1..6] gives symmetric nonzero entries in every block.

    Hand calculation: entry ij is (i+1)*(j+1) N-based; dividing each entry
    by 1000 gives kN-based stiffness and damping, including the mixed blocks.
    """
    matrix = np.outer(np.arange(1., 7.), np.arange(1., 7.)).tolist()
    spec = synthetic_spec(external_stiffness=matrix, external_damping=matrix,
                          external_stiffness_reference_point=point,
                          external_damping_reference_point=point)
    path = OrcaWaveBackend().generate_single(spec, tmp_path)
    text = path.read_text()
    body = yaml.safe_load(text)["Bodies"][0]
    for kind in ("Stiffness", "Damping"):
        assert f"BodyExternal{kind}MatrixOriginType: {native}" in text
        key = next(k for k in body if k.startswith(f"BodyExternal{kind}Matrixx,"))
        np.testing.assert_array_equal(body[key], np.asarray(matrix) / 1000.)


def test_aqwa_stiffness_is_refused_until_native_semantics_are_qualified():
    with pytest.raises(ValueError, match="AQWA external stiffness.*NOT_EVALUATED"):
        AQWABackend().build_deck7(synthetic_spec())


def test_aqwa_damping_in_declared_cog_axes():
    """At CoG the SI-to-SI mapping is identity; diagonal [2,3,5,7,11,13]."""
    spec = synthetic_spec(external_stiffness=None, external_stiffness_units=None,
                          external_stiffness_reference_point=None,
                          external_damping_reference_point="centre_of_gravity")
    text = "\n".join(AQWABackend().build_deck7(spec))
    assert "centre_of_gravity" in text
    rows = [line for line in text.splitlines() if line[6:10] == "FIDP"]
    matrix = [[float(row[20 + 10*j:30 + 10*j]) for j in range(6)] for row in rows]
    np.testing.assert_array_equal(matrix, np.diag([2., 3., 5., 7., 11., 13.]))


def test_free_floating_cog_matrix_is_refused(tmp_path):
    spec = synthetic_spec()
    spec.vessel.inertia.mode = "free_floating"
    with pytest.raises(ValueError, match="free_floating"):
        OrcaWaveBackend().generate_single(spec, tmp_path)


def test_native_roundtrip_normalizes_units_and_retains_point(tmp_path):
    from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

    spec = synthetic_spec()
    path = OrcaWaveBackend().generate_single(spec, tmp_path)
    parsed = OrcaWaveInputParser().parse(path)
    assert parsed.vessel.external_stiffness == spec.vessel.external_stiffness
    assert parsed.vessel.external_damping == spec.vessel.external_damping
    assert parsed.vessel.external_stiffness_reference_point == "centre_of_gravity"
    assert parsed.vessel.external_damping_reference_point == "body_origin"


@pytest.mark.parametrize("point", [None, "unsupported", "User specified"])
def test_native_unknown_or_missing_reference_is_refused(tmp_path, point):
    from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

    path = OrcaWaveBackend().generate_single(synthetic_spec(), tmp_path)
    data = yaml.safe_load(path.read_text())
    key = "BodyExternalDampingMatrixOriginType"
    if point is None:
        del data["Bodies"][0][key]
    else:
        data["Bodies"][0][key] = point
    path.write_text(yaml.safe_dump(data))
    with pytest.raises(ValueError, match="origin"):
        OrcaWaveInputParser().parse(path)


def test_provenance_records_native_and_normalized_values(tmp_path):
    import json

    OrcaWaveBackend().generate_single(synthetic_spec(), tmp_path)
    records = json.loads((tmp_path / "external-matrix-provenance.json").read_text())
    assert len(records) == 2
    assert records[0]["canonical_reference_point"] == "centre_of_gravity"
    assert records[0]["native_values"][0][0] == 0.002
    assert records[0]["normalized_values"][0][0] == 2.0
    assert records[0]["native_units_by_block"] == ["kN/m", "kN/rad", "kN.m/m", "kN.m/rad"]


def test_same_canonical_damping_has_equivalent_native_values(tmp_path):
    """Use the independently expanded polynomial in the transfer test.

    The body-origin diagonal is (2,3,5,19,19,13), couplings 15=4 and
    24=-6. Moving to CoG at z=2 cancels those lever arms and leaves
    diagonal (2,3,5,7,11,13). OrcaWave keeps origin and divides by 1000.
    """
    origin = np.diag([2., 3., 5., 19., 19., 13.])
    origin[0, 4] = origin[4, 0] = 4.
    origin[1, 3] = origin[3, 1] = -6.
    spec = synthetic_spec(external_stiffness=None, external_stiffness_units=None,
                          external_stiffness_reference_point=None,
                          external_damping=origin.tolist())
    path = OrcaWaveBackend().generate_single(spec, tmp_path)
    body = yaml.safe_load(path.read_text())["Bodies"][0]
    key = next(k for k in body if k.startswith("BodyExternalDampingMatrixx,"))
    np.testing.assert_array_equal(body[key], origin / 1000.)
    assert body["BodyExternalDampingMatrixOriginType"] == "Body origin"
    cards = AQWABackend().build_deck7(spec)
    rows = [line for line in cards if line[6:10] == "FIDP"]
    native = [[float(row[20 + 10*j:30 + 10*j]) for j in range(6)] for row in rows]
    np.testing.assert_array_equal(native, np.diag([2., 3., 5., 7., 11., 13.]))


@pytest.mark.parametrize("field,value", [("UnitsSystem", "US"), ("UnitsSystem", None)])
def test_native_unknown_units_are_refused(tmp_path, field, value):
    from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

    path = OrcaWaveBackend().generate_single(synthetic_spec(), tmp_path)
    data = yaml.safe_load(path.read_text())
    if value is None:
        del data[field]
    else:
        data[field] = value
    path.write_text(yaml.safe_dump(data))
    with pytest.raises(ValueError, match="explicit OrcaWave SI"):
        OrcaWaveInputParser().parse(path)


def test_mutated_contract_is_revalidated_at_emission(tmp_path):
    spec = synthetic_spec()
    spec.vessel.external_damping_reference_point = None
    with pytest.raises(ValueError, match="external_damping_reference_point"):
        OrcaWaveBackend().generate_single(spec, tmp_path)


@pytest.mark.parametrize("field", ["attitude", "position", "reference_point"])
def test_aqwa_unsupported_coordinate_mapping_is_refused(field):
    spec = synthetic_spec(external_stiffness=None, external_stiffness_units=None,
                          external_stiffness_reference_point=None)
    body = spec.get_bodies()[0]
    if field == "reference_point":
        body.vessel.geometry.reference_point = [0., 0., 1.]
    else:
        setattr(body, field, [0., 0., 1.])
    spec.bodies = [body]
    spec.vessel = None
    with pytest.raises(ValueError, match="AQWA external matrices require"):
        AQWABackend().build_deck7(spec)
