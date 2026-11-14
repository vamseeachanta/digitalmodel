"""Checks on the sourced IEA 15 MW / VolturnUS-S generator data file.

The file is extracted from public reference files (see its ``source_sha256``)
by ``scripts/fowt/extract_iea15_generator_data.py``. These tests check that
every value carries a unit and a pinned source, and that the extracted values
agree with the independently curated ``basis.yml`` and with the published
report values that basis records.
"""
from __future__ import annotations

import copy
import importlib.util
import math
import os
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
FOLDER = ROOT / "docs" / "domains" / "orcaflex" / "wind" / "fowt-volturnus"
DATA = FOLDER / "generator_data.yml"
BASIS = FOLDER / "basis.yml"
COMMIT = "e4993d63de10f165389534461dd544006750fe60"


@pytest.fixture(scope="module")
def data() -> dict:
    return yaml.safe_load(DATA.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def basis() -> dict:
    return yaml.safe_load(BASIS.read_text(encoding="utf-8"))


def _leaves(node, path=()):
    if isinstance(node, dict):
        if "value" in node:
            yield path, node
            return
        for key, child in node.items():
            yield from _leaves(child, path + (key,))


def test_every_value_has_unit_and_pinned_source(data):
    leaves = list(_leaves({k: data[k] for k in ("turbine", "rotor_performance",
                                                 "hydrodynamics")}))
    assert len(leaves) >= 20
    for path, leaf in leaves:
        assert leaf.get("unit"), path
        assert COMMIT in leaf["source"], path
    assert data["rotor_performance"]["columns"][3] == "rotor thrust N"


def test_source_digests_recorded(data):
    digests = data["source_sha256"]
    assert len(digests) == 4
    assert all(len(v) == 64 for v in digests.values())


@pytest.mark.parametrize("key", ["rotor_center_of_mass_from_tower_top",
                                 "rna_component_centre_discrepancy"])
def test_derived_centres_have_twelve_decimal_place_precision(data, key):
    """Derived geometry uses a fixed precision for byte reproduction."""
    assert all(value == round(value, 12) for value in data["turbine"][key]["value"])


def test_rotor_plus_nacelle_equals_rna_and_basis(data, basis):
    turbine = data["turbine"]
    total = turbine["rotor_mass"]["value"] + turbine["nacelle_mass"]["value"]
    assert total == pytest.approx(turbine["rna_mass"]["value"], rel=1e-9)
    assert turbine["rna_mass"]["value"] == pytest.approx(
        basis["system"]["rna_mass"]["value"], rel=1e-12)


def test_rna_row_matches_basis_and_component_discrepancy_is_recorded(data, basis):
    """The RNA row is adopted; its offset from the component rows is recorded.

    The workbook's RNA row is not the mass-weighted combination of its own
    nacelle, blade and hub rows. The recorded discrepancy must equal the
    recomputed one, so the file cannot silently drop or alter it.
    """
    turbine = data["turbine"]
    top = basis["system"]["tower_top"]["value"]
    rna = turbine["rna_center_of_mass_from_tower_top"]["value"]
    rna_cg = basis["system"]["rna_center_of_gravity"]["value"]
    for axis in range(3):
        assert rna[axis] + (top if axis == 2 else 0.0) == pytest.approx(rna_cg[axis], abs=1e-9)
    m_r, m_n = turbine["rotor_mass"]["value"], turbine["nacelle_mass"]["value"]
    r_r = turbine["rotor_center_of_mass_from_tower_top"]["value"]
    r_n = turbine["nacelle_center_of_mass_from_tower_top"]["value"]
    recorded = turbine["rna_component_centre_discrepancy"]["value"]
    for axis in range(3):
        combined = (m_r * r_r[axis] + m_n * r_n[axis]) / (m_r + m_n)
        assert combined - rna[axis] == pytest.approx(recorded[axis], abs=1e-9)
    # Size of the source discrepancy at the time of extraction: 0.533 m fore-aft.
    assert recorded[0] == pytest.approx(0.533, abs=0.001)


def test_hub_height_is_tower_top_plus_hub_offset(data, basis):
    turbine = data["turbine"]
    top = basis["system"]["tower_top"]["value"]
    assert top + turbine["tower_top_to_hub"]["value"] == pytest.approx(
        turbine["hub_height"]["value"], abs=1e-9)


def test_rated_row_matches_basis_thrust(data, basis):
    rows = data["rotor_performance"]["value"]
    rated_speed = basis["turbine"]["rated_wind_speed"]["value"]
    rated = next(r for r in rows if r[0] == pytest.approx(rated_speed, rel=1e-12))
    assert rated[3] == pytest.approx(basis["turbine"]["rated_rotor_thrust"]["value"], rel=1e-12)
    assert rated[4] == pytest.approx(15.0e6, rel=1e-6)
    assert max(r[3] for r in rows) == rated[3]


def test_hydrostatics_against_published_table_3(data, basis):
    env = basis["environment"]
    rho_g = env["water_density"]["value"] * env["gravity"]["value"]
    c = data["hydrodynamics"]["hydrostatic_stiffness_nondimensional"]["value"]
    published = basis["published"]["hydrostatic_stiffness"]["value"]
    assert c[2][2] * rho_g == pytest.approx(published[2], rel=0.02)
    assert c[3][3] * rho_g == pytest.approx(published[3], rel=0.03)
    assert c[4][4] * rho_g == pytest.approx(published[4], rel=0.03)


def test_reference_volume_against_published_displacement(data, basis):
    volume = data["hydrodynamics"]["reference_volume"]["value"]
    assert volume == pytest.approx(basis["published"]["displacement"]["value"], rel=0.01)


@pytest.mark.parametrize("key", ["added_mass_zero_frequency_nondimensional",
                                 "added_mass_infinite_frequency_nondimensional"])
def test_added_mass_symmetric_to_one_percent_of_diagonal_scale(data, key):
    matrix = data["hydrodynamics"][key]["value"]
    for i in range(6):
        for j in range(6):
            scale = (abs(matrix[i][i]) * abs(matrix[j][j])) ** 0.5
            assert abs(matrix[i][j] - matrix[j][i]) <= 0.01 * scale, (i, j)


def test_hydrostatic_heave_roll_pitch_block_symmetric(data):
    """The heave/roll/pitch block is symmetric; the roll-yaw entry is not.

    A hydrostatic matrix can carry a non-symmetric roll-yaw term (-9.083 at
    row 4, column 6, zero at row 6, column 4); OrcaFlex takes only the heave,
    roll and pitch rows and columns, which this test checks.
    """
    matrix = data["hydrodynamics"]["hydrostatic_stiffness_nondimensional"]["value"]
    for i in (2, 3, 4):
        for j in (2, 3, 4):
            assert matrix[i][j] == pytest.approx(matrix[j][i], abs=1e-9)


def test_quadratic_damping_symmetric(data):
    matrix = data["hydrodynamics"]["additional_quadratic_damping"]["value"]
    for i in range(6):
        for j in range(6):
            assert matrix[i][j] == matrix[j][i]


def test_radiation_rows_cover_natural_frequencies(data):
    rows = data["hydrodynamics"]["radiation_by_period_nondimensional"]["value"]
    columns = data["hydrodynamics"]["radiation_by_period_nondimensional"]["columns"]
    assert columns == ["period s", "A11", "A33", "A55", "A15", "A66",
                       "B11", "B33", "B55", "B15", "B66"]
    assert len(rows) == 100
    assert all(len(row) == len(columns) for row in rows)
    assert all(math.isfinite(value) for row in rows for value in row)
    periods = [r[0] for r in rows]
    assert len(set(periods)) == 100
    # Source .1 rows at the first, middle and last periods, in exported column order.
    expected = {
        0: [1.256637, 9231.548, 24148.55, 11326590.0, -98121.86, 19268110.0,
            13.99775, 0.00157575, -11.74371, 21.6608, 22459.44],
        49: [2.463995, 8691.975, 23991.02, 11252500.0, -95514.23, 17933450.0,
             325.9279, 0.03290099, 953.9129, -551.9385, 752555.2],
        99: [125.6637, 12346.81, 26529.58, 12165730.0, -117375.5, 25324850.0,
             0.8817627, 240.8925, 7.673256, 2.615074, 0.02889738],
    }
    for index, values in expected.items():
        assert rows[index] == pytest.approx(values, rel=1e-12)
    assert periods == sorted(periods)
    # Published heave and pitch periods (about 20 s and 28 s) lie inside the table.
    assert periods[0] < 20.0 and periods[-1] > 28.0
    assert all(r[2] > 0 and r[3] > 0 for r in rows)


@pytest.mark.parametrize("corruption", ["missing_periods", "nan_damping", "short_row",
                                        "infinite_mass", "source_value", "duplicate_period"])
def test_radiation_validation_rejects_corruption(data, corruption):
    damaged = copy.deepcopy(data)
    rows = damaged["hydrodynamics"]["radiation_by_period_nondimensional"]["value"]
    if corruption == "missing_periods":
        rows[:] = [rows[0], rows[-1]]
    elif corruption == "nan_damping":
        for row in rows:
            row[6:] = [float("nan")] * 5
    elif corruption == "short_row":
        rows[10].pop()
    elif corruption == "infinite_mass":
        rows[10][1] = float("inf")
    elif corruption == "source_value":
        rows[49][8] *= 2.0
    elif corruption == "duplicate_period":
        rows[10][0] = rows[9][0]
    with pytest.raises(AssertionError):
        test_radiation_rows_cover_natural_frequencies(damaged)


def test_finite_frequency_export_declares_diagnostic_subset(data):
    radiation = data["hydrodynamics"]["radiation_by_period_nondimensional"]
    assert radiation["coverage"] == "diagnostic subset"
    assert "omitted entries must not be treated as zero" in radiation["limitations"]
    assert "18" in radiation["limitations"]


@pytest.fixture(scope="module")
def extractor():
    script = ROOT / "scripts" / "fowt" / "extract_iea15_generator_data.py"
    spec = importlib.util.spec_from_file_location("extract_iea15", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("key", ["tabular", "wamit1", "hst", "hydrodyn"])
def test_extraction_rejects_altered_input_before_parsing(tmp_path, monkeypatch, extractor,
                                                       data, key):
    altered = tmp_path / Path(extractor.FILES[key]).name
    altered.write_bytes(b"altered source bytes")
    real_sha256 = extractor.sha256

    def input_digest(path):
        if path == altered:
            return real_sha256(path)
        rel = next(rel for rel in extractor.FILES.values() if Path(rel).name == path.name)
        return data["source_sha256"][rel]

    def unexpected_parse(path):
        pytest.fail("source parsing started before digest verification completed")

    monkeypatch.setattr(extractor, "sha256", input_digest)
    monkeypatch.setattr(extractor, "read_tabular", unexpected_parse)
    with pytest.raises(ValueError, match="source digest mismatch") as error:
        extractor.build(tmp_path)
    assert Path(extractor.FILES[key]).name in str(error.value)
    assert str(tmp_path) not in str(error.value)


def test_extractor_pins_match_recorded_digests(extractor, data):
    assert extractor.COMMIT == data["reference_commit"] == COMMIT
    assert {extractor.FILES[key]: digest for key, digest in
            extractor.EXPECTED_SHA256.items()} == data["source_sha256"]


@pytest.mark.skipif(not os.environ.get("DIGITALMODEL_IEA15_RAW"),
                    reason="raw reference files not present; set DIGITALMODEL_IEA15_RAW")
def test_extraction_reproduces_committed_file(tmp_path, extractor):
    out = tmp_path / "generator_data.yml"
    extractor.main(["x", os.environ["DIGITALMODEL_IEA15_RAW"], str(out)])
    assert out.read_bytes() == DATA.read_bytes()
