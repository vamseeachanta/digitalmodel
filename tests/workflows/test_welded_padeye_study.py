"""Independent equilibrium checks for an idealized rectangular welded padeye."""

from dataclasses import replace
import math

import pytest

from digitalmodel.lifting_lug import welded_study as study


@pytest.fixture
def geometry():
    # Synthetic dimensions in inches, deliberately unrelated to a source drawing.
    return study.Geometry(4, 6, 0, 3, 0.5, 0.25, "idealized rectangle")


@pytest.fixture
def load():
    # Synthetic load basis: 12 kip * 1.5 / 3 equally loaded lugs = 6 kip/lug.
    return study.Load(12, 1.5, 3, "independent arithmetic example")


def test_independent_endpoint_and_section_calculation(geometry, load):
    """Hand calculation, equilibrium (OpenStax, 2016, §12.1).

    Two coincident projected lines x=-2..2: L=8, J=2*(4**3/12)=32/3.
    At 45 degrees: Fx=Fy=6, Mz=0*6-3*6=-18 kip-in.
    Endpoint x=-2: q=(0.75, 0.75+27/8); |q|=sqrt(1125)/8.
    Throat=0.25/sqrt(2). For B=4,t=1: axial=1.5,
    bending=6*(-18)/16=-6.75; left=8.25, right=-5.25; shear=1.5.
    """
    result = study.demands(
        geometry, load, thickness=1, scale=1, weld_multiplier=1, angle=45, direction=1
    )
    assert result["fx_kip"] == pytest.approx(6)
    assert result["fy_kip"] == pytest.approx(6)
    assert result["mz_kip_in"] == pytest.approx(-18)
    assert result["resultant_kip"] == pytest.approx(math.sqrt(72))
    assert result["qmax_kip_per_in"] == pytest.approx(math.sqrt(1125) / 8)
    assert result["throat_ksi"] == pytest.approx(math.sqrt(1125) / math.sqrt(2))
    assert result["sigma_left_ksi"] == pytest.approx(8.25)
    assert result["sigma_right_ksi"] == pytest.approx(-5.25)
    assert result["tau_average_ksi"] == pytest.approx(1.5)


def test_scaling_thickness_and_direction(geometry, load):
    """Doubling B and lever arms halves q; doubling t halves plate stress.

    Exterior scaling keeps hole radius fixed; weld leg stays absolute.
    Negative Fx reverses Mz for rx=0 and swaps left/right plate stresses.
    """
    base = study.demands(geometry, load, 1, 1, 1, 45, 1)
    thick = study.demands(geometry, load, 2, 1, 1, 45, 1)
    scaled = study.demands(geometry, load, 1, 2, 1, 45, 1)
    negative = study.demands(geometry, load, 1, 1, 1, 45, -1)
    welded = study.demands(geometry, load, 1, 1, 2, 45, 1)
    assert thick["sigma_left_ksi"] == pytest.approx(base["sigma_left_ksi"] / 2)
    assert thick["qmax_kip_per_in"] == base["qmax_kip_per_in"]
    assert scaled["qmax_kip_per_in"] == pytest.approx(base["qmax_kip_per_in"] / 2)
    assert negative["sigma_right_ksi"] == base["sigma_left_ksi"]
    assert welded["throat_ksi"] == base["throat_ksi"] / 2


def test_grid_accounting_and_envelopes(geometry, load):
    data = study.build_study(geometry, load)
    assert len(data["rows"]) == 225  # plan Table 1: 5*5*3*3
    directions = [d for r in data["rows"] for d in r["directions"].values()]
    assert sum(d["status"] == "CALCULATED_SCREENING" for d in directions) == 375
    assert sum(d["status"] == "NOT_APPLICABLE" for d in directions) == 75
    row = study.lookup(data, data["revision"], [1.25, 1, 1, 45], "screening")
    assert row["envelopes"]["mz_kip_in"]["direction"] == "positive"
    assert row["benchmark_status"] == "UNVERIFIED_SOURCE"
    assert set(row["envelopes"]) == set(study.UNITS)
    assert all(v["verification"] == "NOT_EVALUATED" for v in row["envelopes"].values())


@pytest.mark.parametrize("keys", [[1.251, 1, 1, 45], [1.25, 1, 1, 44]])
def test_off_grid_rejected(geometry, load, keys):
    data = study.build_study(geometry, load)
    with pytest.raises(KeyError):
        study.lookup(data, data["revision"], keys, "screening")


def test_revision_use_and_tampering_rejected(geometry, load):
    data = study.build_study(geometry, load)
    with pytest.raises(ValueError, match="revision"):
        study.lookup(data, "stale", [1.25, 1, 1, 45], "screening")
    with pytest.raises(ValueError, match="intended use"):
        study.lookup(data, data["revision"], [1.25, 1, 1, 45], "design")
    data["rows"][0]["directions"]["positive"]["values"]["fy_kip"] = 999
    with pytest.raises(ValueError, match="digest"):
        study.lookup(data, data["revision"], [1.25, 1, 1, 45], "screening")


def test_invalid_geometry_retained_without_demands(geometry, load):
    data = study.build_study(replace(geometry, hole_radius=20), load)
    assert len(data["rows"]) == 225
    assert all(r["status"] == "INVALID_GEOMETRY" for r in data["rows"])
    assert all(r["envelopes"] is None for r in data["rows"])
    assert all(r["directions"]["positive"]["values"] is None for r in data["rows"])


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), True])
def test_bad_load_rejected(value):
    with pytest.raises(ValueError):
        study.Load(value, 1, 2, "test")


@pytest.mark.parametrize("angle", [-1, 90, float("nan")])
def test_bad_angle_rejected(geometry, load, angle):
    with pytest.raises(ValueError):
        study.demands(geometry, load, 1, 1, 1, angle, 1)


def test_missing_provenance_rejected():
    with pytest.raises(ValueError, match="source"):
        study.Load(12, 1.5, 3, "")


def test_export_roundtrip_and_report(geometry, load, tmp_path):
    import csv
    import json

    data = study.build_study(geometry, load)
    study.write_outputs(data, tmp_path)
    saved = json.loads((tmp_path / "study.json").read_text())
    assert saved == data
    assert study.lookup(saved, data["revision"], [1.25, 1, 1, 45], "screening")
    with (tmp_path / "study.csv").open() as stream:
        records = list(csv.DictReader(stream))
    assert len(records) == 450
    assert sum(r["status"] == "NOT_APPLICABLE" for r in records) == 75
    report = (tmp_path / "report.html").read_text()
    assert "Revision history" in report
    assert "UNVERIFIED_SOURCE" in report
    assert "<svg" in report
    assert "NOT_EVALUATED" in report
    assert data["revision"] in report


@pytest.mark.parametrize("thickness", [0.75, 1.25, 1.75])
@pytest.mark.parametrize("scale", [0.8, 1, 1.2])
@pytest.mark.parametrize("weld", [1, 1.5])
@pytest.mark.parametrize("angle", [0, 30, 45])
def test_independent_minimum_subgrid(geometry, load, thickness, scale, weld, angle):
    """Independent force-couple reconstruction: 54 cases, 90 directions.

    OpenStax (2016) §12.1: sum forces and moments. Divide each line into
    four intervals and integrate x² exactly as (x1³-x0³)/3; both welds
    contribute. Each endpoint's rotational vertical line load is M*x/J.
    Section modulus is derived from I=integral(t*x² dx), divided by B/2.
    Tolerance 1e-12 is an arithmetic regression tolerance, NOT an engineering
    acceptance limit or historical-source verification criterion.
    """
    geometry = replace(geometry, hole_x=0.25)  # synthetic asymmetry
    width = 4 * scale
    nodes = [-width / 2 + i * width / 4 for i in range(5)]
    integral = sum((end**3 - start**3) / 3 for start, end in zip(nodes, nodes[1:]))
    polar = 2 * integral
    modulus = thickness * integral / (width / 2)
    for sign in [1] if angle == 0 else [1, -1]:
        fy = 6
        fx = sign * 6 * math.tan(math.radians(angle))
        moment = 0.25 * scale * fy - 3 * scale * fx
        direct = complex(fx, fy) / (2 * width)
        q = max(
            abs(direct + complex(0, moment * x / polar)) for x in (nodes[0], nodes[-1])
        )
        expected = [
            fx,
            fy,
            moment,
            abs(complex(fx, fy)),
            q,
            q / (0.25 * weld / math.sqrt(2)),
            fy / (width * thickness) - moment / modulus,
            fy / (width * thickness) + moment / modulus,
            fx / (width * thickness),
        ]
        actual = study.demands(geometry, load, thickness, scale, weld, angle, sign)
        assert list(actual.values()) == pytest.approx(expected, rel=1e-12, abs=1e-12)


def test_no_overwrite_and_html_escaping(geometry, load, tmp_path):
    data = study.build_study(
        replace(geometry, source='<script>alert("x")</script>'), load
    )
    study.write_outputs(data, tmp_path)
    original = (tmp_path / "study.json").read_bytes()
    with pytest.raises(FileExistsError):
        study.write_outputs(data, tmp_path)
    assert (tmp_path / "study.json").read_bytes() == original
    report = (tmp_path / "report.html").read_text()
    assert "<script>" not in report
    assert "&lt;script&gt;" in report


def test_lookup_cannot_mutate_dataset(geometry, load):
    data = study.build_study(geometry, load)
    row = study.lookup(data, data["revision"], [1.25, 1, 1, 45], "screening")
    row["status"] = "DESIGN_APPROVED"
    assert study.validate(data)
    assert (
        study.lookup(data, data["revision"], [1.25, 1, 1, 45], "screening")["status"]
        == "CALCULATED_SCREENING"
    )
