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
    saved = json.loads((tmp_path / "study.json").read_text(encoding="utf-8"))
    assert saved == data
    assert study.lookup(saved, data["revision"], [1.25, 1, 1, 45], "screening")
    with (tmp_path / "study.csv").open(encoding="utf-8") as stream:
        records = list(csv.DictReader(stream))
    assert len(records) == 450
    assert sum(r["status"] == "NOT_APPLICABLE" for r in records) == 75
    report = (tmp_path / "report.html").read_text(encoding="utf-8")
    assert "Revision history" in report
    assert "UNVERIFIED_SOURCE" in report
    assert "<svg" in report
    assert "NOT_EVALUATED" in report
    assert data["revision"] in report


@pytest.mark.parametrize("thickness", [0.75, 1.25, 1.75])
@pytest.mark.parametrize("scale", [0.8, 1, 1.2])
@pytest.mark.parametrize("weld", [1, 1.5])
@pytest.mark.parametrize("angle", [0, 30, 45])
def test_regression_minimum_subgrid(geometry, load, thickness, scale, weld, angle):
    """Regression re-implementation: 54 cases, 90 directions.

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
    report = (tmp_path / "report.html").read_text(encoding="utf-8")
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


def test_revision_is_independent_of_source_line_endings(
    geometry, load, tmp_path, monkeypatch
):
    from pathlib import Path

    source = Path(study.__file__).read_bytes().replace(b"\r\n", b"\n")
    lf = tmp_path / "lf.py"
    crlf = tmp_path / "crlf.py"
    lf.write_bytes(source)
    crlf.write_bytes(source.replace(b"\n", b"\r\n"))
    monkeypatch.setattr(study, "__file__", str(lf))
    first = study.build_study(geometry, load)
    monkeypatch.setattr(study, "__file__", str(crlf))
    second = study.build_study(geometry, load)
    assert first["code_sha256"] == second["code_sha256"]
    assert first["revision"] == second["revision"]
    crlf.write_bytes(source + b"# changed source\n")
    changed = study.build_study(geometry, load)
    assert changed["code_sha256"] != first["code_sha256"]
    assert changed["revision"] != first["revision"]


@pytest.mark.parametrize("edge", ["left", "right", "bottom", "top"])
def test_zero_ligament_boundary_is_invalid(geometry, load, edge):
    changes = {
        "left": {"hole_x": -1.5},
        "right": {"hole_x": 1.5},
        "bottom": {"hole_y": 0.5},
        "top": {"hole_y": 5.5},
    }
    boundary = replace(geometry, **changes[edge])
    assert (
        boundary.invalid_reason(1)
        == "hole lacks positive edge ligament in rectangular profile"
    )
    with pytest.raises(ValueError, match="positive edge ligament"):
        study.demands(boundary, load, 1, 1, 1, 45, 1)
    key, value = next(iter(changes[edge].items()))
    interior = replace(
        boundary, **{key: math.nextafter(value, 0 if "hole_x" == key else 3)}
    )
    assert interior.invalid_reason(1) is None
    assert all(
        math.isfinite(v) for v in study.demands(interior, load, 1, 1, 1, 45, 1).values()
    )


def test_mixed_validity_retains_nulls_and_report_gaps(geometry, load, tmp_path):
    import csv
    import json

    data = study.build_study(replace(geometry, hole_radius=1.7), load)
    invalid = [row for row in data["rows"] if row["status"] == "INVALID_GEOMETRY"]
    # Only s=0.8 fails: B/2=1.6 < r=1.7; 5 thicknesses * 3 welds * 3 angles.
    assert len(data["rows"]) == 225
    assert len(invalid) == 45
    assert all(row["key"][1] == "0.8" for row in invalid)
    assert all(row["envelopes"] is None for row in invalid)
    assert all(
        d["values"] is None for row in invalid for d in row["directions"].values()
    )
    directions = [d for row in data["rows"] for d in row["directions"].values()]
    # Four valid scales give 4*5*3*(1+2+2)=300 calculations; zero-angle duplicates stay 75.
    assert sum(d["status"] == "CALCULATED_SCREENING" for d in directions) == 300
    assert sum(d["status"] == "INVALID_GEOMETRY" for d in directions) == 75
    assert sum(d["status"] == "NOT_APPLICABLE" for d in directions) == 75
    study.write_outputs(data, tmp_path)
    saved = json.loads((tmp_path / "study.json").read_text(encoding="utf-8"))
    assert saved == data
    with (tmp_path / "study.csv").open(encoding="utf-8") as stream:
        records = list(csv.DictReader(stream))
    assert len(records) == 450
    assert sum(r["status"] == "INVALID_GEOMETRY" for r in records) == 75
    assert all(
        r[name] == ""
        for r in records
        if r["status"] != "CALCULATED_SCREENING"
        for name in study.UNITS
    )
    report = (tmp_path / "report.html").read_text(encoding="utf-8")
    assert "225 cases; 180 calculated cases; 300 calculated directions." in report
    assert report.count(">invalid</text>") == 1
    assert report.count("<td>INVALID_GEOMETRY</td>") == 45
    assert report.count("<td>NOT_EVALUATED</td>") == 405


def test_report_thickness_has_three_decimals(geometry, load):
    from digitalmodel.lifting_lug.welded_study_output import render_report

    report = render_report(study.build_study(geometry, load))
    assert "<td>0.750, 0.8, 1, 0</td>" in report
    assert "<td>1.000, 1, 1, 45</td>" in report
    assert "<td>1.750, 1.2, 1.5, 45</td>" in report


def test_exact_lookup_accepts_negative_zero(geometry, load):
    data = study.build_study(geometry, load)
    row = study.lookup(data, data["revision"], [1.25, 1, 1, -0.0], "screening")
    assert row["key"] == ["1.25", "1", "1", "0"]
    assert row["directions"]["positive"]["values"]["fx_kip"] == pytest.approx(0)
    assert row["directions"]["negative"]["status"] == "NOT_APPLICABLE"


def test_table_captions_precede_rows(geometry, load):
    from html.parser import HTMLParser
    from pathlib import Path
    from digitalmodel.lifting_lug.welded_study_output import render_report

    class Tables(HTMLParser):
        def __init__(self):
            super().__init__()
            self.first_children = []
            self.awaiting_child = False

        def handle_starttag(self, tag, attrs):
            if tag == "table":
                self.awaiting_child = True
            elif self.awaiting_child:
                self.first_children.append(tag)
                self.awaiting_child = False

    document = (
        Path(__file__).resolve().parents[2] / "docs/domains/lifting/welded-padeye.html"
    )
    for html in (
        render_report(study.build_study(geometry, load)),
        document.read_text(encoding="utf-8"),
    ):
        parser = Tables()
        parser.feed(html)
        assert parser.first_children
        assert set(parser.first_children) == {"caption"}


@pytest.mark.parametrize("stage", ["csv", "html", "readback"])
@pytest.mark.parametrize("preexisting", [False, True])
def test_failed_export_leaves_no_partial_issue(
    geometry, load, tmp_path, monkeypatch, stage, preexisting
):
    from digitalmodel.lifting_lug import welded_study_output as output

    data = study.build_study(geometry, load)
    destination = tmp_path / "issue"
    if preexisting:
        destination.mkdir()

    def fail(*args, **kwargs):
        raise OSError("injected output failure")

    with monkeypatch.context() as patch:
        if stage == "csv":
            patch.setattr(output.csv.DictWriter, "writerow", fail)
        elif stage == "html":
            patch.setattr(output, "render_report", fail)
        else:
            original_validate = output.validate

            def fail_readback(payload):
                if payload is not data:
                    fail()
                return original_validate(payload)

            patch.setattr(output, "validate", fail_readback)
        with pytest.raises(OSError, match="injected output failure"):
            study.write_outputs(data, destination)
    assert not destination.exists() or list(destination.iterdir()) == []
    assert sorted(p.name for p in tmp_path.iterdir()) == (
        ["issue"] if preexisting else []
    )
    study.write_outputs(data, destination)
    assert sorted(p.name for p in destination.iterdir()) == [
        "report.html",
        "study.csv",
        "study.json",
    ]


def test_atomic_export_preserves_unrelated_content(geometry, load, tmp_path):
    note = tmp_path / "retained.txt"
    note.write_text("retained data\n", encoding="utf-8", newline="\n")
    with pytest.raises(FileExistsError, match="new revision directory"):
        study.write_outputs(study.build_study(geometry, load), tmp_path)
    assert note.read_text(encoding="utf-8") == "retained data\n"
    assert sorted(p.name for p in tmp_path.iterdir()) == ["retained.txt"]


def test_exports_use_utf8_and_lf_without_rounding_inputs(geometry, load, tmp_path):
    data = study.build_study(
        replace(geometry, weld_leg=0.0254, source="synthetic rectangle — exact"), load
    )
    study.write_outputs(data, tmp_path)
    for name in ("study.json", "study.csv", "report.html"):
        contents = (tmp_path / name).read_bytes()
        assert b"\r\n" not in contents
        contents.decode("utf-8")
    report = (tmp_path / "report.html").read_text(encoding="utf-8")
    assert "synthetic rectangle — exact" in report
    assert "<dt>geometry.weld_leg</dt><dd>0.0254</dd>" in report


def test_atomic_export_resolves_destination_before_staging(geometry, load, tmp_path):
    destination = tmp_path / "unused" / ".." / "issue"
    study.write_outputs(study.build_study(geometry, load), destination)
    assert sorted(p.name for p in tmp_path.iterdir()) == ["issue"]
    assert sorted(p.name for p in (tmp_path / "issue").iterdir()) == [
        "report.html",
        "study.csv",
        "study.json",
    ]
