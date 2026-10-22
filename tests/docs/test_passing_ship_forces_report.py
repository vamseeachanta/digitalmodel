# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check report reproduction, numerical readback and the offline document contract."""

import copy
import importlib.util
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.wang_passing_ship import WangVessel, wang_passing_forces


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/passingshipforces"
spec = importlib.util.spec_from_file_location("force_report", REPORT / "generate_report.py")
report = importlib.util.module_from_spec(spec)


class Document(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.ids = []
        self.links = []
        self.numbers = {}
        self.current = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if "data-number" in attrs:
            self.current = attrs["data-number"]
            assert self.current not in self.numbers

    def handle_data(self, data):
        if self.current:
            self.numbers[self.current] = data

    def handle_endtag(self, tag):
        if tag == "span":
            self.current = None


@pytest.fixture(scope="module")
def generated(tmp_path_factory):
    spec.loader.exec_module(report)
    work = tmp_path_factory.mktemp("force-report")
    model = report.calculate(work / "calculation")
    text = report.render(model)
    return model, text, work


def test_headlines_have_independent_values_labels_and_units(generated):
    _, text, _ = generated
    parsed = Document(text)
    # Public input example and sampled locations, pinned without generator helpers.
    expected_inputs = {
        "moored_vessel.length": "941.109", "passing_vessel.length": "941.109",
        "moored_vessel.midship_area": "8,103.879", "passing_vessel.midship_area": "8,103.879",
        "water_density": "1.9905", "passing_velocity": "6", "separation_distance": "364.042",
        "water_depth": "55", "first_stagger": "−1,882.2", "last_stagger": "1,882.2",
        "n_stagger": "201", "quadrature_nodes": "96", "n_images": "10",
        "max_surge_force_stagger": "376.4", "min_surge_force_stagger": "−376.4",
        "max_sway_force_stagger": "0", "min_sway_force_stagger": "−752.9",
        "max_yaw_moment_stagger": "301.2", "min_yaw_moment_stagger": "−301.2",
    }
    for key, expected in expected_inputs.items():
        assert parsed.numbers[key] == expected
    keys = set(expected_inputs)
    # Raw-kernel quadrature readback, independent of report formatting helpers.
    for component, magnitude, unit in (
        ("surge_force", 34484.7, "lbf"),
        ("sway_force", 87637.8, "lbf"),
        ("yaw_moment", 14304244.2, "ft·lbf"),
    ):
        for direction, sign in (("max", 1), ("min", -1)):
            key = f"{direction}_{component}"
            keys.update((key, f"summary-{key}"))
            expected = -36095.3 if key == "min_sway_force" else sign * magnitude
            displayed = float(parsed.numbers[key].replace(",", "").replace("−", "-"))
            assert displayed == pytest.approx(expected, abs=0.1)
            assert parsed.numbers[f"summary-{key}"] == parsed.numbers[key]
            row = next(row for row in re.findall(r"<tr>.*?</tr>", text)
                       if f'data-number="{key}"' in row)
            label = {"surge_force": "surge", "sway_force": "sway",
                     "yaw_moment": "yaw moment"}[component]
            assert f"<td>{direction.title()}imum {label}</td>" in row
            assert f"<td>{unit}</td>" in row
    assert set(parsed.numbers) == keys
    summary = text.split('<section id="summary">', 1)[1].split('</section>', 1)[0]
    summary = re.sub(r"<[^>]+>", "", summary)
    assert ("The finite-depth sampled ranges are surge −34,484.7 to 34,484.7 lbf, "
            "sway −36,095.3 to 87,637.8 lbf and yaw −14,304,244.2 to 14,304,244.2 ft·lbf.") in summary
    # Stored workbook oracle: tests/hydrodynamics/test_wang_passing_ship.py,
    # WB_TABLE at zero stagger; the rounded oracle has a 1% tolerance.
    sway = float(parsed.numbers["max_sway_force"].replace(",", ""))
    assert sway == pytest.approx(87640.0, rel=1e-2)
    assert parsed.numbers["max_sway_force_stagger"] == "0"


def test_plot_is_workflow_consistency_check(generated):
    model, text, _ = generated
    settings = model["settings"]
    # This is internal consistency, not an independent numerical oracle.
    for mode in ("finite", "deep"):
        result = wang_passing_forces(
            [row["stagger"] for row in model[mode]["forces"]],
            settings["separation_distance"],
            WangVessel(**settings["moored_vessel"]),
            WangVessel(**settings["passing_vessel"]),
            settings["passing_velocity"], settings["water_density"],
            depth=settings["water_depth"] if mode == "finite" else None,
        )
        for field, values in (("stagger", result.stagger),
                              ("surge_force", result.surge),
                              ("sway_force", result.sway),
                              ("yaw_moment", result.yaw)):
            np.testing.assert_array_equal(
                [row[field] for row in model[mode]["forces"]], values
            )
    payload = re.search(r'<script id="chart-data" type="application/json">(.*?)</script>',
                        text, re.S).group(1)
    payload = json.loads(payload)
    for mode in ("finite", "deep"):
        for actual, expected in zip(payload[mode], model[mode]["forces"], strict=True):
            assert set(actual) == set(expected)
            for field in actual:
                assert actual[field] == pytest.approx(expected[field], abs=0.051, rel=0)
                assert actual[field] == round(actual[field], 1)


def test_rebuild_is_identical_and_uses_house_stylesheet(generated):
    model, text, work = generated
    out = work / "rebuilt"
    report.build(out, work / "rebuild-calculation")
    assert (out / "report.html").read_text(encoding="utf-8") == text
    issued = (REPORT / "report.html").read_text(encoding="utf-8")
    pattern = r'(<script id="chart-data" type="application/json">)(.*?)(</script>)'
    assert re.sub(pattern, r"\1\3", text, flags=re.S) == re.sub(
        pattern, r"\1\3", issued, flags=re.S
    )
    rebuilt_data = json.loads(re.search(pattern, text, re.S)[2])
    issued_data = json.loads(re.search(pattern, issued, re.S)[2])
    for mode in ("finite", "deep"):
        assert len(rebuilt_data[mode]) == len(issued_data[mode]) == 201
        for actual, expected in zip(rebuilt_data[mode], issued_data[mode], strict=True):
            assert actual.keys() == expected.keys()
            np.testing.assert_allclose(list(actual.values()), list(expected.values()),
                                       rtol=0, atol=0.1)
    assert (out / "changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()
    assert report.house_style() in text
    assert '<body class="hs-doc">' in text
    assert '<script src=' not in text and '<link rel="stylesheet"' not in text
    assert "showDirectoryPicker" in text and "createWritable" in text
    assert "report.comments.json" in text and 'type="file"' in text
    assert "localStorage" in text and "textContent" in text
    assert "changes-panel" in text


def test_document_structure_numbers_and_public_wording(generated):
    _, text, _ = generated
    doc = Document(text)
    assert len(doc.ids) == len(set(doc.ids))
    assert set(doc.links) <= set(doc.ids)
    assert len(re.findall(r"<h2[ >]", text)) == 10
    assert not re.search(r"<h[4-6][ >]|<strong|<b[ >]", text)
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    assert not re.search(r"\b(could|should|would|own" + r"er|validated|certified|compliant)\b",
                         visible, re.I)
    assert "NOT_EVALUATED" in visible
    assert "ASSUMED - to be confirmed" in visible
    assert "Revision history" in visible
    assert "9.2 Source locators" in visible
    assert "Field validation" in visible
    assert "Limit: NOT_EVALUATED" in text
    # No typed numerical claims in worked-example prose: all come from spans.
    worked = text.split('id="worked-example"', 1)[1].split('</section>', 1)[0]
    worked = re.sub(r'<h2>.*?</h2>', "", worked)
    worked = re.sub(r'<span data-number="[^"]+">.*?</span>', "", worked)
    worked = re.sub(r"<[^>]+>", "", worked)
    assert not re.search(r"\d", worked)


def test_extrema_do_not_depend_on_opposite_sign_ties(generated):
    model, _, _ = generated
    result = dict(model["finite"])
    result["forces"] = [
        {"stagger": -2e-13, "surge_force": -10.0, "sway_force": 30.0, "yaw_moment": 20.0},
        {"stagger": 1.0, "surge_force": 10.0, "sway_force": -5.0, "yaw_moment": -20.0},
    ]
    a = report.worked_numbers(model["settings"], result)
    result["peak_surge_force"] = 10.0  # A different absolute-peak tie selection.
    b = report.worked_numbers(model["settings"], result)
    assert a == b
    assert a["min_surge_force"] == -10.0
    assert a["max_surge_force"] == 10.0
    assert a["max_sway_force_stagger"] == 0.0


def test_rebuild_survives_platform_roundoff(generated):
    model, text, _ = generated
    perturbed = copy.deepcopy(model)
    for mode in ("finite", "deep"):
        for row in perturbed[mode]["forces"]:
            for field in ("surge_force", "sway_force", "yaw_moment"):
                # Opposite perturbations break odd and even symmetry ties.
                row[field] *= 1 + (1e-14 if row["stagger"] > 0 else -1e-14)
    assert report.render(perturbed) == text


def raw_kernel_loads(stagger, images):
    """Integrate both hulls directly without the implementation antiderivatives."""
    q, w = np.polynomial.legendre.leggauss(100)
    length, area = 941.109, 8103.879
    x1, x2 = q[:, None] * length / 2, q[None, :] * length / 2
    s1 = area * (1 - 4 * x1**2 / length**2)
    ds1, ds2 = -8 * area * x1 / length**2, -8 * area * x2 / length**2
    weights = w[:, None] * w[None, :] * (length / 2)**2
    r = x2 - x1 + stagger
    total = np.zeros(3)
    for image in range(-images, images + 1):
        eta = np.hypot(364.042, 2 * image * 55.0)
        kernel = ds2 / (eta**2 + r**2)**1.5 * weights
        total += [np.sum(ds1 * r * kernel) / 2,
                  364.042 * np.sum(ds1 * kernel),
                  364.042 * np.sum((ds1 * x1 + s1) * kernel)]
    return total * 1.9905 * 6.0**2 / np.pi


def test_image_truncation_against_raw_kernel_quadrature(generated):
    model, text, _ = generated
    cases = model["truncation"]
    assert len(cases) == 4
    for case in cases:
        field = case["component"]
        index = ("surge_force", "sway_force", "yaw_moment").index(field)
        legacy = raw_kernel_loads(case["stagger"], 10)[index]
        reference = raw_kernel_loads(case["stagger"], 400)[index]
        assert case["legacy"] == pytest.approx(legacy, rel=1e-8)
        assert case["reference"] == pytest.approx(reference, rel=1e-8)
        assert case["difference_percent"] == pytest.approx(
            100 * (legacy - reference) / reference, abs=1e-6
        )
        assert f'{case["difference_percent"]:+.1f} %' in text
    assert "21 image terms" in text and "801 image terms" in text
    assert "reproduces the legacy tool" in text


def test_sources_signs_and_neutral_status(generated):
    _, text, _ = generated
    rows = re.findall(r"<tr>.*?</tr>", text)
    for row in rows:
        if 'data-number=' not in row:
            continue
        source = re.search(r'href="#([^"]+)"', row)[1]
        if any(key in row for key in ('data-number="quadrature_nodes"',)):
            assert source == "source-theory"
        elif any(key in row for key in ('data-number="max_', 'data-number="min_',
                                       'data-number="first_stagger"',
                                       'data-number="last_stagger"',
                                       'data-number="n_stagger"', 'data-number="n_images"')):
            assert source == "source-workflow"
        else:
            assert source == "source-example"
    assert "X &gt; 0 acts toward +x" in text
    assert "N &gt; 0 turns the +x end toward the passing hull" in text
    assert "ASCE, Vol. 101, WW3, p. 247, 1975" in text
    assert "MARSIM'03" in text
    assert "Draft for engineering review" in text


def test_utf8_reads_and_lf_writes(generated, monkeypatch):
    _, _, work = generated
    read_text, write_text = Path.read_text, Path.write_text

    def read(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        return read_text(path, *args, **kwargs)

    def write(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        assert kwargs.get("newline") == "\n"
        return write_text(path, *args, **kwargs)

    monkeypatch.setattr(Path, "read_text", read)
    monkeypatch.setattr(Path, "write_text", write)
    output = work / "encoding"
    report.build(output, work / "encoding-calculation")
    for name in ("report.html", "changes.json"):
        data = (output / name).read_bytes()
        assert b"\r" not in data
        data.decode("utf-8")
