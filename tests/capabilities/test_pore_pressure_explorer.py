# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regression checks for the synthetic pore-pressure explorer.

Input basis: synthetic illustrative inputs defined in the builder, assumed
and unsourced, based on previous experience rather than measured well data.
Equation source: Eaton (1975), SPE 5544, sonic relation documented in
src/digitalmodel/well/drilling/pore_pressure.py (Methods).

Plan-lite:
Check that the normal sonic trend gives normal pressure independently of
synthetic overburden, and that a JSON round trip preserves null predictions
and the missing-calibration status and reason. Run the focused suites with
temporary output directed outside the repository:
    PYTHONPATH=src uv run --no-project --with pytest --with PyYAML --with pandas
        python -m pytest tests/capabilities/test_pore_pressure_explorer.py
        tests/well/test_pore_pressure.py -q -rA -p no:cacheprovider
The command is shown across lines for readability; join them when running.
Synthetic, uncalibrated inputs do not qualify an engineering prediction.
"""

import importlib.util
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/capabilities/build_pore_pressure_explorer.py"
OUTPUT = ROOT / "docs/api/structural"


class _BodyText(HTMLParser):
    """Collect body text outside scripts/styles; no computed CSS visibility."""

    def __init__(self, page):
        super().__init__()
        self.in_body = False
        self.hidden = 0
        self.parts = []
        self.feed(page)
        self.close()

    def handle_starttag(self, tag, attrs):
        if tag == "body":
            self.in_body = True
        if tag in ("script", "style"):
            self.hidden += 1

    def handle_endtag(self, tag):
        if tag == "body":
            self.in_body = False
        if tag in ("script", "style"):
            self.hidden -= 1

    def handle_data(self, data):
        if self.in_body and not self.hidden:
            self.parts.append(data)


def _body_text(page):
    return " ".join(" ".join(_BodyText(page).parts).split())


def _assert_disclosure(visible, phrase):
    assert phrase in visible


@pytest.mark.parametrize("page", [
    "<html><head><title>{phrase}</title></head><body>Example</body></html>",
    '<html><head><style>p::before{{content:"{phrase}"}}</style></head>'
    "<body>Example</body></html>",
    '<body><style>p::before{{content:"{phrase}"}}</style>Example</body>',
    '<body><script>const disclosure = "{phrase}";</script>Example</body>',
    "<body><!-- {phrase} -->Example</body>",
    '<body><p title="{phrase}">Example</p></body>',
], ids=["title", "head-style", "body-style", "body-script", "comment", "attribute"])
def test_disclosure_rejects_non_body_text(page):
    phrase = "no window ceiling is established"
    with pytest.raises(AssertionError):
        _assert_disclosure(_body_text(page.format(phrase=phrase)), phrase)


@pytest.mark.parametrize("markup", [
    "<p>no window ceiling is established</p>",
    "<p>no window <em>ceiling</em> is established</p>",
    "<p>no&#32;window&nbsp;ceiling is established</p>",
    '<script>const example = "ignored";</script><p>no window ceiling is established</p>',
], ids=["body", "inline", "entities", "after-script"])
def test_disclosure_accepts_body_text(markup):
    _assert_disclosure(_body_text(f"<body>\n{markup}\n</body>"),
                       "no window ceiling is established")


def test_disclosure_requires_body_markup():
    page = "<html><head><title>no window ceiling is established</title></head></html>"
    assert _body_text(page) == ""
    with pytest.raises(AssertionError):
        _assert_disclosure(_body_text(page), "no window ceiling is established")


@pytest.fixture
def non_utf8_default(monkeypatch):
    """Emulate a platform whose default text decoding uses a legacy codepage."""
    original = Path.read_text

    def read_text(path, encoding=None, errors=None):
        return original(path, encoding=encoding or "cp1252", errors=errors)

    monkeypatch.setattr(Path, "read_text", read_text)


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("pore_pressure_explorer", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


# Depths are the existing synthetic grid up to and including its onset.
@pytest.mark.parametrize("depth", [5000, 5500, 6000, 6500, 7000, 7500, 8000])
def test_normal_interval_uses_normal_pressure(builder, depth):
    """Hand calculation: dto=dtn, so Sv-(Sv-8.6)*1**3=8.6 ppg.

    The illustrative mud indicator adds the existing assumed 0.3 ppg margin:
    8.6+0.3=8.9 ppg. Exact decimal equality checks the serialized precision;
    no engineering acceptance tolerance is introduced.
    """
    point = next(p for p in builder.build_study()["series"] if p["z"] == depth)
    assert point["pp"] == 8.6
    assert point["mw"] == 8.9


@pytest.mark.parametrize("overburden_ppg", [12.0, 16.5, 19.0])
def test_on_trend_pressure_is_independent_of_synthetic_overburden(
    builder, monkeypatch, overburden_ppg
):
    """Synthetic inputs for issue #2707, assumed, based on previous experience.

    Depths are 5,000 through 8,000 ft at 500 ft spacing, including the
    8,000 ft onset. Normal sonic is 170 - 0.004*z us/ft; observed sonic
    equals normal throughout this interval. Each case holds overburden at
    12.0, 16.5 or 19.0 ppg. Pn is independently declared as 8.6 ppg and
    the illustrative mud margin is 0.3 ppg; no calibration is supplied.

    The sonic relation cited in pore_pressure.py (Methods, SPE 5544) gives
    observed/normal = 1, hence Pp = Sv - (Sv - Pn)*1**n = Pn for any n.
    Thus every overburden case gives 8.6 ppg pressure and 8.6+0.3=8.9 ppg
    mud indicator. Exact equality checks the builder's decimal output.
    """
    depths = [5000, 5500, 6000, 6500, 7000, 7500, 8000]
    monkeypatch.setattr(builder, "_DEPTH", depths)
    monkeypatch.setattr(builder, "_PN", 8.6)
    monkeypatch.setattr(builder, "_KICK_MARGIN", 0.3)
    monkeypatch.setattr(builder, "_overburden", lambda z: overburden_ppg)

    study = builder.build_study()

    assert study["meta"]["pn"] == 8.6
    assert study["meta"]["kick_margin"] == 0.3
    assert study["meta"]["governing_method"] == "eaton"
    assert study["meta"]["bowers_status"] == "NOT_EVALUATED"
    assert study["meta"]["bowers_reason"] == (
        "No Bowers calibration is supplied for the synthetic trend."
    )
    assert study["meta"]["data_basis"] == "assumed, based on previous experience"
    assert study["series"] == [
        {"z": z, "sv": overburden_ppg, "eaton": 8.6, "bowers": None,
         "pp": 8.6, "mw": 8.9}
        for z in depths
    ]


def test_missing_calibration_survives_serialization(builder):
    """Round-trip the synthetic study for issue #2707 without a calibration.

    Inputs from build_pore_pressure_explorer.py are assumed, based on
    previous experience: depths 5,000..14,000 ft every 500 ft; Sv rises
    linearly from 16.5 to 19.0 ppg; Pn=8.6 ppg; margin=0.3 ppg; normal
    sonic=170-0.004*z us/ft. Observed/normal is unity through the 8,000 ft
    onset, then rises linearly to 1.35 at 14,000 ft. No calibration is given.

    Using the sonic relation cited in pore_pressure.py (Methods, SPE 5544),
    unity gives Pp=Sv-(Sv-Pn)=8.6 ppg and mud=8.6+0.3=8.9 ppg. At the
    endpoint, Pp=19-(19-8.6)/1.35**3 rounds to 14.77 ppg and the mud
    indicator rounds to 15.07 ppg. These are independent hand calculations.
    """
    study = builder.build_study()
    encoded = json.dumps(study, allow_nan=False)
    restored = json.loads(encoded)

    assert restored == study
    assert restored["meta"]["governing_method"] == "eaton"
    assert restored["meta"]["bowers_status"] == "NOT_EVALUATED"
    assert restored["meta"]["bowers_reason"] == (
        "No Bowers calibration is supplied for the synthetic trend."
    )
    assert restored["meta"]["data_basis"] == "assumed, based on previous experience"
    assert all(point["bowers"] is None for point in restored["series"])
    assert all(point["pp"] == point["eaton"] for point in restored["series"])
    normal_points = [point for point in restored["series"] if point["z"] <= 8000]
    assert [(point["z"], point["pp"], point["mw"]) for point in normal_points] == [
        (z, 8.6, 8.9) for z in [5000, 5500, 6000, 6500, 7000, 7500, 8000]
    ]
    endpoint = next(point for point in restored["series"] if point["z"] == 14000)
    assert (endpoint["pp"], endpoint["mw"]) == (14.77, 15.07)


def test_overpressure_ramp_retains_eaton_response(builder):
    """Hand calculation at the existing synthetic endpoint (14,000 ft).

    Sv=19, Pn=8.6, dto/dtn=1.35. Eaton: 19-(19-8.6)/1.35**3
    rounds to 14.77 ppg; adding the assumed 0.3 margin rounds to 15.07.
    These are arithmetic expectations, independent of the implementation.
    """
    point = next(p for p in builder.build_study()["series"] if p["z"] == 14000)
    assert point["eaton"] == 14.77
    assert point["pp"] == 14.77
    assert point["mw"] == 15.07


def test_uncalibrated_bowers_is_not_evaluated(builder, monkeypatch):
    """A missing calibration supplies no numeric prediction or governing vote."""
    from digitalmodel.well.drilling import pore_pressure

    def reject_uncalibrated_call(**kwargs):
        pytest.fail("The explorer must not evaluate uncalibrated Bowers defaults")

    monkeypatch.setattr(pore_pressure, "bowers_pore_pressure", reject_uncalibrated_call)
    # Also catch the old builder's direct import before it is removed.
    if hasattr(builder, "bowers_pore_pressure"):
        monkeypatch.setattr(builder, "bowers_pore_pressure", reject_uncalibrated_call)
    study = builder.build_study()
    assert study["meta"]["bowers_status"] == "NOT_EVALUATED"
    assert "calibrat" in study["meta"]["bowers_reason"].lower()
    assert study["meta"]["governing_method"] == "eaton"
    assert study["meta"]["data_basis"] == "assumed, based on previous experience"
    assert all(p["bowers"] is None for p in study["series"])
    assert all(p["pp"] == p["eaton"] for p in study["series"])


def test_generated_page_and_json_expose_missing_calibration(
    builder, monkeypatch, tmp_path, non_utf8_default
):
    """Generated files agree and the page handles a missing Bowers number."""
    monkeypatch.setattr(builder, "_REPO", tmp_path)
    monkeypatch.setattr(builder, "_OUT_DIR", tmp_path)
    builder.main()
    study = json.loads((tmp_path / "pore-pressure-explorer.json").read_text(encoding="utf-8"))
    page = (tmp_path / "pore-pressure-explorer.html").read_text(encoding="utf-8")
    payload = page.split("const DATA = ", 1)[1].split(";\nconst M", 1)[0]
    assert page == builder._HTML.replace(
        "{data}", json.dumps(study, separators=(",", ":"))
    )
    assert json.loads(payload) == study
    assert "p.bowers === null" in page
    assert "NOT_EVALUATED" in page
    assert "Bowers is not evaluated" in page
    assert "assumed, based on previous experience" in page


def test_committed_outputs_match_builder(builder, non_utf8_default):
    """A regenerated page and JSON must carry the same prediction and status."""
    study = builder.build_study()
    assert (OUTPUT / "pore-pressure-explorer.json").read_text(encoding="utf-8") == (
        json.dumps(study, indent=2) + "\n"
    )
    assert (OUTPUT / "pore-pressure-explorer.html").read_text(encoding="utf-8") == (
        builder._HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    )


def test_page_does_not_draw_an_unsupported_window(builder):
    """Overburden supplies no evaluated fracture or stability boundary."""
    page = builder._HTML
    assert 'Mud-weight indicator<b id="rM"' in page
    assert 'id="win"' not in page
    assert "win.style" not in page
    assert ".win{" not in page
    assert "Shaded:" not in page
    # Unsupported-window claim vocabulary; even negated "safety" is denied.
    claims = r"\b(?:safe|safely|safer|safest|safety)\b"
    assert re.search(claims, page, re.IGNORECASE) is None
    assert re.search(claims, _body_text(page), re.IGNORECASE) is None
    assert "Min. mud weight" not in page


@pytest.mark.parametrize("word", ["safe", "safely", "safer", "safest", "safety"])
@pytest.mark.parametrize("location", ["title", "script", "body"])
def test_unsupported_claim_variants_are_rejected(builder, monkeypatch, word, location):
    closing = f"</{location}>"
    page = builder._HTML.replace(closing, f" {word.upper()}{closing}")
    monkeypatch.setattr(builder, "_HTML", page)
    with pytest.raises(AssertionError):
        test_page_does_not_draw_an_unsupported_window(builder)


@pytest.fixture
def generated_explorer(builder, monkeypatch, tmp_path):
    """Read actual generated files, keeping page disclosures separate from JSON."""
    monkeypatch.setattr(builder, "_REPO", tmp_path)
    monkeypatch.setattr(builder, "_OUT_DIR", tmp_path)
    builder.main()
    study = json.loads((tmp_path / "pore-pressure-explorer.json").read_text(encoding="utf-8"))
    page = (tmp_path / "pore-pressure-explorer.html").read_text(encoding="utf-8")
    return study, page, _body_text(page)


def test_missing_fracture_ceiling_is_disclosed(generated_explorer):
    """No supplied fracture gradient means no established window ceiling."""
    study, _, visible = generated_explorer
    assert study["meta"]["drilling_window_status"] == "NOT_EVALUATED"
    assert "no fracture gradient" in study["meta"]["drilling_window_reason"].lower()
    assert "NOT_EVALUATED" in visible
    _assert_disclosure(visible.lower(), "no window ceiling is established")
    assert "no window ceiling is established" in visible.lower()
    assert "no fracture gradient is supplied or calculated" in visible.lower()
    assert "overburden is a vertical-stress reference" in visible.lower()
    assert "assumed 0.3 ppg margin" in visible
    assert "no mud-weight recommendation" in visible.lower()


@pytest.mark.parametrize("exclusion", [
    "collapse (shear failure)",
    "weak bedding planes",
    "natural fractures",
    "chemical or time-dependent effects",
])
def test_window_exclusions_are_visible(generated_explorer, exclusion):
    """Exclusions must appear to readers as well as in machine-readable output."""
    study, _, visible = generated_explorer
    assert exclusion in study["meta"]["not_covered"]
    assert exclusion in visible


def test_page_links_existing_stability_screen(generated_explorer):
    """The screen link targets the local public workflow and states its limit."""
    _, page, visible = generated_explorer
    target = "examples/workflows/wellbore-stability/README.md"
    assert (ROOT / target).is_file()
    assert "NOT_VALIDATED" in (ROOT / target).read_text(encoding="utf-8")
    assert f'href="https://github.com/vamseeachanta/digitalmodel/blob/main/{target}"' in page
    assert "wellbore stability screen" in visible.lower()
    assert "NOT_VALIDATED" in visible
