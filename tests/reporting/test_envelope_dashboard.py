# ABOUTME: Tests for the generic utilisation envelope dashboard builder.
# ABOUTME: Pins band edges, governing-check selection, data injection and input validation.
"""Tests for :mod:`digitalmodel.reporting.envelope_dashboard`.

The dashboard is a review aid: its value rests on the colour band of every
cell being right at the edges, on the governing check being the true maximum,
and on bad input failing loudly instead of rendering a misleading green cell.
"""

from __future__ import annotations

import json
import math
import re
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.reporting.envelope_dashboard import (
    DEFAULT_AMBER_THRESHOLD,
    REQUIRED_COLUMNS,
    build_payload,
    classify_band,
    governing_checks,
    load_records,
    main,
    render_dashboard,
    write_dashboard,
)

EXAMPLE_CSV = (
    Path(__file__).resolve().parents[2]
    / "examples"
    / "reporting"
    / "envelope_dashboard"
    / "synthetic_envelope.csv"
)


def _rows():
    """Two sea states x three checks, synthetic."""
    return [
        {"hs_m": 1.0, "tp_s": 6.0, "check": "A", "utilisation": 0.40},
        {"hs_m": 1.0, "tp_s": 6.0, "check": "B", "utilisation": 0.85},
        {"hs_m": 1.0, "tp_s": 6.0, "check": "C", "utilisation": 0.10},
        {"hs_m": 2.0, "tp_s": 6.0, "check": "A", "utilisation": 1.20},
        {"hs_m": 2.0, "tp_s": 6.0, "check": "B", "utilisation": 0.95},
        {"hs_m": 2.0, "tp_s": 6.0, "check": "C", "utilisation": 0.30},
    ]


def _embedded(html: str) -> dict:
    match = re.search(
        r'<script id="envelope-data" type="application/json">(.*?)</script>',
        html,
        re.S,
    )
    assert match, "data block not found"
    return json.loads(match.group(1))


# ---------------------------------------------------------------------------
# Band classification
# ---------------------------------------------------------------------------


def test_default_threshold_is_0_80():
    assert DEFAULT_AMBER_THRESHOLD == 0.80


@pytest.mark.parametrize(
    "u, band",
    [
        (0.0, "green"),
        (0.7999, "green"),
        (0.80, "amber"),
        (1.0, "amber"),
        (1.0001, "red"),
    ],
)
def test_band_edges_default_threshold(u, band):
    assert classify_band(u) == band


def test_custom_threshold_moves_amber_edge():
    assert classify_band(0.65, amber_threshold=0.70) == "green"
    assert classify_band(0.70, amber_threshold=0.70) == "amber"
    assert classify_band(0.75, amber_threshold=0.70) == "amber"
    assert classify_band(0.75) == "green"
    # red edge is fixed at U = 1.0 whatever the amber threshold
    assert classify_band(1.0, amber_threshold=0.70) == "amber"
    assert classify_band(1.0001, amber_threshold=0.70) == "red"


@pytest.mark.parametrize("bad", [0.0, 1.0, 1.2, -0.1, math.nan])
def test_invalid_threshold_rejected(bad):
    with pytest.raises(ValueError):
        classify_band(0.5, amber_threshold=bad)


@pytest.mark.parametrize("bad", [math.nan, math.inf, -0.01])
def test_classify_rejects_invalid_utilisation(bad):
    with pytest.raises(ValueError):
        classify_band(bad)


# ---------------------------------------------------------------------------
# Governing check
# ---------------------------------------------------------------------------


def test_governing_check_per_sea_state():
    gov = governing_checks(_rows())
    assert gov[(1.0, 6.0)] == ("B", 0.85)
    assert gov[(2.0, 6.0)] == ("A", 1.20)


def test_governing_tie_breaks_on_first_check_order():
    rows = [
        {"hs_m": 1.0, "tp_s": 6.0, "check": "A", "utilisation": 0.5},
        {"hs_m": 1.0, "tp_s": 6.0, "check": "B", "utilisation": 0.5},
    ]
    assert governing_checks(rows)[(1.0, 6.0)] == ("A", 0.5)


def test_payload_cells_carry_governing_and_band():
    payload = build_payload(_rows(), title="t")
    assert payload["checks"] == [{"id": "A"}, {"id": "B"}, {"id": "C"}]
    assert payload["hs"] == [1.0, 2.0]
    assert payload["tp"] == [6.0]
    cells = {(c["hs"], c["tp"]): c for c in payload["cells"]}
    assert cells[(1.0, 6.0)]["governing"] == 1
    assert cells[(1.0, 6.0)]["band"] == "amber"
    assert cells[(2.0, 6.0)]["governing"] == 0
    assert cells[(2.0, 6.0)]["band"] == "red"
    assert cells[(2.0, 6.0)]["u"] == [1.2, 0.95, 0.3]
    assert payload["amber_threshold"] == 0.80


def test_payload_custom_threshold_changes_band():
    payload = build_payload(_rows(), title="t", amber_threshold=0.90)
    cells = {(c["hs"], c["tp"]): c for c in payload["cells"]}
    assert cells[(1.0, 6.0)]["band"] == "green"
    assert payload["amber_threshold"] == 0.90


def test_optional_demand_limit_units_carried():
    rows = [
        dict(r, demand=r["utilisation"] * 50.0, limit=50.0, units="kN") for r in _rows()
    ]
    payload = build_payload(rows, title="t")
    assert payload["checks"][0] == {"id": "A", "limit": 50.0, "units": "kN"}
    cell = payload["cells"][0]
    assert cell["demand"][0] == pytest.approx(20.0)


# ---------------------------------------------------------------------------
# Builder output
# ---------------------------------------------------------------------------


def test_writes_single_self_contained_html(tmp_path):
    out = write_dashboard(_rows(), tmp_path / "env.html", title="Synthetic envelope")
    assert out == tmp_path / "env.html"
    assert [p.name for p in tmp_path.iterdir()] == ["env.html"]
    html = out.read_text(encoding="utf-8")
    assert html.lower().startswith("<!doctype html>")
    assert "<title>Synthetic envelope</title>" in html
    data = _embedded(html)
    assert data["title"] == "Synthetic envelope"
    assert len(data["cells"]) == 2
    # no placeholders left and no network references
    assert "__ENVELOPE_DATA__" not in html and "__TITLE__" not in html
    assert not re.search(r'(src|href)\s*=\s*["\']https?://', html)
    assert "@import" not in html
    # colours are CSS tokens with a dark mode
    assert "--amber:" in html and "prefers-color-scheme: dark" in html


def test_closing_tags_in_data_are_escaped():
    rows = _rows()
    for r in rows:
        if r["check"] == "A":
            r["check"] = "A</script><script>alert(1)</script>"
    html = render_dashboard(rows, title="T </title> & co")
    assert "A</script>" not in html
    assert "<title>T &lt;/title&gt; &amp; co</title>" in html
    data = _embedded(html)
    assert data["checks"][0]["id"] == "A</script><script>alert(1)</script>"
    block = re.search(
        r'id="envelope-data" type="application/json">(.*?)</script>', html, re.S
    )
    assert "</" not in block.group(1)


def test_write_refuses_to_overwrite(tmp_path):
    out = tmp_path / "env.html"
    out.write_text("x", encoding="utf-8")
    with pytest.raises(FileExistsError):
        write_dashboard(_rows(), out, title="t")
    write_dashboard(_rows(), out, title="t", overwrite=True)
    assert "envelope-data" in out.read_text(encoding="utf-8")


def test_custom_threshold_reaches_html():
    data = _embedded(render_dashboard(_rows(), title="t", amber_threshold=0.70))
    assert data["amber_threshold"] == 0.70


# ---------------------------------------------------------------------------
# Invalid input fails loudly
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("column", REQUIRED_COLUMNS)
def test_missing_column_rejected(column):
    rows = [{k: v for k, v in r.items() if k != column} for r in _rows()]
    with pytest.raises(ValueError, match=column):
        build_payload(rows, title="t")


@pytest.mark.parametrize("field", ["utilisation", "hs_m", "tp_s"])
def test_nan_rejected(field):
    rows = _rows()
    rows[2][field] = math.nan
    with pytest.raises(ValueError, match="finite"):
        build_payload(rows, title="t")


def test_negative_utilisation_rejected():
    rows = _rows()
    rows[0]["utilisation"] = -0.2
    with pytest.raises(ValueError, match="negative"):
        build_payload(rows, title="t")


def test_non_numeric_utilisation_rejected():
    rows = _rows()
    rows[0]["utilisation"] = "high"
    with pytest.raises(ValueError):
        build_payload(rows, title="t")


def test_duplicate_row_rejected():
    rows = _rows() + [dict(_rows()[0])]
    with pytest.raises(ValueError, match="[Dd]uplicate"):
        build_payload(rows, title="t")


def test_empty_input_rejected():
    with pytest.raises(ValueError):
        build_payload([], title="t")


def test_csv_missing_column_rejected(tmp_path):
    p = tmp_path / "bad.csv"
    p.write_text("hs_m,tp_s,check\n1,6,A\n", encoding="utf-8")
    with pytest.raises(ValueError, match="utilisation"):
        load_records(p)


def test_csv_blank_value_rejected(tmp_path):
    p = tmp_path / "bad.csv"
    p.write_text("hs_m,tp_s,check,utilisation\n1,6,A,\n", encoding="utf-8")
    with pytest.raises(ValueError):
        build_payload(load_records(p), title="t")


# ---------------------------------------------------------------------------
# Shipped synthetic example
# ---------------------------------------------------------------------------


def test_synthetic_example_builds(tmp_path):
    assert EXAMPLE_CSV.read_text(encoding="utf-8").startswith("# SYNTHETIC")
    records = load_records(EXAMPLE_CSV)
    payload = build_payload(records, title="Synthetic example", synthetic=True)
    bands = {c["band"] for c in payload["cells"]}
    assert bands == {"green", "amber", "red"}
    assert payload["synthetic"] is True
    out = write_dashboard(
        records, tmp_path / "example.html", title="Synthetic example", synthetic=True
    )
    assert "SYNTHETIC" in out.read_text(encoding="utf-8")


@pytest.mark.parametrize("token", ["__ENVELOPE_DATA__", "__TITLE__"])
def test_title_placeholders_stay_literal(tmp_path, token):
    title = f"Literal {token} <&>"
    out = write_dashboard(_rows(), tmp_path / "literal.html", title=title)
    page = out.read_text(encoding="utf-8")
    assert f"<title>Literal {token} &lt;&amp;&gt;</title>" in page
    blocks = re.findall(
        r'<script id="envelope-data" type="application/json">(.*?)</script>',
        page,
        re.S,
    )
    assert len(blocks) == 1 and page.count(blocks[0]) == 1
    assert json.loads(blocks[0]) == build_payload(_rows(), title=title)
    assert blocks[0] not in re.search(r"<title>(.*?)</title>", page, re.S)[1]


def test_dataframe_input_matches_mapping_rows():
    rows = _rows()
    assert _embedded(render_dashboard(pd.DataFrame(rows), title="t")) == _embedded(
        render_dashboard(rows, title="t")
    )


def test_main_builds_example_and_refuses_overwrite(tmp_path):
    out = tmp_path / "example.html"
    args = [str(EXAMPLE_CSV), str(out), "--amber-threshold", "0.70"]
    assert main(args) == 0
    assert list(tmp_path.iterdir()) == [out]
    page = out.read_bytes()
    assert _embedded(page.decode("utf-8"))["amber_threshold"] == 0.70
    with pytest.raises(FileExistsError, match="overwrite=True"):
        main(args)
    assert out.read_bytes() == page


def test_sparse_grid_preserves_missing_check_as_null():
    page = render_dashboard(_rows()[:-1], title="t")
    cell = _embedded(page)["cells"][1]
    assert cell["u"] == [1.2, 0.95, None]
    assert cell["governing"] == 0
    assert 'na: "Not evaluated"' in page
    assert 'u == null || isNaN(u) ? "na"' in page
    assert 'background:var(--grid);color:var(--muted)' in page


def test_inconsistent_limit_error_names_check():
    rows = _rows()
    rows[0]["limit"], rows[3]["limit"] = 50.0, 60.0
    with pytest.raises(ValueError, match="check 'A': inconsistent limit"):
        build_payload(rows, title="t")


@pytest.mark.parametrize("mode", ["absent", "supplied", "mixed"])
def test_payload_marks_supplied_demand(mode):
    rows = [dict(r, limit=50.0) for r in _rows()]
    for n, row in enumerate(rows):
        if mode == "supplied" or (mode == "mixed" and n == 0):
            row["demand"] = row["utilisation"] * 50.0
    cells = build_payload(rows, title="t")["cells"]
    expected = [mode == "supplied"] * 3
    assert cells[1]["demand_supplied"] == expected
    if mode == "mixed":
        expected[0] = True
    assert cells[0]["demand_supplied"] == expected


def test_template_labels_derived_demand():
    page = render_dashboard(_rows(), title="t")
    assert "Demand (derived: U x limit)" in page
    assert "p.interp || !p.cell.demand_supplied[r.k]" in page
    assert '"Demand"' in page
