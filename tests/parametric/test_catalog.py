"""Atlas catalog (#835)."""

from __future__ import annotations

from digitalmodel.parametric.catalog import build_catalog, render_markdown


def test_catalog_lists_every_committed_atlas():
    rows = build_catalog()
    names = {r["atlas"] for r in rows}
    # the computed fan-out + the licensed library
    assert {"mooring_fatigue", "code_check", "rao_tabulation",
            "diffraction_library"} <= names
    assert len(rows) >= 11
    for r in rows:
        assert r["cells"] > 0
        assert r["response"]


def test_library_is_flagged_distinctly():
    by_name = {r["atlas"]: r for r in build_catalog()}
    lib = by_name["diffraction_library"]
    assert lib["kind"] == "library"
    assert lib["max_err"] == "library"
    assert "STUB" in lib["provenance"]


def test_computed_atlas_shows_error_and_standard():
    by_name = {r["atlas"]: r for r in build_catalog()}
    mooring = by_name["mooring_fatigue"]
    assert mooring["kind"] == "computed"
    float(mooring["max_err"])  # parses as a number
    assert "DNV-RP-C203" in mooring["provenance"]


def test_render_markdown_table():
    md = render_markdown(build_catalog())
    assert md.startswith("# Parametric atlas catalog")
    assert "| Atlas |" in md
    assert "mooring_fatigue" in md
