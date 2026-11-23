# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic pressure readback through the public citation-resolution API.

Equation source: OpenStax, University Physics Volume 1 (2016), section 14.1,
equation 14.4, p = p0 + rho*g*h (constant-density static fluid):
https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure

The local record is synthetic resolution metadata, not a retained textbook or
an independently qualified wiki entry. These checks establish numeric and unit
preservation; they do not qualify a pressure solver or validate section content.
"""

import pytest

from digitalmodel.citations import (
    Citation,
    CitationResolutionError,
    CitedValue,
    resolve_wiki_path,
    validate_citation,
)


@pytest.fixture(params=["standalone", "overlay"])
def hydrostatic_record(request, tmp_path, monkeypatch):
    """Create isolated metadata in each supported layout, without a real wiki."""
    root = tmp_path / "reference"
    wiki_path = "wikis/physics/wiki/references/hydrostatic-pressure.md"
    base = root if request.param == "standalone" else root / "knowledge"
    page = base / wiki_path
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: OPENSTAX-UP1\npublisher: OpenStax\nrevision: 2016\n---\n"
        "Synthetic metadata for the equation cited in the test module.\n",
        encoding="utf-8",
    )
    monkeypatch.setenv("LLM_WIKI_PATH", str(root))
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    citation = Citation(
        code_id="OPENSTAX-UP1",
        publisher="OpenStax",
        revision="2016",
        section="Section 14.1, equation 14.4",
        wiki_path=wiki_path,
        source_sibling="generic",
        note="Synthetic constant-density gauge-pressure preservation check",
    )
    return citation, page


@pytest.mark.parametrize(
    ("depth_m", "expected_pa"),
    # OpenStax (2016), section 14.1, eq. 14.4; synthetic inputs, not measurements.
    [(0.0, 0.0), (2.5, 25000.0), (7.25, 72500.0)],
)
def test_hydrostatic_pressure_preserved_after_resolution(
    hydrostatic_record, depth_m, expected_pa,
):
    """Hand checks: gauge p = rho*g*h = p_absolute - p_surface.

    Choose synthetic rho = 1000 kg/m^3 and g = 10 m/s^2; neither is a
    material property or gravity default. OpenStax (2016), section 14.1,
    eq. 14.4 gives the following independent arithmetic in Pa:
    1000*10*0 = 0; 1000*10*2.5 = 25000; 1000*10*7.25 = 72500.
    All operands and products are exactly representable, so equality needs
    no acceptance tolerance. Pa units must remain attached to each result.
    """
    citation, page = hydrostatic_record
    # Synthetic inputs to OpenStax (2016), section 14.1, equation 14.4.
    density_kg_m3, gravity_m_s2 = 1000.0, 10.0
    result = CitedValue(
        value=density_kg_m3 * gravity_m_s2 * depth_m,
        citation=citation,
        units="Pa",
    )

    validate_citation(result.citation)

    assert resolve_wiki_path(result.citation.wiki_path) == page.resolve()
    assert result.value == expected_pa
    assert result.units == "Pa"
    assert result.citation == citation


@pytest.mark.parametrize("defect", ["missing", "revision"])
def test_hydrostatic_pressure_requires_resolvable_citation(hydrostatic_record, defect):
    """The 1000*10*2.5 = 25000 Pa hand case cannot pass missing/stale metadata.

    Numeric basis: OpenStax (2016), section 14.1, equation 14.4, using the
    synthetic inputs documented in the positive check. Resolution is a separate
    prerequisite; a valid number cannot establish the provenance of a record.
    """
    citation, page = hydrostatic_record
    # Independent hand result above, in Pa; no engineering default is adopted.
    result = CitedValue(value=25000.0, citation=citation, units="Pa")
    if defect == "missing":
        page.rename(page.with_suffix(".unavailable"))
        expected_reason = "page_missing"
    else:
        page.write_text(
            page.read_text(encoding="utf-8").replace("revision: 2016", "revision: stale"),
            encoding="utf-8",
        )
        expected_reason = "frontmatter_mismatch:revision"

    with pytest.raises(CitationResolutionError) as error:
        validate_citation(result.citation)

    assert error.value.code_id == citation.code_id
    assert error.value.reason.startswith(expected_reason)
