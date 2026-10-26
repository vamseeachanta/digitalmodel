# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Package-output regressions for the equations in digitalmodel issue #1730.

The issue, rather than an independently checked standard, is the formula basis.
Intensity is a fraction; relative_flow_angle is in degrees. Expected factors
below are hand substitutions into the issue equations, never helper outputs.
Synthetic geometry and response inputs isolate reductions from calibration.
"""

import math
from dataclasses import asdict, replace

import pytest

from digitalmodel.citations import Citation, CitationResolutionError, validate_citation
from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue, PipeSpanInput
from digitalmodel.subsea.pipeline.free_span.span_viv_response import SpanVIVAmplitude


_WIKI_PAGE = "wikis/engineering-standards/wiki/standards/dnv-rp-f105.md"
_METADATA = "---\ncode_id: dnv-rp-f105\npublisher: DNV\nrevision: 2006\n---\n"


@pytest.fixture(autouse=True)
def citation_page(tmp_path, monkeypatch):
    """Supply synthetic metadata, never licensed standard content."""
    page = tmp_path / _WIKI_PAGE
    page.parent.mkdir(parents=True)
    page.write_text(_METADATA, encoding="utf-8", newline="\n")
    monkeypatch.setenv("LLM_WIKI_PATH", tmp_path.as_posix())
    return page


@pytest.fixture
def span_input():
    """Use the existing F105 test geometry with a synthetic overlapping current.

    OD=0.2731 m, WT=0.0127 m and L=34 m come from test_free_span_f105.py.
    The chosen 0.3 m/s excites both response branches, so a zero baseline cannot
    hide an omitted factor. Other physical inputs retain compatibility defaults.
    """
    return PipeSpanInput(
        od_m=0.2731, wt_m=0.0127, span_length_m=34.0, current_velocity_ms=0.3
    )


def _assess(inp):
    return FreespanVIVFatigue(inp).assess()


def _cited_output(inp, consumer):
    if consumer == "facade":
        return _assess(inp)
    return SpanVIVAmplitude(inp, Ks=0.0, fn_IL=1.0, fn_CF=0.5, EI=1.0, m_e=1.0)


@pytest.mark.parametrize("consumer", ["amplitude", "facade"])
@pytest.mark.parametrize("layout", ["", "knowledge"])
def test_turbulence_citation_sidecar(span_input, tmp_path, citation_page, consumer, layout):
    """Catch absent sidecars without changing the numeric result payload."""
    if layout:
        overlay = tmp_path / layout / _WIKI_PAGE
        overlay.parent.mkdir(parents=True)
        citation_page.rename(overlay)
    output = _cited_output(span_input, consumer)
    assert len(output.citations) == 1
    citation = output.citations[0]
    assert isinstance(citation, Citation)
    assert citation.code_id == "dnv-rp-f105"
    assert citation.publisher == "DNV"
    assert citation.revision == "2006"
    assert citation.section == "Section 4.3.6 (turbulence reduction factors)"
    assert citation.wiki_path == _WIKI_PAGE
    assert citation.source_sibling == "generic"
    assert citation.source_project is None
    validate_citation(citation)
    if consumer == "facade":
        assert "citations" not in asdict(output)
        assert output.il_A_over_D > 0.0
        assert output.cf_A_over_D > 0.0
    else:
        assert isinstance(output.il_amplitude_over_D(), float)
        assert isinstance(output.cf_amplitude_over_D(), float)


@pytest.mark.parametrize("consumer", ["amplitude", "facade"])
def test_missing_turbulence_citation_rejected(span_input, citation_page, consumer):
    """A previous success must not authorize a later calculation without its source."""
    _cited_output(span_input, consumer)
    citation_page.rename(citation_page.with_suffix(".absent"))
    for _ in range(2):
        with pytest.raises(CitationResolutionError) as caught:
            _cited_output(span_input, consumer)
        assert caught.value.code_id == "dnv-rp-f105"
        assert caught.value.reason == "page_missing"


@pytest.mark.parametrize("consumer", ["amplitude", "facade"])
@pytest.mark.parametrize("field", ["code_id", "publisher", "revision"])
@pytest.mark.parametrize("invalid", [None, "mismatched"])
def test_invalid_turbulence_citation_rejected(
    span_input, citation_page, consumer, field, invalid
):
    """Catch skipped source-identity checks, including after a successful calculation."""
    _cited_output(span_input, consumer)
    lines = [line for line in _METADATA.splitlines() if not line.startswith(field + ":")]
    if invalid is not None:
        lines.insert(-1, f"{field}: {invalid}")
    citation_page.write_text("\n".join(lines) + "\n", encoding="utf-8", newline="\n")
    with pytest.raises(CitationResolutionError) as caught:
        _cited_output(span_input, consumer)
    assert caught.value.code_id == "dnv-rp-f105"
    reason = "frontmatter_missing" if invalid is None else "frontmatter_mismatch"
    assert caught.value.reason.startswith(f"{reason}:{field}")


@pytest.mark.parametrize("consumer", ["amplitude", "facade"])
def test_invalid_turbulence_resolver_rejected(span_input, tmp_path, monkeypatch, consumer):
    """An invalid configured source root must never return a numeric assessment."""
    monkeypatch.setenv("LLM_WIKI_PATH", (tmp_path / "unavailable").as_posix())
    with pytest.raises(CitationResolutionError) as caught:
        _cited_output(span_input, consumer)
    assert caught.value.reason.startswith("llm_wiki_path_invalid:")


@pytest.mark.parametrize("angle", [0.0, 30.0, 90.0])
def test_neutral_factors_preserve_response(span_input, angle):
    """At Ic=0.03 the issue's (Ic-0.03) vanishes: R1=R2=1 at any angle.

    This is the documented compatibility default, rather than the issue's
    non-neutral Ic=0.05 example. Compare every facade output, including fatigue.
    """
    neutral = _assess(span_input)
    explicit = _assess(
        replace(span_input, turbulence_intensity=0.03, relative_flow_angle=angle)
    )
    assert neutral.il_A_over_D > 0.0
    assert neutral.cf_A_over_D > 0.0
    assert explicit == neutral


def test_r1_scales_il_only(span_input):
    """Issue #1730: 30 degrees = pi/6 rad, Ic=0.05 gives an interior R1.

    Hand substitution: R1 = 1 - 0.02*pi**3*(1/2 - sqrt(2)/6).
    At the same intensity and 90 degrees the bracket is negative, hence R1=1
    after the upper clamp. R2=15/17 in both cases, so CF must stay unchanged.
    This nontrivial IL ratio also detects applying R1 twice.
    """
    normal = _assess(
        replace(span_input, turbulence_intensity=0.05, relative_flow_angle=90.0)
    )
    oblique = _assess(
        replace(span_input, turbulence_intensity=0.05, relative_flow_angle=30.0)
    )
    expected_r1 = 1.0 - 0.02 * math.pi**3 * (0.5 - math.sqrt(2.0) / 6.0)
    assert oblique.il_A_over_D == pytest.approx(normal.il_A_over_D * expected_r1)
    assert oblique.il_stress_mpa == pytest.approx(normal.il_stress_mpa * expected_r1)
    assert oblique.cf_A_over_D == normal.cf_A_over_D
    assert oblique.cf_stress_mpa == normal.cf_stress_mpa


@pytest.mark.parametrize("intensity, expected_r2", [(0.05, 15.0 / 17.0), (0.115, 0.5)])
def test_r2_scales_cf_only(span_input, intensity, expected_r2):
    """At 90 degrees R1 clamps to one for Ic above the neutral 0.03.

    Issue #1730 hand arithmetic: Ic=0.05 -> 1-2/17=15/17;
    Ic=0.115 -> 1-0.085/0.17=1/2. CF and its stress scale once; IL stays fixed.
    """
    neutral = _assess(span_input)
    reduced = _assess(
        replace(span_input, turbulence_intensity=intensity, relative_flow_angle=90.0)
    )
    assert reduced.cf_A_over_D == pytest.approx(neutral.cf_A_over_D * expected_r2)
    assert reduced.cf_stress_mpa == pytest.approx(neutral.cf_stress_mpa * expected_r2)
    assert reduced.il_A_over_D == neutral.il_A_over_D
    assert reduced.il_stress_mpa == neutral.il_stress_mpa
    assert reduced.damage_per_year < neutral.damage_per_year


@pytest.mark.parametrize(
    "intensity, angle, expected_r1, expected_r2",
    [
        (0.0, 0.0, 1.0, 1.0),
        (0.0, 90.0, 1.0 + 0.015 * math.pi**3 * (1.0 - math.sqrt(2.0)), 1.0),
        (0.2, 0.0, 0.0, 0.0),
        (1.0, 90.0, 1.0, 0.0),
    ],
)
def test_reduction_clamps(span_input, intensity, angle, expected_r1, expected_r2):
    """Hand bounds from issue #1730, with both factors clamped to [0, 1].

    Ic=0, theta=0 gives raw R1=1+0.015*pi**3 and R2=1+3/17 (>1).
    Ic=0, theta=90 gives R1=1+0.015*pi**3*(1-sqrt(2)), an interior value.
    Ic=0.2, theta=0 gives raw R1=1-0.085*pi**3 (<0), R2=0.
    Ic=1, theta=90 gives raw R1>1, R2=1-0.97/0.17 (<0).
    The fractional-intensity endpoints are synthetic domain boundary cases.
    """
    neutral = _assess(span_input)
    reduced = _assess(
        replace(span_input, turbulence_intensity=intensity, relative_flow_angle=angle)
    )
    assert reduced.il_A_over_D == pytest.approx(neutral.il_A_over_D * expected_r1)
    assert reduced.cf_A_over_D == pytest.approx(neutral.cf_A_over_D * expected_r2)


def test_issue_intensity_angle_example():
    """The issue's Ic=0.05, theta=90 degrees means theta=pi/2 radians.

    R1_raw=1+0.01*pi**3*(sqrt(2)-1)>1 -> R1=1; R2=15/17.
    Independent synthetic response: D=1 m, U=3 m/s, Ks=0, fn_IL=1 Hz,
    fn_CF=0.5 Hz, both onset safety factors=1 and gap/D=1.
    Existing piecewise equations (span_viv_response.py, F105 figure citations):
    IL: 0.18+(0.13-0.18)*(3-2.8)/(4.24-2.8)=623/3600.
    CF: 0.9+(0.3-0.9)*(6-5.5)/(16-5.5)=61/70.
    Expected amplitudes multiply these independently derived values once.
    EI=1 and mass=1 are unused placeholders in this amplitude-only calculation.
    """
    inp = PipeSpanInput(
        od_m=1.0, wt_m=0.05, span_length_m=10.0,
        current_velocity_ms=3.0, seabed_gap_m=1.0,
        gamma_on_IL=1.0, gamma_on_CF=1.0,
        turbulence_intensity=0.05, relative_flow_angle=90.0,
    )
    amplitude = SpanVIVAmplitude(inp, Ks=0.0, fn_IL=1.0, fn_CF=0.5, EI=1.0, m_e=1.0)
    assert amplitude.il_amplitude_over_D() == pytest.approx(623.0 / 3600.0)
    assert amplitude.cf_amplitude_over_D() == pytest.approx((61.0 / 70.0) * (15.0 / 17.0))


@pytest.mark.parametrize("field", ["turbulence_intensity", "relative_flow_angle"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
def test_nonfinite_reduction_inputs_rejected(span_input, field, value):
    """Neither issue equation defines a finite reduction for NaN or infinity."""
    with pytest.raises(ValueError, match=field):
        replace(span_input, **{field: value})


@pytest.mark.parametrize(
    "field, value",
    [
        ("turbulence_intensity", -0.01),
        ("turbulence_intensity", 1.01),
        ("relative_flow_angle", -1.0),
        ("relative_flow_angle", 91.0),
    ],
)
def test_reduction_input_domain_rejected(span_input, field, value):
    """API contract: intensity fraction [0,1], acute angle [0,90] degrees.

    Synthetic invalid inputs lie just outside those declared domains;
    1.01 also detects interpreting intensity as a percentage.
    """
    with pytest.raises(ValueError, match=field):
        replace(span_input, **{field: value})
