# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for a constant-MCT internal-transfer screen.

Synthetic inputs below are arithmetic examples, not loading conditions.
The moment balance follows EN400 course notes (Summer 2020), section 3.6,
pages 3-28 and 3-32, cited in the implementation. Metric conversion uses
100 centimetres per metre. Expected values are calculated here by hand.
"""

import math

import pytest

from digitalmodel.citations import Citation, CitationResolutionError
from digitalmodel.naval_architecture import trim_ballast
from digitalmodel.naval_architecture.trim_ballast import (
    drafts_after_weight_shift,
    even_keel_transfer_mass,
    trim_change_from_weight_shift,
)
from digitalmodel.naval_architecture.trim_hydrostatics import end_drafts


@pytest.mark.parametrize("lever_arm_m, expected_trim_m", [(20.0, 0.1), (-20.0, -0.1)])
def test_weight_shift_trim_sign(lever_arm_m, expected_trim_m):
    """50 t * (+/-20 m) / (100 t m/cm) = +/-10 cm = +/-0.1 m."""
    change = trim_change_from_weight_shift(
        mass_t=50.0, lever_arm_m=lever_arm_m, mct_t_m_per_cm=100.0
    )
    assert change == pytest.approx(expected_trim_m)
    drafts = drafts_after_weight_shift(
        mean_draft_m=5.0,
        initial_trim_m=0.0,
        mass_t=50.0,
        lever_arm_m=lever_arm_m,
        mct_t_m_per_cm=100.0,
    )
    # Half the trim acts at each end: forward movement gives 5.05 m, 4.95 m.
    assert drafts == pytest.approx(
        (5.05, 4.95) if lever_arm_m > 0.0 else (4.95, 5.05)
    )
    assert drafts == pytest.approx(end_drafts(5.0, expected_trim_m))


@pytest.mark.parametrize(
    "initial_trim_m, lever_arm_m, expected_mass_t",
    [(-0.4, 20.0, 200.0), (0.4, -20.0, 200.0), (0.4, 20.0, -200.0)],
)
def test_even_keel_transfer_round_trip(initial_trim_m, lever_arm_m, expected_mass_t):
    """A 40 cm correction at 100 t m/cm needs 4000 t m, or 200 t over 20 m."""
    mass = even_keel_transfer_mass(
        initial_trim_m=initial_trim_m,
        lever_arm_m=lever_arm_m,
        mct_t_m_per_cm=100.0,
    )
    assert mass == pytest.approx(expected_mass_t)
    change = trim_change_from_weight_shift(
        mass_t=mass, lever_arm_m=lever_arm_m, mct_t_m_per_cm=100.0
    )
    assert initial_trim_m + change == pytest.approx(0.0)
    assert drafts_after_weight_shift(
        mean_draft_m=5.0,
        initial_trim_m=initial_trim_m,
        mass_t=mass,
        lever_arm_m=lever_arm_m,
        mct_t_m_per_cm=100.0,
    ) == pytest.approx((5.0, 5.0))


@pytest.mark.parametrize("lever_arm_m", [20.0, -20.0])
def test_zero_initial_trim_needs_no_transfer(lever_arm_m):
    """Zero trim requires zero corrective moment in either direction."""
    assert even_keel_transfer_mass(
        initial_trim_m=0.0, lever_arm_m=lever_arm_m, mct_t_m_per_cm=100.0
    ) == 0.0
    assert drafts_after_weight_shift(
        mean_draft_m=5.0,
        initial_trim_m=0.0,
        mass_t=0.0,
        lever_arm_m=lever_arm_m,
        mct_t_m_per_cm=100.0,
    ) == (5.0, 5.0)


def test_transfer_reversal_preserves_mean_draft():
    """-0.4 m + 0.1 m gives -0.3 m trim, hence drafts 4.85 m and 5.15 m."""
    options = dict(lever_arm_m=20.0, mct_t_m_per_cm=100.0)
    drafts = drafts_after_weight_shift(
        mean_draft_m=5.0, initial_trim_m=-0.4, mass_t=50.0, **options
    )
    assert drafts == pytest.approx((4.85, 5.15))
    assert sum(drafts) / 2.0 == pytest.approx(5.0)
    # Removing the same mass from the destination and returning it cancels
    # the original moment; neither transfer adds mass to the system.
    reverse = trim_change_from_weight_shift(mass_t=-50.0, **options)
    assert reverse == pytest.approx(-0.1)
    restored = drafts_after_weight_shift(
        mean_draft_m=5.0,
        initial_trim_m=drafts[0] - drafts[1],
        mass_t=-50.0,
        **options,
    )
    assert restored == pytest.approx((4.8, 5.2))


def test_supplied_mct_controls_the_response():
    """40 t * 12 m / (80 t m/cm) = 6 cm = 0.06 m.

    Correcting -0.3 m needs 30 cm * 80 t m/cm / 12 m = 200 t.
    """
    assert trim_change_from_weight_shift(
        mass_t=40.0, lever_arm_m=12.0, mct_t_m_per_cm=80.0
    ) == pytest.approx(0.06)
    assert even_keel_transfer_mass(
        initial_trim_m=-0.3, lever_arm_m=12.0, mct_t_m_per_cm=80.0
    ) == pytest.approx(200.0)


_HELPERS = [
    (trim_change_from_weight_shift, dict(mass_t=50.0)),
    (even_keel_transfer_mass, dict(initial_trim_m=-0.4)),
    (
        drafts_after_weight_shift,
        dict(mean_draft_m=5.0, initial_trim_m=-0.4, mass_t=50.0),
    ),
]


@pytest.mark.parametrize("helper, extra", _HELPERS)
@pytest.mark.parametrize(
    "field, bad_value",
    [
        ("mct_t_m_per_cm", 0.0),
        ("mct_t_m_per_cm", -100.0),
        ("mct_t_m_per_cm", math.nan),
        ("mct_t_m_per_cm", math.inf),
        ("mct_t_m_per_cm", -math.inf),
        ("lever_arm_m", 0.0),
        ("lever_arm_m", math.nan),
        ("lever_arm_m", math.inf),
        ("lever_arm_m", -math.inf),
    ],
)
def test_invalid_transfer_inputs(helper, extra, field, bad_value):
    """Invalid MCT or lever must fail even if other inputs are valid."""
    options = dict(lever_arm_m=20.0, mct_t_m_per_cm=100.0, **extra)
    options[field] = bad_value
    with pytest.raises(ValueError, match=field):
        helper(**options)


@pytest.mark.parametrize("helper, extra", _HELPERS)
@pytest.mark.parametrize("bad_value", [math.nan, math.inf, -math.inf])
def test_other_inputs_must_be_finite(helper, extra, bad_value):
    """Each mass, trim and draft input is checked independently."""
    for field in extra:
        options = dict(lever_arm_m=20.0, mct_t_m_per_cm=100.0, **extra)
        options[field] = bad_value
        with pytest.raises(ValueError, match=field):
            helper(**options)


@pytest.mark.parametrize("helper, extra", _HELPERS)
def test_mct_must_be_explicit(helper, extra):
    """No hydrostatic stiffness is assumed when MCT is omitted."""
    with pytest.raises(TypeError, match="mct_t_m_per_cm"):
        helper(lever_arm_m=20.0, **extra)


@pytest.mark.parametrize("helper, extra", _HELPERS)
def test_zero_transfer_still_requires_a_nonzero_lever(helper, extra):
    """A zero-moment case does not waive the transfer geometry contract."""
    options = dict.fromkeys(extra, 0.0)
    with pytest.raises(ValueError, match="lever_arm_m"):
        helper(lever_arm_m=0.0, mct_t_m_per_cm=100.0, **options)


@pytest.mark.parametrize(
    "helper, options, field",
    [
        (
            trim_change_from_weight_shift,
            dict(mass_t=1e308, lever_arm_m=1e308, mct_t_m_per_cm=100.0),
            "trim_change_m",
        ),
        (
            even_keel_transfer_mass,
            dict(initial_trim_m=1e308, lever_arm_m=20.0, mct_t_m_per_cm=100.0),
            "transfer_mass_t",
        ),
        (
            drafts_after_weight_shift,
            dict(
                mean_draft_m=5.0,
                initial_trim_m=1.79e308,
                mass_t=1e308,
                lever_arm_m=1.0,
                mct_t_m_per_cm=1.0,
            ),
            "final_trim_m",
        ),
        (
            drafts_after_weight_shift,
            dict(
                mean_draft_m=1e308,
                initial_trim_m=1.6e308,
                mass_t=0.0,
                lever_arm_m=20.0,
                mct_t_m_per_cm=100.0,
            ),
            "draft_fwd_m",
        ),
    ],
)
def test_unrepresentable_results_are_rejected(helper, options, field):
    """Synthetic extreme operands overflow the moment, sum or end draft.

    These are floating-point boundary cases, not physical examples. The
    moment products exceed finite double precision, as do the trim sum
    1.79e308 + 1e306 and the forward draft 1e308 + 1.6e308 / 2.
    """
    with pytest.raises(ValueError, match=field):
        helper(**options)


_CITED_CASES = [
    (
        "trim_change_from_weight_shift_cited",
        dict(mass_t=50.0, lever_arm_m=20.0, mct_t_m_per_cm=100.0),
        0.1,  # 50 * 20 / 100 = 10 cm = 0.1 m.
        "m",
    ),
    (
        "even_keel_transfer_mass_cited",
        dict(initial_trim_m=-0.4, lever_arm_m=20.0, mct_t_m_per_cm=100.0),
        200.0,  # 40 cm * 100 / 20 = 200 t.
        "t",
    ),
    (
        "drafts_after_weight_shift_cited",
        dict(mean_draft_m=5.0, initial_trim_m=-0.4, mass_t=50.0,
             lever_arm_m=20.0, mct_t_m_per_cm=100.0),
        (4.85, 5.15),  # -0.4 + 0.1 = -0.3 m; 5 +/- (-0.3 / 2).
        "m",
    ),
]
_REFERENCE_RECORDS = [
    ("marine-engineering", "en400.md", "EN400", "USNA", "Summer 2020"),
    ("engineering-standards", "nist-sp-811.md", "NIST-SP-811", "NIST", "2008"),
]


def _write_reference_records(root):
    """Write synthetic metadata only; no source document or loading data."""
    for domain, filename, code_id, publisher, revision in _REFERENCE_RECORDS:
        target = root / "wikis" / domain / "wiki" / "standards" / filename
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(
            f"---\ncode_id: {code_id}\npublisher: {publisher}\n"
            f"revision: {revision}\n---\n",
            encoding="utf-8", newline="\n",
        )


def _cited_helper(name):
    helper = getattr(trim_ballast, name, None)
    assert callable(helper), f"Missing cited entry point: {name}"
    return helper


@pytest.mark.parametrize("name, options, expected, units", _CITED_CASES)
@pytest.mark.parametrize("layout", ["standalone", "overlay"])
def test_cited_results_resolve_both_sources(tmp_path, name, options,
                                          expected, units, layout):
    root = tmp_path if layout == "standalone" else tmp_path / "knowledge"
    _write_reference_records(root)
    result = _cited_helper(name)(**options, repo_root=tmp_path)
    assert result["value"] == pytest.approx(expected)
    assert result["units"] == units
    citations = result["citations"]
    assert len(citations) == 2
    assert all(isinstance(citation, Citation) for citation in citations)
    assert [(c.code_id, c.publisher, c.revision) for c in citations] == [
        ("EN400", "USNA", "Summer 2020"),
        ("NIST-SP-811", "NIST", "2008"),
    ]
    assert "3.6" in citations[0].section
    assert "4.3" in citations[1].section and "Table 5" in citations[1].section
    assert all(c.source_sibling == "generic" for c in citations)
    assert all(c.source_project is None for c in citations)


@pytest.mark.parametrize("name, options, expected, units", _CITED_CASES)
def test_cited_results_use_configured_resolver(tmp_path, monkeypatch, name,
                                             options, expected, units):
    _write_reference_records(tmp_path)
    monkeypatch.setenv("LLM_WIKI_PATH", tmp_path.as_posix())
    result = _cited_helper(name)(**options)
    assert result["value"] == pytest.approx(expected)
    assert len(result["citations"]) == 2


@pytest.mark.parametrize("name, options, expected, units", _CITED_CASES)
@pytest.mark.parametrize("record", _REFERENCE_RECORDS)
@pytest.mark.parametrize("defect", ["page", "code_id", "publisher", "revision",
                                  "missing_revision"])
def test_cited_results_fail_closed(tmp_path, name, options, expected, units,
                                   record, defect):
    _write_reference_records(tmp_path)
    domain, filename, code_id, _, _ = record
    target = tmp_path / "wikis" / domain / "wiki" / "standards" / filename
    if defect == "page":
        # Leave the reference absent without deleting any supplied data.
        target.rename(target.with_suffix(".unavailable"))
        reason = "page_missing"
    else:
        lines = target.read_text(encoding="utf-8").splitlines()
        field = "revision" if defect == "missing_revision" else defect
        lines = [line for line in lines if not line.startswith(f"{field}:")]
        if defect != "missing_revision":
            lines.insert(1, f"{field}: invalid")
        target.write_text("\n".join(lines) + "\n", encoding="utf-8", newline="\n")
        prefix = "frontmatter_missing" if defect == "missing_revision" else "frontmatter_mismatch"
        reason = f"{prefix}:{field}"
    with pytest.raises(CitationResolutionError) as error:
        _cited_helper(name)(**options, repo_root=tmp_path)
    assert error.value.code_id == code_id
    assert error.value.reason.startswith(reason)


@pytest.mark.parametrize("name, options, expected, units", _CITED_CASES)
def test_cited_results_propagate_unconfigured_resolver(monkeypatch, name,
                                                     options, expected, units):
    from digitalmodel.citations import resolver

    def unavailable(*args, **kwargs):
        raise CitationResolutionError(code_id="EN400", wiki_path="wikis/missing.md",
                                      reason="resolver_unconfigured:test")

    monkeypatch.setattr(resolver, "resolve_wiki_path", unavailable)
    with pytest.raises(CitationResolutionError, match="resolver_unconfigured"):
        _cited_helper(name)(**options)


@pytest.mark.parametrize("name, options, expected, units", _CITED_CASES)
@pytest.mark.parametrize("bad_mct", [0.0, -1.0, math.nan, math.inf])
def test_cited_results_reject_invalid_mct(tmp_path, name, options,
                                        expected, units, bad_mct):
    _write_reference_records(tmp_path)
    with pytest.raises(ValueError, match="mct_t_m_per_cm"):
        _cited_helper(name)(**{**options, "mct_t_m_per_cm": bad_mct},
                            repo_root=tmp_path)
