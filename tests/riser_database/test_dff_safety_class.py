# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""DFF input contract; no licensed safety-class table is reproduced.

DNV-OS-F201 (2010), design fatigue factors: the committed fixture contains
metadata only, so table values and the table locator are NOT_EVALUATED.
The factors below are synthetic caller inputs, not standard recommendations.
Hand calculations: low 1/2 = 0.5; normal 1/4 = 0.25; high 1/8 = 0.125.
The reciprocal follows the usage-factor definition in touchdown.py, step 5.
"""
from pathlib import Path

import pytest

from digitalmodel.riser_database.getters import get_riser_dff, riser_citations

FIXTURE = Path(__file__).resolve().parents[1] / "citations" / "fixtures"


@pytest.mark.parametrize("safety_class,factor,allowable", [
    ("low", 2.0, 0.5), ("normal", 4.0, 0.25), ("high", 8.0, 0.125),
])
def test_explicit_class_factor(safety_class, factor, allowable):
    cv = get_riser_dff(safety_class=safety_class, class_dff=factor, repo_root=FIXTURE)
    assert 1 / cv.value == allowable
    assert safety_class in cv.citation.note
    assert "NOT_EVALUATED" in cv.citation.note
    assert "user-supplied class value" in cv.citation.note
    assert cv.citation.revision == "2010"


def test_missing_class_refused():
    with pytest.raises(ValueError, match="safety_class is required"):
        get_riser_dff(class_dff=4.0, repo_root=FIXTURE)


def test_unknown_class_refused():
    with pytest.raises(ValueError, match="low, normal, high"):
        get_riser_dff(safety_class="medium", class_dff=4.0, repo_root=FIXTURE)


@pytest.mark.parametrize("safety_class", ["low", "normal", "high"])
def test_unconfirmed_factor_is_required(safety_class):
    with pytest.raises(ValueError, match="NOT_EVALUATED.*class_dff is required"):
        get_riser_dff(safety_class=safety_class, repo_root=FIXTURE)


def test_override_keeps_class_value_and_selected_value():
    """Synthetic override 5 replaces class input 4; hand allowable = 1/5 = 0.2."""
    cv = get_riser_dff(safety_class="normal", class_dff=4.0, override=5.0,
                       repo_root=FIXTURE)
    assert 1 / cv.value == 0.2
    assert "normal" in cv.citation.note
    assert "class value 4.0" in cv.citation.note
    assert "user override 5.0" in cv.citation.note


@pytest.mark.parametrize("field", ["class_dff", "override"])
@pytest.mark.parametrize("invalid", [0, -1, float("nan"), float("inf")])
def test_invalid_factor_refused(field, invalid):
    kwargs = {"safety_class": "normal", "class_dff": 4.0, field: invalid}
    with pytest.raises(ValueError, match="finite and positive"):
        get_riser_dff(**kwargs, repo_root=FIXTURE)


def test_bundle_cannot_hide_missing_class():
    with pytest.raises(ValueError, match="safety_class is required"):
        riser_citations(repo_root=FIXTURE)
