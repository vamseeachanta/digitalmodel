# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Executable example checks with an independent Rayleigh reference."""

import math
from pathlib import Path
import runpy

import pytest


def example_namespace():
    return runpy.run_path(
        str(
            Path(__file__).resolve().parents[2]
            / "examples/orcawave/weibull_response_extremes.py"
        )
    )


def test_worked_example_inputs_method_and_answer():
    """NIST W: sigma=1, N=1000 gives q=sqrt(2ln1000).

    The example's synthetic spectrum has m0=1,m2=4.5 by hand trapezoids.
    Require the computed reference, diagnostics, interval and qualifications.
    """
    api = example_namespace()
    result = api["run_example"](n_resamples=20)
    assert result["inputs"]["sample_count"] == 5000
    assert result["fit"]["method"] == "mle"
    assert result["comparison"]["rayleigh_return_level"] == pytest.approx(
        math.sqrt(2 * math.log(1000))
    )
    assert result["bootstrap"]["completed"] == 20
    assert result["bootstrap"]["status"] == "EVALUATED"
    assert result["bootstrap"]["lower"] < result["bootstrap"]["upper"]
    assert result["goodness_of_fit"]["ks"]["status"] == "NOT_EVALUATED"
    assert result["goodness_of_fit"]["ad"]["status"] == "NOT_EVALUATED"
    rendered = api["render_report"](result)
    for text in (
        "Synthetic",
        "Revision history",
        "NOT_EVALUATED",
        "Return level",
        "Rayleigh",
        "location",
        "Confidence",
    ):
        assert text in rendered


def test_example_invalid_sample_count():
    with pytest.raises(ValueError):
        example_namespace()["run_example"](sample_count=0, n_resamples=2)
