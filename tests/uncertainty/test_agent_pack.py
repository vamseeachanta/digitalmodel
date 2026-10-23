# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic, independently calculated uncertainty pack anchors."""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.uncertainty import (
    Prior,
    fan,
    percentile_summary,
    run_matrix,
    sample_lhs,
    spearman_ranks,
    tornado,
)


def test_uniform_quantiles():
    """Uniform [2,6] inverse CDF: 2+(6-2)u -> [2,3,4,5,6].

    Basis: SciPy 1.15.3 uniform, Notes (loc and scale); u=[0,.25,.5,.75,1].
    """
    prior = Prior(name="x", dist="uniform", low=2, high=6)
    assert prior.ppf([0, 0.25, 0.5, 0.75, 1]).tolist() == [2, 3, 4, 5, 6]


def test_lhs_four_strata():
    """Four equiprobable strata: [0,.25),[.25,.5),[.5,.75),[.75,1).

    Each column must have counts [1,1,1,1], regardless of within-cell jitter.
    Basis: 1979 'A Comparison of Three Methods for Selecting Values of Input
    Variables in the Analysis of Output from a Computer Code', as cited in
    SciPy 1.15.3 LatinHypercube, Notes and References [1]. Seed 7 is a
    reproducibility choice, not a published numerical reference sample.
    """
    priors = [Prior(name=name, dist="uniform", low=0, high=1) for name in ["x", "y"]]
    samples = sample_lhs(priors, n_samples=4, seed=7)
    for name in ["x", "y"]:
        cells = np.floor(samples[name].to_numpy() * 4).astype(int)
        assert np.bincount(cells, minlength=4).tolist() == [1, 1, 1, 1]
        assert sorted(cells.tolist()) == [0, 1, 2, 3]


def test_spearman_five_pairs():
    """Pairs (1,1),(2,3),(3,2),(4,5),(5,4), no ties.

    Rank differences [0,-1,1,-1,1], sum(d²)=4. Thus rho=1-6*4/(5*(25-1))
    =0.8. Equivalently centered rank products sum to 8, each square sum 10.
    Basis: 'The Proof and Measurement of Association between Two Things'
    (1904), rank association; SciPy 1.15.3 spearmanr, statistic definition.
    Only the coefficient is anchored here; small-sample p-values are not.
    """
    rows = pd.DataFrame({"x": [1, 2, 3, 4, 5], "y": [1, 3, 2, 5, 4]})
    assert spearman_ranks(rows, ["x"], "y").coefficient.iloc[0] == pytest.approx(0.8)


def test_percentiles_and_fan():
    """Sorted [0,10,20,30,40]: index=(5-1)q gives P10=4,P50=20,P90=36.

    A second column plus 10 gives [14,30,46]. Basis: NumPy 2.2 percentile,
    Notes, method='linear', index q*(n-1); endpoints joined by straight lines.
    """
    rows = pd.DataFrame({"a": [0, 10, 20, 30, 40], "b": [10, 20, 30, 40, 50]})
    assert percentile_summary(rows, "a") == {"p10": 4.0, "p50": 20.0, "p90": 36.0}
    assert fan(rows, ["a", "b"])[["p10", "p50", "p90"]].values.tolist() == [
        [4.0, 20.0, 36.0], [14.0, 30.0, 46.0],
    ]


def test_evaluator_and_tornado():
    """For x=[1,2,3], y=2x+1=[3,5,7]. Standardized x and y coincide.

    Least squares on that one column yields coefficient 1 and rank 1.
    Basis: NumPy 2.2 linalg.lstsq, least-squares solution; z=(x-mean)/std.
    """
    rows = run_matrix(lambda sample: {"y": 2 * sample["x"] + 1},
                      pd.DataFrame({"x": [1.0, 2.0, 3.0]}))
    assert rows.y.tolist() == [3.0, 5.0, 7.0]
    result = tornado(rows, ["x"], "y")
    assert result.coefficient.iloc[0] == pytest.approx(1.0)
    assert result.abs_rank.tolist() == [1]
