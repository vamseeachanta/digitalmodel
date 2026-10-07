#!/usr/bin/env python3
"""
ABOUTME: Tests for the mesh-convergence study helper and Richardson
extrapolation. Pure Python — no solver needed.
"""

import pytest

from digitalmodel.solvers.mystran.convergence import (
    MeshConvergenceStudy, richardson_extrapolation,
)


def _seq_evaluator(values):
    it = iter(values)
    counter = {"n": 0}

    def evaluate(level):
        counter["n"] += 1
        n = counter["n"]
        return {"value": next(it), "n_nodes": n * 10, "n_elements": n, "note": "x"}
    return evaluate


class TestMeshConvergenceStudy:

    def test_relative_change_and_convergence(self):
        study = MeshConvergenceStudy(tolerance=0.01)
        study.run([1, 2, 3, 4], _seq_evaluator([1.0, 1.5, 1.6, 1.61]))
        rc = [l.rel_change for l in study.levels]
        assert rc[0] is None
        assert rc[1] == pytest.approx(0.5 / 1.5)
        assert rc[2] == pytest.approx(0.1 / 1.6)
        assert rc[3] == pytest.approx(0.01 / 1.61)
        assert study.converged
        assert study.converged_at == "4"
        assert study.is_monotone()

    def test_not_converged(self):
        study = MeshConvergenceStudy(tolerance=0.001)
        study.run([1, 2], _seq_evaluator([1.0, 1.5]))
        assert not study.converged
        assert study.converged_at is None

    def test_single_level_not_converged(self):
        study = MeshConvergenceStudy()
        study.run([1], _seq_evaluator([1.0]))
        assert not study.converged

    def test_reference_error(self):
        study = MeshConvergenceStudy(tolerance=0.05, reference=2.0)
        study.run([1, 2], _seq_evaluator([1.5, 1.9]))
        assert study.levels[0].error_vs_reference == pytest.approx(0.25)
        assert study.levels[1].error_vs_reference == pytest.approx(0.05)

    def test_non_monotone_detected(self):
        study = MeshConvergenceStudy()
        study.run([1, 2, 3], _seq_evaluator([1.0, 1.1, 1.5]))
        assert not study.is_monotone()

    def test_labels_and_records(self):
        study = MeshConvergenceStudy(reference=1.0)
        study.run(
            [(2, 1), (4, 2)], _seq_evaluator([0.8, 0.95]),
            label=lambda l: f"{l[0]}x{l[1]}",
        )
        recs = study.to_records()
        assert [r["label"] for r in recs] == ["2x1", "4x2"]
        assert recs[0]["note"] == "x"
        assert recs[1]["n_nodes"] == 20
        assert recs[1]["n_elements"] == 2

    def test_summary_table(self):
        study = MeshConvergenceStudy(reference=1.0)
        study.run([1, 2], _seq_evaluator([0.8, 0.95]))
        table = study.summary_table("tip deflection")
        assert table.splitlines()[0].startswith("| level | nodes | elements | tip deflection")
        assert "| 2 | 20 | 2 |" in table
        assert "5.000%" in table  # error vs ref at level 2

    def test_bad_tolerance(self):
        with pytest.raises(ValueError):
            MeshConvergenceStudy(tolerance=0)

    def test_zero_value_does_not_divide_by_zero(self):
        study = MeshConvergenceStudy()
        study.run([1, 2], _seq_evaluator([1.0, 0.0]))
        assert study.levels[1].rel_change == pytest.approx(1.0)


class TestRichardson:

    def test_extrapolation_second_order(self):
        # f(h) = f_inf - c h^2 with f_inf=2, c=1: h=1 -> 1.0, h=0.5 -> 1.75
        assert richardson_extrapolation(1.0, 1.75, 2.0, 2.0) == pytest.approx(2.0)

    def test_extrapolation_first_order(self):
        # f_inf=2, c=1: h=1 -> 1.0, h=0.5 -> 1.5
        assert richardson_extrapolation(1.0, 1.5, 2.0, 1.0) == pytest.approx(2.0)

    def test_bad_args(self):
        with pytest.raises(ValueError):
            richardson_extrapolation(1, 2, refinement_ratio=1.0)
        with pytest.raises(ValueError):
            richardson_extrapolation(1, 2, order=0)
