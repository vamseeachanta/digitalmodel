"""Generic parametric-sweep driver (parametrics P0, #968)."""

from __future__ import annotations

from digitalmodel.parametrics import (
    CaseResult,
    ParameterSweep,
    ParametricStudy,
    run_sweep,
)


def _study() -> ParametricStudy:
    return ParametricStudy(
        name="t",
        parameters=[
            ParameterSweep(name="a", values=[1.0, 2.0]),
            ParameterSweep(name="b", values=[10.0, 20.0, 30.0]),
        ],
    )


def test_run_sweep_covers_full_factorial_matrix():
    study = _study()

    def runner(cfg: dict) -> CaseResult:
        return CaseResult(
            case_id=cfg["case_id"],
            parameters={"a": cfg["a"], "b": cfg["b"]},
            status="completed",
            max_utilisation=cfg["a"] * cfg["b"],
        )

    summary = run_sweep(study, runner)
    assert len(summary.results) == 6  # 2 x 3 full factorial
    assert all(r.status == "completed" for r in summary.results)
    critical = summary.get_critical_case("max_utilisation")
    assert critical is not None
    assert critical.max_utilisation == 60.0  # a=2 * b=30


def test_run_sweep_is_fail_soft_per_case():
    study = _study()

    def runner(cfg: dict) -> CaseResult:
        if cfg["a"] == 2.0 and cfg["b"] == 20.0:
            raise ValueError("boom")
        return CaseResult(case_id=cfg["case_id"], parameters={}, status="completed")

    summary = run_sweep(study, runner)
    failed = [r for r in summary.results if r.status == "failed"]
    assert len(failed) == 1
    assert "boom" in failed[0].notes
    assert len([r for r in summary.results if r.status == "completed"]) == 5
