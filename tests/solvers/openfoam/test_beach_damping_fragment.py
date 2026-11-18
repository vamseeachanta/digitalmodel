# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compile the rendered arithmetic against stubs; no CFD or MPI validation.

Synthetic cases use the issue #2046 (2026) policy s_eff=min(s,c*r).
The fixture cap c=1/2 is an arithmetic input, not an absorption calibration.
Every expected rate is independently calculated in its test docstring.
"""

import importlib.util
import shutil
import subprocess
from pathlib import Path

import pytest


@pytest.fixture(scope="module")
def fragment_executable(tmp_path_factory):
    compiler = shutil.which("c++")
    if compiler is None:
        pytest.skip("C++ compiler unavailable for arithmetic harness")
    root = Path(__file__).resolve().parents[3]
    spec = importlib.util.spec_from_file_location(
        "beach_damping", root / "src/digitalmodel/solvers/openfoam/beach_damping.py"
    )
    beach = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(beach)
    scratch = tmp_path_factory.mktemp("fragment")
    (scratch / "limited.H").write_text(
        beach.render_beach_limiter(max_damping_fraction=0.5), encoding="utf-8"
    )
    executable = scratch / "limiter"
    subprocess.run(
        [compiler, "-std=c++11", "-Wall", "-Wextra", "-Werror",
         "-I", str(scratch),
         str(Path(__file__).parent / "fixtures/beach_limiter_harness.cpp"),
         "-o", str(executable)],
        check=True, capture_output=True, text=True,
    )
    return executable


def run_fragment(executable, scheme, *, rates, reciprocal, cells, delta_t=1,
                 has_field=True, wrong_dimensions=False):
    data = [scheme, delta_t, int(has_field), int(wrong_dimensions),
            len(reciprocal), len(rates), len(cells), *rates, *reciprocal, *cells]
    completed = subprocess.run(
        [str(executable)], input=" ".join(map(str, data)),
        check=True, capture_output=True, text=True,
    )
    return completed.stdout


def test_fragment_maps_noncontiguous_zone_and_local_steps(fragment_executable):
    """r=(1,unused,20,4), s=4: limits=(1/2,unused,10,2).

    Zone indices (3,0,2) give mesh-ordered rates (1/2,0,4,2).
    Two of three zone cells clip; the non-zone cell remains zero.
    """
    text = run_fragment(
        fragment_executable, "localEuler", rates=[4, 99, 4, 4],
        reciprocal=[1, 0, 20, 4], cells=[3, 0, 2], delta_t=0.001,
    )
    assert "effective: 0.5 0 4 2\n" in text
    assert "clipped=2 cells=3" in text
    assert "maxEffectiveFraction=0.5" in text


def test_fragment_euler_ignores_stale_field(fragment_executable):
    """Euler dt=1/8 yields cap=4: rates (0,2,4) stay unchanged."""
    text = run_fragment(
        fragment_executable, "Euler", rates=[0, 2, 4],
        reciprocal=[1, 1, 1], cells=[0, 1, 2], delta_t=0.125,
    )
    assert "effective: 0 2 4\n" in text
    assert "clipped=0 cells=3 fraction=0" in text


def test_fragment_euler_fallback_and_exact_cap(fragment_executable):
    """No LTS field: dt=2 gives cap=1/4; only s=1 exceeds (0,1/4,1)."""
    text = run_fragment(
        fragment_executable, "Euler", rates=[0, 0.25, 1],
        reciprocal=[0, 0, 0], cells=[0, 1, 2], delta_t=2, has_field=False,
    )
    assert "effective: 0 0.25 0.25\n" in text
    assert "clipped=1 cells=3" in text


def test_fragment_empty_partition(fragment_executable):
    """An empty set has zero counts and undefined fraction and maximum."""
    text = run_fragment(
        fragment_executable, "localEuler", rates=[1], reciprocal=[1], cells=[],
    )
    assert "clipped=0 cells=0 fraction=NOT_EVALUATED" in text
    assert "maxEffectiveFraction=NOT_EVALUATED" in text


@pytest.mark.parametrize("override", [
    {"has_field": False}, {"wrong_dimensions": True},
    {"reciprocal": [0]}, {"reciprocal": [-1]},
    {"reciprocal": [float("nan")]}, {"reciprocal": [float("inf")]},
    {"rates": [-1]}, {"rates": [float("nan")]},
    {"rates": [float("inf")]}, {"rates": []},
])
def test_fragment_rejects_invalid_local_inputs(fragment_executable, override):
    args = dict(rates=[1], reciprocal=[1], cells=[0])
    args.update(override)
    text = run_fragment(fragment_executable, "localEuler", **args)
    assert "REJECTED" in text
    assert "effective:" not in text


@pytest.mark.parametrize("step", [0, -1, float("nan"), float("inf")])
def test_fragment_rejects_invalid_euler_steps(fragment_executable, step):
    text = run_fragment(
        fragment_executable, "Euler", rates=[1], reciprocal=[1],
        cells=[0], delta_t=step,
    )
    assert "REJECTED" in text


def test_fragment_rejects_other_schemes(fragment_executable):
    text = run_fragment(
        fragment_executable, "steadyState", rates=[1], reciprocal=[1], cells=[0],
    )
    assert "REJECTED" in text
