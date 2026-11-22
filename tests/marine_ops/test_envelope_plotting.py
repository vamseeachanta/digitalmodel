# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Plotting error boundaries and figure lifecycle for issue #2768.

The synthetic points are assigned test data, not solver output. Independently,
basis B2 in src/digitalmodel/marine_ops/docs/envelope-basis.md gives an Hs limit
of 2 m for unit heave response, unit criterion and alpha = 1: significant heave
is Hs / 2, so the limit is 1 / (1 / 2). Periods 8 s and 12 s are arbitrary
positive abscissae; heading 0 degrees and full captured variance describe this
synthetic example. No numerical envelope calculation is exercised here.
"""

import builtins
from dataclasses import replace
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

from digitalmodel.marine_ops.operation_envelope import (
    EnvelopePoint,
    EnvelopeResult,
    plot_envelopes,
)


@pytest.fixture
def envelopes():
    """Return the hand-derived, fully evaluated example described above."""
    return {
        "synthetic": EnvelopeResult(
            vessel="",
            operation="synthetic",
            alpha=1.0,
            points=[
                EnvelopePoint(
                    tp_s=tp,
                    hs_limit_m=2.0,
                    governing_dof="heave",
                    governing_heading_deg=0.0,
                    evaluated=True,
                    status="evaluated",
                    captured_variance_fraction=1.0,
                )
                for tp in (8.0, 12.0)
            ],
        )
    }


def inject_plotter(monkeypatch, *, import_name=None, error=None):
    """Intercept explicit imports even when another test has cached pyplot."""
    original_import = builtins.__import__
    plotter = SimpleNamespace(use=Mock())
    imports = []

    def import_with_fault(name, *args, **kwargs):
        if name in ("matplotlib", "matplotlib.pyplot"):
            imports.append(name)
            if name == import_name:
                raise error
            if name == "matplotlib":
                return plotter
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", import_with_fault)
    return plotter, imports


def test_missing_optional_plotter_returns_none(monkeypatch, envelopes, tmp_path):
    missing = ModuleNotFoundError("No module named 'matplotlib'", name="matplotlib")
    plotter, imports = inject_plotter(
        monkeypatch, import_name="matplotlib", error=missing
    )
    output = tmp_path / "absent.png"

    assert plot_envelopes(envelopes, output) is None
    assert imports == ["matplotlib"]
    plotter.use.assert_not_called()
    assert not output.exists()


@pytest.mark.parametrize(
    "error",
    [
        RuntimeError("backend initialization failed"),
        ImportError("backend import failed"),
        ModuleNotFoundError("backend module missing", name="matplotlib"),
    ],
)
def test_backend_error_propagates(monkeypatch, envelopes, tmp_path, error):
    plotter, imports = inject_plotter(monkeypatch)
    cause = OSError("backend resource unavailable")
    error.__cause__ = cause
    plotter.use.side_effect = error

    with pytest.raises(type(error)) as caught:
        plot_envelopes(envelopes, tmp_path / "backend.png")

    assert caught.value is error
    assert caught.value.__cause__ is cause
    assert imports == ["matplotlib"]
    plotter.use.assert_called_once_with("Agg")


@pytest.mark.parametrize("import_name", ["matplotlib", "matplotlib.pyplot"])
@pytest.mark.parametrize("dependency", ["synthetic_dependency", "matplotlib.internal"])
def test_transitive_import_error_propagates(
    monkeypatch, envelopes, tmp_path, import_name, dependency
):
    error = ModuleNotFoundError("plotting dependency missing", name=dependency)
    cause = ImportError("dependency initialization failed")
    error.__cause__ = cause
    plotter, imports = inject_plotter(
        monkeypatch, import_name=import_name, error=error
    )

    with pytest.raises(ModuleNotFoundError) as caught:
        plot_envelopes(envelopes, tmp_path / "transitive.png")

    assert caught.value is error
    assert caught.value.name == dependency
    assert caught.value.__cause__ is cause
    assert imports[-1] == import_name
    if import_name == "matplotlib":
        plotter.use.assert_not_called()
    else:
        plotter.use.assert_called_once_with("Agg")


@pytest.mark.parametrize("import_name", ["matplotlib", "matplotlib.pyplot"])
def test_import_initialization_error_propagates(
    monkeypatch, envelopes, tmp_path, import_name
):
    error = ImportError("plotter initialization failed")
    inject_plotter(monkeypatch, import_name=import_name, error=error)

    with pytest.raises(ImportError) as caught:
        plot_envelopes(envelopes, tmp_path / "initialization.png")

    assert caught.value is error


@pytest.mark.parametrize("state", ["empty", "unevaluated", "nonpositive"])
def test_invalid_envelope_is_checked_without_plotter(
    monkeypatch, envelopes, tmp_path, state
):
    missing = ModuleNotFoundError("No module named 'matplotlib'", name="matplotlib")
    plotter, imports = inject_plotter(
        monkeypatch, import_name="matplotlib", error=missing
    )
    result = envelopes["synthetic"]
    point = result.points[0]
    if state == "empty":
        result.points = []
    elif state == "unevaluated":
        result.points = [replace(point, evaluated=False, status="unevaluated")]
    else:
        # Zero is the boundary excluded by the strictly positive limit contract.
        result.points = [replace(point, hs_limit_m=0.0)]

    with pytest.raises(ValueError, match="envelope contains"):
        plot_envelopes(envelopes, tmp_path / "invalid.png")

    assert not imports
    plotter.use.assert_not_called()


@pytest.fixture
def pyplot():
    """Use real Agg rendering and preserve figures belonging to other callers."""
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    before = set(plt.get_fignums())
    existing = plt.figure()
    try:
        yield plt, set(plt.get_fignums())
    finally:
        for number in set(plt.get_fignums()) - before:
            plt.close(number)
        plt.close(existing)


def test_real_agg_rendering_closes_figure(pyplot, envelopes, tmp_path):
    plt, existing = pyplot
    output = tmp_path / "envelope.png"

    assert plot_envelopes(envelopes, output) == output
    assert output.is_file()
    # PNG's fixed signature identifies actual rendered bytes, not an empty file.
    assert output.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
    assert plt.get_backend().lower() == "agg"
    assert set(plt.get_fignums()) == existing


@pytest.mark.parametrize("stage", ["plot", "savefig"])
def test_rendering_error_closes_figure(
    monkeypatch, pyplot, envelopes, tmp_path, stage
):
    from matplotlib.axes import Axes
    from matplotlib.figure import Figure

    plt, existing = pyplot
    error = OSError("rendering failed")

    def fail(*args, **kwargs):
        raise error

    target = Axes if stage == "plot" else Figure
    monkeypatch.setattr(target, stage, fail)
    with pytest.raises(OSError) as caught:
        plot_envelopes(envelopes, tmp_path / "failed.png")

    assert caught.value is error
    assert set(plt.get_fignums()) == existing
