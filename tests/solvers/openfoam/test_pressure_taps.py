#!/usr/bin/env python3
"""
ABOUTME: Tests for named wall pressure taps (dm#661) - function-object
rendering for point / patch-point / surface taps, case-builder integration
(taps are optional and additive), and per-tap post-processing statistics on a
synthetic probe file. None of these tests require an OpenFOAM installation.
"""

import textwrap
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.models import CaseType, OpenFOAMCase
from digitalmodel.solvers.openfoam.post_processing import OpenFOAMPostProcessor
from digitalmodel.solvers.openfoam.pressure_taps import (
    PressureTap,
    PressureTapStatistics,
    b1546_default_taps,
    compute_tap_statistics,
    point_tap_names,
    read_tap_statistics,
    render_patch_probes_entry,
    render_pressure_tap_functions,
    render_probes_entry,
    render_surface_entry,
)


# ============================================================================
# PressureTap model
# ============================================================================


class TestPressureTap:
    """PressureTap kind inference and validation."""

    def test_point_tap_kind(self):
        tap = PressureTap(name="p1", location=(1.0, 2.0, 3.0))
        assert tap.kind == "point"

    def test_surface_tap_kind(self):
        tap = PressureTap(name="s1", patch="sideShell")
        assert tap.kind == "surface"

    def test_patch_point_tap_kind(self):
        tap = PressureTap(name="pp1", location=(1.0, 2.0, 3.0), patch="floor")
        assert tap.kind == "patch_point"

    def test_requires_location_or_patch(self):
        with pytest.raises(ValueError):
            PressureTap(name="bad")

    def test_requires_name(self):
        with pytest.raises(ValueError):
            PressureTap(name="  ", location=(0.0, 0.0, 0.0))

    def test_bad_location_length(self):
        with pytest.raises(ValueError):
            PressureTap(name="p", location=(0.0, 0.0))  # type: ignore[arg-type]


# ============================================================================
# Function-object rendering
# ============================================================================


class TestRenderProbes:
    """Point taps render as a single probes function object."""

    def test_point_taps_render_probes(self):
        taps = [
            PressureTap(name="tank_top", location=(1.0, 0.5, 2.99)),
            PressureTap(name="floor_1", location=(0.5, 0.5, 0.01)),
        ]
        text = render_probes_entry(taps)
        assert "type            probes;" in text
        assert "libs            (sampling);" in text
        assert "probeLocations" in text
        # Names appear as comments for traceability.
        assert "// tank_top" in text
        assert "// floor_1" in text
        # Coordinates present.
        assert "(1 0.5 2.99)" in text
        assert "(0.5 0.5 0.01)" in text

    def test_probes_field_union_pressure_first(self):
        taps = [
            PressureTap(name="a", location=(0, 0, 0), fields=("p_rgh",)),
            PressureTap(name="b", location=(1, 1, 1), fields=("p", "p_rgh")),
        ]
        text = render_probes_entry(taps)
        assert "fields          (p p_rgh);" in text

    def test_empty_raises(self):
        with pytest.raises(ValueError):
            render_probes_entry([])


class TestRenderSurface:
    """A whole-patch tap renders as surfaceFieldValue."""

    def test_surface_tap_renders_surface_field_value(self):
        tap = PressureTap(
            name="side_shell_avg",
            patch="sideShell",
            fields=("p",),
            operation="areaAverage",
        )
        text = render_surface_entry(tap)
        assert "type            surfaceFieldValue;" in text
        assert "regionType      patch;" in text
        assert "name            sideShell;" in text
        assert "operation       areaAverage;" in text
        assert text.strip().startswith("side_shell_avg")

    def test_surface_render_rejects_point_tap(self):
        tap = PressureTap(name="p", location=(0, 0, 0))
        with pytest.raises(ValueError):
            render_surface_entry(tap)


class TestRenderPatchProbes:
    """Point-on-wall taps render as patchProbes for a named patch."""

    def test_patch_probes_render(self):
        taps = [PressureTap(name="wtap", location=(1, 2, 0), patch="floor")]
        text = render_patch_probes_entry(taps, "floor")
        assert "type            patchProbes;" in text
        assert "patch           floor;" in text
        assert "// wtap" in text


class TestRenderFunctionsBlock:
    """The combined functions{} block groups taps by kind."""

    def test_empty_taps_returns_empty_string(self):
        assert render_pressure_tap_functions([]) == ""

    def test_mixed_taps_emit_expected_objects(self):
        taps = [
            PressureTap(name="top", location=(1, 0.5, 3)),
            PressureTap(name="floor", location=(0.5, 0.5, 0)),
            PressureTap(name="wall_pt", location=(0, 0.5, 1), patch="side"),
            PressureTap(name="side_avg", patch="side"),
        ]
        block = render_pressure_tap_functions(taps)
        assert block.startswith("functions")
        # One probes object for the two point taps.
        assert block.count("type            probes;") == 1
        # One patchProbes object for the patch-point tap.
        assert block.count("type            patchProbes;") == 1
        # One surfaceFieldValue for the surface tap.
        assert block.count("type            surfaceFieldValue;") == 1

    def test_duplicate_names_raise(self):
        taps = [
            PressureTap(name="dup", location=(0, 0, 0)),
            PressureTap(name="dup", location=(1, 1, 1)),
        ]
        with pytest.raises(ValueError):
            render_pressure_tap_functions(taps)

    def test_patch_point_taps_group_by_patch(self):
        taps = [
            PressureTap(name="a", location=(0, 0, 0), patch="floor"),
            PressureTap(name="b", location=(1, 0, 0), patch="floor"),
            PressureTap(name="c", location=(0, 1, 0), patch="side"),
        ]
        block = render_pressure_tap_functions(taps)
        # Two patchProbes objects: one per patch.
        assert block.count("type            patchProbes;") == 2


# ============================================================================
# B1546 default taps
# ============================================================================


class TestB1546DefaultTaps:
    """The named B1546 interest points."""

    def test_named_families_present(self):
        taps = b1546_default_taps(10.0, 4.0, 3.0, n_floor=2, n_side=2)
        names = {t.name for t in taps}
        assert "tank_top_centreline" in names
        assert "floor_long_1" in names and "floor_long_2" in names
        assert "side_shell_port_1" in names
        assert "side_shell_stbd_2" in names

    def test_all_taps_are_points_within_the_tank(self):
        L, W, H = 10.0, 4.0, 3.0
        taps = b1546_default_taps(L, W, H)
        for tap in taps:
            assert tap.kind == "point"
            x, y, z = tap.location
            assert 0.0 <= x <= L
            assert 0.0 <= y <= W
            assert 0.0 <= z <= H

    def test_multiphase_fields_by_default(self):
        taps = b1546_default_taps(10.0, 4.0, 3.0)
        assert taps[0].fields == ("p", "p_rgh")

    def test_bad_dimensions_raise(self):
        with pytest.raises(ValueError):
            b1546_default_taps(0.0, 4.0, 3.0)


# ============================================================================
# Case-builder integration (optional + additive)
# ============================================================================


class TestCaseBuilderIntegration:
    """Taps are optional; a case with taps emits them, without does not."""

    def _sloshing_case(self, name):
        return OpenFOAMCase.for_case_type(CaseType.SLOSHING, name)

    def test_case_without_taps_has_no_functions(self, tmp_path):
        builder = OpenFOAMCaseBuilder(self._sloshing_case("no_taps"))
        case_dir = builder.build(tmp_path)
        control = (case_dir / "system" / "controlDict").read_text()
        assert "functions" not in control
        assert "probes" not in control

    def test_case_with_taps_emits_functions(self, tmp_path):
        taps = b1546_default_taps(10.0, 4.0, 3.0)
        builder = OpenFOAMCaseBuilder(
            self._sloshing_case("with_taps"), pressure_taps=taps
        )
        case_dir = builder.build(tmp_path)
        control = (case_dir / "system" / "controlDict").read_text()
        assert "functions" in control
        assert "type            probes;" in control
        assert "// tank_top_centreline" in control
        # Base controlDict entries still present (additive, not replacing).
        assert "application" in control
        assert "endTime" in control

    def test_taps_do_not_change_other_files(self, tmp_path):
        base = OpenFOAMCaseBuilder(self._sloshing_case("case")).build(
            tmp_path / "a"
        )
        taps = b1546_default_taps(10.0, 4.0, 3.0)
        withtaps = OpenFOAMCaseBuilder(
            self._sloshing_case("case"), pressure_taps=taps
        ).build(tmp_path / "b")
        # fvSchemes identical - taps only touch controlDict.
        assert (
            (base / "system" / "fvSchemes").read_text()
            == (withtaps / "system" / "fvSchemes").read_text()
        )


# ============================================================================
# Post-processing statistics on a synthetic probe file
# ============================================================================


def _write_probe_file(path: Path) -> None:
    """A two-tap probes output: tap0 = sine at 2 Hz, tap1 = constant + spike."""
    fs = 100.0
    t = np.arange(0, 4.0, 1.0 / fs)
    tap0 = 1000.0 * np.sin(2 * np.pi * 2.0 * t)  # 2 Hz tone, +/-1000 Pa
    tap1 = np.full_like(t, 50.0)
    tap1[123] = 9000.0  # single-sample spike (should not set design pressure)

    lines = [
        "# Probe 0 (1 0.5 2.99)",
        "# Probe 1 (0.5 0.5 0.01)",
        "#   Time         p0            p1",
    ]
    for ti, a, b in zip(t, tap0, tap1):
        lines.append(f"{ti:.6f}   {a:.6f}   {b:.6f}")
    path.write_text("\n".join(lines) + "\n")


class TestPostProcessing:
    """Per-tap statistics from a parsed probe series."""

    def test_stats_named_and_peak_envelope(self, tmp_path):
        probe = tmp_path / "p"
        _write_probe_file(probe)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        series = pp.parse_probe_file(probe, field_name="p")

        stats = compute_tap_statistics(
            series,
            tap_names=["tank_top", "floor_1"],
            min_frequency=0.5,
        )
        assert set(stats) == {"tank_top", "floor_1"}

        top = stats["tank_top"]
        assert isinstance(top, PressureTapStatistics)
        # Envelope of the +/-1000 Pa tone.
        assert top.peak == pytest.approx(1000.0, rel=0.02)
        lo, hi = top.envelope
        assert lo == pytest.approx(-1000.0, rel=0.02)
        assert hi == top.max
        # Dominant frequency of the 2 Hz tone.
        assert top.dominant_frequency == pytest.approx(2.0, abs=0.3)
        # Full time-history handle retained.
        assert top.pressure.shape == top.times.shape

    def test_design_equivalent_robust_to_single_spike(self, tmp_path):
        probe = tmp_path / "p"
        _write_probe_file(probe)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        series = pp.parse_probe_file(probe, field_name="p")
        stats = compute_tap_statistics(series, tap_names=["top", "floor"])

        floor = stats["floor"]
        # True peak captures the spike ...
        assert floor.peak == pytest.approx(9000.0)
        # ... but the 99th-percentile design-equivalent ignores the lone spike.
        assert floor.design_equivalent == pytest.approx(50.0, abs=1.0)
        assert floor.design_percentile == 99.0

    def test_tap_names_length_mismatch_raises(self, tmp_path):
        probe = tmp_path / "p"
        _write_probe_file(probe)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        series = pp.parse_probe_file(probe, field_name="p")
        with pytest.raises(ValueError):
            compute_tap_statistics(series, tap_names=["only_one"])

    def test_default_names_when_unlabelled(self, tmp_path):
        probe = tmp_path / "p"
        _write_probe_file(probe)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        series = pp.parse_probe_file(probe, field_name="p")
        stats = compute_tap_statistics(series)
        assert set(stats) == {"tap_0", "tap_1"}

    def test_read_tap_statistics_wrapper(self, tmp_path):
        probe = tmp_path / "p"
        _write_probe_file(probe)
        stats = read_tap_statistics(probe, tap_names=["top", "floor"])
        assert set(stats) == {"top", "floor"}

    def test_point_tap_names_helper(self):
        taps = [
            PressureTap(name="a", location=(0, 0, 0)),
            PressureTap(name="surf", patch="side"),
            PressureTap(name="b", location=(1, 1, 1)),
        ]
        assert point_tap_names(taps) == ["a", "b"]
