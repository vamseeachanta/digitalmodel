"""Tests for LinesBuilder property mapping."""
from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import (
    BuilderContext,
)
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import (
    LinesBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_pipeline_spec(
    segments=None,
    tugs=None,
    vessel=None,
    stinger=None,
    tensioner=None,
    buoyancy_modules=None,
    seabed_slope=0,
):
    """Create a pipeline spec for LinesBuilder testing."""
    data = {
        "metadata": {
            "name": "test",
            "description": "test",
            "structure": "pipeline",
            "operation": "installation",
            "project": "TEST",
        },
        "environment": {
            "water": {"depth": 100, "density": 1.025},
            "seabed": {"slope": seabed_slope, "stiffness": {"normal": 100, "shear": 50}},
        },
        "pipeline": {
            "name": "30'' Line",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.762, "wall_thickness": 0.0254},
            "coatings": {"corrosion": {"thickness": 0.003, "density": 1.1}},
            "segments": segments or [
                {"type": "X65+CWC80", "length": 1000, "segment_length": 5},
            ],
        },
    }
    equipment = {}
    if tugs:
        equipment["tugs"] = tugs
    if buoyancy_modules:
        equipment["buoyancy_modules"] = buoyancy_modules
    if vessel:
        equipment["vessel"] = vessel
    if stinger:
        equipment["stinger"] = stinger
    if tensioner:
        equipment["tensioner"] = tensioner
    if equipment:
        data["equipment"] = equipment
    return ProjectInputSpec(**data)


def _build(spec, end_buoy_name="6D buoy1", bm_buoy_name="BM"):
    """Run builder and return the Lines list."""
    ctx = BuilderContext()
    ctx.end_buoy_name = end_buoy_name
    ctx.bm_buoy_name = bm_buoy_name
    builder = LinesBuilder(spec, ctx)
    result = builder.build()
    return result["Lines"]


# ---------------------------------------------------------------------------
# Single-section line
# ---------------------------------------------------------------------------

class TestSingleSectionLine:
    def test_single_segment(self):
        lines = _build(_make_pipeline_spec())
        assert len(lines) == 1
        line = lines[0]
        segment_data = line["LineType, Length, TargetSegmentLength"]
        assert len(segment_data) == 1
        assert segment_data[0] == ["X65+CWC80", 1000, 5]

    def test_line_name(self):
        lines = _build(_make_pipeline_spec())
        assert lines[0]["Name"] == "30'' Line"


# ---------------------------------------------------------------------------
# Multi-section line
# ---------------------------------------------------------------------------

class TestMultiSectionLine:
    def test_multiple_segments(self):
        """Multiple segments produce multiple entries in segment table."""
        lines = _build(_make_pipeline_spec(segments=[
            {"type": "X65+CWC80", "length": 500, "segment_length": 5},
            {"type": "X65+CWC120", "length": 300, "segment_length": 3},
            {"type": "X65+CWC80", "length": 200, "segment_length": 5},
        ]))
        segment_data = lines[0]["LineType, Length, TargetSegmentLength"]
        assert len(segment_data) == 3
        assert segment_data[0] == ["X65+CWC80", 500, 5]
        assert segment_data[1] == ["X65+CWC120", 300, 3]
        assert segment_data[2] == ["X65+CWC80", 200, 5]

    def test_segment_order_preserved(self):
        lines = _build(_make_pipeline_spec(segments=[
            {"type": "TypeA", "length": 100, "segment_length": 2},
            {"type": "TypeB", "length": 200, "segment_length": 4},
        ]))
        segment_data = lines[0]["LineType, Length, TargetSegmentLength"]
        assert segment_data[0][0] == "TypeA"
        assert segment_data[1][0] == "TypeB"


# ---------------------------------------------------------------------------
# EndA / EndB connections — floating
# ---------------------------------------------------------------------------

class TestFloatingConnections:
    def test_end_a_fixed(self):
        """Floating install: End A is Fixed."""
        lines = _build(_make_pipeline_spec())
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = lines[0][conn_key]
        assert connections[0][0] == "Fixed"

    def test_end_b_buoy(self):
        """Floating install: End B connects to the 6D buoy."""
        lines = _build(_make_pipeline_spec(), end_buoy_name="6D buoy1")
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = lines[0][conn_key]
        assert connections[1][0] == "6D buoy1"

    def test_seabed_slope_in_declination(self):
        """Seabed slope is added to End A declination."""
        lines = _build(_make_pipeline_spec(seabed_slope=3))
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = lines[0][conn_key]
        # End A declination = 90 + slope
        assert connections[0][5] == 93


# ---------------------------------------------------------------------------
# EndA / EndB connections — S-lay
# ---------------------------------------------------------------------------

class TestSlayConnections:
    def _make_slay(self, seabed_slope=0):
        return _make_pipeline_spec(
            vessel={
                "name": "Eclipse",
                "properties": {
                    "loa": 130, "beam": 28, "draft": 4, "gmt": 2.5,
                    "cog": [0, 0, 3], "gyration_radii": [10, 30, 30],
                },
            },
            stinger={"radius": 150},
            tensioner={"capacity_kn": 500, "tension_value": 400},
            seabed_slope=seabed_slope,
        )

    def test_end_a_vessel(self):
        """S-lay: End A connects to vessel."""
        lines = _build(self._make_slay())
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = lines[0][conn_key]
        assert connections[0][0] == "Eclipse"

    def test_end_b_anchored(self):
        """S-lay: End B is Anchored."""
        lines = _build(self._make_slay())
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = lines[0][conn_key]
        assert connections[1][0] == "Anchored"

    def test_slay_end_b_declination_with_slope(self):
        lines = _build(self._make_slay(seabed_slope=2))
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = lines[0][conn_key]
        assert connections[1][5] == 92  # 90 + slope


# ---------------------------------------------------------------------------
# Winch wire
# ---------------------------------------------------------------------------

class TestWinchWire:
    def test_winch_wire_added_with_tugs(self):
        lines = _build(_make_pipeline_spec(tugs={
            "count": 1, "spacing": 100, "first_position": [500, -20, 0],
            "properties": {"mass": 30, "volume": 100},
        }))
        names = [l["Name"] for l in lines]
        assert "Winch wire" in names

    def test_no_winch_wire_without_tugs(self):
        lines = _build(_make_pipeline_spec())
        names = [l["Name"] for l in lines]
        assert "Winch wire" not in names

    def test_winch_wire_segment_type(self):
        lines = _build(_make_pipeline_spec(tugs={
            "count": 1, "spacing": 100, "first_position": [500, -20, 0],
            "properties": {"mass": 30, "volume": 100},
        }))
        winch = [l for l in lines if l["Name"] == "Winch wire"][0]
        segments = winch["LineType, Length, TargetSegmentLength"]
        assert segments[0][0] == "Winch wire_LT"


# ---------------------------------------------------------------------------
# Line properties
# ---------------------------------------------------------------------------

class TestLineProperties:
    def test_representation(self):
        lines = _build(_make_pipeline_spec())
        assert lines[0]["Representation"] == "Finite element"

    def test_include_torsion(self):
        lines = _build(_make_pipeline_spec())
        assert lines[0]["IncludeTorsion"] == "Yes"

    def test_top_end(self):
        lines = _build(_make_pipeline_spec())
        assert lines[0]["TopEnd"] == "End A"


# ---------------------------------------------------------------------------
# Context registration
# ---------------------------------------------------------------------------

class TestContextRegistration:
    def test_line_names_registered(self):
        spec = _make_pipeline_spec()
        ctx = BuilderContext()
        ctx.end_buoy_name = "6D buoy1"
        ctx.bm_buoy_name = "BM"
        builder = LinesBuilder(spec, ctx)
        builder.build()
        entities = builder.get_generated_entities()
        assert entities["line_names"] == ["30'' Line"]
        assert entities["main_pipeline_name"] == "30'' Line"

    def test_additional_line_names_with_tugs(self):
        spec = _make_pipeline_spec(tugs={
            "count": 1, "spacing": 100, "first_position": [500, -20, 0],
            "properties": {"mass": 30, "volume": 100},
        })
        ctx = BuilderContext()
        ctx.end_buoy_name = "6D buoy1"
        ctx.bm_buoy_name = "BM"
        builder = LinesBuilder(spec, ctx)
        builder.build()
        entities = builder.get_generated_entities()
        assert "Winch wire" in entities["additional_line_names"]


# ---------------------------------------------------------------------------
# should_generate
# ---------------------------------------------------------------------------

class TestShouldGenerate:
    def test_pipeline_model_generates(self):
        spec = _make_pipeline_spec()
        builder = LinesBuilder(spec, BuilderContext())
        assert builder.should_generate() is True

    def test_generic_model_skips(self):
        spec = ProjectInputSpec(**{
            "metadata": {"name": "t", "description": "t", "structure": "generic", "operation": "generic", "project": "t"},
            "environment": {"water": {"depth": 100}, "seabed": {"stiffness": {"normal": 100, "shear": 50}}},
            "generic": {"line_types": [], "vessels": []},
        })
        builder = LinesBuilder(spec, BuilderContext())
        assert builder.should_generate() is False
