"""
ABOUTME: Tests for 3D interactive frame viewer (HTML/three.js)
ABOUTME: Validates JSON serialization, arc geometry, and HTML generation
"""

import json
import math
import os
import tempfile

import pytest

from digitalmodel.structural.parachute.frame_geometry_3d import (
    build_gt1r_frame_3d,
)
from digitalmodel.structural.parachute.frame_viewer_3d import (
    compute_arc_midpoint,
    frame_to_json,
    generate_viewer_html,
)


@pytest.fixture
def frame():
    return build_gt1r_frame_3d()


@pytest.fixture
def frame_json(frame):
    return frame_to_json(frame)


class TestFrameToJson:
    """Validate JSON serialization of FrameGeometry3D."""

    def test_node_count(self, frame_json):
        assert len(frame_json["nodes"]) == 15

    def test_member_count(self, frame_json):
        assert len(frame_json["members"]) == 16

    def test_connection_count(self, frame_json):
        assert len(frame_json["connections"]) == 15

    def test_node_has_required_keys(self, frame_json):
        node = frame_json["nodes"]["0"]
        for key in ("x", "y", "z", "label", "assembly"):
            assert key in node

    def test_member_has_required_keys(self, frame_json):
        member = frame_json["members"][0]
        for key in ("start", "end", "label", "assembly", "is_bend", "od"):
            assert key in member

    def test_bend_members_have_arc_mid(self, frame_json):
        bends = [m for m in frame_json["members"] if m["is_bend"]]
        assert len(bends) == 4
        for m in bends:
            assert "arc_mid" in m
            assert len(m["arc_mid"]) == 3

    def test_straight_members_no_arc_mid(self, frame_json):
        straights = [m for m in frame_json["members"] if not m["is_bend"]]
        assert len(straights) == 12
        for m in straights:
            assert "arc_mid" not in m

    def test_connection_has_bc_type(self, frame_json):
        for c in frame_json["connections"]:
            assert "bc_type" in c
            assert "conn_type" in c
            assert "node_id" in c

    def test_json_serializable(self, frame_json):
        """Ensure the dict can be serialized to JSON without error."""
        result = json.dumps(frame_json)
        assert len(result) > 100


class TestComputeArcMidpoint:
    """Validate arc midpoint sag computation."""

    def test_known_bend_n6_n7(self, frame):
        """Right bar bend (N6→N7): known geometry, verify sag direction."""
        # Member 9: bar_right_bend, N6→N7, CLR=5.25
        bend = [m for m in frame.members if m.label == "bar_right_bend"][0]
        mid = compute_arc_midpoint(frame.nodes, bend)

        n6 = frame.nodes[6]
        n7 = frame.nodes[7]
        chord_mid = ((n6.x + n7.x) / 2, (n6.y + n7.y) / 2, (n6.z + n7.z) / 2)

        # Arc mid should differ from chord midpoint (sag > 0)
        dist = math.sqrt(sum((a - b) ** 2 for a, b in zip(mid, chord_mid)))
        assert dist > 0.01, "Arc midpoint should be offset from chord midpoint"

    def test_sag_magnitude(self, frame):
        """Verify sag = R - sqrt(R^2 - (chord/2)^2) for a known bend."""
        bend = [m for m in frame.members if m.label == "bar_right_bend"][0]
        n6 = frame.nodes[bend.start_node]
        n7 = frame.nodes[bend.end_node]
        chord_len = math.sqrt(
            (n7.x - n6.x) ** 2 + (n7.y - n6.y) ** 2 + (n7.z - n6.z) ** 2
        )
        R = bend.bend_clr
        expected_sag = R - math.sqrt(R * R - (chord_len / 2) ** 2)

        mid = compute_arc_midpoint(frame.nodes, bend)
        chord_mid = ((n6.x + n7.x) / 2, (n6.y + n7.y) / 2, (n6.z + n7.z) / 2)
        actual_sag = math.sqrt(sum((a - b) ** 2 for a, b in zip(mid, chord_mid)))

        assert abs(actual_sag - expected_sag) < 0.01

    def test_returns_three_floats(self, frame):
        bend = frame.bend_members()[0]
        mid = compute_arc_midpoint(frame.nodes, bend)
        assert len(mid) == 3
        assert all(isinstance(v, float) for v in mid)


class TestGenerateViewerHtml:
    """Validate HTML generation."""

    def test_creates_file(self, frame):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        try:
            result = generate_viewer_html(frame, path)
            assert os.path.exists(result)
            size = os.path.getsize(result)
            assert size > 1000, f"HTML too small: {size} bytes"
        finally:
            os.unlink(path)

    def test_contains_three_js(self, frame):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        try:
            generate_viewer_html(frame, path)
            html = open(path).read()
            assert "THREE.Scene" in html or "new Scene" in html
        finally:
            os.unlink(path)

    def test_contains_frame_data(self, frame):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        try:
            generate_viewer_html(frame, path)
            html = open(path).read()
            assert "parachute_bracket" in html
            assert "coupler_pin" in html
            assert "c2_weld_junction" in html
        finally:
            os.unlink(path)

    def test_contains_html_structure(self, frame):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        try:
            generate_viewer_html(frame, path)
            html = open(path).read()
            assert "<!DOCTYPE html>" in html
            assert "<canvas" in html or "renderer" in html.lower()
        finally:
            os.unlink(path)

    def test_contains_toggle_controls(self, frame):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        try:
            generate_viewer_html(frame, path)
            html = open(path).read()
            assert "checkbox" in html.lower() or "toggle" in html.lower()
        finally:
            os.unlink(path)
