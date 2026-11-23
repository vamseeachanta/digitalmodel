"""
ABOUTME: Tests for 3D interactive frame viewer (HTML/three.js)
ABOUTME: Validates JSON serialization, arc geometry, and HTML generation
"""

import base64
import colorsys
import hashlib
import re
from collections import Counter
from pathlib import Path

import json
import math
import os
import tempfile

import pytest

from digitalmodel.structural.parachute import frame_viewer_3d as viewer

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


    def test_member_chord_in_matches_member_length(self, frame, frame_json):
        for m, entry in zip(frame.members, frame_json["members"]):
            assert entry["chord_in"] == pytest.approx(m.length(frame.nodes), abs=5e-4, rel=0)

    def test_dimension_text_and_position_per_member(self, frame, frame_json):
        assert len(frame_json["members"]) == 16
        for m, entry in zip(frame.members, frame_json["members"]):
            a, b = frame.nodes[m.start_node], frame.nodes[m.end_node]
            expected = f"{m.length(frame.nodes):.3f} in"
            pos = [(getattr(a, k) + getattr(b, k)) / 2 for k in ("x", "y", "z")]
            if m.is_bend:
                expected = f"chord {expected}, R {m.bend_clr:.3f} in"
                pos = entry["arc_mid"]
            assert entry["dim_text"] == expected
            assert entry["dim_pos"] == pytest.approx(pos, abs=1e-9, rel=0)
        labels = {m["label"]: m["dim_text"] for m in frame_json["members"]}
        assert labels["bar_right_inner"] == "12.000 in"
        assert labels["bar_right_bend"] == "chord 4.243 in, R 5.250 in"

    def test_origin_node_is_n0(self, frame_json):
        assert frame_json["origin_node"] == 0


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
            html = open(path, encoding="utf-8").read()
            assert "THREE.Scene" in html or "new Scene" in html
        finally:
            os.unlink(path)

    def test_contains_frame_data(self, frame):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        try:
            generate_viewer_html(frame, path)
            html = open(path, encoding="utf-8").read()
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
            html = open(path, encoding="utf-8").read()
            assert "<!DOCTYPE html>" in html
            assert "<canvas" in html or "renderer" in html.lower()
        finally:
            os.unlink(path)

    def test_contains_toggle_controls(self, frame):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        try:
            generate_viewer_html(frame, path)
            html = open(path, encoding="utf-8").read()
            assert "checkbox" in html.lower() or "toggle" in html.lower()
        finally:
            os.unlink(path)


    def test_writes_utf8_explicitly(self, frame, tmp_path, monkeypatch):
        encodings, original = [], Path.write_text
        def spy(path, text, *args, **kwargs):
            encodings.append(kwargs.get("encoding"))
            return original(path, text, *args, **kwargs)
        monkeypatch.setattr(Path, "write_text", spy)
        path = tmp_path / "viewer.html"
        generate_viewer_html(frame, path)
        assert encodings == ["utf-8"]
        assert "3D Viewer" in path.read_bytes().decode("utf-8")

    def test_bend_tubes_follow_arc_points(self, frame, tmp_path):
        path = tmp_path / "viewer.html"
        generate_viewer_html(frame, path)
        html = path.read_text(encoding="utf-8")
        assert "QuadraticBezierCurve3" not in html
        assert "arc_points" in html

    def test_legend_swatches_match_assembly_colors(self, frame, tmp_path):
        path = tmp_path / "viewer.html"
        generate_viewer_html(frame, path)
        html = path.read_text(encoding="utf-8")
        for name, color in viewer.ASSEMBLY_COLORS.items():
            assert re.search(fr'id="tog-{name}"[^<]*>\s*<span[^>]*background:{color}', html)

    def test_assembly_hues_match_issue_text(self):
        for name, bounds in (("rear_trunk", (200, 250)), ("under_chassis", (15, 45))):
            color = viewer.ASSEMBLY_COLORS[name].lstrip("#")
            rgb = [int(color[i:i+2], 16) / 255 for i in (0, 2, 4)]
            hue = colorsys.rgb_to_hsv(*rgb)[0] * 360
            assert bounds[0] <= hue <= bounds[1]


class TestArcPoints:
    def test_bend_arc_points_lie_on_bend_radius(self, frame, frame_json):
        for m, entry in zip(frame.members, frame_json["members"]):
            if not m.is_bend:
                continue
            a, b = frame.nodes[m.start_node], frame.nodes[m.end_node]
            start, end = [a.x, a.y, a.z], [b.x, b.y, b.z]
            mid = [(x + y) / 2 for x, y in zip(start, end)]
            apex = compute_arc_midpoint(frame.nodes, m)
            sag = math.dist(mid, apex)
            # Circle centre from the independently evaluated sagitta relation.
            h = (m.length(frame.nodes) ** 2 / 4 - sag ** 2) / (2 * sag)
            centre = [x - h * (y - x) / sag for x, y in zip(mid, apex)]
            points = entry["arc_points"]
            assert len(points) == 17
            assert points[0] == pytest.approx(start, abs=1e-9, rel=0)
            assert points[-1] == pytest.approx(end, abs=1e-9, rel=0)
            for point in points:
                assert math.dist(point, centre) == pytest.approx(m.bend_clr, abs=1e-9, rel=0)

    def test_bend_arc_points_pass_through_arc_mid(self, frame, frame_json):
        for m, entry in zip(frame.members, frame_json["members"]):
            if m.is_bend:
                assert entry["arc_points"][8] == pytest.approx(
                    compute_arc_midpoint(frame.nodes, m), abs=1e-9, rel=0)

    def test_straight_members_have_no_arc_points(self, frame_json):
        assert all("arc_points" not in m for m in frame_json["members"] if not m["is_bend"])


class TestSelfContained:
    def test_inlined_modules_leave_no_network_urls(self, frame, tmp_path, monkeypatch):
        files = {}
        for key in ("THREEJS_CDN", "ORBIT_CDN", "CSS2D_CDN"):
            data = f"// {key} stub".encode()
            (tmp_path / key).write_bytes(data)
            files[key] = (key, hashlib.sha256(data).hexdigest())
        license_text = "MIT copyright and permission notice"
        (tmp_path / "LICENSE").write_text(license_text, encoding="utf-8")
        monkeypatch.setattr(viewer, "THREEJS_FILES", files, raising=False)
        monkeypatch.setattr(viewer, "THREEJS_LICENSE_SHA256",
                            hashlib.sha256(license_text.encode()).hexdigest(), raising=False)
        path = tmp_path / "viewer.html"
        generate_viewer_html(frame, path, threejs_dir=tmp_path)
        html = path.read_text(encoding="utf-8")
        assert not re.search(r"https?://", html)
        urls = re.findall(r'data:text/javascript;base64,([^"]+)', html)
        assert len(urls) == 3
        assert {base64.b64decode(u) for u in urls} == {
            (tmp_path / name).read_bytes() for name, _ in files.values()}
        assert f'<script type="text/plain" id="threejs-license">{license_text}</script>' in html

    def test_digest_mismatch_raises_before_writing(self, frame, tmp_path):
        for name, _ in getattr(viewer, "THREEJS_FILES", {}).values():
            path = tmp_path / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(b"corrupt")
        output = tmp_path / "viewer.html"
        with pytest.raises(ValueError, match=r"three.module.min.js.*expected.*actual"):
            generate_viewer_html(frame, output, threejs_dir=tmp_path)
        assert not output.exists()

    def test_missing_module_file_raises(self, frame, tmp_path):
        with pytest.raises(ValueError, match=r"three.module.min.js.*expected.*missing"):
            generate_viewer_html(frame, tmp_path / "viewer.html", threejs_dir=tmp_path)
        assert not (tmp_path / "viewer.html").exists()

    def test_cdn_mode_without_threejs_dir(self, frame, tmp_path):
        path = tmp_path / "viewer.html"
        generate_viewer_html(frame, path)
        html = path.read_text(encoding="utf-8")
        assert all(url in html for url in
                   (viewer.THREEJS_CDN, viewer.THREEJS_ORBIT_CDN, viewer.THREEJS_CSS2D_CDN))


class TestBrowserRender:
    def test_offline_render_labels_markers_and_toggles(self, frame, frame_json, tmp_path):
        api = pytest.importorskip("playwright.sync_api")
        threejs_dir = os.environ.get("DIGITALMODEL_THREEJS_DIR")
        if not threejs_dir:
            pytest.skip("DIGITALMODEL_THREEJS_DIR is unset")
        with api.sync_playwright() as pw:
            browser = None
            for channel in ("msedge", "chrome"):
                try:
                    browser = pw.chromium.launch(channel=channel, headless=True,
                        executable_path=os.environ.get("CHROME_EXECUTABLE") if channel == "chrome" else None,
                        args=["--use-angle=swiftshader", "--enable-unsafe-swiftshader"])
                    break
                except api.Error:
                    continue
            if browser is None:
                pytest.skip("No launchable Edge or Chrome channel")
            try:
                path = tmp_path / "viewer.html"
                generate_viewer_html(frame, path, threejs_dir=threejs_dir)
                page = browser.new_page()
                requests, errors = [], []
                page.route(re.compile(r"https?://"), lambda route: (requests.append(route.request.url), route.abort()))
                page.on("pageerror", lambda error: errors.append(str(error)))
                page.goto(path.as_uri())
                api.expect(page.locator("canvas")).to_have_count(1)
                api.expect(page.locator(".node-label")).to_have_count(15)
                dims = page.locator(".dim-label:visible")
                api.expect(dims).to_have_count(16)
                assert Counter(dims.all_text_contents()) == Counter(m["dim_text"] for m in frame_json["members"])
                page.locator("#tog-dimensions").uncheck()
                api.expect(dims).to_have_count(0)
                page.locator("#tog-dimensions").check()
                api.expect(dims).to_have_count(16)
                page.locator("#tog-labels").uncheck()
                api.expect(page.locator(".node-label:visible")).to_have_count(0)
                for name in ("rear_trunk", "under_chassis", "connections"):
                    page.locator(f"#tog-{name}").uncheck()
                    assert page.evaluate(f"window.frameViewer.groups.{name}.visible") is False
                gold = page.evaluate("""() => window.frameViewer.groups.connections.children
                    .filter(m => m.material.color.getHexString() === 'daa520')
                    .map(m => m.position.toArray())""")
                assert gold == [[0, 0, 0]]
                midpoints = page.evaluate("""() => Object.values(window.frameViewer.groups)
                    .flatMap(g => g.children).filter(m => m.geometry?.type === 'TubeGeometry')
                    .map(m => m.geometry.parameters.path.getPoint(0.5).toArray())""")
                assert len(midpoints) == 4
                for m in frame_json["members"]:
                    if m["is_bend"]:
                        assert min(math.dist(p, m["arc_mid"]) for p in midpoints) < 1e-3
                assert requests == []
                assert errors == []
                print(f"Browser: {channel} {browser.version}; pinned digests verified; network requests: 0")
            finally:
                browser.close()
