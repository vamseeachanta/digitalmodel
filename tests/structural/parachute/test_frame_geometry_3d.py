"""
ABOUTME: Tests for 3D frame geometry data model
ABOUTME: Validates rear trunk + under-hood assemblies and connectivity
"""

import math

from digitalmodel.structural.parachute.frame_geometry_3d import (
    FrameGeometry3D,
    build_gt1r_frame_3d,
)


class TestFrameGeometry3DStructure:
    """Structural tests for the 3D frame geometry."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()

    def test_returns_frame_geometry_3d(self):
        assert isinstance(self.frame, FrameGeometry3D)

    def test_has_rear_and_hood_assemblies(self):
        rear = self.frame.assembly_members("rear_trunk")
        hood = self.frame.assembly_members("under_hood")
        assert len(rear) > 0
        assert len(hood) > 0

    def test_rear_trunk_node_count(self):
        nodes = self.frame.assembly_nodes("rear_trunk")
        assert len(nodes) >= 6

    def test_rear_trunk_member_count(self):
        members = self.frame.assembly_members("rear_trunk")
        assert len(members) >= 6


class TestRearTrunkDimensions:
    """Verify rear trunk geometry matches hand sketches (Page 1)."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()
        self.nodes = self.frame.nodes

    def test_total_bar_width_36in(self):
        assert abs(self.nodes[4].x - self.nodes[0].x - 36.0) < 0.01

    def test_segment_6_12_12_6(self):
        assert abs(self.nodes[1].x - self.nodes[0].x - 6.0) < 0.01
        assert abs(self.nodes[2].x - self.nodes[1].x - 12.0) < 0.01
        assert abs(self.nodes[3].x - self.nodes[2].x - 12.0) < 0.01
        assert abs(self.nodes[4].x - self.nodes[3].x - 6.0) < 0.01

    def test_coupler_pin_vertical_offset_7_25in(self):
        assert abs(self.nodes[2].z - self.nodes[5].z - 7.25) < 0.01

    def test_coupler_pin_centered_at_x18(self):
        assert abs(self.nodes[5].x - 18.0) < 0.01

    def test_horizontal_bar_is_coplanar_z0(self):
        for nid in range(5):
            assert abs(self.nodes[nid].z) < 0.01

    def test_v_strut_length(self):
        expected = math.sqrt(12**2 + 7.25**2)
        m4 = self.frame.members[4]
        assert abs(m4.length(self.nodes) - expected) < 0.01

    def test_v_struts_symmetric(self):
        m4 = self.frame.members[4]
        m5 = self.frame.members[5]
        assert abs(m4.length(self.nodes) - m5.length(self.nodes)) < 0.01

    def test_parachute_arm_length(self):
        m6 = self.frame.members[6]
        assert abs(m6.length(self.nodes) - 18.0) < 0.01

    def test_frame_symmetric_about_x18(self):
        assert abs(self.nodes[0].x + self.nodes[4].x - 36.0) < 0.01
        assert abs(self.nodes[1].x + self.nodes[3].x - 36.0) < 0.01
        assert abs(self.nodes[0].z - self.nodes[4].z) < 0.01


class TestUnderHoodDimensions:
    """Verify under-hood geometry (Page 2)."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()
        self.nodes = self.frame.nodes

    def test_hood_bar_main_length_14in(self):
        member = [m for m in self.frame.members if m.label == "hood_bar_main"]
        assert len(member) == 1
        assert abs(member[0].length(self.nodes) - 14.0) < 0.01

    def test_hood_bar_end_approx_5in(self):
        member = [m for m in self.frame.members if m.label == "hood_bar_end"]
        assert len(member) == 1
        expected = math.sqrt(5**2 + 1**2)
        assert abs(member[0].length(self.nodes) - expected) < 0.01

    def test_b1_vertical_offset_1in(self):
        assert abs(self.nodes[8].z - self.nodes[9].z - 1.0) < 0.01


class TestConnectionMetadata:
    """Verify connection types and boundary conditions."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()

    def test_fixed_bc_nodes(self):
        fixed = self.frame.fixed_node_ids()
        assert 0 in fixed
        assert 4 in fixed

    def test_load_application_node(self):
        assert self.frame.load_node_id() == 5

    def test_b1_bolted_connection(self):
        b1 = [c for c in self.frame.connections if c.conn_type == "B1"]
        assert len(b1) == 1
        assert b1[0].bc_type == "bolted"

    def test_p1_pinned_connection(self):
        p1 = [c for c in self.frame.connections if c.conn_type == "P1"]
        assert len(p1) == 1
        assert p1[0].bc_type == "pinned"

    def test_all_connections_have_description(self):
        for c in self.frame.connections:
            assert c.description and len(c.description.strip()) > 0

    def test_frame_rail_assembly_exists(self):
        rails = self.frame.assembly_members("frame_rail")
        assert len(rails) == 2


class TestSectionProperties:
    """Verify section property assignment on members."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()

    def test_all_members_have_section_area(self):
        for m in self.frame.members:
            assert "A" in m.section
            assert m.section["A"] > 0

    def test_all_members_have_inertia(self):
        for m in self.frame.members:
            assert "I" in m.section

    def test_custom_tube_dimensions(self):
        frame = build_gt1r_frame_3d(bar_od=1.75, bar_wall=0.095)
        m0 = frame.members[0]
        assert abs(m0.section["OD"] - 1.75) < 0.001
