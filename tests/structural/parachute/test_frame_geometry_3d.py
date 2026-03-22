"""
ABOUTME: Tests for 3D frame geometry data model
ABOUTME: Validates rear trunk + under-chassis assemblies, new coordinate system
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

    def test_has_rear_trunk_and_under_chassis_assemblies(self):
        rear = self.frame.assembly_members("rear_trunk")
        chassis = self.frame.assembly_members("under_chassis")
        assert len(rear) > 0
        assert len(chassis) > 0

    def test_total_node_count(self):
        assert len(self.frame.nodes) == 15

    def test_total_member_count(self):
        assert len(self.frame.members) == 16

    def test_rear_trunk_member_count(self):
        members = self.frame.assembly_members("rear_trunk")
        assert len(members) == 12

    def test_under_chassis_member_count(self):
        members = self.frame.assembly_members("under_chassis")
        assert len(members) == 4


class TestOriginAndCoordinateSystem:
    """Verify origin at N0 (C2 weld) and coordinate conventions."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()
        self.n = self.frame.nodes

    def test_origin_at_n0(self):
        assert self.n[0].x == 0.0
        assert self.n[0].y == 0.0
        assert self.n[0].z == 0.0

    def test_origin_is_c2_weld(self):
        c2 = [c for c in self.frame.connections if c.node_id == 0]
        assert len(c2) == 1
        assert c2[0].conn_type == "C2"

    def test_parachute_is_rearward(self):
        assert self.n[1].y < 0  # rearward = -Y

    def test_bar_is_forward(self):
        assert self.n[4].y > 0  # forward = +Y

    def test_right_side_positive_x(self):
        assert self.n[5].x > 0  # right = +X
        assert self.n[8].x < 0  # left = -X


class TestRearTrunkDimensions:
    """Verify rear trunk geometry matches finalized spec."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()
        self.n = self.frame.nodes

    def test_bar_total_width_36in(self):
        width = abs(self.n[7].x - self.n[10].x)
        assert abs(width - 36.0) < 0.01

    def test_bar_symmetric_about_x0(self):
        assert abs(self.n[7].x + self.n[10].x) < 0.01
        assert abs(self.n[5].x + self.n[8].x) < 0.01
        assert abs(self.n[6].x + self.n[9].x) < 0.01

    def test_bar_inner_segments_12in(self):
        dx_right = abs(self.n[5].x - self.n[4].x)
        dx_left = abs(self.n[8].x - self.n[4].x)
        assert abs(dx_right - 12.0) < 0.01
        assert abs(dx_left - 12.0) < 0.01

    def test_bar_mid_segments_3in(self):
        dx_right = abs(self.n[6].x - self.n[5].x)
        dx_left = abs(self.n[9].x - self.n[8].x)
        assert abs(dx_right - 3.0) < 0.01
        assert abs(dx_left - 3.0) < 0.01

    def test_bar_end_segments_3in(self):
        dx_right = abs(self.n[7].x - self.n[6].x)
        dx_left = abs(self.n[10].x - self.n[9].x)
        assert abs(dx_right - 3.0) < 0.01
        assert abs(dx_left - 3.0) < 0.01

    def test_bar_z_at_2_5(self):
        for nid in [4, 5, 6, 8, 9]:
            assert abs(self.n[nid].z - 2.5) < 0.01

    def test_c3_mount_z_drop(self):
        assert abs(self.n[7].z - (-0.5)) < 0.01
        assert abs(self.n[10].z - (-0.5)) < 0.01

    def test_coupler_pin_at_origin_z(self):
        assert abs(self.n[2].z) < 0.01

    def test_parachute_arm_length(self):
        m0 = [m for m in self.frame.members if m.label == "parachute_arm"][0]
        assert abs(m0.length(self.n) - 3.25) < 0.01

    def test_v_struts_symmetric(self):
        m_right = [m for m in self.frame.members if m.label == "v_strut_right"][0]
        m_left = [m for m in self.frame.members if m.label == "v_strut_left"][0]
        assert abs(m_right.length(self.n) - m_left.length(self.n)) < 0.01

    def test_v_struts_connect_to_n3(self):
        m_right = [m for m in self.frame.members if m.label == "v_strut_right"][0]
        m_left = [m for m in self.frame.members if m.label == "v_strut_left"][0]
        assert m_right.end_node == 3
        assert m_left.end_node == 3

    def test_center_spine_passes_through_origin(self):
        m1 = [m for m in self.frame.members if m.label == "center_spine_lower"][0]
        assert m1.end_node == 0  # ends at origin


class TestUnderChassisDimensions:
    """Verify under-chassis geometry."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()
        self.n = self.frame.nodes

    def test_chassis_bar_right_length_16in(self):
        m = [m for m in self.frame.members if m.label == "chassis_bar_right"][0]
        assert abs(m.length(self.n) - 16.0) < 0.01

    def test_chassis_bar_symmetric(self):
        assert abs(self.n[11].x + self.n[13].x) < 0.01
        assert abs(self.n[12].x + self.n[14].x) < 0.01

    def test_b1_bolted_z_drop(self):
        assert abs(self.n[12].z - (-4.0)) < 0.01
        assert abs(self.n[14].z - (-4.0)) < 0.01

    def test_b1_bolted_y_forward(self):
        assert self.n[12].y > 0
        assert abs(self.n[12].y - 5.0) < 0.01


class TestConnectionMetadata:
    """Verify connection types and boundary conditions."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()

    def test_fixed_bc_nodes(self):
        fixed = self.frame.fixed_node_ids()
        assert 7 in fixed   # right C3
        assert 10 in fixed  # left C3

    def test_bolted_bc_nodes(self):
        bolted = self.frame.bolted_node_ids()
        assert 12 in bolted  # right B1
        assert 14 in bolted  # left B1

    def test_load_application_node(self):
        assert self.frame.load_node_id() == 2  # coupler pin

    def test_b1_bolted_connections(self):
        b1 = [c for c in self.frame.connections if c.conn_type == "B1"]
        assert len(b1) == 2
        for c in b1:
            assert c.bc_type == "bolted"

    def test_all_connections_have_description(self):
        for c in self.frame.connections:
            assert c.description and len(c.description.strip()) > 0

    def test_total_connection_count(self):
        assert len(self.frame.connections) == 15


class TestBendMembers:
    """Verify bend member properties."""

    def setup_method(self):
        self.frame = build_gt1r_frame_3d()

    def test_four_bend_members(self):
        bends = self.frame.bend_members()
        assert len(bends) == 4

    def test_bend_clr_default(self):
        for m in self.frame.bend_members():
            assert abs(m.bend_clr - 5.25) < 0.01

    def test_bend_labels(self):
        bend_labels = {m.label for m in self.frame.bend_members()}
        assert "bar_right_bend" in bend_labels
        assert "bar_left_bend" in bend_labels
        assert "chassis_bar_right_bend" in bend_labels
        assert "chassis_bar_left_bend" in bend_labels

    def test_straight_members_not_bends(self):
        straight = [m for m in self.frame.members if not m.is_bend]
        assert len(straight) == 12


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

    def test_default_tube_1_75_od(self):
        m0 = self.frame.members[0]
        assert abs(m0.section["OD"] - 1.75) < 0.001

    def test_custom_tube_dimensions(self):
        frame = build_gt1r_frame_3d(tube_od=2.0, tube_wall=0.095)
        m0 = frame.members[0]
        assert abs(m0.section["OD"] - 2.0) < 0.001
