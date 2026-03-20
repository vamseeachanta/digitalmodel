"""
ABOUTME: Tests for 3D frame geometry data model
ABOUTME: Validates rear trunk + under-hood assemblies and connectivity
"""

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
