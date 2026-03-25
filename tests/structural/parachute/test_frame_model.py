"""
Tests for parachute frame geometry and material model (Child-A + Child-B).

Geometry from hand sketches:
  - Horizontal bar: 6" + 12" + 12" + 6" = 36" total
  - Vertical offset to coupler pin: 7.25"
  - V-struts from bar junctions to coupler pin

Material: 4130 chromoly steel (ASTM A519, normalized)
  - Fy = 63 ksi = 63,000 psi
  - Fu = 97 ksi = 97,000 psi
  - E = 29.7 Msi = 29,700,000 psi
"""

import math
import pytest

from digitalmodel.structural.parachute.frame_model import (
    build_gt1r_frame,
    FrameModel,
    CHROMOLY_4130,
    tube_section_properties,
)


class TestChromoly4130:
    def test_yield_strength(self):
        assert CHROMOLY_4130["fy_psi"] == 63_000

    def test_ultimate_strength(self):
        assert CHROMOLY_4130["fu_psi"] == 97_000

    def test_elastic_modulus(self):
        assert CHROMOLY_4130["E_psi"] == 29_700_000

    def test_poisson_ratio(self):
        assert abs(CHROMOLY_4130["nu"] - 0.29) < 0.01

    def test_density(self):
        assert CHROMOLY_4130["density_pci"] > 0


class TestTubeSectionProperties:
    """Verify tube section properties against hand calcs."""

    def test_area_1_5od_0_120wall(self):
        props = tube_section_properties(od=1.5, wall=0.120)
        # A = pi/4 * (OD^2 - ID^2), ID = 1.5 - 2*0.12 = 1.26
        expected_a = math.pi / 4 * (1.5**2 - 1.26**2)
        assert abs(props["A"] - expected_a) < 0.001

    def test_moment_of_inertia_1_5od_0_120wall(self):
        props = tube_section_properties(od=1.5, wall=0.120)
        od, idd = 1.5, 1.26
        expected_i = math.pi / 64 * (od**4 - idd**4)
        assert abs(props["I"] - expected_i) < 0.0001

    def test_section_modulus(self):
        props = tube_section_properties(od=1.5, wall=0.120)
        expected_s = props["I"] / (1.5 / 2)
        assert abs(props["S"] - expected_s) < 0.0001

    def test_radius_of_gyration(self):
        props = tube_section_properties(od=1.5, wall=0.120)
        expected_r = math.sqrt(props["I"] / props["A"])
        assert abs(props["r"] - expected_r) < 0.0001

    def test_zero_wall_raises(self):
        with pytest.raises(ValueError):
            tube_section_properties(od=1.5, wall=0.0)

    def test_wall_exceeds_radius_raises(self):
        with pytest.raises(ValueError):
            tube_section_properties(od=1.5, wall=0.8)


class TestGT1RFrameGeometry:
    """Verify geometry dimensions match hand sketch data."""

    def setup_method(self):
        self.model = build_gt1r_frame()

    def test_node_count(self):
        assert len(self.model.nodes) == 6

    def test_member_count(self):
        assert len(self.model.members) == 6

    def test_total_bar_width(self):
        n0 = self.model.nodes[0]
        n4 = self.model.nodes[4]
        assert abs((n4[0] - n0[0]) - 36.0) < 0.01

    def test_coupler_pin_offset(self):
        n5 = self.model.nodes[5]
        n2 = self.model.nodes[2]
        assert abs((n2[1] - n5[1]) - 7.25) < 0.01

    def test_coupler_pin_centered(self):
        n5 = self.model.nodes[5]
        assert abs(n5[0] - 18.0) < 0.01

    def test_segment_lengths(self):
        """Verify 6+12+12+6 bar segmentation."""
        nodes = self.model.nodes
        assert abs(nodes[1][0] - nodes[0][0] - 6.0) < 0.01
        assert abs(nodes[2][0] - nodes[1][0] - 12.0) < 0.01
        assert abs(nodes[3][0] - nodes[2][0] - 12.0) < 0.01
        assert abs(nodes[4][0] - nodes[3][0] - 6.0) < 0.01

    def test_v_strut_length(self):
        """V-strut: 12" horizontal + 7.25" vertical."""
        n1 = self.model.nodes[1]
        n5 = self.model.nodes[5]
        dx = n5[0] - n1[0]
        dy = n5[1] - n1[1]
        length = math.sqrt(dx**2 + dy**2)
        expected = math.sqrt(12.0**2 + 7.25**2)
        assert abs(length - expected) < 0.01

    def test_symmetry(self):
        """Frame is symmetric about x=18."""
        nodes = self.model.nodes
        assert abs(nodes[0][0] + nodes[4][0] - 36.0) < 0.01
        assert abs(nodes[1][0] + nodes[3][0] - 36.0) < 0.01
        assert abs(nodes[0][1] - nodes[4][1]) < 0.01

    def test_fixed_boundary_nodes(self):
        assert 0 in self.model.fixed_nodes
        assert 4 in self.model.fixed_nodes

    def test_load_node(self):
        assert self.model.load_node == 5

    def test_connection_types(self):
        conns = self.model.connections
        assert conns[0]["type"] == "C3"  # left mount
        assert conns[4]["type"] == "C3"  # right mount
        assert conns[5]["type"] == "coupler_pin"

    def test_all_members_have_section_props(self):
        for m in self.model.members:
            assert "A" in m["section"]
            assert "I" in m["section"]
            assert "S" in m["section"]
