"""Tests for orcawave.vessel_database module."""

import numpy as np
import pytest

from digitalmodel.orcawave.vessel_database import (
    VesselParameters,
    generate_parametric_hull,
    get_representative_raos,
    get_vessel,
    get_vessels_by_type,
    list_vessels,
)


class TestVesselDatabase:
    """Test vessel parameter retrieval."""

    def test_list_vessels_nonempty(self):
        vessels = list_vessels()
        assert len(vessels) >= 5

    def test_get_fpso(self):
        v = get_vessel("FPSO_Generic_250m")
        assert v.vessel_type == "FPSO"
        assert v.length_m == 250.0
        assert v.displacement_te > 0
        assert v.gm_t_m > 0

    def test_get_semisub(self):
        v = get_vessel("SemiSub_Generic")
        assert v.vessel_type == "SemiSubmersible"
        assert v.length_m > 0

    def test_get_nonexistent_raises(self):
        with pytest.raises(KeyError, match="not found"):
            get_vessel("NonExistentVessel")

    def test_get_by_type_fpso(self):
        vessels = get_vessels_by_type("FPSO")
        assert len(vessels) >= 1
        assert all(v.vessel_type == "FPSO" for v in vessels)

    def test_get_by_type_empty(self):
        vessels = get_vessels_by_type("Submarine")
        assert len(vessels) == 0


class TestRepresentativeRAOs:
    """Test representative RAO generation."""

    def test_fpso_raos(self):
        raos = get_representative_raos("FPSO")
        assert raos.vessel_type == "FPSO"
        assert len(raos.periods) > 5
        assert len(raos.headings_deg) == 5
        assert len(raos.heave_rao) == 5
        assert len(raos.roll_rao) == 5

    def test_semisub_raos(self):
        raos = get_representative_raos("SemiSubmersible")
        assert raos.vessel_type == "SemiSubmersible"
        # Semi-sub heave RAO should be small (small waterplane area)
        heave_beam = np.array(raos.heave_rao[2])  # heading 90
        # All positive
        assert np.all(heave_beam >= 0)

    def test_roll_rao_heading_dependency(self):
        raos = get_representative_raos("FPSO")
        roll_head = np.max(raos.roll_rao[0])     # 0 deg
        roll_beam = np.max(raos.roll_rao[2])      # 90 deg
        # Roll should be much larger in beam seas
        assert roll_beam > roll_head

    def test_invalid_type_raises(self):
        with pytest.raises(ValueError, match="No vessels"):
            get_representative_raos("Submarine")


class TestParametricHull:
    """Test parametric hull generation."""

    def test_fpso_hull(self):
        hull = generate_parametric_hull("FPSO", 250.0, 46.0, 16.0)
        assert hull.vessel_type == "FPSO"
        assert hull.length_m == 250.0
        assert hull.bow_shape == "pointed"
        assert len(hull.waterline_offsets) == 20

    def test_barge_hull(self):
        hull = generate_parametric_hull("Barge", 100.0, 30.0, 5.0)
        assert hull.bow_shape == "blunt"
        # Barge should have near-constant half-breadth in middle
        offsets = np.array(hull.waterline_offsets)
        mid_section = offsets[len(offsets) // 2, 1]
        np.testing.assert_allclose(mid_section, 15.0, rtol=0.01)

    def test_semisub_hull(self):
        hull = generate_parametric_hull("SemiSubmersible", 110.0, 80.0, 22.0)
        assert hull.vessel_type == "SemiSubmersible"
        # Pontoon width should be narrower than full beam
        offsets = np.array(hull.waterline_offsets)
        max_half_breadth = offsets[:, 1].max()
        assert max_half_breadth < 80.0 / 2.0

    def test_custom_sections(self):
        hull = generate_parametric_hull("FPSO", 200.0, 40.0, 14.0, n_sections=50)
        assert hull.n_waterline_sections == 50
        assert len(hull.waterline_offsets) == 50
