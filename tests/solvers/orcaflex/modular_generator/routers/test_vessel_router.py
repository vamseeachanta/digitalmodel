"""Tests for VesselRouter."""

from __future__ import annotations

from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.routers.vessel_router import (
    VesselRouter,
)


class TestVesselRouter:
    """Test VesselRouter.route() method."""

    @staticmethod
    def _make_vessel_spec(**overrides):
        """Create a minimal vessel spec for testing."""
        defaults = {
            "name": "test_vessel",
            "type_name": "test_vessel_type",
            "loa": 100.0,
            "beam": 20.0,
            "depth": 10.0,
            "draft": 8.0,
            "mass": 10000.0,
            "gmt": 3.0,
            "cog": [50, 0, -4],
            "gyration_radii": SimpleNamespace(roll=8.0, pitch=25.0, yaw=25.0),
            "position": [0, 0, -8],
            "orientation": [0, 0, 0],
            "primary_motion": "None",
            "superimposed_motion": "RAOs + harmonics",
            "included_in_statics": "None",
            "raos": None,
        }
        defaults.update(overrides)
        return SimpleNamespace(**defaults)

    def test_route_returns_vessel_types_and_vessels(self):
        spec = self._make_vessel_spec()
        router = VesselRouter()
        result = router.route(spec)

        assert "vessel_types" in result
        assert "vessels" in result
        assert len(result["vessel_types"]) == 1
        assert len(result["vessels"]) == 1

    def test_vessel_type_has_correct_name(self):
        spec = self._make_vessel_spec()
        router = VesselRouter()
        result = router.route(spec)

        vt = result["vessel_types"][0]
        assert vt["name"] == "test_vessel_type"

    def test_vessel_type_has_length_and_draught(self):
        spec = self._make_vessel_spec(loa=200.0, draft=12.0)
        router = VesselRouter()
        result = router.route(spec)

        props = result["vessel_types"][0]["properties"]
        assert props["Length"] == 200.0
        assert props["Draught"] == 12.0

    def test_vessel_type_has_mass(self):
        spec = self._make_vessel_spec(mass=50000.0)
        router = VesselRouter()
        result = router.route(spec)

        props = result["vessel_types"][0]["properties"]
        assert props["Mass"] == 50000.0

    def test_vessel_type_has_cog(self):
        spec = self._make_vessel_spec(cog=[100, 0, -5])
        router = VesselRouter()
        result = router.route(spec)

        props = result["vessel_types"][0]["properties"]
        assert props["CentreOfMassX"] == 100
        assert props["CentreOfMassY"] == 0
        assert props["CentreOfMassZ"] == -5

    def test_vessel_type_has_gyration_radii(self):
        spec = self._make_vessel_spec(
            gyration_radii=SimpleNamespace(roll=10, pitch=30, yaw=30)
        )
        router = VesselRouter()
        result = router.route(spec)

        props = result["vessel_types"][0]["properties"]
        assert props["RadiiOfGyrationRx"] == 10
        assert props["RadiiOfGyrationRy"] == 30
        assert props["RadiiOfGyrationRz"] == 30

    def test_vessel_type_has_metacentric_height(self):
        spec = self._make_vessel_spec(gmt=5.0)
        router = VesselRouter()
        result = router.route(spec)

        props = result["vessel_types"][0]["properties"]
        assert props["TransverseMetacentricHeight"] == 5.0

    def test_vessel_has_correct_position(self):
        spec = self._make_vessel_spec(position=[100, 50, -12])
        router = VesselRouter()
        result = router.route(spec)

        props = result["vessels"][0]["properties"]
        assert props["InitialX"] == 100
        assert props["InitialY"] == 50
        assert props["InitialZ"] == -12

    def test_vessel_references_vessel_type(self):
        spec = self._make_vessel_spec()
        router = VesselRouter()
        result = router.route(spec)

        v_props = result["vessels"][0]["properties"]
        vt_name = result["vessel_types"][0]["name"]
        assert v_props["VesselType"] == vt_name

    def test_vessel_has_motion_settings(self):
        spec = self._make_vessel_spec(
            primary_motion="Calculated (6 DOF)",
            superimposed_motion="None",
        )
        router = VesselRouter()
        result = router.route(spec)

        props = result["vessels"][0]["properties"]
        assert props["PrimaryMotion"] == "Calculated (6 DOF)"
        assert props["SuperimposedMotion"] == "None"


class TestVesselRouterFromHullCatalog:
    """Test VesselRouter.from_hull_catalog() class method."""

    def test_from_hull_catalog_returns_types_and_vessels(self):
        entry = {
            "hull_id": "test_barge",
            "name": "barge_100m",
            "length_m": 100,
            "beam_m": 20,
            "draft_m": 8,
        }
        result = VesselRouter.from_hull_catalog(entry)

        assert "vessel_types" in result
        assert "vessels" in result

    def test_from_hull_catalog_uses_name(self):
        entry = {"hull_id": "test", "name": "my_vessel", "length_m": 50, "draft_m": 5}
        result = VesselRouter.from_hull_catalog(entry)

        assert result["vessels"][0]["name"] == "my_vessel"
        assert result["vessel_types"][0]["name"] == "my_vessel_type"

    def test_from_hull_catalog_sets_position(self):
        entry = {"hull_id": "t", "name": "v", "length_m": 50, "draft_m": 5}
        result = VesselRouter.from_hull_catalog(entry, position=[100, 50, -5])

        props = result["vessels"][0]["properties"]
        assert props["InitialX"] == 100
        assert props["InitialY"] == 50
        assert props["InitialZ"] == -5

    def test_from_hull_catalog_sets_heading(self):
        entry = {"hull_id": "t", "name": "v", "length_m": 50, "draft_m": 5}
        result = VesselRouter.from_hull_catalog(entry, heading=45.0)

        props = result["vessels"][0]["properties"]
        assert props["InitialHeading"] == 45.0

    def test_from_hull_catalog_with_rao_file(self):
        entry = {"hull_id": "t", "name": "v", "length_m": 50, "draft_m": 5}
        result = VesselRouter.from_hull_catalog(
            entry, rao_file=Path("data/raos/test.owr")
        )

        vt_props = result["vessel_types"][0]["properties"]
        assert "DisplacementRAOCalculationFile" in vt_props

    def test_from_hull_catalog_length_and_draught(self):
        entry = {"hull_id": "t", "name": "v", "length_m": 270, "draft_m": 15}
        result = VesselRouter.from_hull_catalog(entry)

        props = result["vessel_types"][0]["properties"]
        assert props["Length"] == 270
        assert props["Draught"] == 15
