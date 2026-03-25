"""Integration tests for property routing through the full pipeline."""

from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.schema.root import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.mooring import (
    ChainGrade,
    MooringEndpoint,
    MooringLine,
    MooringSegment,
    MooringSegmentType,
    MooringSystem,
)
from digitalmodel.solvers.orcaflex.modular_generator.routers.mooring_router import (
    MooringRouter,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema.generic import GenericModel


class TestMooringRouterIntegration:
    """Test mooring router output can be used as GenericModel input."""

    @staticmethod
    def _make_mooring_system():
        """Create a minimal mooring system for testing."""
        return MooringSystem(
            lines=[
                MooringLine(
                    name="ML1",
                    segments=[
                        MooringSegment(
                            type=MooringSegmentType.CHAIN,
                            diameter=0.076,
                            length=200,
                            grade=ChainGrade.R3,
                            segment_length=10,
                        ),
                    ],
                    anchor=MooringEndpoint(
                        type="anchor",
                        position=[800, 0, -200],
                    ),
                    fairlead=MooringEndpoint(
                        type="fairlead",
                        position=[10, 0, 0],
                        vessel="Vessel1",
                    ),
                ),
            ]
        )

    def test_mooring_router_output_has_line_types(self):
        ms = self._make_mooring_system()
        result = MooringRouter().route(ms)
        assert len(result["line_types"]) >= 1

    def test_mooring_router_output_has_lines(self):
        ms = self._make_mooring_system()
        result = MooringRouter().route(ms)
        assert len(result["lines"]) == 1
        assert result["lines"][0]["name"] == "ML1"

    def test_mooring_router_linetype_has_required_properties(self):
        ms = self._make_mooring_system()
        result = MooringRouter().route(ms)
        lt_props = result["line_types"][0]["properties"]
        assert "Category" in lt_props
        assert "OD" in lt_props
        assert "MassPerUnitLength" in lt_props
        assert "EA" in lt_props

    def test_mooring_router_output_can_create_generic_model(self):
        """Verify router output is compatible with GenericModel schema."""
        ms = self._make_mooring_system()
        result = MooringRouter().route(ms)

        # Should not raise — proves compatibility
        generic = GenericModel(**result)
        assert len(generic.line_types) >= 1
        assert len(generic.lines) == 1

    def test_mooring_with_pretension_creates_winch(self):
        ms = MooringSystem(
            lines=[
                MooringLine(
                    name="ML1",
                    segments=[
                        MooringSegment(
                            type=MooringSegmentType.CHAIN,
                            diameter=0.076,
                            length=200,
                            grade=ChainGrade.R3,
                            segment_length=10,
                        ),
                    ],
                    anchor=MooringEndpoint(
                        type="anchor",
                        position=[800, 0, -200],
                    ),
                    fairlead=MooringEndpoint(
                        type="fairlead",
                        position=[10, 0, 0],
                        vessel="Vessel1",
                    ),
                    pretension=500.0,
                ),
            ]
        )
        result = MooringRouter().route(ms)
        assert "winches" in result
        assert len(result["winches"]) == 1
        assert result["winches"][0]["properties"]["Tension"] == 500.0

    def test_chain_deduplication(self):
        """Same chain type used in two segments should produce one LineType."""
        ms = MooringSystem(
            lines=[
                MooringLine(
                    name="ML1",
                    segments=[
                        MooringSegment(
                            type=MooringSegmentType.CHAIN,
                            diameter=0.076,
                            length=200,
                            grade=ChainGrade.R3,
                            segment_length=10,
                        ),
                        MooringSegment(
                            type=MooringSegmentType.CHAIN,
                            diameter=0.076,
                            length=50,
                            grade=ChainGrade.R3,
                            segment_length=5,
                        ),
                    ],
                    anchor=MooringEndpoint(
                        type="anchor",
                        position=[800, 0, -200],
                    ),
                    fairlead=MooringEndpoint(
                        type="fairlead",
                        position=[10, 0, 0],
                        vessel="Vessel1",
                    ),
                ),
            ]
        )
        result = MooringRouter().route(ms)
        # Two segments with same type/diameter/grade → one LineType
        chain_types = [lt for lt in result["line_types"] if "chain" in lt["name"]]
        assert len(chain_types) == 1
