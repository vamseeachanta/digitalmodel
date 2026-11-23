"""Tests for DataCenterPowerPath cascaded power chain modeling.

AC2: DataCenterPowerPath with cascaded power chain modeling — ≥8 tests.
"""

from __future__ import annotations

import pytest

from digitalmodel.power.datacenter.models import (
    RedundancyScheme,
    UPSModule,
    STSModule,
    PowerComponent,
)
from digitalmodel.power.datacenter.topology import DataCenterPowerPath


class TestDataCenterPowerPath:
    """Test cascaded power chain construction and analysis."""

    def test_empty_path(self):
        path = DataCenterPowerPath(name="Path-A")
        assert len(path.components) == 0

    def test_add_component(self):
        path = DataCenterPowerPath(name="Path-A")
        ups = UPSModule(rated_kva=500.0)
        path.add_component(ups)
        assert len(path.components) == 1

    def test_cascaded_efficiency_single(self):
        path = DataCenterPowerPath(name="Path-A")
        path.add_component(UPSModule(rated_kva=500.0, efficiency=0.95))
        assert path.cascaded_efficiency == pytest.approx(0.95)

    def test_cascaded_efficiency_chain(self):
        """Utility → Transformer (0.98) → UPS (0.95) → PDU (0.99)."""
        path = DataCenterPowerPath(name="Path-A")
        path.add_component(
            PowerComponent(component_type="transformer", rated_kva=2000.0, efficiency=0.98)
        )
        path.add_component(UPSModule(rated_kva=500.0, efficiency=0.95))
        path.add_component(
            PowerComponent(component_type="pdu", rated_kva=200.0, efficiency=0.99)
        )
        expected = 0.98 * 0.95 * 0.99
        assert path.cascaded_efficiency == pytest.approx(expected)

    def test_bottleneck_capacity(self):
        """Bottleneck is the minimum rated_kva in the chain."""
        path = DataCenterPowerPath(name="Path-A")
        path.add_component(
            PowerComponent(component_type="transformer", rated_kva=2000.0)
        )
        path.add_component(UPSModule(rated_kva=500.0))
        path.add_component(
            PowerComponent(component_type="pdu", rated_kva=200.0)
        )
        assert path.bottleneck_kva == pytest.approx(200.0)

    def test_effective_capacity(self):
        """Effective = bottleneck × cascaded efficiency."""
        path = DataCenterPowerPath(name="Path-A")
        path.add_component(
            PowerComponent(component_type="transformer", rated_kva=2000.0, efficiency=0.98)
        )
        path.add_component(UPSModule(rated_kva=500.0, efficiency=0.95))
        expected = 500.0 * (0.98 * 0.95)
        assert path.effective_capacity_kva == pytest.approx(expected)

    def test_component_order_preserved(self):
        path = DataCenterPowerPath(name="Path-A")
        path.add_component(PowerComponent(component_type="utility", rated_kva=5000.0, name="UTL"))
        path.add_component(PowerComponent(component_type="switchgear", rated_kva=3000.0, name="SWG"))
        path.add_component(UPSModule(rated_kva=500.0, name="UPS-1"))
        names = [c.name for c in path.components]
        assert names == ["UTL", "SWG", "UPS-1"]

    def test_has_ups(self):
        path = DataCenterPowerPath(name="Path-A")
        assert path.has_ups is False
        path.add_component(UPSModule(rated_kva=500.0))
        assert path.has_ups is True

    def test_has_sts(self):
        path = DataCenterPowerPath(name="Path-A")
        assert path.has_sts is False
        path.add_component(STSModule(rated_kva=500.0))
        assert path.has_sts is True
