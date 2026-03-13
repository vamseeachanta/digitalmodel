"""Tests for data center power topology data models.

AC1: RedundancyScheme + UPSModule + STSModule data models — ≥8 tests.
"""

from __future__ import annotations

import pytest

from digitalmodel.power.datacenter.models import (
    RedundancyScheme,
    UPSModule,
    STSModule,
    PowerComponent,
)


class TestRedundancyScheme:
    """Test RedundancyScheme enum values and membership."""

    def test_enum_values(self):
        assert RedundancyScheme.N.value == "n"
        assert RedundancyScheme.N_PLUS_1.value == "n+1"
        assert RedundancyScheme.TWO_N.value == "2n"
        assert RedundancyScheme.TWO_N_PLUS_1.value == "2n+1"

    def test_enum_count(self):
        assert len(RedundancyScheme) == 4


class TestUPSModule:
    """Test UPS module data model."""

    def test_construction_defaults(self):
        ups = UPSModule(rated_kva=500.0)
        assert ups.rated_kva == 500.0
        assert ups.battery_runtime_min == 10.0
        assert ups.efficiency == 0.95
        assert ups.bypass_enabled is True

    def test_construction_custom(self):
        ups = UPSModule(
            rated_kva=1000.0,
            battery_runtime_min=15.0,
            efficiency=0.97,
            bypass_enabled=False,
            name="UPS-A1",
        )
        assert ups.rated_kva == 1000.0
        assert ups.battery_runtime_min == 15.0
        assert ups.efficiency == 0.97
        assert ups.bypass_enabled is False
        assert ups.name == "UPS-A1"

    def test_output_kva(self):
        ups = UPSModule(rated_kva=500.0, efficiency=0.95)
        assert ups.output_kva == pytest.approx(475.0)

    def test_invalid_efficiency_raises(self):
        with pytest.raises(ValueError, match="efficiency"):
            UPSModule(rated_kva=500.0, efficiency=1.5)

    def test_invalid_rated_kva_raises(self):
        with pytest.raises(ValueError, match="rated_kva"):
            UPSModule(rated_kva=-100.0)


class TestSTSModule:
    """Test STS (Static Transfer Switch) module data model."""

    def test_construction_defaults(self):
        sts = STSModule(rated_kva=500.0)
        assert sts.rated_kva == 500.0
        assert sts.transfer_time_ms == 4.0
        assert sts.preferred_source == "A"
        assert sts.alternate_source == "B"
        assert sts.retransfer_enabled is True

    def test_construction_custom(self):
        sts = STSModule(
            rated_kva=750.0,
            transfer_time_ms=8.0,
            preferred_source="Bus-1",
            alternate_source="Bus-2",
            retransfer_enabled=False,
            name="STS-01",
        )
        assert sts.transfer_time_ms == 8.0
        assert sts.preferred_source == "Bus-1"
        assert sts.name == "STS-01"

    def test_invalid_transfer_time_raises(self):
        with pytest.raises(ValueError, match="transfer_time_ms"):
            STSModule(rated_kva=500.0, transfer_time_ms=-1.0)


class TestPowerComponent:
    """Test generic power component base model."""

    def test_construction(self):
        comp = PowerComponent(
            component_type="transformer",
            rated_kva=2000.0,
            name="TX-01",
        )
        assert comp.component_type == "transformer"
        assert comp.rated_kva == 2000.0
        assert comp.efficiency == 1.0
