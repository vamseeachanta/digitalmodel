"""Tests for microgrid domain models — enums + dataclasses."""

from __future__ import annotations

import pytest

from digitalmodel.power.microgrid.models import (
    DERAsset,
    DERType,
    MicrogridMode,
    MicrogridState,
)


class TestMicrogridMode:
    def test_grid_connected_is_default(self):
        """GRID_CONNECTED should be a valid mode value."""
        assert MicrogridMode.GRID_CONNECTED.value == "grid_connected"

    def test_all_modes_exist(self):
        """Enum must include grid_connected, islanded, and black_start."""
        modes = {m.value for m in MicrogridMode}
        assert modes == {"grid_connected", "islanded", "black_start"}


class TestDERType:
    def test_all_der_types_exist(self):
        """DER types: PV, WIND, BESS, GENSET, FUEL_CELL."""
        types = {t.value for t in DERType}
        assert types == {"pv", "wind", "bess", "genset", "fuel_cell"}


class TestDERAsset:
    def test_valid_der_asset_creation(self):
        """A DER asset with valid parameters should be created."""
        asset = DERAsset(
            asset_id="pv_01",
            der_type=DERType.PV,
            capacity_kw=500.0,
            marginal_cost=0.0,
            dispatch_priority=1,
        )
        assert asset.asset_id == "pv_01"
        assert asset.available is True  # default

    def test_negative_capacity_raises(self):
        """Capacity must be positive."""
        with pytest.raises(ValueError, match="capacity_kw"):
            DERAsset(
                asset_id="bad",
                der_type=DERType.PV,
                capacity_kw=-10.0,
                marginal_cost=0.0,
                dispatch_priority=1,
            )

    def test_negative_dispatch_priority_raises(self):
        """Dispatch priority must be >= 1."""
        with pytest.raises(ValueError, match="dispatch_priority"):
            DERAsset(
                asset_id="bad",
                der_type=DERType.GENSET,
                capacity_kw=100.0,
                marginal_cost=50.0,
                dispatch_priority=0,
            )


class TestMicrogridState:
    def test_valid_state_creation(self):
        """MicrogridState with valid fields should be created."""
        state = MicrogridState(
            mode=MicrogridMode.GRID_CONNECTED,
            total_load_kw=1000.0,
            total_generation_kw=800.0,
            grid_import_kw=200.0,
            frequency_hz=60.0,
        )
        assert state.net_load_kw == pytest.approx(200.0)

    def test_negative_frequency_raises(self):
        """Frequency must be positive."""
        with pytest.raises(ValueError, match="frequency_hz"):
            MicrogridState(
                mode=MicrogridMode.GRID_CONNECTED,
                total_load_kw=100.0,
                total_generation_kw=100.0,
                grid_import_kw=0.0,
                frequency_hz=-1.0,
            )
