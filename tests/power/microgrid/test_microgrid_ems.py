"""Tests for MicrogridEMS — merit-order dispatch, mode transitions, black start."""

from __future__ import annotations

import pytest

from digitalmodel.power.microgrid.bess_controller import BESSController
from digitalmodel.power.microgrid.island_detector import IslandDetector
from digitalmodel.power.microgrid.microgrid_ems import MicrogridEMS
from digitalmodel.power.microgrid.models import (
    DERAsset,
    DERType,
    MicrogridMode,
)


def _make_ems() -> MicrogridEMS:
    """Helper: build an EMS with PV, BESS, and GENSET."""
    assets = [
        DERAsset("pv_01", DERType.PV, 500.0, marginal_cost=0.0, dispatch_priority=1),
        DERAsset("bess_01", DERType.BESS, 200.0, marginal_cost=5.0, dispatch_priority=2),
        DERAsset("genset_01", DERType.GENSET, 300.0, marginal_cost=50.0, dispatch_priority=3),
    ]
    bess = BESSController(capacity_kwh=400.0, max_power_kw=200.0, soc=0.8)
    detector = IslandDetector(rocof_threshold=1.0, vector_shift_threshold=10.0)
    return MicrogridEMS(assets=assets, bess=bess, island_detector=detector)


class TestMeritOrderDispatch:
    def test_dispatches_cheapest_first(self):
        """PV (priority 1) dispatched before GENSET (priority 3)."""
        ems = _make_ems()
        dispatch = ems.merit_order_dispatch(load_kw=400.0)
        # PV has priority 1, capacity 500 → covers 400 kW entirely
        assert dispatch["pv_01"] == pytest.approx(400.0)
        assert dispatch.get("genset_01", 0.0) == pytest.approx(0.0)

    def test_dispatch_spills_to_next_der(self):
        """Load exceeding PV capacity spills to BESS then GENSET."""
        ems = _make_ems()
        dispatch = ems.merit_order_dispatch(load_kw=800.0)
        assert dispatch["pv_01"] == pytest.approx(500.0)
        assert dispatch["bess_01"] == pytest.approx(200.0)
        assert dispatch["genset_01"] == pytest.approx(100.0)

    def test_unserved_load_when_demand_exceeds_capacity(self):
        """Total capacity = 500+200+300 = 1000 kW; demand 1200 → 200 unserved."""
        ems = _make_ems()
        dispatch = ems.merit_order_dispatch(load_kw=1200.0)
        total_dispatched = sum(dispatch.values())
        assert total_dispatched == pytest.approx(1000.0)

    def test_unavailable_asset_skipped(self):
        """Unavailable asset should not be dispatched."""
        ems = _make_ems()
        ems.assets[0].available = False  # PV unavailable
        dispatch = ems.merit_order_dispatch(load_kw=300.0)
        assert dispatch.get("pv_01", 0.0) == pytest.approx(0.0)
        assert dispatch["bess_01"] == pytest.approx(200.0)
        assert dispatch["genset_01"] == pytest.approx(100.0)

    def test_bess_constrained_by_soc(self):
        """BESS with low SOC should dispatch less than rated capacity."""
        ems = _make_ems()
        # Set SOC to exactly soc_min → 0 discharge available
        ems.bess.soc = 0.10
        dispatch = ems.merit_order_dispatch(load_kw=600.0)
        assert dispatch.get("bess_01", 0.0) == pytest.approx(0.0)
        # PV 500 + GENSET 100
        assert dispatch["pv_01"] == pytest.approx(500.0)
        assert dispatch["genset_01"] == pytest.approx(100.0)


class TestModeTransitions:
    def test_transition_to_island_on_detection(self):
        """Island detection should transition mode from grid to islanded."""
        ems = _make_ems()
        assert ems.mode == MicrogridMode.GRID_CONNECTED
        # Trigger island detection via high ROCOF
        timestamps = [0.0, 0.25, 0.5, 0.75, 1.0]
        frequencies = [60.0, 59.5, 59.0, 58.5, 58.0]  # -2 Hz/s
        state = ems.update(
            load_kw=400.0,
            timestamps=timestamps,
            frequencies=frequencies,
            voltage_angle_prev=0.0,
            voltage_angle_curr=0.0,
        )
        assert state.mode == MicrogridMode.ISLANDED

    def test_stays_grid_connected_normal(self):
        """Normal conditions should keep grid-connected mode."""
        ems = _make_ems()
        timestamps = [0.0, 0.5, 1.0]
        frequencies = [60.0, 60.0, 60.0]
        state = ems.update(
            load_kw=400.0,
            timestamps=timestamps,
            frequencies=frequencies,
            voltage_angle_prev=0.0,
            voltage_angle_curr=2.0,
        )
        assert state.mode == MicrogridMode.GRID_CONNECTED


class TestBlackStart:
    def test_black_start_requires_minimum_soc(self):
        """Black start with SOC < 0.30 should raise ValueError."""
        ems = _make_ems()
        ems.bess.soc = 0.20
        with pytest.raises(ValueError, match="SOC"):
            ems.black_start()

    def test_black_start_succeeds_with_sufficient_soc(self):
        """Black start with SOC >= 0.30 should transition to BLACK_START."""
        ems = _make_ems()
        ems.bess.soc = 0.80
        ems.black_start()
        assert ems.mode == MicrogridMode.BLACK_START

    def test_black_start_to_island_transition(self):
        """After black start, reconnect transitions to islanded mode."""
        ems = _make_ems()
        ems.black_start()
        assert ems.mode == MicrogridMode.BLACK_START
        ems.reconnect()
        assert ems.mode == MicrogridMode.ISLANDED
