"""Tests for TopologyValidator — SPOF detection and tier classification.

AC3: TopologyValidator with SPOF detection and tier classification — ≥10 tests.
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
from digitalmodel.power.datacenter.validator import TopologyValidator


def _make_single_path(with_ups: bool = True, with_sts: bool = False) -> DataCenterPowerPath:
    """Helper: single power path utility → transformer → (UPS) → (STS) → PDU."""
    path = DataCenterPowerPath(name="Path-A")
    path.add_component(PowerComponent(component_type="utility", rated_kva=5000.0, name="UTL-1"))
    path.add_component(
        PowerComponent(component_type="transformer", rated_kva=2000.0, efficiency=0.98, name="TX-1")
    )
    if with_ups:
        path.add_component(UPSModule(rated_kva=500.0, name="UPS-1"))
    if with_sts:
        path.add_component(STSModule(rated_kva=500.0, name="STS-1"))
    path.add_component(PowerComponent(component_type="pdu", rated_kva=200.0, name="PDU-1"))
    return path


def _make_dual_paths() -> list[DataCenterPowerPath]:
    """Helper: two independent power paths (2N topology)."""
    paths = []
    for label in ("A", "B"):
        path = DataCenterPowerPath(name=f"Path-{label}")
        path.add_component(
            PowerComponent(component_type="utility", rated_kva=5000.0, name=f"UTL-{label}")
        )
        path.add_component(
            PowerComponent(component_type="transformer", rated_kva=2000.0, efficiency=0.98, name=f"TX-{label}")
        )
        path.add_component(UPSModule(rated_kva=500.0, name=f"UPS-{label}"))
        path.add_component(
            PowerComponent(component_type="pdu", rated_kva=200.0, name=f"PDU-{label}")
        )
        paths.append(path)
    return paths


class TestSPOFDetection:
    """Test single-point-of-failure detection."""

    def test_single_path_has_spof(self):
        validator = TopologyValidator(paths=[_make_single_path()])
        spofs = validator.find_spofs()
        assert len(spofs) > 0

    def test_dual_path_no_spof(self):
        validator = TopologyValidator(paths=_make_dual_paths())
        spofs = validator.find_spofs()
        assert len(spofs) == 0

    def test_spof_identifies_component(self):
        validator = TopologyValidator(paths=[_make_single_path()])
        spofs = validator.find_spofs()
        component_types = {s.component_type for s in spofs}
        assert "utility" in component_types

    def test_shared_utility_is_spof(self):
        """Two paths sharing the same utility source = SPOF."""
        shared_util = PowerComponent(
            component_type="utility", rated_kva=5000.0, name="UTL-SHARED"
        )
        path_a = DataCenterPowerPath(name="Path-A")
        path_a.add_component(shared_util)
        path_a.add_component(UPSModule(rated_kva=500.0, name="UPS-A"))

        path_b = DataCenterPowerPath(name="Path-B")
        path_b.add_component(shared_util)
        path_b.add_component(UPSModule(rated_kva=500.0, name="UPS-B"))

        validator = TopologyValidator(paths=[path_a, path_b])
        spofs = validator.find_spofs()
        spof_names = {s.name for s in spofs}
        assert "UTL-SHARED" in spof_names


class TestTierClassification:
    """Test Uptime Institute tier classification (Tier I–IV)."""

    def test_tier_i_no_redundancy(self):
        """Single path, no UPS = Tier I."""
        path = _make_single_path(with_ups=False)
        validator = TopologyValidator(paths=[path])
        assert validator.classify_tier() == 1

    def test_tier_ii_single_path_with_ups(self):
        """Single path with UPS = Tier II."""
        path = _make_single_path(with_ups=True)
        validator = TopologyValidator(paths=[path])
        assert validator.classify_tier() == 2

    def test_tier_iii_dual_path_with_sts(self):
        """Dual path + STS for concurrently maintainable = Tier III."""
        paths = _make_dual_paths()
        # Add STS to connect paths
        paths[0].add_component(STSModule(rated_kva=500.0, name="STS-1"))
        validator = TopologyValidator(paths=paths)
        assert validator.classify_tier() == 3

    def test_tier_iv_fully_fault_tolerant(self):
        """2N+1 with STS and independent utilities = Tier IV."""
        paths = _make_dual_paths()
        # Add STS to each path
        for p in paths:
            p.add_component(STSModule(rated_kva=500.0, name=f"STS-{p.name}"))
        # Add a third standby path
        standby = DataCenterPowerPath(name="Path-Standby")
        standby.add_component(
            PowerComponent(component_type="generator", rated_kva=5000.0, name="GEN-1")
        )
        standby.add_component(UPSModule(rated_kva=500.0, name="UPS-Standby"))
        standby.add_component(STSModule(rated_kva=500.0, name="STS-Standby"))
        paths.append(standby)

        validator = TopologyValidator(
            paths=paths, redundancy_scheme=RedundancyScheme.TWO_N_PLUS_1
        )
        assert validator.classify_tier() == 4

    def test_redundancy_scheme_validation(self):
        validator = TopologyValidator(
            paths=_make_dual_paths(),
            redundancy_scheme=RedundancyScheme.TWO_N,
        )
        assert validator.redundancy_scheme == RedundancyScheme.TWO_N

    def test_path_count(self):
        validator = TopologyValidator(paths=_make_dual_paths())
        assert validator.path_count == 2
