"""Tests for example data center topology configurations.

AC4: Example configurations for Tier II, III, IV topologies — ≥3 tests.
AC5: Module importable from digitalmodel.power.datacenter.
"""

from __future__ import annotations

import pytest


class TestImportability:
    """AC5: Module importable from digitalmodel.power.datacenter."""

    def test_import_from_datacenter(self):
        from digitalmodel.power.datacenter import (
            RedundancyScheme,
            UPSModule,
            STSModule,
            PowerComponent,
            DataCenterPowerPath,
            TopologyValidator,
        )
        assert RedundancyScheme is not None
        assert UPSModule is not None
        assert DataCenterPowerPath is not None
        assert TopologyValidator is not None


class TestTierIIExample:
    """Tier II: single path with redundant UPS (N+1)."""

    def test_tier_ii_topology(self):
        from digitalmodel.power.datacenter import (
            RedundancyScheme,
            UPSModule,
            PowerComponent,
            DataCenterPowerPath,
            TopologyValidator,
        )

        path = DataCenterPowerPath(name="Tier-II-Path")
        path.add_component(
            PowerComponent(component_type="utility", rated_kva=10000.0, name="Utility-Feed")
        )
        path.add_component(
            PowerComponent(component_type="mv_switchgear", rated_kva=5000.0, efficiency=0.995, name="MV-SWG")
        )
        path.add_component(
            PowerComponent(component_type="transformer", rated_kva=2500.0, efficiency=0.98, name="TX-1")
        )
        path.add_component(UPSModule(rated_kva=750.0, battery_runtime_min=10.0, name="UPS-1"))
        path.add_component(
            PowerComponent(component_type="pdu", rated_kva=200.0, efficiency=0.99, name="PDU-1")
        )

        validator = TopologyValidator(
            paths=[path], redundancy_scheme=RedundancyScheme.N_PLUS_1
        )
        assert validator.classify_tier() == 2
        assert path.has_ups is True
        assert path.cascaded_efficiency == pytest.approx(0.995 * 0.98 * 0.95 * 0.99, rel=1e-3)


class TestTierIIIExample:
    """Tier III: dual path with STS, concurrently maintainable."""

    def test_tier_iii_topology(self):
        from digitalmodel.power.datacenter import (
            RedundancyScheme,
            UPSModule,
            STSModule,
            PowerComponent,
            DataCenterPowerPath,
            TopologyValidator,
        )

        paths = []
        for label in ("A", "B"):
            path = DataCenterPowerPath(name=f"Path-{label}")
            path.add_component(
                PowerComponent(component_type="utility", rated_kva=10000.0, name=f"UTL-{label}")
            )
            path.add_component(
                PowerComponent(component_type="transformer", rated_kva=2500.0, efficiency=0.98, name=f"TX-{label}")
            )
            path.add_component(UPSModule(rated_kva=750.0, name=f"UPS-{label}"))
            paths.append(path)

        # STS connects paths for automatic failover
        paths[0].add_component(STSModule(rated_kva=750.0, name="STS-1"))

        validator = TopologyValidator(
            paths=paths, redundancy_scheme=RedundancyScheme.TWO_N
        )
        assert validator.classify_tier() == 3
        assert validator.path_count == 2
        assert len(validator.find_spofs()) == 0


class TestTierIVExample:
    """Tier IV: 2N+1 fully fault-tolerant with independent utilities."""

    def test_tier_iv_topology(self):
        from digitalmodel.power.datacenter import (
            RedundancyScheme,
            UPSModule,
            STSModule,
            PowerComponent,
            DataCenterPowerPath,
            TopologyValidator,
        )

        paths = []
        for label in ("A", "B"):
            path = DataCenterPowerPath(name=f"Path-{label}")
            path.add_component(
                PowerComponent(component_type="utility", rated_kva=10000.0, name=f"UTL-{label}")
            )
            path.add_component(
                PowerComponent(component_type="transformer", rated_kva=2500.0, efficiency=0.98, name=f"TX-{label}")
            )
            path.add_component(UPSModule(rated_kva=750.0, name=f"UPS-{label}"))
            path.add_component(STSModule(rated_kva=750.0, name=f"STS-{label}"))
            paths.append(path)

        # Standby generator path
        gen_path = DataCenterPowerPath(name="Path-Gen")
        gen_path.add_component(
            PowerComponent(component_type="generator", rated_kva=10000.0, name="GEN-1")
        )
        gen_path.add_component(UPSModule(rated_kva=750.0, name="UPS-Gen"))
        gen_path.add_component(STSModule(rated_kva=750.0, name="STS-Gen"))
        paths.append(gen_path)

        validator = TopologyValidator(
            paths=paths, redundancy_scheme=RedundancyScheme.TWO_N_PLUS_1
        )
        assert validator.classify_tier() == 4
        assert validator.path_count == 3
        assert len(validator.find_spofs()) == 0
