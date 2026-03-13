"""Topology validation — SPOF detection and Uptime Institute tier classification."""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass, field

from digitalmodel.power.datacenter.models import (
    PowerComponent,
    RedundancyScheme,
    UPSModule,
    STSModule,
)
from digitalmodel.power.datacenter.topology import DataCenterPowerPath


@dataclass
class TopologyValidator:
    """Validate data center power topology for redundancy and fault tolerance.

    Attributes
    ----------
    paths : list[DataCenterPowerPath]
        All independent power distribution paths.
    redundancy_scheme : RedundancyScheme | None
        Declared redundancy scheme (used for Tier IV classification).
    """

    paths: list[DataCenterPowerPath]
    redundancy_scheme: RedundancyScheme | None = None

    @property
    def path_count(self) -> int:
        """Number of power distribution paths."""
        return len(self.paths)

    def find_spofs(self) -> list[PowerComponent]:
        """Identify single points of failure across all paths.

        A component is a SPOF if:
        - There is only one path (every component is a SPOF), or
        - The same component instance appears in multiple paths (shared resource).

        Returns
        -------
        list[PowerComponent]
            Components that are single points of failure.
        """
        if self.path_count <= 1:
            # Single path — every component is a SPOF
            if self.paths:
                return list(self.paths[0].components)
            return []

        # Multi-path: find shared components (same object identity)
        component_ids: Counter[int] = Counter()
        id_to_component: dict[int, PowerComponent] = {}

        for path in self.paths:
            for comp in path.components:
                component_ids[id(comp)] += 1
                id_to_component[id(comp)] = comp

        # A component shared across all paths is a SPOF
        return [
            id_to_component[cid]
            for cid, count in component_ids.items()
            if count >= self.path_count
        ]

    def classify_tier(self) -> int:
        """Classify topology per Uptime Institute tier standards.

        Tier I   — Single path, no redundancy
        Tier II  — Single path with redundant components (UPS)
        Tier III — Multiple paths, concurrently maintainable (STS present)
        Tier IV  — Fault tolerant, 2N+1 with STS on all paths

        Returns
        -------
        int
            Tier level (1–4).
        """
        any_ups = any(p.has_ups for p in self.paths)
        any_sts = any(p.has_sts for p in self.paths)
        all_sts = all(p.has_sts for p in self.paths)
        no_spofs = len(self.find_spofs()) == 0

        # Tier IV: 2N+1, all paths have STS, no SPOFs
        if (
            self.path_count >= 3
            and all_sts
            and no_spofs
            and self.redundancy_scheme == RedundancyScheme.TWO_N_PLUS_1
        ):
            return 4

        # Tier III: multiple paths + STS + no SPOFs
        if self.path_count >= 2 and any_sts and no_spofs:
            return 3

        # Tier II: UPS present (redundant component)
        if any_ups:
            return 2

        # Tier I: basic
        return 1
