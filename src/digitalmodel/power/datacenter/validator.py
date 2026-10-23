"""Topology checks — identity-based SPOFs and a local tier heuristic."""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass

from digitalmodel.power.datacenter.models import (
    PowerComponent,
    RedundancyScheme,
)
from digitalmodel.power.datacenter.topology import DataCenterPowerPath


@dataclass
class TopologyValidator:
    """Inspect shared components and classify topology with local rules.

    The model does not establish concurrent maintainability, fault tolerance
    or compliance with Uptime Institute tier definitions.

    Attributes
    ----------
    paths : list[DataCenterPowerPath]
        All independent power distribution paths.
    redundancy_scheme : RedundancyScheme | None
        Declared redundancy scheme (used only for local level 4).
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
        """Compatibility alias for :meth:`classify_local_tier`.

        The returned integer is a local heuristic, not a certified Uptime tier.
        """
        return self.classify_local_tier()

    def classify_local_tier(self) -> int:
        """Return the historical local level, without certification equivalence.

        Ordered local rules (issue 2749, item 1):
        4: at least three paths, STS on all, no detected SPOFs, declared 2N+1.
        3: at least two paths, any STS, no detected SPOFs.
        2: any UPS.
        1: otherwise, including an empty topology.

        UPS presence does not prove redundant capacity. STS presence does not
        prove concurrent maintainability. A two-path 2N system returns local 3
        even with STS on both paths. Active-path state, capacity redundancy and
        fault isolation are not evaluated; N/N+1 declarations do not alter the
        rules. See docs/agent-pack.md, sections 3 and 6, for divergences.

        Returns
        -------
        int
            Local heuristic level (1–4), not a certified tier.
        """
        any_ups = any(p.has_ups for p in self.paths)
        any_sts = any(p.has_sts for p in self.paths)
        all_sts = all(p.has_sts for p in self.paths)
        no_spofs = len(self.find_spofs()) == 0

        # Local level 4 (issue 2749 item 1); not a standards-derived threshold.
        if (
            self.path_count >= 3
            and all_sts
            and no_spofs
            and self.redundancy_scheme == RedundancyScheme.TWO_N_PLUS_1
        ):
            return 4

        # Local level 3: multiple paths + STS + no detected SPOFs.
        if self.path_count >= 2 and any_sts and no_spofs:
            return 3

        # Local level 2: UPS presence alone, without a redundancy count.
        if any_ups:
            return 2

        # Local level 1: fallback, including an empty topology.
        return 1
