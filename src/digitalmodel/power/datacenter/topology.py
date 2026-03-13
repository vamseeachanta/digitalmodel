"""Data center power path modeling — cascaded power chain analysis."""

from __future__ import annotations

import math
from dataclasses import dataclass, field

from digitalmodel.power.datacenter.models import (
    PowerComponent,
    UPSModule,
    STSModule,
)


@dataclass
class DataCenterPowerPath:
    """A single power distribution path from source to load.

    Models the cascaded chain: utility → MV switchgear → transformer →
    UPS → STS → PDU → rack. Components are ordered from source to load.

    Attributes
    ----------
    name : str
        Path identifier (e.g., "Path-A", "Path-B").
    components : list[PowerComponent]
        Ordered list of power components in the chain.
    """

    name: str
    components: list[PowerComponent] = field(default_factory=list)

    def add_component(self, component: PowerComponent) -> None:
        """Append a component to the end of the power chain."""
        self.components.append(component)

    @property
    def cascaded_efficiency(self) -> float:
        """Product of all component efficiencies in the chain."""
        if not self.components:
            return 1.0
        result = 1.0
        for c in self.components:
            result *= c.efficiency
        return result

    @property
    def bottleneck_kva(self) -> float:
        """Minimum rated kVA among all components (capacity bottleneck)."""
        if not self.components:
            return 0.0
        return min(c.rated_kva for c in self.components)

    @property
    def effective_capacity_kva(self) -> float:
        """Bottleneck capacity derated by cascaded efficiency."""
        return self.bottleneck_kva * self.cascaded_efficiency

    @property
    def has_ups(self) -> bool:
        """Whether any component in the path is a UPS module."""
        return any(isinstance(c, UPSModule) for c in self.components)

    @property
    def has_sts(self) -> bool:
        """Whether any component in the path is an STS module."""
        return any(isinstance(c, STSModule) for c in self.components)
