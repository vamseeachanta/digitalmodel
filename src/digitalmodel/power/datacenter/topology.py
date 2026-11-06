"""Data center power path modeling — cascaded power chain analysis."""

from __future__ import annotations

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

    Ratings use the local input-side apparent-power convention. Equal, nonzero
    power factors are assumed throughout; no power-factor model is present.
    The effective-capacity approximation applies all losses to the minimum
    rating, without position-dependent load propagation or nameplate sizing.
    It is a lower bound on propagated capacity only for finite nonnegative ratings
    and efficiencies in (0, 1]; generic components do not enforce these bounds.

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
        """Product of real-power efficiencies, assuming a common steady flow.

        Cascading follows eta=P_out/P_in (ENERGY STAR UPS Test Method,
        Dec-2017, section 5 A, equation 2); no load-dependent loss curve is used.
        """
        if not self.components:
            return 1.0
        result = 1.0
        for c in self.components:
            result *= c.efficiency
        return result

    @property
    def bottleneck_kva(self) -> float:
        """Minimum local input-side rating; not a propagated load limit."""
        if not self.components:
            return 0.0
        return min(c.rated_kva for c in self.components)

    @property
    def effective_capacity_kva(self) -> float:
        """Local approximation: minimum input rating times full-chain efficiency.

        Every component loss is included regardless of bottleneck position.
        With finite nonnegative ratings and every efficiency in (0, 1], this is a
        lower bound on min_i(rating_i * product(efficiency_j for j >= i)).
        Generic components do not enforce these bounds. For input ratings
        (400, 200, 100) kVA and efficiencies (0.5, 0.75, 0.8), the estimate is
        30.0 kVA versus a propagated limit of 80.0 kVA, 62.5% lower.
        Equal, nonzero power factors are assumed. This is not a design-load
        check; the underestimate depends on ratings, losses and their order.
        Basis: local convention in docs/agent-pack.md, section 3, and issue 2749
        item 4; no standards sizing method is claimed.
        """
        return self.bottleneck_kva * self.cascaded_efficiency

    @property
    def has_ups(self) -> bool:
        """Whether any component in the path is a UPS module."""
        return any(isinstance(c, UPSModule) for c in self.components)

    @property
    def has_sts(self) -> bool:
        """Whether any component in the path is an STS module."""
        return any(isinstance(c, STSModule) for c in self.components)
