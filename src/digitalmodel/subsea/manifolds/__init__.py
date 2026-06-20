"""digitalmodel.subsea.manifolds — subsea production manifolds (API 17P).

A subsea manifold is the gathering hub of a multi-well field development: it
commingles the production of ``N`` subsea trees into a common header, balances the
contributing wells against the shared header pressure, and feeds the downstream
flowline/riser system.

API 17P ("Design and Operation of Subsea Production Systems — Subsea Structures
and Manifolds") frames the manifold structure, piping and interfaces.  This
package implements the deterministic, public-standard gathering-node core:

- ``flow_aggregator``  — steady-state additive flow gathering from N trees
                         (commingled phase rates, water-cut, GOR, header
                         capacity check).
- ``pressure_balance`` — well-to-header commingling feasibility: each well's
                         delivered pressure vs. the common header pressure and
                         the required choke differential / choke authority.

Deferred (integration / requires solver or proprietary data): manifold-type
catalog/registry, chemical-injection (CIM) distribution, API 17P structural /
protection-frame design checks, hydraulic/electrical distribution layout, and the
tree (17D) -> manifold -> flowline OrcaFlex/network wiring.  A full transient,
multiphase network solver is out of scope for this deterministic core.
"""

from digitalmodel.subsea.manifolds.flow_aggregator import (
    AggregateResult,
    aggregate_flow,
)
from digitalmodel.subsea.manifolds.pressure_balance import (
    ManifoldBalanceResult,
    WellBalance,
    manifold_balance,
    well_balance,
)

__all__ = [
    "AggregateResult",
    "aggregate_flow",
    "ManifoldBalanceResult",
    "WellBalance",
    "manifold_balance",
    "well_balance",
]
