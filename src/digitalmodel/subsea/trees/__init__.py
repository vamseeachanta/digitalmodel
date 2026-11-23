# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Deterministic subsea-tree rating, liquid-flow and equipment-registry core.

Delivered modules:
- ``design_checks``: existing API 6A / API 17D rating-envelope check.
- ``tree_catalog``: source-bearing registry for single/dual-bore, HXT/VXT trees.
- ``pressure_control``: liquid Cv, ideal trim, series losses, no-fittings
  choking differential from supplied fluid/recovery factors and choking screen.
- ``flow_capacity``: caller-coefficient velocity limits and separate-port handoff.

No seeded public equipment catalogue or verified clause-level citation basis
is supplied by the new modules. They do not qualify an API design. Their
screening limits, SI coefficient requirement and mandatory capacities are
recorded in the module docstrings. A handoff is data, not a network solution.

Deferred: full FEA / API 6A PSL/PR material qualification, combined bending and
external-pressure checks, and tree-to-connector/manifold/flowline network wiring.
Gas and transient multiphase network simulation remain outside this core.
"""

from digitalmodel.subsea.trees.design_checks import (
    TreeRatingResult,
    select_rated_working_pressure,
    tree_rating_check,
)
from digitalmodel.subsea.trees.flow_capacity import (
    BoreCapacity,
    FlowHandoff,
    bore_capacity,
    flow_handoff,
)
from digitalmodel.subsea.trees.pressure_control import (
    ChokeFlow,
    liquid_choke_flow,
    liquid_critical_drop,
    series_pressure_drop,
    trim_cv,
    valve_pressure_drop,
)
from digitalmodel.subsea.trees.tree_catalog import TreeCatalog, TreeEntry

__all__ = [
    "TreeRatingResult", "select_rated_working_pressure", "tree_rating_check",
    "BoreCapacity", "FlowHandoff", "bore_capacity", "flow_handoff",
    "ChokeFlow", "liquid_choke_flow", "liquid_critical_drop", "series_pressure_drop", "trim_cv",
    "valve_pressure_drop", "TreeCatalog", "TreeEntry",
]
