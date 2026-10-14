# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Subsea capping-stack emergency-containment mechanics (API 17W).

Bounded deterministic core for emergency-response planning of a subsea
capping stack (the post-2010 GoM blowout source-control device lowered onto a
flowing wellhead):

* ``containment_capacity`` — central pressure-adequacy check: stack rated
  working pressure vs the maximum shut-in wellhead pressure (SIWHP) the
  well can present (API 17W / API STD 53).
* ``interface_check`` — capping-stack / wellhead interface compatibility
  (bore, pressure class, connector hub family; API 17W / API 17D).
* ``deployment`` — deployment lift feasibility screen: crane SWL vs
  submerged + dynamic hook load and sea-state limit (DNV-ST-N001).
* ``containment_flow`` — constant-density IPR–VLP/choke operating point,
  staged quasi-steady closure and explicit caller-supplied transient bounds.

Deferred follow-on (see issue #1997): coupled transient flowing-well solver
and standards qualification. Other deferred work: compositional gas gradient,
ROV heavy-lift and lower-line current dynamics, and #484 tree-module integration.
"""

from .containment_capacity import (
    ContainmentResult,
    GAS_COLUMN_DENSITY,
    OIL_COLUMN_DENSITY,
    BRINE_COLUMN_DENSITY,
    shut_in_wellhead_pressure,
    check_containment,
)
from .interface_check import (
    PressureClass,
    ConnectorClass,
    WellheadSpec,
    CappingStackSpec,
    InterfaceResult,
    check_compatibility,
    PSI_TO_PA,
)
from .deployment import (
    CappingStack,
    DeploymentScenario,
    DeploymentResult,
    submerged_weight,
    feasibility,
)
from .containment_flow import (
    FlowOperatingPoint, StagedContainment, linear_inflow, vogel_inflow,
    gas_backpressure_inflow, solve_operating_point, staged_shut_in,
)

__all__ = [
    # containment
    "ContainmentResult",
    "GAS_COLUMN_DENSITY",
    "OIL_COLUMN_DENSITY",
    "BRINE_COLUMN_DENSITY",
    "shut_in_wellhead_pressure",
    "check_containment",
    # interface
    "PressureClass",
    "ConnectorClass",
    "WellheadSpec",
    "CappingStackSpec",
    "InterfaceResult",
    "check_compatibility",
    "PSI_TO_PA",
    # deployment
    "CappingStack",
    "DeploymentScenario",
    "DeploymentResult",
    "submerged_weight",
    "feasibility",
    "FlowOperatingPoint",
    "StagedContainment",
    "linear_inflow",
    "vogel_inflow",
    "gas_backpressure_inflow",
    "solve_operating_point",
    "staged_shut_in",
]
