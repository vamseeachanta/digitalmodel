"""Subsea capping-stack emergency-containment mechanics (API 17W).

Bounded deterministic core for emergency-response planning of a subsea
capping stack (the post-Macondo source-control device lowered onto a
flowing wellhead):

* ``containment_capacity`` — central pressure-adequacy check: stack rated
  working pressure vs the maximum shut-in wellhead pressure (SIWHP) the
  well can present (API 17W / API STD 53).
* ``interface_check`` — capping-stack / wellhead interface compatibility
  (bore, pressure class, connector hub family; API 17W / API 17D).
* ``deployment`` — deployment lift feasibility screen: crane SWL vs
  submerged + dynamic hook load and sea-state limit (DNV-ST-N001).

Deferred follow-on (see issue #490): transient flowing-well / choke-Cv
containment-flow solver, compositional gas gradient, ROV heavy-lift and
lower-line current dynamics, and #484 tree-module integration.
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
]
