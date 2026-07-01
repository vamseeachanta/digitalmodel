"""digitalmodel.floating_wind -- floating-wind global sizing & concept screening.

A PyFloatSizer-class capability (epic #1022): hold floater archetypes
(semi-submersible / spar / TLP / barge) as parametric models, screen design
variants across site load cases, and surface the trade space. Closed-form /
licence-free first tier; OrcaWave/OrcaFlex is the high-fidelity tier (#1025).
"""

from .floaters import (
    RHO_SEAWATER,
    G_STANDARD,
    FloaterArchetype,
    TurbineTopside,
    FloaterProperties,
    SemiSubmersible,
    Spar,
    TLP,
    Barge,
    build_floater,
    IEA_15MW_RNA,
)
from .economics import (
    FinancialParameters,
    CapexBreakdown,
    ProjectEconomics,
    LCOEResult,
    compute_lcoe,
    base_case,
)

__all__ = [
    "RHO_SEAWATER",
    "G_STANDARD",
    "FloaterArchetype",
    "TurbineTopside",
    "FloaterProperties",
    "SemiSubmersible",
    "Spar",
    "TLP",
    "Barge",
    "build_floater",
    "IEA_15MW_RNA",
    "FinancialParameters",
    "CapexBreakdown",
    "ProjectEconomics",
    "LCOEResult",
    "compute_lcoe",
    "base_case",
]
