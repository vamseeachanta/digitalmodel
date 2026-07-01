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
from .reliability import (
    ReliabilityScenario,
    apply_reliability,
    lcoe_with_reliability,
)
from .sweep import (
    LeverChange,
    DriverScenario,
    SweepRow,
    apply_change,
    run_sweep,
)
from .qualification import (
    DNV_TRL_MAX,
    trl_to_maturity,
    QualificationVerdict,
    Criterion,
    QualificationCriteria,
    ConceptMaturity,
    QualificationResult,
    score_concept,
    rank_concepts,
    default_criteria,
)
from .tradespace_economics import (
    LCOE_METRIC_KEY,
    variant_lcoe,
    mean_steel_mass_t,
    lcoe_records,
    pareto_front_with_lcoe,
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
    "ReliabilityScenario",
    "apply_reliability",
    "lcoe_with_reliability",
    "LeverChange",
    "DriverScenario",
    "SweepRow",
    "apply_change",
    "run_sweep",
    "DNV_TRL_MAX",
    "trl_to_maturity",
    "QualificationVerdict",
    "Criterion",
    "QualificationCriteria",
    "ConceptMaturity",
    "QualificationResult",
    "score_concept",
    "rank_concepts",
    "default_criteria",
    "LCOE_METRIC_KEY",
    "variant_lcoe",
    "mean_steel_mass_t",
    "lcoe_records",
    "pareto_front_with_lcoe",
]
