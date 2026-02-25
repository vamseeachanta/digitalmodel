# ABOUTME: Production Engineering module — well test quality, nodal analysis, GIGO detection
# ABOUTME: Data quality is the foundation of reliable nodal analysis (WRK-164)

"""
Production Engineering — Well Test Quality & Nodal Analysis Foundation
=======================================================================

Core insight: reliable nodal analysis starts with production test data quality,
not with algorithm sophistication. (See WRK-164 user voice.)

Public API:
    test_quality_scorer  — score a production test record (0-100 + flags)
    nonlinearity_flags   — detect nonlinear flow conditions in a test
    ipr_models           — Vogel, Fetkovich, Composite IPR
    vlp_correlations     — Hagedorn-Brown, Beggs-Brill VLP traverses
    nodal_solver         — IPR/VLP intersection with confidence bounds
    gigo_detector        — model vs test divergence with physics diagnosis
    reconciliation_workflow — end-to-end QC → calibrate → confidence pipeline
"""

from digitalmodel.production_engineering.ipr_models import (
    CompositeIpr,
    FetkovichIpr,
    LinearIpr,
    ReservoirConditions,
    VogelIpr,
)
from digitalmodel.production_engineering.nodal_solver import (
    ConfidenceBound,
    NodalOperatingPoint,
    NodalSolver,
)
from digitalmodel.production_engineering.nonlinearity_flags import (
    NonlinearityDetector,
    NonlinearityFlag,
)
from digitalmodel.production_engineering.reconciliation_workflow import (
    ReconciliationResult,
    ReconciliationWorkflow,
)
from digitalmodel.production_engineering.test_quality_scorer import (
    ProductionTestQualityScorer,
    ProductionTestRecord,
    QualityFlag,
    QualityScore,
    WellType,
)
from digitalmodel.production_engineering.vlp_correlations import (
    FlowConditions,
    FluidProperties,
    TubingConfig,
    beggs_brill_pwf,
    hagedorn_brown_pwf,
    vlp_curve,
)

__all__ = [
    "CompositeIpr",
    "ConfidenceBound",
    "FetkovichIpr",
    "FlowConditions",
    "FluidProperties",
    "LinearIpr",
    "NodalOperatingPoint",
    "NodalSolver",
    "NonlinearityDetector",
    "NonlinearityFlag",
    "ProductionTestQualityScorer",
    "ProductionTestRecord",
    "QualityFlag",
    "QualityScore",
    "ReconciliationResult",
    "ReconciliationWorkflow",
    "ReservoirConditions",
    "TubingConfig",
    "VogelIpr",
    "WellType",
    "beggs_brill_pwf",
    "hagedorn_brown_pwf",
    "vlp_curve",
]
