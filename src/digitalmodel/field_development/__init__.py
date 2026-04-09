# ABOUTME: Field development analysis and visualisation module (WRK-192)
# ABOUTME: Concept selection framework added in issue #1843.
# ABOUTME: Economics facade over worldenergydata backends — #1858.
"""digitalmodel.field_development — layout schematics, concept selection, cost estimation, and economics.

Modules:
- schematic_generator: Field development layout schematics (WRK-192)
- concept_selection:   Host facility ranking (TLP/Spar/Semi/FPSO) — #1843
- capex_estimator:     GoM-benchmark CAPEX estimation — #1843
- opex_estimator:      Annual OPEX estimation — #1843
- economics:           Economics facade over worldenergydata backends — #1858
- workflow:            End-to-end FDP screening orchestrator — #1858/#1848
"""

from .schematic_generator import generate_field_schematic, SOLVEIG_PHASE2_CONFIG
from .concept_selection import (
    HostType,
    ConceptOption,
    ConceptSelectionResult,
    concept_selection,
    concept_selection_with_benchmarks,
)
from .capex_estimator import CAPEXEstimate, estimate_capex
from .opex_estimator import OPEXEstimate, estimate_opex
from .economics import (
    EconomicsInput,
    EconomicsResult,
    FiscalRegime,
    CostEstimates,
    EvaluationMetrics,
    evaluate_economics,
    build_economics_schedule,
    carbon_sensitivity,
)
from .benchmarks import (
    SubseaProject,
    ConceptPrediction,
    concept_probability_matrix,
    predict_concept_type,
    validate_against_cases,
    concept_benchmark_bands,
    load_projects,
    DEPTH_BANDS,
)
from .workflow import (
    FieldDevelopmentSpec,
    FieldDevelopmentResult,
    ConceptComparison,
    evaluate_field_development,
    compare_concepts,
)

__all__ = [
    # Schematics (WRK-192)
    "generate_field_schematic",
    "SOLVEIG_PHASE2_CONFIG",
    # Concept selection (#1843, #2053)
    "HostType",
    "ConceptOption",
    "ConceptSelectionResult",
    "concept_selection",
    "concept_selection_with_benchmarks",
    # Benchmarks & probability matrix (#1861, #2053)
    "SubseaProject",
    "ConceptPrediction",
    "concept_probability_matrix",
    "predict_concept_type",
    "validate_against_cases",
    "concept_benchmark_bands",
    "load_projects",
    "DEPTH_BANDS",
    # CAPEX estimation (#1843)
    "CAPEXEstimate",
    "estimate_capex",
    # OPEX estimation (#1843)
    "OPEXEstimate",
    "estimate_opex",
    # Economics facade (#1858)
    "EconomicsInput",
    "EconomicsResult",
    "FiscalRegime",
    "CostEstimates",
    "EvaluationMetrics",
    "evaluate_economics",
    "build_economics_schedule",
    "carbon_sensitivity",
    # Workflow orchestrator (#1858/#1848)
    "FieldDevelopmentSpec",
    "FieldDevelopmentResult",
    "ConceptComparison",
    "evaluate_field_development",
    "compare_concepts",
]
