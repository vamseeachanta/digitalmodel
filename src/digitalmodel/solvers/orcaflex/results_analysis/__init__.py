# ABOUTME: Analysis library for OrcaFlex CSV result sets — parsing, validation,
# ABOUTME: classification, statistics, comparison, sensitivity and anomaly detection.
"""OrcaFlex results analysis.

These nine modules were the only salvageable part of the vendored
``visualization/orcaflex_dashboard/`` FastAPI application removed in #1632. They
were extracted rather than deleted because, unlike the rest of that tree, they
import cleanly, depend on nothing but numpy/pandas/scipy/scikit-learn, and are
covered by 689 passing tests.

Nothing here is web-facing. There is no FastAPI, no SQLAlchemy, no request or
response object: each module is a plain library that takes DataFrames or arrays
and returns dataclasses.

Modules are imported lazily -- ``from digitalmodel.solvers.orcaflex.results_analysis
import csv_parser`` -- so that importing one does not drag in scikit-learn for
callers that only need the parser.
"""

__all__ = [
    "anomaly_detection",
    "comparative_analysis",
    "component_classifier",
    "csv_parser",
    "data_validator",
    "file_monitor",
    "loading_decoder",
    "sensitivity_analysis",
    "statistical_analysis",
]
