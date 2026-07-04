"""digitalmodel.data_systems.data_quality - data-quality guardrails.

Currently exposes the reporting-lag / partial-year *completeness guardrail*
used to stop downstream trend/forecast code from misreading the artificially
low *tail* of a lagged data source (BSEE WAR, OGOR) as a real decline.

See :mod:`digitalmodel.data_systems.data_quality.completeness_guardrail`.
"""

from digitalmodel.data_systems.data_quality.completeness_guardrail import (
    Completeness,
    CompletenessReport,
    IncompleteTrendWarning,
    SourceConfig,
    WAR_DEEPWATER_CONFIG,
    OGOR_ANNUAL_CONFIG,
    classify_completeness,
    guard_trend,
)

__all__ = [
    "Completeness",
    "CompletenessReport",
    "IncompleteTrendWarning",
    "SourceConfig",
    "WAR_DEEPWATER_CONFIG",
    "OGOR_ANNUAL_CONFIG",
    "classify_completeness",
    "guard_trend",
]
