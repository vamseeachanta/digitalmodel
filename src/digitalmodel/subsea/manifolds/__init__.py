# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Deterministic subsea-manifold gathering, screening and distribution core.

Delivered modules:
- ``flow_aggregator``: additive phase gathering and header-capacity checks.
- ``pressure_balance``: well-to-header pressure feasibility and choke authority.
- ``manifold_catalog``: source-bearing equipment registry for four manifold types.
- ``design_checks``: elementary mudmat, header and independent load-case screens.
- ``chemical_injection``: proposed multi-well dosing, shared umbilical delivery
  hydraulics, and balancing restrictions or explicit branch pressure deficits.

No seeded public equipment catalogue or verified API 17P edition/clause basis
is supplied by the new modules. The elementary screens require independently
qualified capacities; they do not establish API 17P design qualification.
Hydraulic/electrical distribution layout belongs to the controls package.

Deferred: tree (17D) -> manifold -> flowline network wiring and a full transient,
multiphase network solver.
"""

from digitalmodel.subsea.manifolds.chemical_injection import (
    ChemicalDistribution,
    InjectionDemand,
    WellDose,
    distribute_chemical,
)
from digitalmodel.subsea.manifolds.design_checks import (
    HeaderScreen,
    LoadCaseScreen,
    MudmatScreen,
    header_screen,
    load_case_screen,
    mudmat_screen,
)
from digitalmodel.subsea.manifolds.flow_aggregator import AggregateResult, aggregate_flow
from digitalmodel.subsea.manifolds.manifold_catalog import ManifoldCatalog, ManifoldEntry
from digitalmodel.subsea.manifolds.pressure_balance import (
    ManifoldBalanceResult,
    WellBalance,
    manifold_balance,
    well_balance,
)

__all__ = [
    "AggregateResult", "aggregate_flow", "ManifoldBalanceResult", "WellBalance",
    "manifold_balance", "well_balance", "ManifoldCatalog", "ManifoldEntry",
    "MudmatScreen", "HeaderScreen", "LoadCaseScreen", "mudmat_screen",
    "header_screen", "load_case_screen", "ChemicalDistribution", "InjectionDemand",
    "WellDose", "distribute_chemical",
]
