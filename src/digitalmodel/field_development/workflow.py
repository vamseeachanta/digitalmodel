# ABOUTME: Field development screening workflow — chains concept selection into economics.
# ABOUTME: Issue #1858/#1848 — thin orchestrator exposing end-to-end FDP screening.
"""
digitalmodel.field_development.workflow
=======================================

Thin orchestrator that chains the field development modules into a
single screening-level evaluation pipeline:

    field parameters → concept selection → economics evaluation

Also provides a **compare-concepts** mode that evaluates the top-N
ranked host concepts side by side for decision support.

Usage
-----
>>> from digitalmodel.field_development.workflow import (
...     FieldDevelopmentSpec, evaluate_field_development,
... )
>>> spec = FieldDevelopmentSpec(
...     field_name="Whale",
...     water_depth_m=2100.0,
...     reservoir_size_mmbbl=400.0,
...     production_capacity_bopd=100_000.0,
...     oil_price_usd_per_bbl=75.0,
...     discount_rate=0.10,
...     fiscal_regime=FiscalRegime.US,
...     field_life_years=30,
... )
>>> result = evaluate_field_development(spec)
>>> print(result.concept.selected_host)
Spar
>>> print(f"NPV ${result.economics.metrics.npv_usd_mm:,.0f} MM")
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from digitalmodel.field_development.concept_selection import (
    ConceptSelectionResult,
    concept_selection,
)
from digitalmodel.field_development.economics import (
    EconomicsInput,
    EconomicsResult,
    FiscalRegime,
    evaluate_economics,
)


# ---------------------------------------------------------------------------
# Input
# ---------------------------------------------------------------------------


@dataclass
class FieldDevelopmentSpec:
    """Input specification for a field development screening evaluation.

    Combines the parameters needed for both concept selection and
    economics evaluation into a single structure.
    """

    # Required
    field_name: str
    water_depth_m: float
    reservoir_size_mmbbl: float
    production_capacity_bopd: float
    oil_price_usd_per_bbl: float
    discount_rate: float
    fiscal_regime: FiscalRegime
    field_life_years: int

    # Concept selection parameters
    distance_to_infra_km: Optional[float] = None
    fluid_type: str = "oil"

    # Optional economics overrides
    opex_usd_per_bbl: Optional[float] = None
    capex_usd_mm: Optional[float] = None
    abex_usd_mm: Optional[float] = None
    carbon_cost_usd_per_tonne: Optional[float] = None
    region: Optional[str] = None

    def __post_init__(self) -> None:
        if self.water_depth_m <= 0:
            raise ValueError(
                f"water_depth_m must be positive, got {self.water_depth_m!r}"
            )
        if self.reservoir_size_mmbbl <= 0:
            raise ValueError(
                f"reservoir_size_mmbbl must be positive, got {self.reservoir_size_mmbbl!r}"
            )
        if self.fluid_type not in ("oil", "gas", "condensate"):
            raise ValueError(
                f"fluid_type must be 'oil', 'gas', or 'condensate', got {self.fluid_type!r}"
            )


# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------


@dataclass
class FieldDevelopmentResult:
    """Complete field development screening result.

    Bundles concept selection ranking with the economics evaluation
    for the top-ranked host concept.
    """

    concept: ConceptSelectionResult
    economics: EconomicsResult


@dataclass
class ConceptComparison:
    """Side-by-side economics for multiple host concepts."""

    results: list[FieldDevelopmentResult]

    @property
    def best(self) -> FieldDevelopmentResult:
        """Return the result with the highest NPV."""
        return max(self.results, key=lambda r: r.economics.metrics.npv_usd_mm)


# ---------------------------------------------------------------------------
# Core workflow
# ---------------------------------------------------------------------------


def _spec_to_economics_input(
    spec: FieldDevelopmentSpec,
    host_type: str,
) -> EconomicsInput:
    """Convert a FieldDevelopmentSpec into an EconomicsInput for a given host."""
    return EconomicsInput(
        field_name=spec.field_name,
        water_depth_m=spec.water_depth_m,
        host_type=host_type,
        production_capacity_bopd=spec.production_capacity_bopd,
        oil_price_usd_per_bbl=spec.oil_price_usd_per_bbl,
        discount_rate=spec.discount_rate,
        fiscal_regime=spec.fiscal_regime,
        field_life_years=spec.field_life_years,
        reservoir_size_mmbbl=spec.reservoir_size_mmbbl,
        opex_usd_per_bbl=spec.opex_usd_per_bbl,
        capex_usd_mm=spec.capex_usd_mm,
        abex_usd_mm=spec.abex_usd_mm,
        carbon_cost_usd_per_tonne=spec.carbon_cost_usd_per_tonne,
        region=spec.region,
    )


def evaluate_field_development(
    spec: FieldDevelopmentSpec,
) -> FieldDevelopmentResult:
    """Run end-to-end field development screening.

    Steps:
    1. Concept selection — rank host facility options by suitability.
    2. Economics evaluation — evaluate the top-ranked concept through
       the worldenergydata-backed economics facade.

    Parameters
    ----------
    spec : FieldDevelopmentSpec
        Combined field and economic parameters.

    Returns
    -------
    FieldDevelopmentResult
        Concept ranking + economics for the selected host.
    """
    # Step 1: Concept selection
    concept = concept_selection(
        water_depth=spec.water_depth_m,
        reservoir_size_mmbbl=spec.reservoir_size_mmbbl,
        distance_to_infra_km=spec.distance_to_infra_km,
        fluid_type=spec.fluid_type,
    )

    # Step 2: Economics evaluation for the selected host
    econ_input = _spec_to_economics_input(spec, concept.selected_host.value)
    econ_result = evaluate_economics(econ_input)

    return FieldDevelopmentResult(concept=concept, economics=econ_result)


def compare_concepts(
    spec: FieldDevelopmentSpec,
    top_n: int = 3,
) -> ConceptComparison:
    """Evaluate economics for the top-N ranked host concepts side by side.

    Runs concept selection once, then evaluates economics for each of
    the top *top_n* viable concepts (score > 0).

    Parameters
    ----------
    spec : FieldDevelopmentSpec
        Combined field and economic parameters.
    top_n : int
        Number of top-ranked concepts to evaluate (default 3).

    Returns
    -------
    ConceptComparison
        Side-by-side results, with a ``.best`` property for the
        highest-NPV option.
    """
    concept = concept_selection(
        water_depth=spec.water_depth_m,
        reservoir_size_mmbbl=spec.reservoir_size_mmbbl,
        distance_to_infra_km=spec.distance_to_infra_km,
        fluid_type=spec.fluid_type,
    )

    # Only evaluate viable concepts (score > 0)
    viable = [opt for opt in concept.ranked_options if opt.score > 0][:top_n]

    results = []
    for option in viable:
        econ_input = _spec_to_economics_input(spec, option.host_type.value)
        econ_result = evaluate_economics(econ_input)
        results.append(
            FieldDevelopmentResult(concept=concept, economics=econ_result)
        )

    return ConceptComparison(results=results)
