"""Ship-impact kinetic-energy screen, not structural resistance or collision QRA.

Method: RISP JIP report, version 2 (October 2023, published by Offshore Norge),
part "Ship collision risk" ST-16725-2, section 3.4, p.11: E = (m + a) v² / 2;
a = C_a m. The coefficient is a required, cited scenario input; no
direction-specific added mass is assumed.
https://www.offshorenorge.no/contentassets/9e11b988dccb4207acfd71c96832164a/risp-report-version-2-final---october-2023.pdf

Unit basis: NIST SP 811 (2008), appendix B.8 (kg, pound, tonne, foot,
international knot); conversion uses digitalmodel.units' shared Pint registry.
https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8

Comparison values are cited edition-specific inputs. The bundled historical
14 MJ (S-DP-001 Rev.2, 1996, 7.8/8.1) is superseded and tests-only.
The target is assumed stationary; this is the vessel's available kinetic
energy, not energy absorbed by the installation, force, damage or fatalities.
"""

from __future__ import annotations

from dataclasses import dataclass
import math
from typing import Literal

from pydantic import field_validator

from digitalmodel.foam_system.sizing import Citation
from digitalmodel.units import ureg
from .criteria import (
    CitedValue,
    EditionCriteria,
    NonNegative,
    Positive,
    ScreeningModel,
    validate_citation,
)


class ShipImpactInputs(ScreeningModel):
    """Displacement mass [kg], impact speed [m/s], added mass / displacement [-]."""

    mass_kg: Positive
    speed_m_s: NonNegative
    added_mass_coefficient: NonNegative
    added_mass_citation: Citation

    _citation_valid = field_validator("added_mass_citation")(validate_citation)

    @classmethod
    def from_units(
        cls,
        *,
        mass: float,
        mass_unit: str,
        speed: float,
        speed_unit: str,
        added_mass_coefficient: float,
        added_mass_citation: Citation,
    ) -> ShipImpactInputs:
        """Explicit mass and speed units; ambiguous 'ton' and force units refused.

        lb means avoirdupois mass. long_ton and short_ton must be spelled out.
        Allowed conversions follow NIST SP 811 (2008), appendix B.8.
        """
        if mass_unit not in ("kg", "tonne", "lb", "slug", "long_ton", "short_ton"):
            raise ValueError(
                "mass_unit must be kg, tonne, lb, slug, long_ton or short_ton"
            )
        if speed_unit not in ("m/s", "ft/s", "knot"):
            raise ValueError("speed_unit must be m/s, ft/s or knot")
        # Validate raw numeric values before Pint can coerce strings or booleans.
        raw = cls(
            mass_kg=mass,
            speed_m_s=speed,
            added_mass_coefficient=added_mass_coefficient,
            added_mass_citation=added_mass_citation,
        )
        return cls(
            mass_kg=ureg.Quantity(raw.mass_kg, mass_unit).to("kg").magnitude,
            speed_m_s=ureg.Quantity(raw.speed_m_s, speed_unit).to("m/s").magnitude,
            added_mass_coefficient=raw.added_mass_coefficient,
            added_mass_citation=raw.added_mass_citation,
        )


@dataclass(frozen=True)
class ShipImpactResult:
    inputs: ShipImpactInputs
    criteria: EditionCriteria
    application: str
    criterion: CitedValue
    energy_mj: float

    @property
    def within_design_energy(self) -> bool:
        """Energy <= selected comparator; no structural acceptance is implied."""
        return self.energy_mj <= self.criterion.value

    @property
    def margin_mj(self) -> float:
        return self.criterion.value - self.energy_mj


def screen_ship_impact(
    inputs: ShipImpactInputs,
    criteria: EditionCriteria,
    *,
    application: Literal["supply_vessel", "riser_protection"] = "supply_vessel",
    allow_test_criteria: bool = False,
) -> ShipImpactResult:
    """E [MJ] = 0.5 m [kg] (1 + C_a) v² [m²/s²] / 10^6.

    Equation: RISP ST-16725-2 (2023), section 3.4. C_a and the comparison
    limit retain their separate Citation sidecars in the returned result.
    """
    keys = {
        "supply_vessel": "supply_vessel_collision_energy",
        "riser_protection": "riser_collision_energy",
    }
    if application not in keys:
        raise ValueError("application must be supply_vessel or riser_protection")
    criterion = criteria.require(
        keys[application], allow_test_criteria=allow_test_criteria
    )
    validate_citation(inputs.added_mass_citation)
    try:
        energy = (
            0.5
            * inputs.mass_kg
            * (1 + inputs.added_mass_coefficient)
            * inputs.speed_m_s**2
            / 1e6
        )
    except OverflowError as exc:
        raise ValueError("collision energy must be finite") from exc
    if not math.isfinite(energy):
        raise ValueError("collision energy must be finite")
    return ShipImpactResult(inputs, criteria, application, criterion, energy)
