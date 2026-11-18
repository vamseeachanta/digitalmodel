# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Stored-energy screening from ideal-gas expansion or liquid compression."""

from __future__ import annotations

from dataclasses import dataclass
import math
from typing import Literal


@dataclass(frozen=True)
class StoredEnergyInputs:
    """Supplied inputs plus gauge pressure converted to Pa.

    Ambient pressure is absolute Pa; volume is the initial contained volume
    in m3. Both fluid properties are recorded, including the inactive one.
    """

    volume_m3: float
    pressure_gauge: float
    pressure_unit: str
    pressure_gauge_pa: float
    fluid_type: Literal["gas", "liquid"]
    specific_heat_ratio: float
    bulk_modulus_pa: float
    ambient_pressure_pa: float
    tnt_energy_j_per_kg: float
    threshold_j: float | None


@dataclass(frozen=True)
class StoredEnergyResult:
    """Energy in J, TNT mass in kg, equation and reproducible input basis.

    ``exceeds_threshold`` is None when no threshold was supplied, otherwise
    it is the strict comparison energy_j > threshold_j. False is not a
    declaration that the equipment is safe.
    """

    energy_j: float
    tnt_equivalent_kg: float
    exceeds_threshold: bool | None
    formula: str
    inputs: StoredEnergyInputs


def _number(name: str, value: float, *, minimum: float, inclusive: bool) -> None:
    valid = False
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        try:
            valid = math.isfinite(value) and (
                value >= minimum if inclusive else value > minimum
            )
        except OverflowError:
            # Integers outside floating-point range are not usable inputs.
            valid = False
    if not valid:
        relation = ">=" if inclusive else ">"
        raise ValueError(f"{name} must be a finite number {relation} {minimum}")


def stored_energy(
    volume_m3: float,
    pressure_gauge: float,
    fluid_type: Literal["gas", "liquid"],
    *,
    pressure_unit: str = "Pa",
    specific_heat_ratio: float = 1.4,
    bulk_modulus_pa: float = 2.2e9,
    ambient_pressure_pa: float = 101325.0,
    tnt_energy_j_per_kg: float = 4.184e6,
    threshold_j: float | None = None,
) -> StoredEnergyResult:
    """Calculate expansion energy and an optional review flag.

    Gas uses total reversible isentropic boundary work for an ideal gas with
    constant gamma, expanded from P1 = p_gauge + P0 to ambient P0:
    E = P1*V/(gamma-1) * [1 - (P0/P1)**((gamma-1)/gamma)]. This follows
    p*V**gamma = constant and the integral of p dV in OpenStax, University
    Physics Volume 2, section 3.6, equation 3.12 and example 3.7:
    https://openstax.org/books/university-physics-volume-2/pages/3-6-adiabatic-processes-for-an-ideal-gas
    It includes atmosphere-displacement work; it is not net useful work,
    the Brode form, or a prediction of blast yield.

    Liquid uses E = p_gauge**2*V/(2*K), integrating the linear bulk-strain
    relation at constant K. Basis: OpenStax, University Physics Volume 1,
    section 12.3, equation 12.39 and table 12.1 (water K = 2.2 GPa):
    https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus
    This small-compression approximation excludes container strain energy,
    trapped gas, phase change and chemical energy.

    The TNT conversion default is 4.184 MJ/kg, from the conventional
    4.184 GJ per metric ton in NIST Guide to the SI, appendix B.8:
    https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8
    Energy equivalence does not imply equivalent blast effects.

    Args:
        volume_m3: Positive initial contained fluid volume, m3.
        pressure_gauge: Nonnegative gauge pressure above ambient.
        fluid_type: Exactly 'gas' or 'liquid'; no mixed-phase model.
        pressure_unit: Exactly 'Pa', 'bar', 'MPa' or 'psi'. Default Pa;
            1 bar = 100000 Pa, 1 MPa = 1000000 Pa, and
            1 psi = 6894.757293168 Pa (pound-force per square inch).
        specific_heat_ratio: Gas gamma > 1; default 1.4 for air screening.
        bulk_modulus_pa: Positive liquid K in Pa; default 2.2e9 for water.
        ambient_pressure_pa: Positive absolute ambient pressure in Pa,
            independent of pressure_unit; default one atmosphere (101325).
        tnt_energy_j_per_kg: Positive TNT energy constant, J/kg.
        threshold_j: Nonnegative review threshold in J. Default None:
            no universal public acceptance threshold is established here.
            The caller must supply the applicable review criterion.

    Returns:
        Immutable result with energy, TNT mass, formula, all inputs and
        strict threshold flag (None when no criterion is supplied).

    Raises:
        ValueError: Invalid selectors, nonfinite or out-of-range numbers,
            or a calculation exceeding finite floating-point range.
            Both fluid properties are validated even when inactive.
    """
    if fluid_type not in ("gas", "liquid"):
        raise ValueError("fluid_type must be 'gas' or 'liquid'")
    units = {"Pa": 1.0, "bar": 1e5, "MPa": 1e6, "psi": 6894.757293168}
    if not isinstance(pressure_unit, str) or pressure_unit not in units:
        raise ValueError("pressure_unit must be 'Pa', 'bar', 'MPa' or 'psi'")
    for name, value, minimum, inclusive in (
        ("volume_m3", volume_m3, 0, False),
        ("pressure_gauge", pressure_gauge, 0, True),
        ("specific_heat_ratio", specific_heat_ratio, 1, False),
        ("bulk_modulus_pa", bulk_modulus_pa, 0, False),
        ("ambient_pressure_pa", ambient_pressure_pa, 0, False),
        ("tnt_energy_j_per_kg", tnt_energy_j_per_kg, 0, False),
    ):
        _number(name, value, minimum=minimum, inclusive=inclusive)
    if threshold_j is not None:
        _number("threshold_j", threshold_j, minimum=0, inclusive=True)

    pressure_pa = pressure_gauge * units[pressure_unit]
    absolute_pressure_pa = pressure_pa + ambient_pressure_pa
    if not math.isfinite(absolute_pressure_pa):
        raise ValueError("pressure conversion exceeds finite numeric range")
    if fluid_type == "gas":
        # log1p/expm1 preserve small positive energies near ambient and gamma=1.
        # The log difference avoids overflowing the ratio for large pressures.
        log_ratio = (
            math.log1p(pressure_pa / ambient_pressure_pa)
            if pressure_pa <= ambient_pressure_pa
            else math.log(absolute_pressure_pa) - math.log(ambient_pressure_pa)
        )
        gamma_minus_one = specific_heat_ratio - 1
        exponent = gamma_minus_one / specific_heat_ratio * log_ratio
        energy_j = (
            absolute_pressure_pa * -math.expm1(-exponent) / gamma_minus_one
        ) * volume_m3
        formula = "P1*V/(gamma-1)*(1-(P0/P1)**((gamma-1)/gamma))"
    else:
        energy_j = (pressure_pa / bulk_modulus_pa) * pressure_pa * (volume_m3 / 2)
        formula = "p_gauge**2*V/(2*K)"
    tnt_equivalent_kg = energy_j / tnt_energy_j_per_kg
    if not math.isfinite(energy_j) or not math.isfinite(tnt_equivalent_kg):
        raise ValueError("energy calculation exceeds finite numeric range")
    return StoredEnergyResult(
        energy_j=energy_j,
        tnt_equivalent_kg=tnt_equivalent_kg,
        exceeds_threshold=None if threshold_j is None else energy_j > threshold_j,
        formula=formula,
        inputs=StoredEnergyInputs(
            volume_m3=volume_m3,
            pressure_gauge=pressure_gauge,
            pressure_unit=pressure_unit,
            pressure_gauge_pa=pressure_pa,
            fluid_type=fluid_type,
            specific_heat_ratio=specific_heat_ratio,
            bulk_modulus_pa=bulk_modulus_pa,
            ambient_pressure_pa=ambient_pressure_pa,
            tnt_energy_j_per_kg=tnt_energy_j_per_kg,
            threshold_j=threshold_j,
        ),
    )
