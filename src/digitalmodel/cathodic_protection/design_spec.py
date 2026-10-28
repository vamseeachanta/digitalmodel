# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit single-zone YAML inputs for preliminary cathodic protection analysis.

The supplied current density and coating fraction are mean values over the
supplied design life. The coating fraction is the fraction of area demanding
current: zero denotes perfect coating and one denotes bare area. Utilisation
is the usable fraction of anode mass, greater than zero and at most one.
These definitions supply the fraction bounds; no environmental, material,
coating or standard-derived design values are selected here.

Units must be declared exactly as shown in ``CPDesignUnits``. No conversion
is performed. The design-year convention is inherited from
``_kernels.HOURS_PER_YEAR`` (365 days times 24 hours, without leap days).

Run the synthetic example from Python with ``src`` on the import path::

    import json
    from digitalmodel.cathodic_protection.design_spec import design_from_yaml

    result = design_from_yaml("examples/cathodic_protection/synthetic_design.yml")
    print(json.dumps(result, indent=2))
"""

from __future__ import annotations

import math
from typing import Annotated, Any, Literal

import yaml
from pydantic import BaseModel, ConfigDict, Field, StringConstraints

from digitalmodel.cathodic_protection import _kernels as kernel

# Bounds follow the physical definitions in the module docstring.
PositiveFinite = Annotated[float, Field(gt=0, allow_inf_nan=False)]
CoatingFraction = Annotated[float, Field(ge=0, le=1, allow_inf_nan=False)]
Utilisation = Annotated[float, Field(gt=0, le=1, allow_inf_nan=False)]
BasisDescription = Annotated[str, StringConstraints(strip_whitespace=True, min_length=1)]


class CPDesignUnits(BaseModel):
    """Required unit declarations; the string '1' denotes dimensionless values."""

    model_config = ConfigDict(extra="forbid", strict=True)

    area_m2: Literal["m2"]
    current_density_A_m2: Literal["A/m2"]
    coating_fraction: Literal["1"]
    design_life_years: Literal["year"]
    electrochemical_capacity_Ah_kg: Literal["Ah/kg"]
    utilisation: Literal["1"]


class CPDesignSpec(BaseModel):
    """Require every design input, its units and a nonempty description of basis.

    Area, current density, life and capacity must be positive finite numbers.
    Perfect coating may yield zero demand, but a zero area or current density
    is rejected as an unspecified design basis. Numeric strings and booleans
    are rejected. No input has a default.
    """

    model_config = ConfigDict(extra="forbid", strict=True)

    area_m2: PositiveFinite = Field(description="Protected surface area [m2].")
    current_density_A_m2: PositiveFinite = Field(
        description="Mean current density on bare area over the design life [A/m2]."
    )
    coating_fraction: CoatingFraction = Field(
        description="Mean fraction of area demanding current [1]."
    )
    design_life_years: PositiveFinite = Field(description="Design life [year].")
    electrochemical_capacity_Ah_kg: PositiveFinite = Field(
        description="User-specified electrochemical capacity [Ah/kg]."
    )
    utilisation: Utilisation = Field(description="Usable fraction of anode mass [1].")
    basis_description: BasisDescription
    units: CPDesignUnits

    @classmethod
    def from_yaml(cls, path: str) -> CPDesignSpec:
        """Read one safe YAML mapping and validate it without filling defaults.

        Args:
            path: UTF-8 YAML input file.

        Returns:
            The validated explicit design basis.

        Raises:
            OSError: The input file cannot be read.
            yaml.YAMLError: YAML is malformed, uses unsafe tags or repeats keys.
            pydantic.ValidationError: The input basis or units are invalid.
        """
        with open(path, encoding="utf-8") as stream:
            # SafeLoader constructors only; reject duplicate keys before their
            # values could silently replace a user-specified design input.
            loader = _UniqueKeySafeLoader(stream)
            try:
                data = loader.get_single_data()
            finally:
                loader.dispose()
        return cls.model_validate(data)


class _UniqueKeySafeLoader(yaml.SafeLoader):
    """Keep SafeLoader's tag restrictions and reject ambiguous mappings."""

    def construct_mapping(self, node: yaml.MappingNode, deep: bool = False) -> dict:
        """Construct a mapping without silently discarding repeated keys."""
        mapping: dict[Any, Any] = {}
        for key_node, value_node in node.value:
            key = self.construct_object(key_node, deep=deep)
            try:
                if key in mapping:
                    raise yaml.constructor.ConstructorError(
                        None, None, "duplicate YAML key", key_node.start_mark
                    )
                mapping[key] = self.construct_object(value_node, deep=deep)
            except TypeError as exc:
                raise yaml.constructor.ConstructorError(
                    None, None, "invalid YAML mapping key", key_node.start_mark
                ) from exc
        return mapping


def design_from_yaml(path: str) -> dict:
    """Calculate mean demand, lifetime charge and net anode mass from YAML.

    Args:
        path: UTF-8 YAML file conforming to ``CPDesignSpec``.

    Returns:
        A JSON-compatible mapping with the validated input basis, explicit
        units, current demand [A], lifetime charge [Ah], net anode mass [kg]
        and a preliminary-screening limitation. No output file is written.

    Raises:
        OSError: The input file cannot be read.
        yaml.YAMLError: The document is malformed, unsafe or ambiguous.
        pydantic.ValidationError: An input or unit declaration is invalid.
        ValueError: The calculation exceeds floating-point range.
    """
    spec = CPDesignSpec.from_yaml(path)
    current = kernel.current_demand(
        spec.area_m2, spec.current_density_A_m2, spec.coating_fraction
    )
    if not math.isfinite(current) or (current == 0 and spec.coating_fraction > 0):
        raise ValueError("Current demand exceeds floating-point range")
    charge = current * spec.design_life_years * kernel.HOURS_PER_YEAR
    if not math.isfinite(charge) or (current > 0 and charge == 0):
        raise ValueError("Required charge exceeds floating-point range")
    try:
        mass = kernel.anode_mass(
            current,
            spec.design_life_years,
            spec.electrochemical_capacity_Ah_kg,
            spec.utilisation,
        )
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("Required anode mass exceeds floating-point range") from exc
    if not math.isfinite(mass) or (current > 0 and mass == 0):
        raise ValueError("Required anode mass exceeds floating-point range")
    return {
        "input_basis": spec.model_dump(),
        "units": {
            **spec.units.model_dump(),
            "current_demand_A": "A",
            "required_charge_Ah": "Ah",
            "required_anode_mass_kg": "kg",
        },
        "current_demand_A": current,
        "required_charge_Ah": charge,
        "required_anode_mass_kg": mass,
        "limitation": (
            "Preliminary screening only: single-zone mean demand and net mass "
            "with a constant user-specified coating fraction over the design life. "
            "No protection potential, anode resistance or current output, anode "
            "count, distribution, interference or installation checks are made. "
            "Confirm the input basis and complete those checks before design use."
        ),
    }
