# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reusable single-case service for the simple catenary CLI contract."""

from dataclasses import dataclass
from typing import Any

from .models import CatenaryRiserResult, RiserConfiguration, get_fluid, get_material
from .simple_catenary import SimpleCatenaryAnalyzer


@dataclass(frozen=True)
class SimpleCatenaryRequest:
    """Simple-command inputs: lengths in m, density in kg/m³, tension in N."""

    diameter: float
    thickness: float
    length: float
    water_depth: float
    offset: float
    material: str = "x65"
    internal_fluid: str = "oil"
    top_tension: float | None = None
    coating_thickness: float | None = None
    coating_density: float | None = None


@dataclass(frozen=True)
class SimpleCatenaryOutcome:
    """Unrounded analyzer result and the existing rounded JSON presentation."""

    result: CatenaryRiserResult
    presentation: dict[str, Any]


def analyze_simple(request: SimpleCatenaryRequest) -> SimpleCatenaryOutcome:
    """Construct and analyze one case without writing files or accessing a network."""
    mat = get_material(request.material)
    fluid = get_fluid(request.internal_fluid)
    riser = RiserConfiguration(
        name="Riser1", outer_diameter=request.diameter,
        wall_thickness=request.thickness, length=request.length,
        material=mat, internal_fluid=fluid, water_depth=request.water_depth,
        horizontal_offset=request.offset, coating_thickness=request.coating_thickness,
        coating_density=request.coating_density,
    )
    analyzer = SimpleCatenaryAnalyzer()
    if request.top_tension is not None:
        result = analyzer.analyze_riser(riser, top_tension=request.top_tension)
    else:
        result = analyzer.analyze_riser(
            riser, water_depth=request.water_depth, horizontal_offset=request.offset,
        )
    if not result.converged:
        raise ValueError("catenary did not converge or exceeds available length")
    return SimpleCatenaryOutcome(result, _presentation(request, result))


def _presentation(request: SimpleCatenaryRequest, result: CatenaryRiserResult) -> dict[str, Any]:
    """Preserve the CLI JSON schema and its two-decimal presentation values."""
    return {
        "configuration": {
            "diameter": request.diameter,
            "thickness": request.thickness,
            "length": request.length,
            "water_depth": request.water_depth,
            "material": request.material,
            "internal_fluid": request.internal_fluid,
        },
        "results": {
            "converged": result.converged,
            "horizontal_tension_N": round(result.horizontal_tension, 2),
            "top_tension_N": round(result.top_tension, 2),
            "touchdown_tension_N": round(result.touchdown_tension, 2),
            "top_angle_deg": round(result.top_angle, 2),
            "arc_length_m": round(result.arc_length, 2),
            "grounded_length_m": round(result.grounded_length, 2),
            "effective_weight_N_per_m": round(result.effective_weight, 2),
            "catenary_parameter_m": round(result.catenary_parameter, 2),
        },
    }
