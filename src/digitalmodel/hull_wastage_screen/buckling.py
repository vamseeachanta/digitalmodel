"""Optional measured-plate check using the existing metal-loss FFS solver.

Nominal capacity enters the retained-capacity ratio. Since measured thickness
cannot exceed nominal, the nominal elastic stress bounds both evaluations.
The inherited solver's half-yield boundary is included in the elastic branch.
"""
import math

from digitalmodel.structural.structural_analysis.buckling import PlateBucklingAnalyzer
from digitalmodel.structural.structural_analysis.models import MaterialProperties, PlateGeometry
from digitalmodel.structural.structural_analysis.plate_metal_loss_ffs import assess_plate_uniform_loss

from .screen import not_evaluated, number, source


def plate_screen(block, items, local):
    """Screen uniform plate loss only within the supported elastic domain."""
    if block is None:
        return not_evaluated("Plate geometry, material, loads and capacity basis required")
    source(block)
    key = block.get("element_id")
    if key not in items:
        raise ValueError("buckling element_id must identify a measured element")
    sx = number(block, "sigma_x_mpa")
    sy = number(block, "sigma_y_mpa")
    tau = number(block, "tau_mpa")
    if sy != 0:
        return not_evaluated("Existing plate solver does not evaluate transverse compression")
    if tau != 0:
        return not_evaluated("Shear capacity and interaction lack primary citations and independent benchmarks")
    material_data = block.get("material")
    if not isinstance(material_data, dict):
        raise ValueError("material must be a mapping")
    values = {key: number(material_data, key, positive=True) for key in (
        "yield_strength", "ultimate_strength", "youngs_modulus", "density")}
    nu = number(material_data, "poissons_ratio")
    if nu >= .5 or values["ultimate_strength"] < values["yield_strength"]:
        raise ValueError("material requires 0 <= Poisson ratio < .5 and ultimate >= yield")
    material = MaterialProperties(**values, poissons_ratio=nu)
    built = items[key]["as_built_mm"]
    current = local[key]["lower_bound_mm"]
    if current > built:
        return not_evaluated("Measured thickness exceeds nominal; metal-loss solver is inapplicable")
    if current == 0:
        return {"status": "RENEW", "reason": "Plate is fully consumed"}
    geometry = PlateGeometry(number(block, "length_mm", positive=True),
                             number(block, "width_mm", positive=True), built)
    elastic = PlateBucklingAnalyzer(material).elastic_buckling_stress(geometry)
    if elastic > .5 * material.yield_strength:
        return not_evaluated("Nominal or measured inelastic capacity lacks primary citations and independent benchmarks")
    result = assess_plate_uniform_loss(
        geometry, material, built - current, fca_mm=0., sigma_x=sx, sigma_y=sy,
        tau=tau, gamma_m=number(block, "gamma_m", positive=True),
    )
    if not math.isfinite(result.utilization):
        raise ValueError("buckling utilization is non-finite")
    return {"status": "ACCEPT" if result.passes else "RENEW",
            "utilization": result.utilization,
            "remaining_thickness_mm": result.remaining_thickness_mm,
            "capacity_retained_fraction": result.capacity_retained_frac,
            "source": block["source"],
            "basis": "NACA TN 3781 equations (1), (20), elastic compression only",
            "code_qualified": False,
            "limitation": "Uniform simply-supported plate only; shear, inelastic capacity, "
                          "stiffener wastage and transverse compression not evaluated"}
