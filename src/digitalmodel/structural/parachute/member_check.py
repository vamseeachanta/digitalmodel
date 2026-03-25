"""
ABOUTME: Member and connection acceptability checks for frame analysis
ABOUTME: Von Mises, ASME combined stress, unity ratios, bolt/weld/pin checks
"""

import math
from dataclasses import dataclass
from typing import Dict, List


@dataclass
class MemberResult:
    """Result of member stress check."""
    member_id: int
    label: str
    von_mises_psi: float
    allowable_psi: float
    unity: float
    passed: bool


def tube_stress(
    axial: float, shear: float, moment: float,
    A: float, I: float, od: float,
) -> Dict[str, float]:
    """
    Compute stress components in a circular hollow tube member.

    Args:
        axial: Axial force (lbs, tension positive)
        shear: Shear force (lbs)
        moment: Bending moment (in-lbs)
        A: Cross-sectional area (in^2)
        I: Moment of inertia (in^4)
        od: Outer diameter (in)

    Returns:
        Dict with axial, bending, shear_avg stresses (psi)
    """
    sigma_axial = axial / A if A > 0 else 0.0
    sigma_bending = moment * (od / 2) / I if I > 0 else 0.0
    # Average shear on half the cross-section (thin-walled approx)
    tau_avg = 2.0 * shear / A if A > 0 else 0.0

    return {
        "axial": sigma_axial,
        "bending": sigma_bending,
        "shear_avg": tau_avg,
    }


def von_mises_tube(
    sigma_axial: float,
    sigma_bending: float,
    tau: float,
) -> float:
    """
    Von Mises stress for tube under combined axial + bending + shear.

    Worst case: axial and bending stresses add at extreme fiber.
    sigma_vm = sqrt((sigma_a + sigma_b)^2 + 3*tau^2)
    """
    sigma_total = sigma_axial + sigma_bending
    return math.sqrt(sigma_total**2 + 3 * tau**2)


def unity_ratio(demand: float, capacity: float) -> float:
    """Unity ratio = demand / capacity. Must be <= 1.0 to pass."""
    if capacity <= 0:
        return float("inf")
    return abs(demand) / capacity


def bolt_shear_check(
    shear_force: float,
    bolt_diameter: float,
    n_bolts: int,
    fv_allowable: float,
) -> Dict[str, float]:
    """
    Single-shear bolt check.

    Args:
        shear_force: Total shear on connection (lbs)
        bolt_diameter: Bolt diameter (in)
        n_bolts: Number of bolts
        fv_allowable: Allowable shear stress (psi)

    Returns:
        Dict with shear_area, demand, capacity, unity
    """
    bolt_area = math.pi / 4 * bolt_diameter**2
    total_area = bolt_area * n_bolts
    demand = abs(shear_force) / total_area if total_area > 0 else float("inf")
    capacity = fv_allowable

    return {
        "shear_area": total_area,
        "demand_psi": demand,
        "capacity_psi": capacity,
        "unity": demand / capacity if capacity > 0 else float("inf"),
    }


def weld_throat_check(
    force: float,
    weld_length: float,
    throat: float,
    fw_allowable: float,
) -> Dict[str, float]:
    """
    Fillet weld throat stress check.

    Args:
        force: Force on weld (lbs)
        weld_length: Total weld length (in)
        throat: Effective throat thickness (in)
        fw_allowable: Allowable weld stress (psi)
    """
    weld_area = weld_length * throat
    demand = abs(force) / weld_area if weld_area > 0 else float("inf")

    return {
        "weld_area": weld_area,
        "demand_psi": demand,
        "capacity_psi": fw_allowable,
        "unity": demand / fw_allowable if fw_allowable > 0 else float("inf"),
    }


def pin_shear_check(
    shear_force: float,
    pin_diameter: float,
    n_shear_planes: int,
    fv_allowable: float,
) -> Dict[str, float]:
    """
    Pin shear check (single or double shear).

    Args:
        shear_force: Total shear on pin (lbs)
        pin_diameter: Pin diameter (in)
        n_shear_planes: Number of shear planes (1 or 2)
        fv_allowable: Allowable shear stress (psi)
    """
    pin_area = math.pi / 4 * pin_diameter**2
    total_area = pin_area * n_shear_planes
    demand = abs(shear_force) / total_area if total_area > 0 else float("inf")

    return {
        "shear_area": total_area,
        "demand_psi": demand,
        "capacity_psi": fv_allowable,
        "unity": demand / fv_allowable if fv_allowable > 0 else float("inf"),
    }


def check_all_members(
    member_forces: List[Dict],
    section: Dict[str, float],
    material: Dict[str, float],
) -> List[Dict]:
    """
    Run von Mises stress check on all members.

    Args:
        member_forces: List of member force dicts from frame solver
        section: Section properties dict (A, I, OD)
        material: Material dict (fy_psi)

    Returns:
        List of result dicts with unity ratios and pass/fail
    """
    fy = material["fy_psi"]
    allowable = 0.6 * fy  # ASD basis

    results = []
    for mf in member_forces:
        # Use worst-case end (max absolute values)
        axial = max(abs(mf["axial_i"]), abs(mf["axial_j"]))
        shear = max(abs(mf["shear_i"]), abs(mf["shear_j"]))
        moment = max(abs(mf["moment_i"]), abs(mf["moment_j"]))

        stresses = tube_stress(
            axial=axial, shear=shear, moment=moment,
            A=section["A"], I=section["I"], od=section["OD"],
        )

        vm = von_mises_tube(
            sigma_axial=stresses["axial"],
            sigma_bending=stresses["bending"],
            tau=stresses["shear_avg"],
        )

        ur = unity_ratio(vm, allowable)

        results.append({
            "id": mf["id"],
            "label": mf.get("label", f"member_{mf['id']}"),
            "axial_psi": stresses["axial"],
            "bending_psi": stresses["bending"],
            "shear_psi": stresses["shear_avg"],
            "von_mises_psi": vm,
            "allowable_psi": allowable,
            "unity_vm": ur,
            "pass": ur <= 1.0,
        })

    return results
