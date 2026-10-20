# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Opt-in provenance sidecar for hand checks; no numeric payload is changed.

Equations identify repository implementations. They are not verified standards
requirements. No standards-derived value or resolved Citation is supplied here.
The missing licensed-edition evidence remains explicit rather than fabricated.
"""

from __future__ import annotations


_EQUATIONS = {
    "_bore_area": "A = pi * bore_id**2 / 4",
    "submerged_length_m": "wet = overlap([top_z - length, top_z], z <= 0)",
    "submerged_weight_n": "W = g * ((mass_per_m + rho_c*A)*L - rho_w*V_per_m*wet)",
    "effective_tension_chain": "T_bottom = T_top - W; z_bottom = z_top - L",
    "ring_weight_n": "W_ring = (mass - rho_w*volume*indicator(z < 0))*g",
    "tension_references": "T_riser_top = T_vertical - W_ring; accumulate riser then stack",
    "tensioned_beam_periods": "(Kb + Kg + Kspring)*q = omega**2*M*q; P = 2*pi/omega",
    "wkb_string_periods": "P_n = (2/n)*sum(L/sqrt(mean(T_top,T_bottom)/mass_per_m))",
    "_split": "n = max(1, ceil(length/max_len - epsilon))",
    "tensioner_lateral_stiffness_n_m": "K_x = sum((T_line/L_line)*(1 - u_x**2))",
    "beam_model": "Hermite elements with section mass, bore contents and wet added mass",
    "reference_periods": "beam_model + joint rotational springs + ring mass and lateral spring",
    "wkb_reference_periods": "WKB sum from tension ring to lower flex joint",
}

_LIMITATIONS = {
    "_bore_area": "Circular main bore; geometry identity only.",
    "submerged_length_m": "Vertical section and flat mean sea level only.",
    "submerged_weight_n": "Uniform section and fluid densities; conventional gravity in hand_checks.G.",
    "effective_tension_chain": "Vertical static balance, not a general pressure/end-load verification.",
    "ring_weight_n": "Binary immersion by ring elevation; partial ring immersion is not represented.",
    "tension_references": "Positive stack bore/contents differs from generated drilling stack; issue 2532.",
    "tensioned_beam_periods": "Linear Hermite beam, mean element tension, consistent mass; numerical inertia regularization.",
    "wkb_string_periods": "Information only; positive, slowly varying tension; no bending stiffness.",
    "_split": "Mesh-count epsilon is a numerical convention, not an engineering criterion.",
    "tensioner_lateral_stiffness_n_m": "Constant line tension, fixed anchors, linearized geometry only.",
    "beam_model": "Inner-barrel axial-stiffness cutoff and mesh controls are implementation conventions.",
    "reference_periods": "Line tensioners only; rigid stack; no solver qualification is established.",
    "wkb_reference_periods": "Information only; excludes the inner barrel and ring coupling.",
}


def hand_check_provenance() -> dict:
    """Return fresh, JSON-compatible provenance, independently of solver execution.

    ``standards_status`` remains NOT_EVALUATED even where an analytic identity
    has an independent reference. Establishing standards applicability requires
    edition, clause title, equation mapping and a resolved Citation sidecar.
    This inventory cannot be used as a standards-compliance verdict.
    """
    return {
        "standards_status": "NOT_EVALUATED",
        "standards_compliance_established": False,
        "reason": "Held-edition clause titles, applicability and Citation resolution are not established.",
        "citations": [],
        "standards_context": [
            {"code_id": code, "edition": None, "clause_title": None, "status": "UNVERIFIED"}
            for code in ("API-RP-16Q", "ISO-13624-1")
        ],
        "context_paths": [
            "src/digitalmodel/drilling_riser/global_model/hang_off.py",
            "src/digitalmodel/drilling_riser/global_model/events.py",
            "src/digitalmodel/drilling_riser/stackup.py",
        ],
        "methods": {
            name: {
                "basis": "implementation_identity",
                "equation": equation,
                "source": f"src/digitalmodel/drilling_riser/global_model/hand_checks.py::{name}",
                "standards_status": "NOT_EVALUATED",
                "limitations": _LIMITATIONS[name],
            }
            for name, equation in _EQUATIONS.items()
        },
        "independent_references": {
            "clamped_beam": {
                "title": "MIT OpenCourseWare, Mechanics and Materials II, Applications: Beam Bending, Buckling and Vibration",
                "year": "2004",
                "locator": "Lecture 2, page 15",
                "url": "https://ocw.mit.edu/courses/2-002-mechanics-and-materials-ii-spring-2004/resources/lec2/",
                "characteristic_equation": "cosh(b) * cos(b) = 1",
                "derivation_status": "derived_from_published_ODE",
                "test": "tests/drilling_riser/global_model/test_hand_checks.py::test_stiff_end_springs_approach_clamped_beam",
                "limitations": "Source supplies the beam ODE and general solution; fixed-end determinant is derived in the test, not quoted from a table.",
            },
        },
    }
