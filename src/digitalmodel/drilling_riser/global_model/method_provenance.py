"""Mechanics provenance sidecar; no standards requirement is qualified here.

The method note supplies reproducible derivations, assumptions and examples.
An empty Citation list is deliberate: bibliographic mechanics references and
unverified standard candidates are not resolved standards Citation records.
"""

from __future__ import annotations

from digitalmodel.citations.schema import CitationResolutionError

_NOTE = "docs/domains/drilling_riser/hand-check-methods.html"
_BEAM = "https://euphonics.org/3-2-1-bending-beams-and-free-free-modes/"
_STRING = (
    "https://openstax.org/books/university-physics-volume-1/pages/"
    "16-6-standing-waves-and-resonance"
)

# Equations below are implementation identities or approximations. Their
# coefficients are derived in the linked note, not attributed to API or ISO.
_METHODS = {
    "submerged_length_m": (
        "wet = clip(L - max(z_top, 0), 0, L)", "m", "geometry",
        "Vertical straight section; mean sea level z=0; positive length.",
    ),
    "submerged_weight_n": (
        "W = g*((mu + rho_c*pi*d_i**2/4)*L - rho_w*V_per_m*wet)",
        "N", "weight", "Uniform mass, bore contents and displacement per length.",
    ),
    "effective_tension_chain": (
        "Te_bottom = Te_top - W; z_bottom = z_top - L", "N, m", "weight",
        "Static vertical equilibrium; input is effective tension, not wall tension.",
    ),
    "ring_weight_n": (
        "W_ring = g*(M_ring - rho_w*V_ring*I(z_ring < 0))", "N", "weight",
        "Lumped ring; fully wet below sea level, fully dry at or above it.",
    ),
    "tension_references": (
        "Te_top = T_vertical - W_ring; chain equilibrium down riser and stack",
        "N", "weight", "Stack fluid is already in mass; stack bore area is zero.",
    ),
    "tensioned_beam_periods": (
        "(Kb + Kg + K_spring)*q = omega**2*M*q; period = 2*pi/omega",
        "s", "beam", "Hermite bending; consistent mass; element-average effective "
        "tension; translational pins; optional rotational clamps and springs.",
    ),
    "wkb_string_periods": (
        "period_n = 2/n * sum(L/sqrt(Te_average/mu))", "s", "string",
        "Information only; positive, slowly varying tension and mass; no bending.",
    ),
    "tensioner_lateral_stiffness_n_m": (
        "kx = sum(F_line/length * (1 - ux**2))", "N/m", "tensioner",
        "Fixed sheaves; constant line force; positive vertical separation; "
        "equally spaced azimuths; in-plane derivative at the ring.",
    ),
    "beam_model": (
        "mu = mu_structure + rho_c*A_bore + Ca*rho_w*V_per_m*wet_fraction",
        "m, N*m^2, N, kg/m", "assembly",
        "Ring-to-flex-joint topology; added-mass coefficient is an input; "
        "inner-barrel slip/load-carrying split is a model convention.",
    ),
    "reference_periods": (
        "beam eigenproblem with flex-joint rotation springs and ring mass/spring",
        "s", "assembly", "Line tensioners only; stack rigid; no solver validation.",
    ),
    "wkb_reference_periods": (
        "string travel time from ring to lower flex joint", "s", "string",
        "Riser below ring only; informational estimate, not a qualification gate.",
    ),
}


def hand_check_provenance(*, require_standard: bool = False) -> dict:
    """Return a JSON-compatible sidecar without changing numeric return values.

    ``require_standard=True`` fails closed: no method has a verified standard
    edition, clause title, applicability and resolvable Citation. This flag does
    not qualify a calculation. A future standards implementation must validate
    its Citation at calculation time before emitting any qualified result.
    """
    if require_standard:
        raise CitationResolutionError(
            code_id="API RP 16Q / ISO 13624-1",
            wiki_path="",
            reason="NOT_EVALUATED: edition, clause title, method applicability "
            "and resolved Citation are not established",
        )
    methods = {}
    for name, (equation, units, anchor, assumptions) in _METHODS.items():
        methods[name] = {
            "equation": equation,
            "units": units,
            "assumptions": assumptions,
            "reference": f"{_NOTE}#{anchor}",
            "expected_value_source": f"{_NOTE}#independent-checks",
            "standards_status": "NOT_EVALUATED",
        }
    return {
        "methods": methods,
        "standards_status": "NOT_EVALUATED",
        "citations": [],
        "standard_candidates": [
            {"code_id": code, "revision": None, "clause_title": None,
             "section": None, "source_sibling": "generic",
             "status": "NOT_EVALUATED"}
            for code in ("API RP 16Q", "ISO 13624-1")
        ],
        "mechanics_sources": {"beam": _BEAM, "uniform_string": _STRING},
        "related_methods": {
            "hang_off": "Hard/soft support and gas-spring settings are model "
            "conventions; API/ISO edition and clause applicability unverified.",
            "events": "Drag, wave-drift reduction and anti-recoil closure are "
            "assumed event models; API/ISO requirements unverified.",
            "stackup.minimum_slip_ring_tension": "Existing ABS 2017 locator "
            "and API 2017 edition do not establish clause title or applicability; "
            "no factor or compliance claim is qualified by this sidecar.",
        },
    }
