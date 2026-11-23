"""
ABOUTME: Radii-of-gyration estimation for vessel mass properties.

Radii of gyration (kxx roll, kyy pitch, kzz yaw) are almost never published for
real vessels, yet diffraction/seakeeping setups need them. This module provides
DOCUMENTED empirical estimates so that an estimated value always carries the
relation it came from -- never a bare invented number (see data/vessels/README.md
"flag, don't fake").

Default relations (ship/monohull form), with the commonly-cited ranges:

- Roll  kxx = 0.35 * beam        (range 0.30-0.40 B; DNV-RP-H103 / Lloyd's guidance)
- Pitch kyy = 0.25 * length_bp   (range 0.24-0.26 Lpp; ~0.25 Loa as a fallback)
- Yaw   kzz = 0.26 * length_bp   (range 0.25-0.27 Lpp; close to kyy)

Column-stabilised and deep-draft forms differ from ship-form and are flagged,
not silently estimated with ship coefficients:

- semisubmersible: roll/pitch gyradii are governed by column spacing, not beam;
  ship-form relations do NOT apply -> returned as caveated estimates.
- spar: pitch/roll dominated by deep draft; ship-form relations do NOT apply.

Each estimate is returned with a human-readable ``basis`` string of the exact
form ``estimated:kxx=0.35*beam`` that callers write straight into the record.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

# Default coefficients (ship/monohull form).
KXX_OVER_BEAM = 0.35
KYY_OVER_LBP = 0.25
KZZ_OVER_LBP = 0.26

# Hull forms for which the ship-form relations above are NOT representative.
_NON_SHIP_FORMS = {
    "semisub",
    "semisubmersible",
    "semi_pontoon",
    "spar",
    "column-stabilised",
    "column-stabilized",
}


@dataclass
class GyradiiEstimate:
    """One estimated radius of gyration plus the relation it came from."""

    value: float
    basis: str  # e.g. "estimated:kxx=0.35*beam"
    caveat: Optional[str] = None

    def as_field(self) -> str:
        """Render as the schema string a record stores, e.g.
        ``estimated:kxx=0.35*beam``. Numeric value lives in a sibling column."""
        return self.basis


def _is_ship_form(vessel_type: str | None) -> bool:
    if not vessel_type:
        return True  # default to ship-form relations
    vt = vessel_type.lower()
    return not any(token in vt for token in _NON_SHIP_FORMS)


def estimate_gyradii(
    beam: float | None,
    length_bp: float | None,
    vessel_type: str | None = None,
) -> dict[str, GyradiiEstimate]:
    """Estimate kxx/kyy/kzz from principal dimensions.

    Parameters
    ----------
    beam : float | None
        Moulded beam [m]. Required for kxx; if missing, kxx is omitted.
    length_bp : float | None
        Length between perpendiculars [m] (use LOA as a documented fallback).
        Required for kyy/kzz; if missing they are omitted.
    vessel_type : str | None
        Used only to attach a caveat for non-ship forms (semisub/spar), where
        the ship-form coefficients are not representative.

    Returns
    -------
    dict[str, GyradiiEstimate]
        Keys among ``kxx_roll``, ``kyy_pitch``, ``kzz_yaw`` for the inputs that
        were available. Empty dict if neither beam nor length_bp is usable.
    """
    out: dict[str, GyradiiEstimate] = {}
    ship_form = _is_ship_form(vessel_type)
    caveat = (
        None
        if ship_form
        else (
            f"ship-form gyradii relation applied to non-ship form "
            f"'{vessel_type}'; column-stabilised/deep-draft gyradii are "
            f"governed by column spacing/draft -- treat as a rough placeholder"
        )
    )

    if beam is not None and beam > 0:
        out["kxx_roll"] = GyradiiEstimate(
            value=round(KXX_OVER_BEAM * beam, 3),
            basis=f"estimated:kxx={KXX_OVER_BEAM}*beam",
            caveat=caveat,
        )
    if length_bp is not None and length_bp > 0:
        out["kyy_pitch"] = GyradiiEstimate(
            value=round(KYY_OVER_LBP * length_bp, 3),
            basis=f"estimated:kyy={KYY_OVER_LBP}*length_bp",
            caveat=caveat,
        )
        out["kzz_yaw"] = GyradiiEstimate(
            value=round(KZZ_OVER_LBP * length_bp, 3),
            basis=f"estimated:kzz={KZZ_OVER_LBP}*length_bp",
            caveat=caveat,
        )
    return out
