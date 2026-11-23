"""RAO phase / heading conventions (digitalmodel #1358).

There is no phase-convention normalisation anywhere in the RAO parsers today —
``aqwa_lis_parser`` and ``orcaflex_yml_parser`` both store phase **raw** (deg).
AQWA and OrcaFlex differ in phase sign (lead vs lag) and heading direction, so a
forecast that mixes sources, or transfers a wave phased at a sensing point, must
pin a single internal convention. This module *defines and enforces* it.

Canonical internal convention (per owner decision 2026-07-04, "normalise
AQWA -> OrcaFlex"):

- Wave elevation:  ``eta = a * cos(omega*t + phi)``  (cosine, phase in rad).
- RAO transfer:    ``x = a * |H| * cos(omega*t + phi + arg H)`` where
  ``arg H`` is a **phase lead** of the response over the incident wave.
- Heading:         *going-to*, degrees from +x.

OrcaFlex displacement-RAO phase is taken as the canonical lead. AQWA reports a
phase **lag**, so AQWA phase is negated to reach the canonical lead.

IMPORTANT: the AQWA sign is an explicit, documented assumption pending
validation against a matched-vessel dataset (no such fixture exists in-repo).
It is centralised here so a single constant flips it if a validation says so.
"""

from __future__ import annotations

from enum import Enum

import numpy as np


class RAOSource(Enum):
    """Origin of an RAO dataset — selects the phase-normalisation rule."""

    ORCAFLEX = "orcaflex"   # canonical
    AQWA = "aqwa"           # phase lag -> negated to canonical lead
    CANONICAL = "canonical"  # already in internal convention (e.g. analytic)


# Per-source multiplier applied to the RAO phase to reach the canonical lead.
# Centralised so a validated correction is a one-line change.
_PHASE_SIGN = {
    RAOSource.ORCAFLEX: +1.0,
    RAOSource.AQWA: -1.0,
    RAOSource.CANONICAL: +1.0,
}


def normalize_phase_deg(phase_deg: np.ndarray, source: RAOSource) -> np.ndarray:
    """Return RAO phase (deg) in the canonical lead convention.

    Parameters
    ----------
    phase_deg:
        Raw RAO phase array as parsed, in degrees.
    source:
        Which solver produced it (selects the sign rule).
    """
    return _PHASE_SIGN[source] * np.asarray(phase_deg, dtype=float)


def heading_going_to_deg(heading_deg: float, coming_from: bool) -> float:
    """Convert a heading to the internal *going-to* convention (deg).

    Parameters
    ----------
    heading_deg:
        Heading in the source convention.
    coming_from:
        True if the source measures the direction the wave comes *from*
        (add 180 deg to get going-to); False if already going-to.
    """
    h = heading_deg + (180.0 if coming_from else 0.0)
    return float(h % 360.0)
