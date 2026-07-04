# ABOUTME: Galvanic (dissimilar-metal) compatibility screening via the published
# ABOUTME: MIL-STD-889 anodic-index method with environment-dependent allowable dV.
"""Galvanic compatibility screening — anodic-index method (MIL-STD-889 basis).

Method
------
Each metal (or alloy group) carries a published *anodic index*: its galvanic
potential difference relative to gold (0.00 V), measured in seawater. For a
dissimilar-metal couple the screening statistic is::

    delta_v = |anodic_index(A) - anodic_index(B)|

and the widely published rule-of-thumb allowable difference depends on the
service environment:

======================  ==================  =======================
Environment class       Allowable ``dV``    Typical description
======================  ==================  =======================
``harsh_marine``        0.15 V              marine / outdoor, high
                                            humidity, salt spray
``normal_industrial``   0.25 V              normal outdoor/indoor
                                            storage, temperate
``controlled_indoor``   0.50 V              temperature & humidity
                                            controlled enclosures
======================  ==================  =======================

Couples over the allowable difference need protection: electrical isolation,
coating (coat the **cathode** — coating only the anode concentrates attack at
coating defects), or cathodic protection. The metal with the **larger** anodic
index is the anodic (sacrificial) member of the couple.

Area-ratio principle (published guidance, e.g. MIL-STD-889 and standard
corrosion texts): a **small anode coupled to a large cathode** is the dangerous
configuration — the anodic current density, hence penetration rate, scales with
the cathode/anode area ratio. Fasteners should therefore be the *cathodic*
member of a joint.

Sources (public)
----------------
- MIL-STD-889 (Dissimilar Metals): galvanic series / groupings and the
  anodic-index compatibility approach.
- Widely republished anodic-index table (gold 0.00 V ... magnesium 1.75 V)
  and the 0.15 / 0.25 / 0.50 V environment rule of thumb, as found in
  standard electronics/structural design references.

No operator-proprietary matrix is transcribed here; verdicts are computed
from the public values below.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Mapping, Optional

import pandas as pd

#: Anodic index (volts, relative to gold = 0.00 V) — standard published
#: MIL-STD-889 values for common construction-metal groups. Values are the
#: group values from the widely republished MIL-STD-889 anodic-index table.
ANODIC_INDEX_V: Dict[str, float] = {
    "gold": 0.00,
    "silver": 0.15,
    "nickel": 0.30,               # nickel, solid or plated
    "monel_nickel_copper": 0.30,  # monel / high nickel-copper alloys
    "copper": 0.35,               # copper, solid or plated; high Cu-Ni alloys
    "cupronickel": 0.35,          # copper-nickel alloys (grouped with copper)
    "brass": 0.40,                # brasses (brass-and-bronze group)
    "bronze": 0.45,               # high brasses and bronzes group
    "stainless_316_passive": 0.50,  # 18% chromium type corrosion-resistant steel
    "stainless_304_passive": 0.50,  # 18% chromium type corrosion-resistant steel
    "carbon_steel": 0.85,         # plain carbon and low-alloy steels
    "cast_iron": 0.85,            # iron, wrought/gray/malleable (same group)
    "aluminum_alloy": 0.90,       # wrought Al other than 2000-series; Si-type castings
    "cadmium_plated": 0.95,       # cadmium, plated and chromated
    "zinc": 1.25,                 # zinc wrought / die-cast / plated
    "magnesium": 1.75,            # magnesium and Mg-base alloys
}

#: Environment class -> allowable anodic-index difference (V).
#: Published rule-of-thumb thresholds (see module docstring).
ENVIRONMENTS: Dict[str, float] = {
    "harsh_marine": 0.15,
    "normal_industrial": 0.25,
    "controlled_indoor": 0.50,
}

#: A couple this far (V) over the allowable is still called MARGINAL rather
#: than PROTECT — "borderline, manageable with area-ratio / coating control".
MARGINAL_BAND_V = 0.05

_VERDICT_ORDER = {"OK": 0, "MARGINAL": 1, "PROTECT": 2}

_AREA_RATIO_NOTE = (
    "Avoid a small anode with a large cathode (e.g. anodic fasteners in a "
    "cathodic structure): anodic penetration rate scales with the "
    "cathode/anode area ratio. Make the fastener/small part the cathodic "
    "member, or isolate."
)


@dataclass(frozen=True)
class GalvanicScreenResult:
    """Outcome of screening one dissimilar-metal couple in one environment."""

    metal_a: str
    metal_b: str
    environment: str
    delta_v: float                 #: |anodic index difference| (V)
    allowable_delta_v: float       #: environment allowable (V)
    verdict: str                   #: OK | MARGINAL | PROTECT
    anodic: Optional[str]          #: sacrificial member (None if same index)
    cathodic: Optional[str]        #: protected member (None if same index)
    notes: List[str] = field(default_factory=list)


def _lookup(metal: str) -> float:
    try:
        return ANODIC_INDEX_V[metal]
    except KeyError:
        known = ", ".join(sorted(ANODIC_INDEX_V))
        raise KeyError(f"Unknown metal {metal!r}. Known metals: {known}") from None


def _allowable(environment: str, environments: Mapping[str, float]) -> float:
    try:
        return environments[environment]
    except KeyError:
        known = ", ".join(sorted(environments))
        raise KeyError(
            f"Unknown environment {environment!r}. Known environments: {known}"
        ) from None


def verdict_for_delta(
    delta_v: float,
    allowable_delta_v: float,
    marginal_band_v: float = MARGINAL_BAND_V,
) -> str:
    """Map an anodic-index difference to OK / MARGINAL / PROTECT."""
    if delta_v <= allowable_delta_v + 1e-12:
        return "OK"
    if delta_v <= allowable_delta_v + marginal_band_v + 1e-12:
        return "MARGINAL"
    return "PROTECT"


def screen_couple(
    metal_a: str,
    metal_b: str,
    environment: str = "harsh_marine",
    *,
    environments: Mapping[str, float] = ENVIRONMENTS,
    marginal_band_v: float = MARGINAL_BAND_V,
) -> GalvanicScreenResult:
    """Screen a dissimilar-metal couple with the anodic-index method.

    Parameters
    ----------
    metal_a, metal_b : str
        Keys of :data:`ANODIC_INDEX_V`.
    environment : str
        Key of ``environments`` (default classes in :data:`ENVIRONMENTS`).
    environments : mapping, optional
        Override the environment->allowable-dV table (parameterizable
        thresholds; defaults are the published 0.15/0.25/0.50 V rule).
    marginal_band_v : float, optional
        Width of the MARGINAL band above the allowable (default 0.05 V).

    Returns
    -------
    GalvanicScreenResult
        Verdict is symmetric in (metal_a, metal_b).
    """
    idx_a, idx_b = _lookup(metal_a), _lookup(metal_b)
    allow = _allowable(environment, environments)
    delta_v = abs(idx_a - idx_b)
    verdict = verdict_for_delta(delta_v, allow, marginal_band_v)

    if delta_v > 0.0:
        # larger anodic index = more active = anodic (sacrificial) member
        anodic, cathodic = (
            (metal_a, metal_b) if idx_a > idx_b else (metal_b, metal_a)
        )
    else:
        anodic = cathodic = None

    notes: List[str] = []
    if anodic is not None:
        notes.append(
            f"{anodic} is the anodic (sacrificial) member; {cathodic} is "
            "cathodic (protected)."
        )
        notes.append(_AREA_RATIO_NOTE)
    if verdict == "PROTECT":
        notes.append(
            "Protect: electrically isolate the joint, coat the cathode (not "
            "only the anode), or apply cathodic protection."
        )
    elif verdict == "MARGINAL":
        notes.append(
            "Marginal (within 0.05 V of the allowable): acceptable with "
            "area-ratio control, coatings, or dry/sheltered detailing."
        )

    return GalvanicScreenResult(
        metal_a=metal_a,
        metal_b=metal_b,
        environment=environment,
        delta_v=round(delta_v, 6),
        allowable_delta_v=allow,
        verdict=verdict,
        anodic=anodic,
        cathodic=cathodic,
        notes=notes,
    )


def compatibility_matrix(
    environment: str = "harsh_marine",
    *,
    metals: Optional[List[str]] = None,
    environments: Mapping[str, float] = ENVIRONMENTS,
    marginal_band_v: float = MARGINAL_BAND_V,
) -> pd.DataFrame:
    """Square verdict matrix (OK/MARGINAL/PROTECT) over ``metals``.

    Rows and columns are metal names (default: all of
    :data:`ANODIC_INDEX_V`, in table order); the matrix is symmetric with an
    all-OK diagonal.
    """
    names = list(metals) if metals is not None else list(ANODIC_INDEX_V)
    data = {
        col: [
            screen_couple(
                row, col, environment,
                environments=environments, marginal_band_v=marginal_band_v,
            ).verdict
            for row in names
        ]
        for col in names
    }
    return pd.DataFrame(data, index=names, columns=names)


def verdict_rank(verdict: str) -> int:
    """Severity rank: OK=0 < MARGINAL=1 < PROTECT=2 (for ordering checks)."""
    return _VERDICT_ORDER[verdict]
