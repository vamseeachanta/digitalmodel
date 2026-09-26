"""DNV-RP-B401 and DNV-RP-F103 edition helpers for cathodic-protection calcs.

Edition tokens are the year of the edition. Aliases accepted by the
``normalize_*`` helpers cover the spellings used in YAML design bases and
legacy solver keys (``DNV_rp_b401_2011``, ``b401-2021`` and so on).

B401 "2011" is the October 2010 edition (printed 2011); it normalizes to the
``"2010"`` token. The 2005 edition with 2008 amendments normalizes to
``"2005"``.
"""

from __future__ import annotations

from typing import Literal
import warnings


Edition = Literal["2005", "2010", "2017", "2021"]
DEFAULT_EDITION: Edition = "2021"
STANDARD_BY_EDITION: dict[Edition, str] = {
    "2005": "DNV-RP-B401 (2005, with 2008 amendments)",
    "2010": "DNV-RP-B401 (October 2010)",
    "2017": "DNVGL-RP-B401 (2017)",
    "2021": "DNV-RP-B401 (2021)",
}

_ALIASES: dict[str, Edition] = {
    "2005": "2005",
    "2005-2008": "2005",
    "2005_2008": "2005",
    "2008": "2005",
    "dnv-rp-b401-2005": "2005",
    "dnv_rp_b401_2005": "2005",
    "dnv-rp-b401-2008": "2005",
    "dnv_rp_b401_2008": "2005",
    "b401-2005": "2005",
    "b401_2005": "2005",
    "b401-2008": "2005",
    "b401_2008": "2005",
    "2010": "2010",
    "2011": "2010",
    "dnv-rp-b401-2010": "2010",
    "dnv_rp_b401_2010": "2010",
    "dnv-rp-b401-2011": "2010",
    "dnv_rp_b401_2011": "2010",
    "b401-2010": "2010",
    "b401_2010": "2010",
    "b401-2011": "2010",
    "b401_2011": "2010",
    "2017": "2017",
    "dnv-rp-b401-2017": "2017",
    "dnv_rp_b401_2017": "2017",
    "dnvgl-rp-b401-2017": "2017",
    "dnvgl_rp_b401_2017": "2017",
    "b401-2017": "2017",
    "b401_2017": "2017",
    "2021": "2021",
    "2021-05": "2021",
    "dnv-rp-b401-2021": "2021",
    "dnv-rp-b401-2021-05": "2021",
    "dnv_rp_b401_2021": "2021",
    "dnv_rp_b401_2021_05": "2021",
    "b401-2021": "2021",
    "b401_2021": "2021",
}


F103Edition = Literal["2010", "2016"]
DEFAULT_F103_EDITION: F103Edition = "2010"
F103_STANDARD_BY_EDITION: dict[F103Edition, str] = {
    "2010": "DNV-RP-F103 (October 2010)",
    "2016": "DNVGL-RP-F103 (2016)",
}

_F103_ALIASES: dict[str, F103Edition] = {
    "2010": "2010",
    "dnv-rp-f103-2010": "2010",
    "dnv_rp_f103_2010": "2010",
    "f103-2010": "2010",
    "f103_2010": "2010",
    "2016": "2016",
    "dnv-rp-f103-2016": "2016",
    "dnv_rp_f103_2016": "2016",
    "dnvgl-rp-f103-2016": "2016",
    "dnvgl_rp_f103_2016": "2016",
    "f103-2016": "2016",
    "f103_2016": "2016",
}


def normalize_edition(edition: str | None, *, stacklevel: int = 2) -> Edition:
    """Return the canonical DNV-RP-B401 edition token.

    ``None`` keeps the P1 transition additive by warning and defaulting to the
    router's current B401 behavior, DNV-RP-B401 2021.
    """
    if edition is None:
        warnings.warn(
            "No DNV-RP-B401 edition supplied; defaulting to DNV-RP-B401 2021.",
            UserWarning,
            stacklevel=stacklevel,
        )
        return DEFAULT_EDITION

    normalized = edition.strip().lower()
    try:
        return _ALIASES[normalized]
    except KeyError as exc:
        supported = ", ".join(repr(token) for token in STANDARD_BY_EDITION)
        raise ValueError(
            f"Unsupported DNV-RP-B401 edition {edition!r}. "
            f"Supported editions are {supported}."
        ) from exc


def standard_for_edition(edition: Edition) -> str:
    """Return the report-facing DNV-RP-B401 standard string for an edition."""
    return STANDARD_BY_EDITION[edition]


def normalize_f103_edition(edition: str | None, *, stacklevel: int = 2) -> F103Edition:
    """Return the canonical DNV-RP-F103 edition token.

    ``None`` warns and defaults to DNV-RP-F103 2010, the edition whose tables
    are held in the wiki datasets.
    """
    if edition is None:
        warnings.warn(
            "No DNV-RP-F103 edition supplied; defaulting to DNV-RP-F103 2010.",
            UserWarning,
            stacklevel=stacklevel,
        )
        return DEFAULT_F103_EDITION

    normalized = edition.strip().lower()
    try:
        return _F103_ALIASES[normalized]
    except KeyError as exc:
        supported = ", ".join(repr(token) for token in F103_STANDARD_BY_EDITION)
        raise ValueError(
            f"Unsupported DNV-RP-F103 edition {edition!r}. "
            f"Supported editions are {supported}."
        ) from exc


def f103_standard_for_edition(edition: F103Edition) -> str:
    """Return the report-facing DNV-RP-F103 standard string for an edition."""
    return F103_STANDARD_BY_EDITION[edition]
