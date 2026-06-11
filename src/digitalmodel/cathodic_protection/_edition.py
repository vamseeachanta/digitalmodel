"""DNV-RP-B401 edition helpers for cathodic-protection calculations."""

from __future__ import annotations

from typing import Literal
import warnings


Edition = Literal["2017", "2021"]
DEFAULT_EDITION: Edition = "2021"
STANDARD_BY_EDITION: dict[Edition, str] = {
    "2017": "DNV-RP-B401 (Oct 2017)",
    "2021": "DNV-RP-B401 (May 2021)",
}

_ALIASES: dict[str, Edition] = {
    "2017": "2017",
    "dnv-rp-b401-2017": "2017",
    "dnv_rp_b401_2017": "2017",
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
        raise ValueError(
            f"Unsupported DNV-RP-B401 edition {edition!r}. "
            "Supported editions are '2017' and '2021'."
        ) from exc


def standard_for_edition(edition: Edition) -> str:
    """Return the report-facing DNV-RP-B401 standard string for an edition."""
    return STANDARD_BY_EDITION[edition]
