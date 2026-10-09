"""DNV-RP-B401 and DNV-RP-F103 edition helpers for cathodic-protection calcs.

Edition tokens are the year of the edition. Aliases accepted by the
``normalize_*`` helpers cover the spellings used in YAML design bases and
legacy solver keys (``DNV_rp_b401_2011``, ``b401-2021`` and so on).

B401 "2011" is the October 2010 edition (printed 2011); it normalizes to the
``"2010"`` token. The 2005 edition with 2008 amendments normalizes to
``"2005"``. The June 2017 (DNVGL) and May 2021 (DNV) editions are ``"2017"``
and ``"2021"``.

F103 ``"2010"`` is the October 2010 edition; ``"2019"`` is the September 2019
DNVGL print (a republication of the July 2016 edition, amended May 2021 with
editorial changes only). ``"2021"`` is accepted as an alias of ``"2019"``
(amended print, same tables). ``"2016"`` is accepted as an alias of
``"2019"`` with a ``UserWarning``: the July 2016 print is not on file and the
2019 print republishes it with unchanged content (owner decision D3: keep as
many edition options for clients as possible).
"""

from __future__ import annotations

from typing import Literal
import warnings


Edition = Literal["2005", "2010", "2017", "2021"]
DEFAULT_EDITION: Edition = "2021"
STANDARD_BY_EDITION: dict[Edition, str] = {
    "2005": "DNV-RP-B401 (2005, with 2008 amendments)",
    "2010": "DNV-RP-B401 (October 2010)",
    "2017": "DNVGL-RP-B401 (June 2017)",
    "2021": "DNV-RP-B401 (May 2021)",
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
    "2017-06": "2017",
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


F103Edition = Literal["2010", "2019"]
DEFAULT_F103_EDITION: F103Edition = "2019"
F103_STANDARD_BY_EDITION: dict[F103Edition, str] = {
    "2010": "DNV-RP-F103 (October 2010)",
    "2019": "DNVGL-RP-F103 (September 2019, amended May 2021)",
}

_F103_ALIASES: dict[str, F103Edition] = {
    "2010": "2010",
    "dnv-rp-f103-2010": "2010",
    "dnv_rp_f103_2010": "2010",
    "f103-2010": "2010",
    "f103_2010": "2010",
    "2019": "2019",
    "2019-09": "2019",
    "dnv-rp-f103-2019": "2019",
    "dnv_rp_f103_2019": "2019",
    "dnvgl-rp-f103-2019": "2019",
    "dnvgl_rp_f103_2019": "2019",
    "f103-2019": "2019",
    "f103_2019": "2019",
    # The May 2021 amended print carries the same tables as September 2019.
    "2021": "2019",
    "2021-05": "2019",
    "2019-2021": "2019",
    "2019_2021": "2019",
    "dnv-rp-f103-2021": "2019",
    "dnv_rp_f103_2021": "2019",
    "f103-2021": "2019",
    "f103_2021": "2019",
}

# Tokens naming the July 2016 DNVGL print, which is not on file. The September
# 2019 print republishes it with unchanged content, so these normalize to
# "2019" with a warning.
_F103_SUPERSEDED_2016: frozenset[str] = frozenset(
    {
        "2016",
        "2016-07",
        "dnv-rp-f103-2016",
        "dnv_rp_f103_2016",
        "dnvgl-rp-f103-2016",
        "dnvgl_rp_f103_2016",
        "f103-2016",
        "f103_2016",
    }
)


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

    ``None`` warns and defaults to DNV-RP-F103 2019, the latest edition on
    file (owner decision 2026-09-27, epic #2206; it was 2010 before, pass
    ``"2010"`` to reproduce earlier results). ``"2021"``
    normalizes to ``"2019"`` (the May 2021 amended print of the September
    2019 edition). ``"2016"`` warns and normalizes to ``"2019"``: the July
    2016 DNVGL print is not on file and the 2019 print republishes it with
    unchanged content.
    """
    if edition is None:
        warnings.warn(
            "No DNV-RP-F103 edition supplied; defaulting to DNV-RP-F103 2019.",
            UserWarning,
            stacklevel=stacklevel,
        )
        return DEFAULT_F103_EDITION

    normalized = edition.strip().lower()
    if normalized in _F103_SUPERSEDED_2016:
        warnings.warn(
            f"DNV-RP-F103 edition {edition!r}: the DNVGL-RP-F103 July 2016 print "
            "is not on file; using the September 2019 republication, which "
            "carries the same tables (edition '2019').",
            UserWarning,
            stacklevel=stacklevel,
        )
        return "2019"
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
