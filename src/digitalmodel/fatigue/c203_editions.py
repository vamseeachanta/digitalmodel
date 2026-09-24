# ABOUTME: DNV-RP-C203 edition -> S-N table-ID map, the single source for table
# ABOUTME: citations in the fatigue package (#2161). Labels only; no curve values.
"""DNV-RP-C203 edition and table identifiers.

The S-N curve tables moved between editions:

========  =========  ================  =============  ==============
Edition   In air     Seawater with CP  Tubular joints Free corrosion
========  =========  ================  =============  ==============
2011      Table 2-1  Table 2-2         (not mapped)   Table 2-3
2019      Table 2-1  Table 2-2         Table 2-3      Table 2-4
2021      Table 2-1  Table 2-2         Table 2-3      Table 2-4
========  =========  ================  =============  ==============

2021 is the amendment of the 2019 edition and keeps its table layout.
Weld detail classification sits in Appendix A (Tables A-1 to A-10) in both
layouts.

:data:`DNV_RP_C203_IMPLEMENTED_EDITION` is the edition whose tables the
S-N library in :mod:`digitalmodel.fatigue.sn_library` and
:mod:`digitalmodel.fatigue.sn_curves` follows for table numbering. The
numeric correspondence of the stored values to that edition is not
verified here; the free-corrosion values are known to differ (#2165).
"""

from __future__ import annotations

from typing import Final, Mapping

#: Edition whose S-N tables the fatigue package implements.
DNV_RP_C203_IMPLEMENTED_EDITION: Final[str] = "2021"

_LAYOUT_2019: Final[Mapping[str, str]] = {
    "air": "Table 2-1",
    "seawater_cp": "Table 2-2",
    "tubular_joint": "Table 2-3",
    "free_corrosion": "Table 2-4",
}

#: S-N table ID by edition and environment. Unmapped entries are not
#: established here and raise ``KeyError`` rather than guess.
DNV_RP_C203_SN_TABLES: Final[Mapping[str, Mapping[str, str]]] = {
    "2011": {
        "air": "Table 2-1",
        "seawater_cp": "Table 2-2",
        "free_corrosion": "Table 2-3",
    },
    "2019": _LAYOUT_2019,
    "2021": _LAYOUT_2019,
}

#: Location of the weld detail classification tables (2011 and 2019/2021).
DNV_RP_C203_DETAIL_CLASSIFICATION: Final[str] = "Appendix A (Tables A-1 to A-10)"


def c203_sn_table(
    environment: str, edition: str = DNV_RP_C203_IMPLEMENTED_EDITION
) -> str:
    """Return the DNV-RP-C203 table ID holding ``environment`` curves in ``edition``.

    Raises ``KeyError`` for an edition or environment that is not mapped.
    """
    try:
        return DNV_RP_C203_SN_TABLES[edition][environment]
    except KeyError:
        raise KeyError(
            f"DNV-RP-C203 table for environment={environment!r}, "
            f"edition={edition!r} is not mapped"
        ) from None


def c203_label(edition: str = DNV_RP_C203_IMPLEMENTED_EDITION) -> str:
    """``"DNV-RP-C203 (2021)"``-style label for ``edition``.

    Raises ``KeyError`` for an edition that is not mapped.
    """
    if edition not in DNV_RP_C203_SN_TABLES:
        raise KeyError(f"DNV-RP-C203 edition {edition!r} is not mapped")
    return f"DNV-RP-C203 ({edition})"
