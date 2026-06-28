# ABOUTME: Engineering code references (standard + edition) carried in strength
# ABOUTME: results so every capacity / FFS verdict states its governing code.
"""Governing-code references for strength and fitness-for-service methods.

Each strength/capacity result carries a ``code_reference`` string (the
:attr:`CodeReference.label`) so the user or inspector sees the governing code and
edition in the returned object — not only in the docstring.  This module is the
single source of truth for those references; the codes register (issue #1093)
enumerates the same objects so the documentation cannot drift from what the code
actually stamps on results.
"""
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class CodeReference:
    """A governing engineering standard and edition.

    Attributes:
        standard: Standard designation (e.g. ``"ASME B31G"``).
        edition: Edition / year (e.g. ``"2012"``); may be empty.
        title: Optional short title of the standard.
    """

    standard: str
    edition: str = ""
    title: str = ""

    @property
    def label(self) -> str:
        """Human-readable reference, e.g. ``"ASME B31G (2012)"``."""
        return f"{self.standard} ({self.edition})" if self.edition else self.standard

    def __str__(self) -> str:
        return self.label


# ---------------------------------------------------------------------------
# Remaining-strength / corroded pipe
# ---------------------------------------------------------------------------
ASME_B31G = CodeReference(
    "ASME B31G", "2012",
    "Manual for Determining the Remaining Strength of Corroded Pipelines")
DNV_RP_F101 = CodeReference(
    "DNV-RP-F101", "2021", "Corroded Pipelines")

# ---------------------------------------------------------------------------
# Fitness-for-service
# ---------------------------------------------------------------------------
API_579 = CodeReference(
    "API 579-1/ASME FFS-1", "2021", "Fitness-For-Service")
BS_7910 = CodeReference(
    "BS 7910", "2019",
    "Guide to methods for assessing the acceptability of flaws in "
    "metallic structures")

# ---------------------------------------------------------------------------
# Buckling / plated structures
# ---------------------------------------------------------------------------
DNV_RP_C201 = CodeReference(
    "DNV-RP-C201", "2010", "Buckling Strength of Plated Structures")
EUROCODE_3 = CodeReference(
    "EN 1993-1-1", "2005", "Eurocode 3: Design of steel structures")

# ---------------------------------------------------------------------------
# Pipe / riser wall thickness & pressure containment
# ---------------------------------------------------------------------------
API_RP_1111 = CodeReference(
    "API RP 1111", "2015",
    "Design, Construction, Operation and Maintenance of Offshore Hydrocarbon "
    "Pipelines (Limit State Design)")
API_RP_2RD = CodeReference("API RP 2RD", "2013", "Dynamic Risers for FPS")
API_STD_2RD = CodeReference("API STD 2RD", "2013", "Dynamic Risers for FPS")
DNV_ST_F201 = CodeReference("DNV-ST-F201", "2021", "Dynamic Risers")
DNV_ST_F101 = CodeReference("DNV-ST-F101", "2021", "Submarine Pipeline Systems")
PD_8010_2 = CodeReference(
    "PD 8010-2", "2015", "Pipeline systems - Subsea pipelines")
ISO_13623 = CodeReference("ISO 13623", "2017", "Pipeline transportation systems")
ASME_B31_4 = CodeReference(
    "ASME B31.4", "2019", "Pipeline Transportation Systems for Liquids")
ASME_B31_8 = CodeReference("ASME B31.8", "2020", "Gas Transmission Piping")

# ---------------------------------------------------------------------------
# Material standards
# ---------------------------------------------------------------------------
API_5L = CodeReference("API 5L / ISO 3183", "2018", "Line Pipe")
IACS_W11 = CodeReference(
    "IACS UR W11", "", "Normal & Higher Strength Hull Structural Steels")
EN_10025_2 = CodeReference("EN 10025-2", "2019", "Hot rolled structural steel")

#: All references, keyed by attribute name — used by the codes register (#1093).
REGISTER: dict[str, CodeReference] = {
    name: value
    for name, value in list(globals().items())
    if isinstance(value, CodeReference)
}
