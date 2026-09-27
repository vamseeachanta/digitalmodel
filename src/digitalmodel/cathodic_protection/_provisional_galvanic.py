"""Provisional literature-sourced constants for the galvanic and ICCP models.

Issue #2247 (owner direction 2026-09-27): the governing standards (NACE
SP0572, ISO 15589-1, BS PD 6484, NACE SP0169) are not on file. Every
default used by the galvanic-corrosion and ICCP anode-life models is
therefore taken from openly published literature (course notes, public
DoD technical papers, manufacturer datasheets) and carried as a
:class:`ProvisionalValue` with ``provisional=True`` and the standard it is
waiting on. Nothing in this module comes from a standard.

A stray-current lane defines the same record in ``_provisional.py``; the
two are to be unified at merge (same field names and order).
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class ProvisionalValue:
    """A constant taken from open literature, pending verification.

    Attributes
    ----------
    value : float
        Numerical value in ``units``.
    units : str
        Units of ``value``.
    source : str
        Literature source: author, title, year, URL or DOI (non-empty).
    note : str
        How the value was read from the source (range end chosen, unit
        conversion, table/section).
    provisional : bool
        Always ``True`` until checked against ``pending_standard``.
    pending_standard : str
        Standard (and clause, if known) that must confirm or replace it.
    """

    value: float
    units: str
    source: str
    note: str = ""
    provisional: bool = True
    pending_standard: str = ""

    def __post_init__(self) -> None:
        if not isinstance(self.source, str) or not self.source.strip():
            raise ValueError("ProvisionalValue.source must be a non-empty string")
        if not isinstance(self.units, str) or not self.units.strip():
            raise ValueError("ProvisionalValue.units must be a non-empty string")


# --- Literature sources (author, title, year, URL) ---------------------------

SRC_DOD_TP16 = (
    "U.S. DoD Tri-Service Electrical Working Group (TSEWG), 'Electrical "
    "Technical Paper 16: Impressed Current Anode Material Selection and Design "
    "Considerations (non-mandatory)', March 2017, "
    "https://nibs-s3-wbdg3-production.s3.us-east-1.amazonaws.com/FFC/DOD/STC/tsewg_tp16.pdf"
)
SRC_USNA_EN380 = (
    "U.S. Naval Academy, EN380 course notes, 'Appendix: Cathodic Protection "
    "Design' (after G. Swain class notes, 1996), Table 7.1, n.d., "
    "https://www.usna.edu/NAOE/_files/documents/Courses/EN380/Course_Notes/"
    "zAppendix_A_Cathodic_Protection_Design.pdf"
)
SRC_GCP_HSCI = (
    "German Cathodic Protection (GCP), 'Impressed Current Anodes - Silicon iron "
    "anodes', datasheet 04-200-R1, n.d., "
    "https://www.gcp.de/wp-content/uploads/04-200-Silicon-iron-anodes.pdf"
)
SRC_CPC_MMO = (
    "Cathodic Protection Co. Ltd, 'Datasheet 2.2.1 - Mixed Metal Oxide Tubular "
    "Anodes', Rev. 0, July 2020, "
    "https://www.cathodic.co.uk/wp-content/uploads/"
    "2.2.1-Mixed-Metal-Oxide-Tubular-Anodes-Rev.0-July-2020.pdf"
)
SRC_CNWRA_GALVANIC = (
    "D.S. Dunn and G.A. Cragnolino, 'An Analysis of Galvanic Coupling Effects "
    "on the Performance of High-Level Nuclear Waste Container Materials', "
    "CNWRA 97-010 (US NRC), August 1997, eqs. 2-2, 2-3, 2-26 and Fig. 2-4, "
    "https://www.nrc.gov/docs/ML0402/ML040200062.pdf"
)

LB_TO_KG = 0.45359237
FT2_TO_M2 = 0.09290304
