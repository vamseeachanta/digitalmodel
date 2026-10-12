"""Evidence locators from the 2026-09-29 CP crosswalk; never qualification.

Wiki: wikis/engineering-standards/wiki/standards/cathodic-protection-criteria-evidence.md
Unaudited original-source claims retain class 'none'.
"""

EN_TABLE_1 = (
    "EN 50162:2004, SIST official preview, Table 1 / s6.1.1, printed p10 / PDF p12; "
    "https://preview.sist.si/sist-preview/110/15df26f5800b4c2d8c29c7462fd52189/"
    "SIST-EN-50162-2005.pdf"
)
BRENNA = (
    "Brenna et al., AC Corrosion of Carbon Steel under Cathodic Protection Condition: "
    "Assessment, Criteria and Mechanism. A Review (2020), "
    "https://pmc.ncbi.nlm.nih.gov/articles/PMC7254396/"
)
PATERLINI = (
    "Paterlini et al., Protection Criteria of Cathodically Protected Pipelines "
    "Under AC Interference (2025), p3; "
    "https://doi.org/10.3390/cmd6010007"
)
CROSSWALK = (
    "Cathodic-protection criteria evidence crosswalk (2026-09-29), "
    "unpaginated, Stray-current checklist / ICCP materials; "
    "https://github.com/vamseeachanta/llm-wiki/blob/main/"
    "wikis/engineering-standards/wiki/standards/cathodic-protection-criteria-evidence.md"
)
TP16 = (
    "DoD TSEWG, Electrical Technical Paper 16: Impressed Current Anode Material "
    "Selection and Design Considerations (March 2017); "
    "https://www.wbdg.org/FFC/DOD/STC/tsewg_tp16.pdf"
)


def iccp_evidence(source: str, note: str) -> tuple[str, str]:
    """Map reviewed TP-16 locators; explicitly retain unaudited supplier gaps."""
    if "TP16 Table 5" in note:
        return "reproduced-by-secondary", (
            TP16 + "; Table 5 p6: SG 7 for chromium-bearing HSCBCI only; "
            "generic HSCI alloy/density convention remains unqualified"
        )
    if "Electrical Technical Paper 16" in source or "Technical Paper 16:" in source:
        if "s.1.1.4.3" in note:
            locator = "s1.1.4.3 p4; uniform consumption within discharge limits"
        elif "Table 3" in note:
            locator = "Table 3 p4; environment-specific discharge"
        elif "s.1.0" in note:
            locator = "s1.0 p1; approximate scrap-steel guidance"
        elif "s.5 worked examples" in note:
            locator = "s5.2.2.3 p56 and s5.9.2.6 p117; examples, not universal range"
        else:
            raise ValueError("unmapped TP-16 evidence locator")
        return "reproduced-by-secondary", TP16 + "; " + locator
    return "none", source + "; page unverified in crosswalk; " + CROSSWALK
