"""
Weld Detail Classification
===========================

Given a textual description of a weld geometry, return the appropriate
fatigue detail class (S-N curve) per:

- DNV-RP-C203 (2021), Tables 2-1 through 2-8
- AWS D1.1 (2020), Table 2.5
- BS 7608:2014, Table 2

The classification is rule-based with keyword matching and provides
both the class and a confidence indicator.

References
----------
- DNV-RP-C203 (2021), Table 2-1 to 2-8
- AWS D1.1/D1.1M:2020, Table 2.5
- BS 7608:2014+A1:2015, Table 2
"""

from typing import Dict, List, Optional, Tuple
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class WeldDetail(BaseModel):
    """Description of a weld detail for classification.

    Attributes
    ----------
    description : str
        Free-text description of the weld geometry.
    joint_type : str
        One of: ``"butt"``, ``"fillet"``, ``"cruciform"``, ``"tubular"``,
        ``"attachment"``, ``"stiffener"``, ``"cope_hole"``.
        Default ``""`` (auto-detect from description).
    ground_flush : bool
        Whether the weld cap is ground flush. Default False.
    full_penetration : bool
        Whether it's a full penetration weld. Default True.
    inspected : bool
        Whether the weld has been NDE inspected. Default False.
    transverse_load : bool
        Whether the weld is loaded transversely. Default True.
    """
    description: str = ""
    joint_type: str = ""
    ground_flush: bool = False
    full_penetration: bool = True
    inspected: bool = False
    transverse_load: bool = True


class ClassificationResult(BaseModel):
    """Result of weld detail classification.

    Attributes
    ----------
    dnv_class : str
        DNV-RP-C203 detail category (e.g. "D", "F", "W3").
    aws_category : str
        AWS D1.1 category (e.g. "C", "D", "E").
    bs_class : str
        BS 7608 class (e.g. "D", "F", "W1").
    confidence : str
        ``"high"``, ``"medium"``, or ``"low"`` — reliability of the
        automated classification.
    dnv_table : str
        Reference to the DNV table used.
    notes : str
        Additional guidance or caveats.
    """
    dnv_class: str
    aws_category: str = ""
    bs_class: str = ""
    confidence: str = "medium"
    dnv_table: str = ""
    notes: str = ""


# ---------------------------------------------------------------------------
# Classification rules — DNV-RP-C203 Tables 2-1 through 2-8
# ---------------------------------------------------------------------------

# Each rule: (keywords, joint_types, conditions) -> (dnv, aws, bs, table, note)
_RULES: List[dict] = [
    # Table 2-1: Base material
    {
        "keywords": ["base metal", "base material", "rolled", "plate", "no weld"],
        "joint_types": [""],
        "conditions": lambda d: not any(
            w in d.description.lower() for w in ["weld", "fillet", "butt"]
        ),
        "dnv": "B1", "aws": "A", "bs": "B",
        "table": "Table 2-1", "note": "Base material, no welds",
    },
    # Table 2-2: Butt welds, ground flush, inspected
    {
        "keywords": ["butt weld", "butt joint", "full penetration butt"],
        "joint_types": ["butt"],
        "conditions": lambda d: d.ground_flush and d.inspected and d.full_penetration,
        "dnv": "C", "aws": "B", "bs": "C",
        "table": "Table 2-2", "note": "Butt weld, ground flush, NDE inspected",
    },
    # Table 2-2: Butt welds, ground flush, not inspected
    {
        "keywords": ["butt weld", "butt joint"],
        "joint_types": ["butt"],
        "conditions": lambda d: d.ground_flush and not d.inspected,
        "dnv": "C1", "aws": "B'", "bs": "C",
        "table": "Table 2-2", "note": "Butt weld, ground flush, not NDE inspected",
    },
    # Table 2-2: Butt welds, as-welded (not ground)
    {
        "keywords": ["butt weld", "butt joint"],
        "joint_types": ["butt"],
        "conditions": lambda d: not d.ground_flush and d.full_penetration,
        "dnv": "D", "aws": "C", "bs": "D",
        "table": "Table 2-2", "note": "Butt weld, as-welded, full penetration",
    },
    # Table 2-3: Transverse butt weld with backing strip
    {
        "keywords": ["backing strip", "backing bar", "permanent backing"],
        "joint_types": ["butt"],
        "conditions": lambda d: True,
        "dnv": "F", "aws": "D", "bs": "F",
        "table": "Table 2-3", "note": "Butt weld on permanent backing strip",
    },
    # Table 2-4: Welded attachments — non-load-carrying fillet weld
    {
        "keywords": ["attachment", "stiffener", "non-load carrying", "bracket"],
        "joint_types": ["attachment", "stiffener", "fillet"],
        "conditions": lambda d: not d.transverse_load,
        "dnv": "E", "aws": "C", "bs": "E",
        "table": "Table 2-4", "note": "Non-load-carrying attachment fillet weld",
    },
    # Table 2-5: Welded attachments — load-carrying fillet weld
    {
        "keywords": ["attachment", "stiffener", "load carrying", "load-carrying"],
        "joint_types": ["attachment", "stiffener", "fillet"],
        "conditions": lambda d: d.transverse_load,
        "dnv": "F1", "aws": "D", "bs": "F2",
        "table": "Table 2-5", "note": "Load-carrying attachment fillet weld",
    },
    # Table 2-5: Fillet welds
    {
        "keywords": ["fillet weld", "fillet", "lap joint"],
        "joint_types": ["fillet"],
        "conditions": lambda d: d.full_penetration,
        "dnv": "F", "aws": "D", "bs": "F",
        "table": "Table 2-5", "note": "Fillet weld, toe failure",
    },
    {
        "keywords": ["fillet weld root", "root failure", "partial penetration"],
        "joint_types": ["fillet"],
        "conditions": lambda d: not d.full_penetration,
        "dnv": "W3", "aws": "E'", "bs": "W1",
        "table": "Table 2-5", "note": "Fillet weld, root failure (partial pen)",
    },
    # Table 2-6: Cruciform joints
    {
        "keywords": ["cruciform", "cross joint", "T-joint"],
        "joint_types": ["cruciform"],
        "conditions": lambda d: d.full_penetration,
        "dnv": "F", "aws": "D", "bs": "F",
        "table": "Table 2-6", "note": "Cruciform joint, full penetration",
    },
    {
        "keywords": ["cruciform", "cross joint"],
        "joint_types": ["cruciform"],
        "conditions": lambda d: not d.full_penetration,
        "dnv": "W3", "aws": "E'", "bs": "W1",
        "table": "Table 2-6", "note": "Cruciform joint, partial penetration",
    },
    # Table 2-7: Cope holes
    {
        "keywords": ["cope hole", "scallop", "access hole", "mouse hole"],
        "joint_types": ["cope_hole"],
        "conditions": lambda d: True,
        "dnv": "E", "aws": "D", "bs": "E",
        "table": "Table 2-7", "note": "Cope hole / weld access hole",
    },
    # Table 2-8: Tubular joints (T-curve)
    {
        "keywords": ["tubular", "tube", "CHS", "circular hollow"],
        "joint_types": ["tubular"],
        "conditions": lambda d: True,
        "dnv": "T", "aws": "ET", "bs": "S1",
        "table": "Table 2-8", "note": "Tubular joint — use T-curve (hotspot stress method)",
    },
]


def classify_weld_detail(detail: WeldDetail) -> ClassificationResult:
    """Classify a weld detail and return the appropriate S-N curve class.

    The classification is performed by matching keywords in the description
    and evaluating geometry conditions against the DNV-RP-C203 tables.

    Parameters
    ----------
    detail : WeldDetail

    Returns
    -------
    ClassificationResult

    Examples
    --------
    >>> detail = WeldDetail(
    ...     description="Butt weld, ground flush",
    ...     joint_type="butt",
    ...     ground_flush=True,
    ...     inspected=True,
    ... )
    >>> result = classify_weld_detail(detail)
    >>> result.dnv_class
    'C'
    """
    desc_lower = detail.description.lower()
    jt = detail.joint_type.lower()

    best_match = None
    best_score = -1

    for rule in _RULES:
        score = 0

        # Keyword matching
        for kw in rule["keywords"]:
            if kw in desc_lower:
                score += 2

        # Joint type matching
        if jt and jt in rule["joint_types"]:
            score += 3
        elif "" in rule["joint_types"] and not jt:
            score += 1

        # Condition check
        try:
            if rule["conditions"](detail):
                score += 1
            else:
                score -= 5  # strong negative for failing conditions
        except Exception:
            pass

        if score > best_score:
            best_score = score
            best_match = rule

    if best_match is None or best_score < 0:
        return ClassificationResult(
            dnv_class="D",
            aws_category="C",
            bs_class="D",
            confidence="low",
            dnv_table="",
            notes="Could not classify — defaulting to DNV Class D. Manual review required.",
        )

    confidence = "high" if best_score >= 5 else "medium" if best_score >= 2 else "low"

    return ClassificationResult(
        dnv_class=best_match["dnv"],
        aws_category=best_match["aws"],
        bs_class=best_match["bs"],
        confidence=confidence,
        dnv_table=best_match["table"],
        notes=best_match["note"],
    )


# ---------------------------------------------------------------------------
# Convenience: list all detail categories
# ---------------------------------------------------------------------------

def list_dnv_detail_categories() -> Dict[str, str]:
    """Return all DNV-RP-C203 detail categories with descriptions.

    Returns
    -------
    dict
        Mapping of class name to description.
    """
    return {
        "B1": "Rolled/extruded base material with no flame-cut edges",
        "B2": "Base material with machine gas-cut or sheared edges",
        "C":  "Full penetration butt weld, ground flush, NDE inspected",
        "C1": "Full penetration butt weld, ground flush, not inspected",
        "C2": "Full penetration butt weld, as-welded on backing",
        "D":  "As-welded butt weld, full penetration",
        "E":  "Non-load-carrying fillet weld attachment / cope hole",
        "F":  "Fillet weld toe failure / cruciform with full pen",
        "F1": "Load-carrying attachment fillet weld",
        "F3": "Partial pen butt weld or fillet weld with high SCF",
        "G":  "Fillet weld in shear / weld termination",
        "W1": "Cruciform partial pen, root failure",
        "W2": "Fillet weld root failure, load-carrying",
        "W3": "Fillet weld root failure, low quality",
        "T":  "Tubular joint (hotspot stress method)",
    }
