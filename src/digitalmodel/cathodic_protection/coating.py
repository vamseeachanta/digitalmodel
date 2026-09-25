"""Coating breakdown factor calculations from cited DNV tables.

Provides coating type definitions, breakdown factor calculations (initial,
mean, and final), coating life estimation and effective bare area.

Constants are sourced from the cited table lookups (issue #2207):

- paint coating categories I, II, III: DNV-RP-B401 Table 10-4 via
  ``b401_tables.coating_breakdown_constants`` (``b`` depends on the depth
  row 0-30 m / >30 m);
- linepipe coatings (FBE, 3LPE, 3LPP, coal tar enamel, asphalt enamel,
  polychloroprene/neoprene): DNV-RP-F103 Table A.1 via
  ``f103_tables.linepipe_coating_constants``;
- ``NONE``: bare steel, f_c = 1.0 by definition, nothing to cite.

``POLYURETHANE`` and ``CONCRETE_WEIGHT`` have no row in either table; the
functions raise ``ValueError`` for them rather than inventing constants.

The breakdown model is the standards' linear one, f_c(t) = a + b·t, with
no temperature or depth correction beyond the Table 10-4 depth row.

References
----------
- DNV-RP-B401 (October 2010, wiki revision "2011") Table 10-4
- DNV-RP-F103 (October 2010) Table A.1
- DNV-RP-F106 (2003) "Factory Applied External Pipeline Coatings" Sec. 5,
  for external coating family selection and inspection data sheets
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Any, Final, Optional

from pydantic import BaseModel, ConfigDict, Field, model_validator

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    DEFAULT_F103_EDITION,
    Edition,
    F103Edition,
    f103_standard_for_edition,
    normalize_edition,
    normalize_f103_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.b401_tables import (
    PaintCategory,
    citation_label,
    coating_breakdown_constants,
    depth_band,
)
from digitalmodel.cathodic_protection.f103_tables import (
    LinepipeCoating,
    linepipe_coating_constants,
)
from digitalmodel.citations import Citation, CitedValue


class CoatingCategory(str, Enum):
    """Coating system categories.

    Linepipe coatings map to DNV-RP-F103 Table A.1 rows; ``PAINT_I`` to
    ``PAINT_III`` are the DNV-RP-B401 Table 10-4 paint categories.
    """

    FBE = "fbe"
    THREE_LAYER_PE = "three_layer_pe"
    THREE_LAYER_PP = "three_layer_pp"
    COAL_TAR_ENAMEL = "coal_tar_enamel"
    ASPHALT_ENAMEL = "asphalt_enamel"
    POLYURETHANE = "polyurethane"  # no table row: lookups raise ValueError
    CONCRETE_WEIGHT = "concrete_weight"  # no table row: lookups raise ValueError
    NEOPRENE = "neoprene"  # F103 Table A.1 "Polychloroprene"
    PAINT_I = "paint_i"  # B401 Table 10-4 category I
    PAINT_II = "paint_ii"  # B401 Table 10-4 category II
    PAINT_III = "paint_iii"  # B401 Table 10-4 category III
    NONE = "none"


# DNV-RP-F103 Table A.1 row for each linepipe coating category.
_F103_BY_CATEGORY: Final[dict[CoatingCategory, LinepipeCoating]] = {
    CoatingCategory.FBE: LinepipeCoating.FBE,
    CoatingCategory.THREE_LAYER_PE: LinepipeCoating.THREE_LAYER_FBE_PE,
    CoatingCategory.THREE_LAYER_PP: LinepipeCoating.THREE_LAYER_FBE_PP,
    CoatingCategory.COAL_TAR_ENAMEL: LinepipeCoating.GFR_COAL_TAR_ENAMEL,
    CoatingCategory.ASPHALT_ENAMEL: LinepipeCoating.GFR_ASPHALT_ENAMEL,
    CoatingCategory.NEOPRENE: LinepipeCoating.POLYCHLOROPRENE,
}

# DNV-RP-B401 Table 10-4 column for each paint category.
_PAINT_BY_CATEGORY: Final[dict[CoatingCategory, PaintCategory]] = {
    CoatingCategory.PAINT_I: PaintCategory.I,
    CoatingCategory.PAINT_II: PaintCategory.II,
    CoatingCategory.PAINT_III: PaintCategory.III,
}

# Categories with no row in B401 Table 10-4 or F103 Table A.1.
_UNCITED_CATEGORIES: Final[dict[CoatingCategory, str]] = {
    CoatingCategory.POLYURETHANE: (
        "polyurethane has no breakdown-constant row in DNV-RP-B401 Table 10-4 "
        "or DNV-RP-F103 Table A.1"
    ),
    CoatingCategory.CONCRETE_WEIGHT: (
        "concrete weight coating is not a corrosion coating: DNV-RP-F103 "
        "Table A.1 only flags it as compatible with a linepipe coating and "
        "gives it no a/b constants; DNV-RP-B401 Table 10-4 has no row"
    ),
}

_BARE_STEEL_A: Final = 1.0
_BARE_STEEL_B: Final = 0.0
_BARE_STEEL_STANDARD: Final = "bare steel (f_c = 1.0 by definition, no coating standard)"


@dataclass(frozen=True)
class CoatingConstants:
    """Breakdown constants ``a`` (dimensionless) and ``b`` (1/yr) with provenance."""

    a: float
    b: float
    citation: Citation | None
    standard: str

    @property
    def citations(self) -> list[str]:
        """Rendered citation labels (empty for bare steel)."""
        return [] if self.citation is None else [citation_label(self.citation)]


def coating_constants(
    coating_type: CoatingCategory,
    depth_m: float = 0.0,
    edition: Edition | None = None,
    f103_edition: F103Edition | None = None,
) -> CoatingConstants:
    """Cited breakdown constants ``a`` and ``b`` for a coating category.

    Parameters
    ----------
    coating_type : CoatingCategory
        Coating system type.
    depth_m : float
        Water depth [m]; selects the Table 10-4 depth row (0-30 m or >30 m)
        for paint categories. Unused for linepipe coatings.
    edition : Edition, optional
        DNV-RP-B401 edition token for paint categories; ``None`` warns and
        defaults to 2021.
    f103_edition : F103Edition, optional
        DNV-RP-F103 edition token for linepipe coatings; ``None`` warns and
        defaults to 2010.

    Returns
    -------
    CoatingConstants
        ``a``, ``b``, the table citation (``None`` for bare steel) and the
        report-facing standard string that agrees with the citation.

    Raises
    ------
    ValueError
        For ``POLYURETHANE`` and ``CONCRETE_WEIGHT``, naming the standards
        that lack a row.
    """
    cat = CoatingCategory(coating_type)
    if cat in _UNCITED_CATEGORIES:
        raise ValueError(
            f"No cited breakdown constants for CoatingCategory.{cat.name}: "
            f"{_UNCITED_CATEGORIES[cat]}"
        )
    if cat is CoatingCategory.NONE:
        return CoatingConstants(
            _BARE_STEEL_A, _BARE_STEEL_B, None, _BARE_STEEL_STANDARD
        )
    a: CitedValue
    b: CitedValue
    if cat in _PAINT_BY_CATEGORY:
        ed = normalize_edition(edition, stacklevel=3)
        a, b = coating_breakdown_constants(
            _PAINT_BY_CATEGORY[cat], depth_band(depth_m), ed
        )
        return CoatingConstants(a.value, b.value, b.citation, standard_for_edition(ed))
    f103_ed = normalize_f103_edition(f103_edition, stacklevel=3)
    a, b = linepipe_coating_constants(_F103_BY_CATEGORY[cat], f103_ed)
    return CoatingConstants(
        a.value, b.value, b.citation, f103_standard_for_edition(f103_ed)
    )


def _snapshot_constants() -> dict[CoatingCategory, tuple[float, float]]:
    """(a, b) per category at 0-30 m depth, for backward-compatible callers."""
    snapshot: dict[CoatingCategory, tuple[float, float]] = {}
    for cat in CoatingCategory:
        if cat in _UNCITED_CATEGORIES:
            continue
        c = coating_constants(
            cat,
            depth_m=0.0,
            edition=DEFAULT_EDITION,
            f103_edition=DEFAULT_F103_EDITION,
        )
        snapshot[cat] = (c.a, c.b)
    return snapshot


# Backward-compatible snapshot of (a, b) per category. Paint categories hold
# the Table 10-4 "0-30 m" row; use ``coating_constants(depth_m=...)`` for the
# depth-aware, cited values. POLYURETHANE and CONCRETE_WEIGHT are absent.
COATING_CONSTANTS: dict[CoatingCategory, tuple[float, float]] = _snapshot_constants()

# Typical coating design life (years): industry practice, uncited. Not a
# table value from any standard; used only as the fallback in
# ``coating_life_estimate`` when the threshold is never reached.
COATING_DESIGN_LIFE: dict[CoatingCategory, float] = {
    CoatingCategory.FBE: 25.0,
    CoatingCategory.THREE_LAYER_PE: 40.0,
    CoatingCategory.THREE_LAYER_PP: 40.0,
    CoatingCategory.COAL_TAR_ENAMEL: 20.0,
    CoatingCategory.ASPHALT_ENAMEL: 20.0,
    CoatingCategory.POLYURETHANE: 20.0,
    CoatingCategory.CONCRETE_WEIGHT: 30.0,
    CoatingCategory.NEOPRENE: 25.0,
    CoatingCategory.NONE: 0.0,
}


class CoatingBreakdownResult(BaseModel):
    """Result of coating breakdown factor calculation."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    coating_type: str = Field(..., description="Coating category name")
    initial_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Breakdown factor at t=0"
    )
    mean_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Mean breakdown factor over design life"
    )
    final_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Breakdown factor at end of design life"
    )
    design_life_years: float = Field(..., gt=0, description="Design life [years]")
    citation: Optional[Citation] = Field(
        None,
        description=(
            "Table citation the constants came from; None for bare steel"
        ),
    )
    edition_used: Edition = Field(
        ..., description="DNV-RP-B401 edition token requested for the calculation"
    )
    f103_edition_used: Optional[F103Edition] = Field(
        None,
        description="DNV-RP-F103 edition used when the coating is a Table A.1 row",
    )
    standard: str = Field(
        ...,
        description="Standards reference matching the citation",
    )
    citations: list[str] = Field(
        default_factory=list,
        description="Rendered citations ('code_id revision section')",
    )

    @property
    def edition(self) -> Edition:
        """Alias of ``edition_used`` for report provenance."""
        return self.edition_used

    @model_validator(mode="before")
    @classmethod
    def _default_legacy_metadata(cls, data: Any) -> Any:
        if not isinstance(data, dict):
            return data

        values = dict(data)
        edition = values.get("edition_used") or DEFAULT_EDITION
        values["edition_used"] = edition
        if not values.get("standard"):
            try:
                values["standard"] = standard_for_edition(edition)
            except KeyError:
                pass
        return values


class CoatingLifeResult(BaseModel):
    """Result of coating life estimation."""

    coating_type: str = Field(..., description="Coating category name")
    estimated_life_years: float = Field(
        ..., description="Estimated coating life [years]"
    )
    threshold_factor: float = Field(
        ...,
        description="Breakdown factor threshold used for life estimate",
    )
    time_to_threshold_years: Optional[float] = Field(
        None,
        description="Time to reach threshold [years], None if never reached",
    )
    citations: list[str] = Field(
        default_factory=list,
        description="Rendered citations ('code_id revision section')",
    )


def coating_breakdown_factors(
    coating_type: CoatingCategory,
    design_life_years: float = 25.0,
    depth_m: float = 0.0,
    temperature_c: float = 20.0,
    edition: Edition | None = None,
    f103_edition: F103Edition | None = None,
) -> CoatingBreakdownResult:
    """Calculate initial, mean, and final coating breakdown factors.

    Linear model of DNV-RP-B401 §6.4 / DNV-RP-F103 Annex 1:
        f_c(t) = a + b * t

    with ``a`` and ``b`` from the cited table for the coating category
    (``coating_constants``), capped at 1.0.

    Parameters
    ----------
    coating_type : CoatingCategory
        Coating system type.
    design_life_years : float
        Design life of the CP system [years].
    depth_m : float
        Water depth [m]; selects the Table 10-4 depth row for paint
        categories (0-30 m or >30 m). No other depth effect is applied.
    temperature_c : float
        Accepted for signature compatibility and unused: neither Table 10-4
        nor Table A.1 gives a temperature correction to ``a`` or ``b``, so
        the former uncited "2 % per °C above 25 °C" factor was removed.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.
    f103_edition : F103Edition, optional
        DNV-RP-F103 edition token for linepipe coatings; ``None`` warns and
        defaults to 2010.

    Returns
    -------
    CoatingBreakdownResult
        Initial, mean, and final breakdown factors with the table citation.

    Raises
    ------
    ValueError
        For ``POLYURETHANE`` and ``CONCRETE_WEIGHT`` (no table row).
    """
    del temperature_c  # no cited temperature correction exists
    ed = normalize_edition(edition, stacklevel=3)
    cat = CoatingCategory(coating_type)
    f103_ed: F103Edition | None = None
    if cat in _F103_BY_CATEGORY:
        f103_ed = normalize_f103_edition(f103_edition, stacklevel=3)
    constants = coating_constants(cat, depth_m, ed, f103_ed)
    a, b = constants.a, constants.b

    fc_initial = kernel.coating_breakdown_linear(a, b, 0.0)
    fc_final = kernel.coating_breakdown_final(a, b, design_life_years)
    fc_mean = kernel.coating_breakdown_mean(a, b, design_life_years)

    return CoatingBreakdownResult(
        coating_type=cat.value,
        initial_factor=fc_initial,
        mean_factor=fc_mean,
        final_factor=fc_final,
        design_life_years=design_life_years,
        citation=constants.citation,
        edition_used=ed,
        f103_edition_used=f103_ed,
        standard=constants.standard,
        citations=constants.citations,
    )


def coating_life_estimate(
    coating_type: CoatingCategory,
    threshold_factor: float = 0.50,
    depth_m: float = 0.0,
    edition: Edition | None = None,
    f103_edition: F103Edition | None = None,
) -> CoatingLifeResult:
    """Estimate coating life based on when breakdown factor reaches threshold.

    Uses the linear degradation model f_c(t) = a + b*t and solves for the
    time when f_c reaches the specified threshold.

    Parameters
    ----------
    coating_type : CoatingCategory
        Coating system type.
    threshold_factor : float
        Breakdown factor threshold defining end of effective coating life
        (default 0.50 = 50% bare area equivalent).
    depth_m : float
        Water depth [m]; selects the Table 10-4 depth row for paints.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.
    f103_edition : F103Edition, optional
        DNV-RP-F103 edition token; ``None`` warns and defaults to 2010.

    Returns
    -------
    CoatingLifeResult
        Estimated coating life and related parameters.
    """
    cat = CoatingCategory(coating_type)
    constants = _constants_for(cat, depth_m, edition, f103_edition)
    a, b = constants.a, constants.b

    if b <= 0 or a >= threshold_factor:
        # Coating never degrades or already exceeds threshold
        time_to_threshold = None
        estimated_life = COATING_DESIGN_LIFE.get(cat, 0.0)
    else:
        time_to_threshold = (threshold_factor - a) / b
        estimated_life = time_to_threshold

    return CoatingLifeResult(
        coating_type=cat.value,
        estimated_life_years=estimated_life,
        threshold_factor=threshold_factor,
        time_to_threshold_years=time_to_threshold,
        citations=constants.citations,
    )


def effective_bare_area_coated(
    total_surface_area_m2: float,
    coating_type: CoatingCategory,
    elapsed_years: float,
    depth_m: float = 0.0,
    edition: Edition | None = None,
    f103_edition: F103Edition | None = None,
) -> float:
    """Calculate effective bare area of a coated structure at a given time.

    Effective bare area = total_surface_area * f_c(t)

    Parameters
    ----------
    total_surface_area_m2 : float
        Total external surface area [m²].
    coating_type : CoatingCategory
        Coating system type.
    elapsed_years : float
        Time since coating application [years].
    depth_m : float
        Water depth [m]; selects the Table 10-4 depth row for paints.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.
    f103_edition : F103Edition, optional
        DNV-RP-F103 edition token; ``None`` warns and defaults to 2010.

    Returns
    -------
    float
        Effective bare area [m²].
    """
    constants = _constants_for(
        CoatingCategory(coating_type), depth_m, edition, f103_edition
    )
    fc = kernel.coating_breakdown_linear(constants.a, constants.b, elapsed_years)
    return total_surface_area_m2 * fc


def _constants_for(
    cat: CoatingCategory,
    depth_m: float,
    edition: Edition | None,
    f103_edition: F103Edition | None,
) -> CoatingConstants:
    """Normalize only the edition the category needs, then look up."""
    if cat in _PAINT_BY_CATEGORY:
        return coating_constants(cat, depth_m, normalize_edition(edition, stacklevel=4))
    if cat in _F103_BY_CATEGORY:
        return coating_constants(
            cat,
            depth_m,
            f103_edition=normalize_f103_edition(f103_edition, stacklevel=4),
        )
    return coating_constants(cat, depth_m)
