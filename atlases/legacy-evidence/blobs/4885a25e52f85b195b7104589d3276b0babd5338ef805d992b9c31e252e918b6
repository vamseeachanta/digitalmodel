"""FOWT (floating offshore wind turbine) watch-circle vs dynamic-cable check.

A FOWT dynamic export cable hangs from the floater hang-off point down to a
touchdown point. Unlike an FPSO, the FOWT mooring watch circle is sized
primarily to protect this *dynamic power cable*: the cable must never bend
tighter than its minimum bend radius (MBR) anywhere along its suspended length,
across the full envelope of horizontal platform excursions (the watch circle).

This module provides a closed-form, solver-free geometric check
(``WatchCircleConstraint`` / ``check_watch_circle_vs_cable``) that returns
pass/fail plus the governing (minimum) bend radius and its margin against the
MBR limit, for a catenary, taut, or hybrid (lazy-wave) mooring × spar/semi/TLP
combination.

Geometric model (circular-arc suspended-cable idealisation)
-----------------------------------------------------------
The suspended portion of the dynamic cable, of fixed length ``L_s``, is modelled
as a single circular arc spanning the straight-line **chord** between the
hang-off point and a fixed touchdown point. For a circular arc of length ``L``
and chord ``c``::

    c = 2 * R * sin(L / (2 * R))                     (1)

so the arc (bend) radius ``R`` is a monotonically *increasing* function of the
chord ``c`` for fixed ``L`` (a longer chord means a straighter, larger-radius
arc; a shorter chord means a tighter bend). The chord between hang-off
(elevation ``h`` above the touchdown reference, nominal horizontal span ``x0``)
and the touchdown point is::

    c(x) = sqrt(h**2 + x**2)

When the floater drifts horizontally by the watch-circle radius ``R_wc`` *toward*
the touchdown, the horizontal span shrinks to ``x = x0 - R_wc``, the chord
shrinks, and equation (1) gives the **smallest** (governing) bend radius. The
near (compressed) excursion is therefore the worst case for over-bending and is
what this check evaluates. The cable is assumed inextensible over the watch-circle
timescale (suspended length ``L_s`` constant); this is conservative because real
axial pay-in/out would relieve some curvature.

Assumptions / limitations
--------------------------
* Single circular-arc idealisation of the suspended span. This bounds the true
  minimum bend radius of a smooth catenary/lazy-wave profile from below (the
  sag-bend of a real catenary is locally tighter than the chord-average arc near
  hang-off, but the arc model captures the *dominant* span-shortening effect of
  watch-circle excursion). For final design use a dynamic FE/lumped-mass solver;
  this is a preliminary feasibility screen.
* Hang-off elevation ``h`` and touchdown point are fixed; only the horizontal
  span varies with platform offset. Vertical heave is not superposed (add it to
  ``h`` envelope if required).
* The watch circle is taken as an isotropic radius (max horizontal offset). The
  governing direction is the floater-toward-touchdown direction.
* Fail-closed: any geometry that cannot form an arc no tighter than the MBR
  limit fails.

Standards references
--------------------
* DNV-RP-0360 ("Subsea power cables in shallow water" / dynamic cable design):
  the minimum bend radius (MBR) is a cable-supplier / standard-derived storage
  and dynamic curvature limit. The numeric MBR is a **cable-specific input** to
  this check (typically the manufacturer MBR with the RP-0360 design factor
  applied); no universal numeric MBR is hard-coded here — the caller supplies
  ``mbr_limit_m``. See issue #574 for the DNV-RP-0360 wiki page that backs the
  citation emitted by this module.
* DNV-OS-E301 (position mooring), API RP 2SK (mooring) — covered in
  ``mooring_design.py``.
"""

from __future__ import annotations

import math
import warnings
from enum import Enum
from pathlib import Path
from typing import List, Optional

from pydantic import BaseModel, Field

from digitalmodel.citations import Citation


# DNV-RP-0360 wiki page backing the MBR citation (depends on #574). Emitted only
# when a live wiki tree is available; standalone mode degrades gracefully.
_DNV_RP_0360_WIKI_PATH = "wikis/engineering/wiki/standards/dnv-rp-0360.md"
_CITATION_EMITTED_CACHE: dict = {}


class MooringType(str, Enum):
    """Mooring configuration class for the host floater."""

    CATENARY = "catenary"
    TAUT = "taut"
    HYBRID = "hybrid"  # e.g. semi-taut / lazy-wave host


class FloaterType(str, Enum):
    """FOWT floater hull type."""

    SPAR = "spar"
    SEMI = "semi"
    TLP = "tlp"


class DynamicCableConfig(BaseModel):
    """Geometry of a FOWT dynamic export cable's suspended span.

    All lengths in metres. The touchdown point is the fixed reference origin;
    the hang-off point sits at elevation ``hang_off_elevation`` above it and a
    nominal horizontal span ``nominal_horizontal_span`` from it (in still-water,
    zero-offset condition).
    """

    suspended_length: float = Field(
        ..., gt=0.0, description="Suspended cable length, hang-off to touchdown (m)"
    )
    hang_off_elevation: float = Field(
        ...,
        gt=0.0,
        description="Hang-off elevation above touchdown reference (m)",
    )
    nominal_horizontal_span: float = Field(
        ...,
        gt=0.0,
        description="Still-water horizontal span, hang-off to touchdown (m)",
    )
    mbr_limit_m: float = Field(
        ...,
        gt=0.0,
        description=(
            "Minimum bend radius limit (m), DNV-RP-0360 design-factored cable MBR"
        ),
    )
    mooring_type: MooringType = MooringType.CATENARY
    floater_type: FloaterType = FloaterType.SEMI


class WatchCircleResult(BaseModel):
    """Outcome of a watch-circle vs cable-curvature check.

    ``passes`` is True iff the governing (minimum) bend radius over the watch
    circle stays at or above the MBR limit.
    """

    passes: bool = Field(..., description="True iff governing radius >= MBR limit")
    governing_bend_radius_m: float = Field(
        ..., description="Minimum suspended-cable bend radius over the watch circle (m)"
    )
    mbr_limit_m: float = Field(..., description="MBR limit checked against (m)")
    margin_m: float = Field(
        ..., description="governing_bend_radius_m - mbr_limit_m (m); <0 means fail"
    )
    governing_offset_m: float = Field(
        ..., description="Platform offset (toward touchdown) giving the governing radius (m)"
    )
    governing_chord_m: float = Field(
        ..., description="Hang-off-to-touchdown chord at the governing offset (m)"
    )
    mooring_type: MooringType
    floater_type: FloaterType
    citations: List[Citation] = Field(default_factory=list)


class WatchCircleConstraint(BaseModel):
    """FOWT watch-circle envelope constraint against dynamic-cable curvature.

    Couples a platform watch-circle radius (max horizontal offset under
    environmental loading) to a dynamic-cable configuration and verifies, in
    closed form, that the cable's curvature stays within its DNV-RP-0360 MBR
    limit across the watch circle.

    Example
    -------
    >>> cfg = DynamicCableConfig(
    ...     suspended_length=250.0, hang_off_elevation=80.0,
    ...     nominal_horizontal_span=200.0, mbr_limit_m=50.0)
    >>> WatchCircleConstraint(watch_circle_radius=20.0,
    ...                       cable=cfg).check().passes
    True
    """

    watch_circle_radius: float = Field(
        ...,
        gt=0.0,
        description="Max horizontal platform offset under environmental loading (m)",
    )
    cable: DynamicCableConfig

    def check(self, *, repo_root: Optional[Path] = None) -> WatchCircleResult:
        """Run the closed-form watch-circle vs cable-curvature check.

        Args:
            repo_root: Optional override to locate the wiki tree for citation
                emission. In standalone mode (no wiki) the result still computes;
                citation list is empty and a one-shot RuntimeWarning is issued.

        Returns:
            WatchCircleResult with pass/fail, governing bend radius, MBR limit,
            margin, and the governing platform offset / chord.
        """
        c = self.cable
        # Governing (worst) case: floater drifts the full watch-circle radius
        # toward the touchdown, shrinking the horizontal span and the chord.
        governing_offset = self.watch_circle_radius
        x_min = c.nominal_horizontal_span - governing_offset

        if x_min <= 0.0:
            # Floater can reach (or pass) the vertical above touchdown: the
            # span collapses; treat the chord as the pure vertical elevation,
            # which is the tightest physically meaningful arc. Fail-closed.
            x_min = 0.0
        chord = math.hypot(c.hang_off_elevation, x_min)

        governing_radius = _arc_radius_from_chord(c.suspended_length, chord)
        margin = governing_radius - c.mbr_limit_m
        passes = governing_radius >= c.mbr_limit_m

        citations = self._emit_citation(repo_root)

        return WatchCircleResult(
            passes=passes,
            governing_bend_radius_m=round(governing_radius, 3),
            mbr_limit_m=c.mbr_limit_m,
            margin_m=round(margin, 3),
            governing_offset_m=round(governing_offset, 3),
            governing_chord_m=round(chord, 3),
            mooring_type=c.mooring_type,
            floater_type=c.floater_type,
            citations=citations,
        )

    def _emit_citation(self, repo_root: Optional[Path]) -> List[Citation]:
        """Emit a DNV-RP-0360 MBR citation when a live wiki tree is available.

        Mirrors the standalone-graceful behaviour of mooring_design.py: fail-soft
        (empty citation list + one-shot warning) when the wiki page is absent,
        rather than blocking the geometric calc. Depends on #574.
        """
        from digitalmodel.citations.resolver import resolve_wiki_base
        from digitalmodel.citations import CitationResolutionError

        try:
            wiki_base = resolve_wiki_base(override=repo_root)
        except CitationResolutionError:
            wiki_base = None

        if wiki_base is None:
            if "standalone" not in _CITATION_EMITTED_CACHE:
                warnings.warn(
                    "digitalmodel standalone mode: DNV-RP-0360 citation "
                    "unavailable (no wiki tree found; set DIGITALMODEL_REPO_ROOT "
                    "or LLM_WIKI_PATH to enable). Watch-circle check proceeds.",
                    RuntimeWarning,
                    stacklevel=3,
                )
                _CITATION_EMITTED_CACHE["standalone"] = True
            return []

        page = (wiki_base / _DNV_RP_0360_WIKI_PATH)
        if not page.exists():
            return []

        return [
            Citation(
                code_id="DNV-RP-0360",
                publisher="DNV",
                revision="2016",
                section="Dynamic cable minimum bend radius (MBR)",
                wiki_path=_DNV_RP_0360_WIKI_PATH,
                note="MBR limit is a cable-specific design input supplied by caller",
            )
        ]


def _arc_radius_from_chord(arc_length: float, chord: float) -> float:
    """Bend radius of a circular arc of given length spanning a given chord.

    Inverts ``chord = 2 * R * sin(arc_length / (2 * R))`` for ``R``. The
    dimensionless ratio ``chord / arc_length = sin(theta) / theta`` (with
    half-angle ``theta = arc_length / (2 * R)``) is strictly decreasing on
    ``(0, pi)``, so a bisection on ``theta`` converges robustly.

    Args:
        arc_length: Suspended arc (cable) length L (m), > 0.
        chord: Straight-line chord c between the arc endpoints (m), 0 <= c <= L.

    Returns:
        Bend radius R (m). Returns ``math.inf`` when ``chord >= arc_length``
        (a straight cable, zero curvature). As ``chord -> 0`` the arc closes on
        itself and R -> arc_length / (2 * pi).
    """
    if arc_length <= 0.0:
        raise ValueError("arc_length must be positive")
    if chord >= arc_length:
        return math.inf
    if chord <= 0.0:
        # Degenerate: full-circle-ish closure; smallest meaningful radius.
        return arc_length / (2.0 * math.pi)

    ratio = chord / arc_length  # = sin(theta)/theta, theta in (0, pi)
    lo, hi = 1e-12, math.pi - 1e-12
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        f = math.sin(mid) / mid
        # sin(t)/t is decreasing on (0, pi): if f > ratio we need a larger theta.
        if f > ratio:
            lo = mid
        else:
            hi = mid
    theta = 0.5 * (lo + hi)
    return arc_length / (2.0 * theta)


def check_watch_circle_vs_cable(
    offset: float,
    cable_config: DynamicCableConfig,
    *,
    repo_root: Optional[Path] = None,
) -> WatchCircleResult:
    """Convenience wrapper: check a watch-circle ``offset`` vs ``cable_config``.

    Args:
        offset: Watch-circle radius / max horizontal platform offset (m).
        cable_config: Dynamic-cable suspended-span geometry + MBR limit.
        repo_root: Optional wiki-tree override for citation emission.

    Returns:
        WatchCircleResult (pass/fail + governing radius + slack margin).
    """
    return WatchCircleConstraint(
        watch_circle_radius=offset, cable=cable_config
    ).check(repo_root=repo_root)
