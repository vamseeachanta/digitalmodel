"""OCIMF convention authority (digitalmodel#616).

Citation-bound source of truth for the OCIMF MEG3/MEG4 Annex A coordinate
and sign conventions. Per the plan r1 M1 enforcement: any test that pins
arrow direction values derives them from this module, NOT from inline literals
and NOT from build_coefficient_explorer.py's internal docstring.

Source citations (verified by direct pdftotext extraction 2026-05-20):

- OCIMF (2008). "Mooring Equipment Guidelines, Third Edition" (MEG3).
  Appendix A §A1 "Sign Convention and Coordinate System": "The sign convention
  for the axis of coordinates for ships refers to a wind or current direction
  as 0 degrees when it flows parallel to the hull from stern to bow. Positive
  angles increase in an anti-clockwise direction."

- OCIMF (2018). "Mooring Equipment Guidelines, Fourth Edition" (MEG4).
  Appendix A §A2 "Sign convention and coordinate system": identical wording —
  "0 degrees when it flows parallel to the hull from stern to bow. Positive
  angles increase in an anti-clockwise direction."

Derived implications:

1. θ is the wind/current FLOW direction (motion direction), NOT the meteorological
   origin direction. At θ=0°, fluid is moving in the +X_body direction
   (stern→bow). The vessel feels a forward push.

2. Positive θ is anti-clockwise viewed from above. From +X_body (bow direction)
   rotating anti-clockwise lands on +Y_body = PORT (the LEFT side of the vessel
   when looking down from the sky with bow up). Therefore in OCIMF convention,
   **+Y_body = port** (NOT starboard, despite what the build script's existing
   make_polar_overlay() docstring at lines 399-405 asserts).

3. At θ=90°, fluid is moving toward port (+Y_body direction). Drag on the
   stationary moored vessel = force in same direction as flow = force toward
   port = positive Cy.

4. Therefore: **positive Cy in OCIMF convention means force in +Y_body = port**.
   Negative Cy means force in -Y_body = starboard.

5. For arrow direction in DATA-FRAME polar coordinates:
   - positive Cy at any θ_incidence  → arrow direction = 90° (data frame +Y_body = port)
   - negative Cy at any θ_incidence  → arrow direction = 270° (data frame -Y_body = starboard)
   The arrow's direction depends on the sign of Cy, NOT on θ_incidence.

6. **Chart-rendering note (NOT addressed in this module):** the existing
   build_coefficient_explorer.py uses Plotly `direction='clockwise', rotation=90`
   which visually places data θ=90° on screen-right and labels it "stbd beam".
   In OCIMF data convention, data θ=90° is actually toward port. The visual
   labeling discrepancy is a pre-existing concern in that build script and is
   OUT OF SCOPE for digitalmodel#616 — the umbrella issue workspace-hub#2768
   tracks the broader chart-labeling cleanup. This module exposes correct
   data-frame angles; visual labeling is the caller's responsibility.
"""
from __future__ import annotations


class _OcimfConventionAuthority:
    """Singleton authority for OCIMF MEG3/MEG4 Annex A direction values.

    Per TDD #17 (dataclass-count-exactly-1): this class is intentionally a
    plain class with NO @dataclass decorator. The visualization package
    declares exactly one @dataclass — `VesselSilhouetteSpec` in types.py.
    The static-analysis test counts decorated classes across the package
    and asserts the count is 1.
    """

    # Per derivation §5 above: positive Cy at any incidence → arrow in data-frame +Y_body direction
    _POSITIVE_CY_ARROW_DEG = 90.0
    _NEGATIVE_CY_ARROW_DEG = 270.0

    _CITATION_TEXT = (
        "OCIMF MEG3 (2008) Appendix A §A1 \"Sign Convention and Coordinate System\" "
        "+ OCIMF MEG4 (2018) Appendix A §A2 \"Sign convention and coordinate system\": "
        "θ=0° when wind/current flows stern→bow; positive angles anti-clockwise from above; "
        "therefore +Y_body=port; positive Cy=force toward port (+Y body-frame direction)."
    )

    def positive_cy_arrow_at_starboard_incidence_deg(self) -> float:
        """Arrow direction (data-frame degrees) for positive Cy.

        Returns 90.0: the OCIMF +Y_body direction = port. Independent of
        θ_incidence because lateral force is along the body Y axis by definition.
        """
        return self._POSITIVE_CY_ARROW_DEG

    def negative_cy_arrow_at_starboard_incidence_deg(self) -> float:
        """Arrow direction (data-frame degrees) for negative Cy.

        Returns 270.0: the OCIMF -Y_body direction = starboard.
        """
        return self._NEGATIVE_CY_ARROW_DEG

    def citation_text(self) -> str:
        return self._CITATION_TEXT


OCIMF_CONVENTION_AUTHORITY = _OcimfConventionAuthority()
