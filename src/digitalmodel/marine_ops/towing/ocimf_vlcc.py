"""OCIMF 2020 VLCC wind coefficients, MARIN 31746-1-PO Table 5, p29.

These are the published mean of ballast/loaded VLCC coefficients, not barge
coefficients. Headings and signs follow the source table. Piecewise linear
interpolation wraps at 360 degrees. Section 4.4, p10 uses Af, Al and Lpp;
an overall vessel length is not a substitute for the caller's explicit Lpp.
Provenance and the retrievable publication copy accompany the packaged sidecar.
"""
import math
from pathlib import Path

from digitalmodel.citations.schema import Citation, validate_citation

_BASIS_ROOT = Path(__file__).with_name('basis')

# (Cx, Cy, Cmz), dimensionless; rows 0 through 360 degrees in steps of 10.
_ROWS = (
    (.69, 0, 0), (.705, .135, -.057), (.675, .295, -.11),
    (.595, .435, -.148), (.48, .56, -.161), (.375, .65, -.161),
    (.275, .73, -.151), (.18, .795, -.133), (.1, .835, -.104),
    (.015, .855, -.077), (-.065, .85, -.048), (-.145, .82, -.026),
    (-.24, .765, -.012), (-.355, .68, 0), (-.48, .55, .006),
    (-.605, .415, .012), (-.73, .255, .011), (-.83, .105, .008),
    (-.915, 0, 0), (-.83, -.105, -.008), (-.73, -.255, -.011),
    (-.605, -.415, -.012), (-.48, -.55, -.006), (-.355, -.68, 0),
    (-.24, -.765, .012), (-.145, -.82, .026), (-.065, -.85, .048),
    (.015, -.855, .077), (.1, -.835, .104), (.18, -.795, .133),
    (.275, -.73, .151), (.375, -.65, .161), (.48, -.56, .161),
    (.595, -.435, .148), (.675, -.295, .11), (.705, -.135, .057),
    (.69, 0, 0),
)


def reference() -> Citation:
    """Validate the packaged provenance at calculation time, failing closed."""
    citation = Citation(
        code_id='OCIMF-STATIC-TOWING-2020', publisher='OCIMF', revision='2020',
        section='MARIN 31746-1-PO: section 4.4 p10; Table 1 p25; Table 5 p29',
        wiki_path='wikis/ocimf-static-towing-2020.md', source_sibling='generic',
        note='VLCC wind only; mean ballast/loaded coefficients; explicit Af, Al and Lpp required.',
    )
    validate_citation(citation, repo_root=_BASIS_ROOT)
    return citation


def wind_coefficients(direction_deg: float) -> tuple[float, float, float]:
    """Interpolate source-table signs without reflecting or taking magnitudes."""
    if not math.isfinite(direction_deg):
        raise ValueError('wind direction must be finite')
    reference()
    position = (direction_deg % 360) / 10
    lower = int(position)
    fraction = position - lower
    return tuple(a + fraction * (b - a) for a, b in zip(_ROWS[lower], _ROWS[lower + 1]))
