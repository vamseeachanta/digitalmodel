"""report_builders — backward-compat shim. Import from sub-modules directly."""
from .report_builders_header import (  # noqa: F401
    HULL_TYPE_NOTES, _get_hull_type_note, _find_closest_idx,
    _build_header_html, _build_toc_html,
    _build_executive_summary_html, _build_hull_description_html,
)
from .report_builders_hydrostatics import (  # noqa: F401
    _build_stability_html, _build_natural_periods_html,
    _build_added_mass_diagonal_html, _build_damping_diagonal_html,
    _build_coupling_assessment_html, _build_infinite_added_mass_html,
)
from .report_builders_responses import (  # noqa: F401
    _build_load_raos_html, _build_roll_damping_html,
    _build_phase_guide_html, _build_appendices_html,
)
__all__ = [
    "HULL_TYPE_NOTES", "_get_hull_type_note", "_find_closest_idx",
    "_build_header_html", "_build_toc_html",
    "_build_executive_summary_html", "_build_hull_description_html",
    "_build_stability_html", "_build_natural_periods_html",
    "_build_added_mass_diagonal_html", "_build_damping_diagonal_html",
    "_build_coupling_assessment_html", "_build_infinite_added_mass_html",
    "_build_load_raos_html", "_build_roll_damping_html",
    "_build_phase_guide_html", "_build_appendices_html",
]
