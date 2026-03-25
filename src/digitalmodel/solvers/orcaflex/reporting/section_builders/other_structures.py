"""
Other Structures section builder.
"""
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_other_structures_html(report: OrcaFlexAnalysisReport) -> str:
    """Builds the other structures and vessels section."""
    if not report.other_structures or not report.other_structures.attached_structures:
        return "" # Conditional/Optional

    others = report.other_structures
    
    # Attached Structures Table
    headers = ["Name", "Type", "Mass [kg]", "Arc Length [m]", "Position (X, Y, Z)", "Connected To"]
    rows = []
    for s in others.attached_structures:
        pos = f"({s.x:.1f}, {s.y:.1f}, {s.z:.1f})" if s.x is not None else "-"
        rows.append([
            _escape(s.name), _escape(s.type), f"{s.mass_kg:.1f}", 
            f"{s.arc_length_m:.1f}" if s.arc_length_m is not None else "-",
            pos, _escape(s.connected_to)
        ])
    table_html = build_table(headers, rows)

    body_html = f"""
    <h3 id="other-struct-positions">Attached Structure Positions</h3>
    {table_html}
    
    <div id="other-struct-clearance">
        <p><strong>Minimum Clearance:</strong> {f"{others.minimum_clearance_m:.2f} m" if others.minimum_clearance_m is not None else "N/A"}</p>
    </div>
    """
    
    return wrap_section("other-structures", "8. Other Structures", body_html)
