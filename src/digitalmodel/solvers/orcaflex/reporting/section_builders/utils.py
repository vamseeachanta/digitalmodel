import html

def _escape(s: str) -> str:
    """Universal HTML escaping for all user-supplied string fields."""
    if s is None:
        return "N/A"
    return html.escape(str(s))

def wrap_section(anchor: str, title: str, body_html: str, empty: bool = False) -> str:
    """Wraps content in a standard section card."""
    if empty:
        return f"""
<section id="{anchor}">
  <div class="section-card section-empty">
    <h2 class="section-title">{title}</h2>
    <p class="no-data">No data available for this analysis.</p>
  </div>
</section>
"""
    return f"""
<section id="{anchor}">
  <div class="section-card">
    <h2 class="section-title">{title}</h2>
    <div class="section-body">
      {body_html}
    </div>
  </div>
</section>
"""

def build_table(headers: list[str], rows: list[list[str]]) -> str:
    """Helper to build a simple HTML table."""
    header_html = "".join([f"<th>{h}</th>" for h in headers])
    rows_html = ""
    for row in rows:
        row_cells = "".join([f"<td>{cell}</td>" for cell in row])
        rows_html += f"<tr>{row_cells}</tr>"
    
    return f"""
<table>
  <thead><tr>{header_html}</tr></thead>
  <tbody>{rows_html}</tbody>
</table>
"""
