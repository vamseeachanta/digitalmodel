"""Section 7: Modal analysis — natural frequencies and mode shapes."""
from __future__ import annotations


def build_modal_analysis(model, config) -> str:
    """Build modal analysis section HTML."""
    try:
        modes = getattr(model, "ModalAnalysisResults", None)
        if modes is None or len(modes) == 0:
            return (
                "<p>No modal analysis results found. "
                "Run modal analysis in OrcaFlex and save results to populate this section.</p>"
            )
        rows = []
        for i, mode in enumerate(modes):
            freq = getattr(mode, "NaturalFrequency", None)
            period = (1.0 / freq) if freq else None
            desc = getattr(mode, "Description", f"Mode {i+1}")
            freq_str = f"{freq:.4f}" if freq else "N/A"
            period_str = f"{period:.2f}" if period else "N/A"
            rows.append(
                f"<tr><td>{i+1}</td><td>{desc}</td>"
                f"<td>{freq_str}</td><td>{period_str}</td></tr>"
            )
        header = "<tr><th>#</th><th>Mode</th><th>Frequency (Hz)</th><th>Period (s)</th></tr>"
        return f'<table class="ofx">{header}{"".join(rows)}</table>'
    except Exception as exc:
        return f"<p>Error extracting modal analysis results: {exc}</p>"
