"""Section 8: QA summary — import and display results from orcaflex_example_qa."""
from __future__ import annotations
from pathlib import Path


_QA_DIR = Path(__file__).parents[7] / "docs/domains/orcaflex/examples/qa"


def build_qa_summary(model, config) -> str:
    """Build QA summary section HTML by reading qa JSON results."""
    import json

    json_files = sorted(_QA_DIR.glob("*_qa_results.json")) if _QA_DIR.exists() else []
    if not json_files:
        return (
            "<p>No QA results found. "
            "Run <code>orcaflex_example_qa.py</code> to generate results.</p>"
        )

    rows_html = []
    for jf in json_files:
        try:
            data = json.loads(jf.read_text())
            example_id = data.get("example_id", jf.stem.replace("_qa_results", ""))
            if data.get("skipped"):
                rows_html.append(
                    f"<tr><td>{example_id}</td>"
                    f'<td colspan="3" class="skip">Skipped: {data.get("skip_reason")}</td></tr>'
                )
                continue
            checks = data.get("checks", [])
            n_pass = sum(1 for c in checks if c.get("passed"))
            n_fail = len(checks) - n_pass
            status_cls = "pass" if n_fail == 0 else "fail"
            status_str = "PASS" if n_fail == 0 else f"FAIL ({n_fail})"
            rows_html.append(
                f"<tr><td>{example_id}</td>"
                f"<td>{n_pass}</td>"
                f"<td>{n_fail}</td>"
                f'<td class="{status_cls}">{status_str}</td></tr>'
            )
        except Exception as exc:
            rows_html.append(f"<tr><td>{jf.name}</td><td colspan='3'>Parse error: {exc}</td></tr>")

    header = "<tr><th>Example</th><th>Pass</th><th>Fail</th><th>Status</th></tr>"
    return f'<table class="ofx">{header}{"".join(rows_html)}</table>'
