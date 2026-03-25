"""Section 8: QA pass/fail table from inline physics checks."""
from __future__ import annotations

import numpy as np

from ..config import QASummaryConfig


def build_qa_summary(diff, config: QASummaryConfig) -> str:
    """Build HTML for QA pass/fail summary section."""
    checks = _run_checks(diff)
    n_pass = sum(1 for c in checks if c["pass"])
    badge_cls = "bg-success" if n_pass == len(checks) else "bg-danger"
    badge = f"<span class='badge {badge_cls}'>{n_pass}/{len(checks)} passed</span>"
    table_html = _render_table(checks)
    return (
        "<div class='card mb-4'>"
        f"<div class='card-header'><strong>QA Summary</strong> {badge}</div>"
        f"<div class='card-body'>{table_html}</div></div>"
    )


def _run_checks(diff) -> list[dict]:
    checks: list[dict] = []

    # 1. RAOs finite
    try:
        raos = np.array(diff.displacementRAOs)
        ok = bool(np.all(np.isfinite(raos)))
        checks.append({"name": "RAOs finite", "pass": ok, "value": "Yes" if ok else "NaN/Inf"})
    except Exception as exc:
        checks.append({"name": "RAOs finite", "pass": False, "value": str(exc)})

    # 2. Added mass finite
    try:
        am = np.array(diff.addedMass)
        ok = bool(np.all(np.isfinite(am)))
        checks.append({"name": "Added mass finite", "pass": ok,
                        "value": "Yes" if ok else "NaN/Inf"})
    except Exception as exc:
        checks.append({"name": "Added mass finite", "pass": False, "value": str(exc)})

    # 3. Damping diagonal non-negative
    try:
        dm = np.array(diff.damping)
        diag_min = float(min(dm[:, i, i].min() for i in range(dm.shape[1])))
        ok = diag_min >= -1e-6
        checks.append({"name": "Damping diagonal ≥ 0", "pass": ok,
                        "value": f"min = {diag_min:.4g}"})
    except Exception as exc:
        checks.append({"name": "Damping diagonal ≥ 0", "pass": False, "value": str(exc)})

    # 4. Heave quasi-static (|RAO| → 1 at longest period, head seas)
    try:
        raos = np.array(diff.displacementRAOs)
        headings = list(diff.headings)
        freqs_hz = np.array(diff.frequencies)
        sort_idx = np.argsort(freqs_hz)
        raos_s = raos[:, sort_idx, :]
        periods = np.where(freqs_hz[sort_idx] > 0, 1.0 / freqs_hz[sort_idx], np.inf)
        valid = np.isfinite(periods)
        head_idx = int(np.argmin([abs(h - 180.0) for h in headings]))
        heave = float(np.abs(raos_s[head_idx, np.where(valid)[0][-1], 2]))
        ok = 0.8 <= heave <= 1.2
        t_long = float(periods[valid][-1])
        checks.append({"name": "Heave quasi-static", "pass": ok,
                        "value": f"{heave:.4f} at T={t_long:.1f} s"})
    except Exception as exc:
        checks.append({"name": "Heave quasi-static", "pass": False, "value": str(exc)})

    return checks


def _render_table(checks: list[dict]) -> str:
    rows = "".join(
        f"<tr class='{'table-success' if c['pass'] else 'table-danger'}'>"
        f"<td>{c['name']}</td>"
        f"<td>{'PASS' if c['pass'] else 'FAIL'}</td>"
        f"<td>{c['value']}</td></tr>"
        for c in checks
    )
    return (
        "<div class='table-responsive'>"
        "<table class='table table-sm table-bordered'>"
        "<thead class='table-light'>"
        "<tr><th>Check</th><th>Result</th><th>Value</th></tr></thead>"
        f"<tbody>{rows}</tbody></table></div>"
    )
