# Session restart handoff — Issue #598 HTML report modification plan

Date: 2026-05-10
Repository: `vamseeachanta/digitalmodel`
Current branch at handoff creation: `main`
Related closed issue: https://github.com/vamseeachanta/digitalmodel/issues/598
Implemented commit: `8867bcfc1c285414c381687ebd832b9c9773cbe0` (`feat: add SIROCCO current heading rudder charts`)

## Restart objective

Restart a session to apply review-driven modifications to the B1528 SIROCCO current-heading/rudder HTML report artifact and its generator, without losing the already-closed issue evidence.

Primary artifact reviewed:

```text
outputs/b1528_sirocco/current_heading_rudder/b1528_sirocco_current_heading_rudder_report.html
```

Source generator and data:

```text
src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py
src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml
tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py
docs/domains/marine-engineering/b1528-sirocco-current-heading-rudder-report.md
```

## Verified current state

The reviewed HTML report currently passes basic structural and visual checks:

- Loads in browser with title `B1528 SIROCCO Current-Heading/Rudder Forces`.
- Browser console showed `0` JavaScript errors.
- Current-speed dropdown exists and defaults to `4.56 kn (chart default extra)`.
- Speed options: `1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 4.56 kn`.
- Rudder selector has `21` values from `-10` to `+10 deg`.
- Chart 1 renders force components vs heading.
- Chart 2 renders yaw-moment heatmap over heading × rudder.
- Interaction verified by changing speed to `2.5 kn` and rudder to `0 deg`; chart titles updated.
- Dataset covers `3969` total rows = `9 speeds * 21 headings * 21 rudders`.
- Requested engineering rows: `3528`; extra default rows for `4.56 kn`: `441`.
- Report caveat already states rudder-induced only; hull current loads and other effects are excluded.

## Recommended modifications

### M1 — Change default rudder angle from `+10 deg` to `0 deg`

Rationale: `+10 deg` opens on an extreme case. `0 deg` is a more neutral interpretation default while the heatmap still exposes the full rudder-angle envelope.

Expected behavior after change:

- Current speed still defaults to `4.56 kn`.
- Rudder angle dropdown defaults to `0.0 deg`.
- Chart 1 title on load should read approximately:

```text
Ship-fixed force component (kN) · current 4.56 kn · rudder 0 deg
```

Implementation target:

- Likely in HTML generation function in `src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py`.
- Also update tests that assert default selected option or initial chart title.

Pseudo-code:

```python
DEFAULT_CHART_RUDDER_ANGLE_DEG = 0.0

for rudder in rudder_angles:
    selected = " selected" if rudder == DEFAULT_CHART_RUDDER_ANGLE_DEG else ""
    emit_option(value=rudder, label=f"{rudder:.1f} deg", selected=selected)
```

### M2 — Clarify title/subtitle as rudder-induced force visualization

Rationale: Current title can be read as total current force components on the ship. The model is intentionally a rudder-induced component model, not validated total hull-current loading.

Recommended title changes:

- HTML `<title>`:

```text
B1528 SIROCCO Rudder-Induced Current-Heading/Rudder Forces
```

- H1:

```text
Rudder-induced current-heading/rudder force component chart set
```

- Existing caveat paragraph can remain, but ensure it still contains:

```text
rudder-induced only; hull current loads are not included; this is not a validated oblique-current hull/rudder interaction model
```

Implementation target:

- Generator strings in `b1528_sirocco_current_heading_rudder_report.py`.
- Durable doc `docs/domains/marine-engineering/b1528-sirocco-current-heading-rudder-report.md` if it repeats the old title.
- Tests for report text/title.

### M3 — Add selected-speed interpretation summary table or panel

Rationale: Current report requires hover interaction to identify max values. A compact summary improves engineering review readability.

Recommended content under the charts or above the method section:

For the selected current speed, show:

| Metric | Value | Heading | Rudder |
|---|---:|---:|---:|
| Max `|X_ship|` | kN | deg | deg |
| Max `|Y_ship|` | kN | deg | deg |
| Max resultant horizontal force | kN | deg | deg |
| Max `|N_ship|` | kN-m | deg | deg |

Interaction behavior:

- Panel updates when current speed changes.
- It does not need to update with Chart 1 rudder selector unless desired; it summarizes all rudders for the selected speed.

Pseudo-code JavaScript:

```javascript
function absMax(rows, field) {
  return rows.reduce((best, row) => {
    const value = Math.abs(row[field]);
    return !best || value > Math.abs(best[field]) ? row : best;
  }, null);
}

function formatSummaryRow(label, row, field, scale, unit) {
  return `<tr>
    <td>${label}</td>
    <td>${Math.abs(row[field] / scale).toFixed(3)} ${unit}</td>
    <td>${row.heading_offset_deg} deg</td>
    <td>${row.rudder_angle_deg} deg</td>
  </tr>`;
}

function updateSelectedSpeedSummary() {
  const speedRows = rowsForSpeed(selectedSpeed());
  const rows = [
    formatSummaryRow('Max |X_ship|', absMax(speedRows, 'force_x_ship_N'), 'force_x_ship_N', 1000, 'kN'),
    formatSummaryRow('Max |Y_ship|', absMax(speedRows, 'force_y_ship_port_N'), 'force_y_ship_port_N', 1000, 'kN'),
    formatSummaryRow('Max resultant H', absMax(speedRows, 'resultant_horizontal_force_N'), 'resultant_horizontal_force_N', 1000, 'kN'),
    formatSummaryRow('Max |N_ship|', absMax(speedRows, 'moment_n_yaw_bow_port_kN_m'), 'moment_n_yaw_bow_port_kN_m', 1, 'kN-m'),
  ];
  document.getElementById('selected-speed-summary-body').innerHTML = rows.join('');
}
```

HTML placeholder:

```html
<h2>Selected-speed envelope summary</h2>
<p>Envelope maxima across all heading and rudder combinations for the selected current speed.</p>
<table class="data-table" id="selected-speed-summary">
  <thead><tr><th>Metric</th><th>Value</th><th>Heading</th><th>Rudder</th></tr></thead>
  <tbody id="selected-speed-summary-body"></tbody>
</table>
```

### M4 — Optional offline Plotly handling

Rationale: The current report depends on Plotly CDN:

```html
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
```

This is acceptable for connected review but not ideal for offline/air-gapped engineering handoff. Treat this as optional unless the user explicitly wants offline artifacts.

Options:

1. Leave CDN and document internet dependency.
2. Bundle a local Plotly file under a durable assets directory and reference it relatively.
3. Inline Plotly for a fully self-contained artifact, accepting a much larger HTML file.

If implementing now, prefer option 1 or 2; do not bloat the artifact unless specifically requested.

## Suggested execution sequence for restart

1. Load required skills if relevant:
   - `development/html-report-verify`
   - `workspace-hub/playwright`
   - `software-development/gh-work-execution` only if a new approved issue exists or the user asks for issue execution.
2. Reconfirm live state:

```bash
cd /mnt/local-analysis/workspace-hub/digitalmodel
git status --short --branch
git log --oneline -5
```

3. Decide workflow route:
   - Because #598 is already closed, either:
     - create a small follow-up GitHub issue for these presentation refinements, plan it, get approval, then implement; or
     - if the user explicitly authorizes direct polish, make a narrow docs/report-generator patch and document that it is post-close artifact refinement.
4. Add/update tests first for M1/M2/M3:
   - default rudder selected is `0.0`;
   - title/H1 include `rudder-induced`;
   - selected-speed summary table/panel exists;
   - JS update function includes summary update on current-speed change;
   - generated CSV row counts remain unchanged.
5. Modify generator and durable doc.
6. Regenerate artifacts.
7. Run focused validation:

```bash
PYTHONPATH=src uv run python -m pytest tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py
PYTHONPATH=src uv run python -m digitalmodel.naval_architecture.b1528_sirocco_current_heading_rudder_report
```

8. Browser/HTML verification:

```bash
cd /mnt/local-analysis/workspace-hub/digitalmodel
uv run python -m http.server 8974
# open:
# http://127.0.0.1:8974/outputs/b1528_sirocco/current_heading_rudder/b1528_sirocco_current_heading_rudder_report.html
```

Check:

- default speed = `4.56 kn`;
- default rudder = `0.0 deg`;
- title/H1 says rudder-induced;
- both charts render;
- selected-speed summary table updates when speed changes;
- browser console has no JS errors;
- no horizontal overflow or chart clipping.

9. If committing, use a narrow commit message such as:

```text
feat: refine SIROCCO rudder-induced report defaults
```

or, if only docs/generated artifact changes:

```text
docs: refine SIROCCO report interpretation defaults
```

## Do-not-lose context notes

- Do not remove the explicit `4.56 kn` extra-default-plane policy unless the user asks; it was intentionally added so the dropdown can default to an exact dataset plane.
- Do not imply these charts are total ship current forces; preserve the rudder-induced-only caveat.
- Do not reopen #598 casually. It was closed with evidence; use a follow-up issue for refinements unless the user directs otherwise.
- Keep generated output paths under `outputs/b1528_sirocco/current_heading_rudder/` unless #597 or repo policy changes artifact placement.
- Preserve row-count invariants: `3528` requested rows, `441` extra default rows, `3969` total rows.

## Copy/paste restart prompt

```text
Resume digitalmodel SIROCCO report refinement from:

docs/session-handoffs/2026-05-10-issue-598-html-report-modification-handoff.md

Goal: apply review-driven HTML report modifications for the B1528 SIROCCO current-heading/rudder artifact: default rudder to 0 deg, clarify report title/H1 as rudder-induced, add a selected-speed envelope summary table/panel, and optionally document/bundle Plotly dependency. Preserve #598 closeout evidence and row-count invariants. Use TDD first, regenerate artifacts, and verify in browser with no JS errors.
```

## Exit closeout evidence — 2026-05-10T07:47:35-05:00

### Task status

- Durable handoff created for the follow-up SIROCCO HTML report refinement work.
- Related GitHub issue remains closed: [#598 — feat(naval-architecture): SIROCCO current-heading/rudder force component chart set](https://github.com/vamseeachanta/digitalmodel/issues/598).
- No follow-up implementation was performed in this exit window; this file is a restart/continuation handoff for the next session.
- No external send/action was performed.

### Repo-state proof captured before committing this handoff

`vamseeachanta/digitalmodel`:

- Branch: `main`
- Local `HEAD`: `8867bcfc1c285414c381687ebd832b9c9773cbe0`
- `origin/main`: `8867bcfc1c285414c381687ebd832b9c9773cbe0`
- Ahead/behind before handoff commit: `0/0`
- Dirty/untracked count before handoff commit: `1`
- Dirty item: `?? docs/session-handoffs/` containing this handoff file.
- Branch/worktree disposition: preserved on `main`; no feature worktree created for this exit-only documentation action.

`vamseeachanta/workspace-hub` control repo:

- Branch: `main`
- Local `HEAD`: `ac936e03fd772b1d76f82603ab1a81c103d8e237`
- `origin/main`: `ac936e03fd772b1d76f82603ab1a81c103d8e237`
- Ahead/behind: `0/0`
- Dirty/untracked count: `65`
- Dirty-state exception: pre-existing unrelated workspace-hub generated/learning/provider-report churn remains outside this digitalmodel handoff scope and was not staged.
- Branch/worktree disposition: preserved on `main`; no cleanup attempted because dirt is unrelated to the SIROCCO handoff.

### Remaining next steps

1. If the SIROCCO report refinements are approved, create or reuse a follow-up issue rather than reopening closed [#598](https://github.com/vamseeachanta/digitalmodel/issues/598) casually.
2. Execute with TDD first: default rudder `0 deg`, rudder-induced title/H1 wording, selected-speed envelope summary table, regenerate report, browser-verify no JavaScript errors.
3. Preserve row-count invariants: `3528` requested rows, `441` extra `4.56 kn` default rows, `3969` total rows.
