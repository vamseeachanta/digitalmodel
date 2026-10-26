# Connected-tank sloshing page, revision 2: layout, chart and prose specification

Status: implemented scope and acceptance record for maintainer review, 2026-10-25. Sections 1–8 retain the earlier layout specification; Section 9 governs the final reporting layout and its verification limits. The specification, builder, generated page, chart CSVs and tests form one connected-tank page change.

- Page: `docs/api/cfd/dual-tank-sloshing-cfd-study.html` at dd24f2094, live at https://vamseeachanta.github.io/digitalmodel/cfd/dual-tank-sloshing-cfd-study.html.
- Builder: `scripts/capabilities/build_dual_tank_sloshing_study.py`, which writes the page.
- Model page to follow: `docs/api/cfd/sloshing-cfd-study.html` (the single-tank study). It has a picture of the physical system at the top, results charts placed with the findings, and short numbered sections.

## Implementation plan

- Scope: the connected-tank page, its builder, this specification and the existing page test module. Restore each historical revision entry, retaining only the required public-reference redaction; append the new revision. Keep the full qualification in Section 7, its executive-summary qualification and a short Appendix A link. Engineering values, claims, conclusions, evidence records, model inputs and all 13 chart CSVs remain unchanged.
- Sequence: run the starting suites; add regression tests before correcting defects; restore history and consolidate qualification through the builder; add rendered narrow-screen, print and interaction tests in the existing page test module; fix defects those tests expose; regenerate through the builder; run the suites, preservation, freshness and direct file checks; record results and limits.
- Checks: the starting report/evidence suite passed 99 tests and the model suite passed 24. Nine added checks cover historical rows, two qualification locations, exact-width rendered frames, print bounds and PDF output, and chart interaction with CSV downloads. Existing independent equation, evidence-binding, table-cell and CSV preservation checks remain enforced. The historical-date assertions now permit only the two restored undated entries. Final counts, direct file checks and unavailable checks are recorded with the implementation review.
- Superseded A11 measurement, 2026-10-25 (before the final reporting layout; not acceptance evidence for the current schematic): Chromium 151.0.7922.34 on Linux aarch64, outer viewport 1280 × 1000 px, separate iframes with zero border and exact widths of 390 px and 600 px. Both iframe bounding widths and `innerWidth` equal the requested widths. Vertical scrollbars leave document client widths of 375 px and 585 px respectively; document and body scroll widths equal those client widths, so neither iframe has sideways scroll. All 14 SVGs were measured: minimum rendered text sizes are 12.6 px and 20.2 px respectively, and no SVG text bounding box exceeds its SVG bounds. These measurements do not establish acceptance of the current schematic.
- Limits: rendered checks use the installed headless Chromium through Playwright and local repository assets; the test environment requires Playwright, a Chromium executable and the PDF reader in addition to pytest. Results cover the stated browser, widths and emulated touch and print. Other browser engines, physical touch devices and physical printing remain unmeasured. These checks establish no physical qualification, new CFD assessment or archived-source digest revalidation.

## 1. Review comments this revision answers

1. General comment (maintainer review, 2026-10-25): follow the single-tank page. Put a schematic at the top and a few charts in the summary, and make every chart interactive.
2. Comment 1, on the chart "Exchange natural period against conduit area": "Follow good plotting standards." Section 5 says what is wrong with that chart, and section 4.2 says what replaces it.
3. The page reads for an outside engineer in this order: what was found, then the limits, then the method. The current introduction states the analytical scope: "This study extends the analytical scope of the single-tank sloshing study to assess", and "an analytical screening model calibrated on CFD assessment". The page does not say that the connected model is based on the single-tank analysis. The page itself states that the connected model is calibrated on its own and that "a matched single-tank and connected-tank comparison is not established".

## 2. Fixed rules (every acceptance check depends on them)

- No figure changes. Every number printed today keeps its value, its precision and its source. The evidence record `docs/api/cfd/data/dual_tank_cfd_evidence.json`, the response record `docs/api/structural/sloshing-tld-twopeak.json` and the model sources are not edited.
- A new number may appear only where the builder derives it from a committed series. It is printed with its source and checked by a test (section 7, check A3).
- Status wording stays as it is: "Draft for maintainer review"; "screening"; "design qualification is not established".
- Public repository rules: no client, vessel, project or private-case names; no internal role or tracker words in the page.
- Style: the page keeps `../_assets/brand.css` and uses its tokens (`--ink`, `--navy`, `--teal`, `--accent`, `--warn`, `--red`, `--grid`, `--mut`, `--panel`). Charts use the brand series colours, in an order distinguishable under colour-vision deficiency, with no new palette. Fonts and sizes come from the stylesheet.
- No external scripts or stylesheets beyond `brand.css`. Interactivity uses inline SVG and, at most, one small inline script. Every chart and the schematic stays readable with scripts off.

## 3. Page order (new)

| # | Section (id) | Content |
| --- | --- | --- |
| 1 | Header | Title "Connected-tank sloshing analysis" (unchanged). Subtitle: "U-tube anti-roll tank: analytical screening model calibrated on archived CFD". Key-facts strip, as on the single-tank page: "Analytical model", "35 archived CFD cases (aggregate)", "Period within ±3 % at 4 of 5 checks", "Draft for maintainer review". |
| 2 | `schematic` | The schematic (4.1), first thing under the header. |
| 3 | `summary` | Summary prose (6.1), then summary charts C1, C2 and C3 (4.2), each with a one-line lead-in. |
| 4 | `limits` | Limits (6.2). Replaces today's "Validity band" and gathers the limit sentences now spread across Summary, Single-tank comparison, CFD evidence and Left. |
| 5 | `revisions` | Revision history: compact, newest first, collapsed in a `<details>` element. |
| 6 | `method` | Method (6.3), then today's subsections in this order: Illustrative geometry; Published validation; Computed curves; Single-tank comparison; CFD evidence (conduit-area sweep, inertia and natural period, fill sweep, loss scaling, forced-roll sweep and numerical checks, evidence classes). |
| 7 | `forward` | Way forward (unchanged list). |
| 8 | `left` | Left (unchanged text, minus sentences moved to Limits). |
| 9 | `basis` | Analysis basis (today's "Analysis basis" links, unchanged targets). |

Navigation links follow this order. The existing anchor ids `summary`, `revisions`, `geometry`, `validation`, `curves`, `comparison`, `limits`, `cfd-evidence`, `forward` and `left` are kept, so that inbound links still work. `limits` moves; `method` and `schematic` are new.

## 4. Figures

### 4.1 Schematic (static inline SVG; not data)

A transverse section of the illustrative U-tube system, drawn by the builder from its input constants, so the drawing changes when an input changes:

- two tank legs (rectangles), each 8.0 m wide (`ILLUSTRATIVE_LEG_WIDTH`), with centroids at ±10.0 m from the centreline (`ILLUSTRATIVE_CENTROID_OFFSET`);
- a horizontal conduit joining the legs at the bottom, labelled "conduit, area A_c = 8.0 m² (`ILLUSTRATIVE_CONDUIT_AREA`), effective length 5.2 m (calibrated reference, transfer assumed)";
- a free surface in each leg at fill depth 4.0 m (`ILLUSTRATIVE_FILL_DEPTH`), shown tilted: one leg higher by a level difference labelled Δh. The tilt is a sketch, not a computed state;
- the roll axis on the centreline, with a curved arrow and the label "roll angle φ";
- the legs' length 30.0 m (`ILLUSTRATIVE_LEG_LENGTH`), stated in a note, because it runs into the page;
- the leg height, not given by the study, drawn as a schematic height with the note "tank height not specified";
- every label in text in the SVG (not in a picture), with `role="img"`, a `<title>` and a `<desc>` that lists the values;
- the caption: "Schematic of the invented, illustrative U-tube system (transverse section; not to scale vertically). It represents no particular vessel."

Not on the schematic: the archived CFD twin-tank geometry, which "is not shown".

### 4.2 Summary charts

Each chart is interactive inline SVG built to section 5. It has a CSV beside it in `docs/api/cfd/data/charts/` and a "Data (CSV)" link under it. Each plotted series is listed with its source.

C1. "Predicted roll response with and without the connected tank (illustrative system)".
- x: forcing period (s), linear, ticks every 10 s over the computed range (16 to 60 s).
- y: roll per unit wave moment (normalized, no units), ticks at a round step from zero.
- Series:
  - "Bare vessel": continuous line, from `compute()["responses"][0]` over `compute()["periods"]`;
  - "With connected tank": continuous line, from `compute()["responses"][1]`.
- The band below 20 s is shaded and labelled "outside the published moment-fit band" (from `published_claims()`).
- Hover: at each computed period, both values and the period.
- CSV `coupled-response.csv`: `forcing_period_s,roll_bare_per_unit_moment,roll_with_tank_per_unit_moment`.
- Lead-in sentence (template; numbers derived by the builder from the CSV): "With the tank, the predicted peak roll falls from {bare_peak} to {tank_peak} per unit wave moment (vessel roll period 20.0 s, hull damping 5.0 %, alpha 12.0 %); this is a screening prediction for the invented system." Both peaks are printed to one decimal, and the test in check A3 computes them from the CSV.

C2. "Exchange natural period against conduit area (archived twin-tank CFD, 50 % fill)". This replaces today's chart; see section 5.
- x: conduit area (m²), linear and to scale, from 0 to 15 m², ticks every 3 m².
- y: exchange natural period (s), from 8 to 28 s, ticks every 4 s.
- Series, all as markers with no connecting lines:
  - "CFD-derived period": filled circles at 3.4, 9.5 and 13.5 m², from `period_checks[*].measured_period` where `fill == 50`;
  - "Calibration reference": a filled square at 6.8 m², from `calibration.measured_period`, labelled on the chart "calibration reference (not an independent check)";
  - "Calibrated model": hollow diamonds at the same four areas, from `period_checks[*].predicted_period` and `calibration.predicted_period`. Each diamond has a ±3 % whisker from `published_band`;
  - "Historical geometric-length model": grey crosses at 3.4, 6.8 and 13.5 m², from `area_sweep[*].historical_period`.
- No fitted curve: the archived geometry is not published, so the model cannot be drawn between the cases from committed inputs.
- Hover: the area, the fill, the period with its unit, the role (independent check or calibration reference) and the evidence pointer.
- CSV `natural-period-vs-area.csv`: `area_m2,fill_pct,role,cfd_period_s,model_period_s,historical_period_s,evidence_pointer`, with empty cells where a value does not exist.

C3. "Calibrated period error against the published ±3 % band".
- x: case (categorical: "3.4 m², 50 %", "13.5 m², 50 %", "6.8 m², 25 %", "6.8 m², 70 %", "9.5 m², 50 %"), in the order of `period_checks`.
- y: period error (%), from −5 to +5, ticks every 1 %.
- Series: markers only, from `period_checks[*].error`. The band from −3 % to +3 % (`published_band`) is shaded and labelled.
- A case outside the band uses the `--red` token and the label "outside the band (low fill)". It is selected by comparing the value with the band, not by its position.
- Hover: the case, the error to one decimal with its unit, the prediction and the CFD-derived period, and the band comparison.
- CSV `period-error.csv`: `area_m2,fill_pct,predicted_period_s,cfd_period_s,error_pct,band_pct,inside_band,evidence_pointer`.
- The calibration case is not plotted, because it is not an independent check. The lead-in says so.

### 4.3 Method charts (kept, brought to section 5)

| Chart (id) | Change |
| --- | --- |
| Rigid-column natural period, frozen-fluid roll inertia, quadratic-loss damping, tank-on-vessel moment (computed curves) | Continuous lines stay. Ticks become round values (today the x ticks are 0.0, 3.4, 5.4, ... from equal fifths of the range). Hover is added on the computed points, and a CSV is added for each |
| Normalized coupled roll response | Moves to Summary as C1. The Method section links to C1 and does not repeat it |
| Single-tank tuned-absorber benchmark | Continuous lines stay; round ticks, hover and a CSV are added. The title becomes "Single-tank tuned absorber (comparison model)", because it is not a benchmark of the connected model |
| Exchange amplitude against conduit area | Linear x axis to scale; markers for CFD and the historical analytical amplitude; no connecting lines; separate axis titles with units; hover; CSV |
| Exchange natural period against conduit area | Replaced by C2 (moves to Summary). The Method section keeps its table and links to C2 |
| Period prediction error: area / fill | Replaced by C3 (moves to Summary). The Method section keeps its table |
| Natural period against fill; equivalent damping against fill | Markers only (two discrete fills), "CFD-derived" against "Predicted"; no lines; hover; CSV |
| Equivalent damping against conduit area | Linear x axis; markers for CFD-derived damping and the original inverse-area prediction; no lines; hover; CSV |
| Medium-mesh forced exchange | Markers at the six periods; no line; round ticks; hover; CSV |
| Evidence classes (case-count chart) | Removed. Two counts in different evidence classes are not a series; a line between them implies a trend. The table that follows stays and already carries the counts |

## 5. Plotting standards (all charts on the page)

1. The title says what is plotted against what, and for which system: illustrative system or archived CFD geometry.
2. Each axis has its own title with units, for example "Conduit area (m²)" and "Natural period (s)". The two axes are never merged into one string such as "Natural period (s); area (m²)".
3. Ticks are round values on a 1, 2 or 5 step, evenly spaced, and printed at one consistent precision per axis. A data value is never used as a tick, and no tick is repeated.
4. A numeric axis is linear and to scale. A categorical axis is used only for true categories, such as named cases.
5. A discrete case is a marker: a distinct shape per series, sized to be visible at 390 px, with a legend inside the chart area. A line is drawn only for a continuous model evaluation. No line joins discrete cases, cases from different conditions, or unrelated series. The phrase "connecting lines guide the eye" does not appear.
6. A calibration point is shown apart from the independent checks, and named so.
7. Bands (a validity band or a published tolerance) are shaded, labelled, and taken from the source record.
8. Hover: every marker and every computed point has a tooltip giving the series, both values with units, the case label and the source pointer. A native SVG `<title>` per point is the baseline, so it works without scripts. Markers can also take keyboard focus (`tabindex="0"`), and a tap on a touch screen shows the same text.
9. A CSV sits beside each chart in `docs/api/cfd/data/charts/<chart-id>.csv`. It is written by the builder, UTF-8 with LF line endings, has a header row naming each column with its unit, and holds the plotted values at source precision. A "Data (CSV)" link sits under the chart.
10. The true minus sign is used. Percentages show one decimal place, and the evidence record keeps the source precision (as today).
11. The chart fits 390 px without sideways scroll. Text in the SVG renders at 12 px or more at that width; long legends wrap below the plot.
12. Accessibility: `role="img"`, `<title>` and a `<desc>` that states the data range and the series, as today.

What is wrong with "Exchange natural period against conduit area" today, item by item:
- The three areas (3.4, 6.8 and 13.5 m²) sit at equal spacing on a categorical axis, so the step from 6.8 to 13.5 m² looks as long as the step from 3.4 to 6.8 m², and the trend is distorted (standard 4).
- The y-axis ticks are the data values themselves (10.6, 11.0, 14.3, 14.3, 19.3, 19.7). "14.3" appears twice, the spacing is uneven, and the labels crowd (standard 3).
- One merged axis string, "Natural period (s); area (m²)", sits on the y axis, and the x axis has no title (standard 2).
- Lines join three discrete archived cases, which implies a measured curve between them (standard 5).
- The 6.8 m² point is the calibration reference, which agrees by construction, but it is drawn like an independent check (standard 6).
- The independent 9.5 m² check and the historical geometric-length prediction, both in the tables, are missing.
- There is no ±3 % band, no hover and no CSV (standards 7 to 9).

C2 replaces it, with the error view in C3.

## 6. Prose (the writer's text; the builder carries it word for word)

### 6.1 Summary

"This study extends the analytical scope of the single-tank sloshing study to assess a U-tube style anti-roll arrangement: two connected tanks with invented geometry, joined by a conduit. The geometry represents no particular vessel or client. The connected-tank model is an analytical screening model calibrated on CFD assessment: the aggregate results of 35 cases from an earlier, archived CFD campaign on a separate twin-tank geometry. Analysis is performed to determine the connected-tank response and how closely the calibrated model reproduces the archived CFD; the key results are summarized below."

Findings (a bulleted list; each number printed through `evidence_value` with its pointer):
- "Conduit loss: a calibrated area exponent of 2.138 fits the archived damping with 8.6 % RMS over 35 cases. The simple inverse-area law, refitted on its own, gives 36.5 % RMS." (`loss.exponent`, `loss.rms`, `loss.cases`, `loss.linear_rms`)
- "Natural period: the calibrated model reproduces the CFD-derived exchange natural period within the published ±3 % band at four of five off-calibration checks. The low-fill check (25 %) is 3.7 % high. The calibration case agrees by construction (14.31 s against 14.31 s)." (`period_checks[*].error`, `published_band`, `calibration`). The count "four of five" is computed by the builder from the band comparison, not typed in.
- "Response: with the tank, the predicted peak roll of the invented system falls from {bare_peak} to {tank_peak} per unit wave moment. This is a screening prediction, not a design result." (C1)

### 6.2 Limits

"These results support screening only; design qualification is not established. The moment fit holds for forcing periods of 20 s and longer (4.3 % RMS) and degrades below about 16 s; the cause is not established. Transfer of the calibration to the invented geometry is an assumption, also inside that band; the new geometry's natural period must be measured before the effective conduit length is recalibrated. The single-tank comparison illustrates peak splitting and merging only: single and connected tanks are not compared at matched mass, tuning, hull damping and forcing. Continuous fill coverage, anti-roll phase and anti-roll benefit remain unestablished."

Every clause above is today's page text, gathered in one place. The duplicates are removed from Summary, Single-tank comparison and Left (one statement in one place).

### 6.3 Method

"The connected-tank model and the tuned-absorber model are evaluated at the illustrative inputs below. The archived CFD evidence is reported separately: the evidence record gives the source reduction, field and digest of each value, and raw fields and the archived geometry are excluded. The analytical scope follows the single-tank sloshing study; the connected-tank model is calibrated on its own."

The other prose stays as it is, apart from these changes:
- The chart lead-ins follow section 4.
- "connecting lines guide the eye" is deleted everywhere.
- "Single-tank tuned-absorber benchmark" becomes "Single-tank tuned absorber (comparison model)".

## 7. Builder job: inputs, outputs and acceptance checks

Inputs, read-only:
- `docs/api/cfd/data/dual_tank_cfd_evidence.json`;
- `docs/api/structural/sloshing-tld-twopeak.json`;
- the model sources the builder already loads;
- this specification.

Outputs:
- the regenerated `docs/api/cfd/dual-tank-sloshing-cfd-study.html`;
- the CSVs in `docs/api/cfd/data/charts/`;
- the builder change in `scripts/capabilities/build_dual_tank_sloshing_study.py`;
- tests in `tests/hydrodynamics/test_dual_tank_study_builder.py`.

The prose in section 6 is carried word for word. Prose is not rewritten in the job; a wording problem goes back to the writer.

Acceptance checks. Each is a test unless it says otherwise.
- A1 Freshness: running the builder reproduces the committed page and every CSV byte for byte, and the generated-HTML freshness check passes for this page.
- A2 No figure changes: the evidence and response records are unchanged (digest). Every table cell printed at dd24f2094 appears with the same text in the new page, apart from the tables named as moved in section 4.3, whose cells also appear unchanged.
- A3 Derived numbers: `bare_peak`, `tank_peak` and "four of five" equal the values computed from `coupled-response.csv` and from the band comparison of `period_checks`.
- A4 CSVs: each chart links its CSV. The header names its units, and the rows equal the plotted values and their source fields.
- A5 Axes: each axis has its own title with a unit in parentheses (or "no units"). The tick labels are evenly spaced round values with no repeats, and no tick label equals a data value unless that value is a round tick. On numeric axes, marker x positions are proportional to value (linear to scale).
- A6 Markers and lines: in the evidence charts and in C2 and C3, the marker count per series equals the source row count, and there is no `<polyline>` or path joining discrete cases. Lines appear only in the computed-curve charts and C1.
- A7 Hover: every marker and computed point has a `<title>` holding both values with units.
- A8 Schematic: an SVG with id `schematic` comes before `#summary`. It contains the labels for both legs, the conduit (with A_c and its value), the free surfaces, Δh and "roll angle φ", and its values equal the builder constants.
- A9 Order: the `h2` ids appear in the section 3 order, and the kept anchor ids exist.
- A10 Wording: the page contains "This study extends the analytical scope of the single-tank sloshing study to assess" and "an analytical screening model calibrated on CFD assessment". It does not contain "based on the benchmarked single tank" or "connecting lines guide the eye". "validated", "verified" and "proven" appear only where they appear today.
- A11 Rendered acceptance: tests in the existing page test module measure document scrolling and all SVG text bounds in exact-width 390 px, 600 px and desktop iframes; cover light and dark themes and scripts disabled; check print layout and a generated PDF; exercise pointer, keyboard, touch, Escape dismissal and CSV downloads. Section 9 records the current measured results.
- A12 Public repository: no client, vessel, project or private-case names, and no internal role or tracker words, on the page or in the CSVs.
- The PR body lists every test run, with its platform, and every test not run, with the reason.

## 8. Change list for the review panel (the writer fills it from the built page)

One line per change, with its reason:
- schematic added (review comment, general);
- three summary charts (general comment);
- the natural-period chart replaced (comment 1, plotting standards);
- the prediction-error chart moved and banded;
- every chart interactive with a CSV (general comment);
- the summary rewritten findings first, then limits, then method (outside-engineer reading order);
- the limits gathered in one place (one statement in one place);
- "connecting lines" removed (plotting standard 5);
- the evidence-class chart removed (not a series);
- "benchmark" renamed in the single-tank comparison title (it is a comparison model);
- the newsletter wording aligned where the page backs it; "based on the benchmarked single tank" not used (the page calibrates the connected model on its own).

The maintainer's own comments on revision 2 govern over this list.


## 9. Final reporting layout and readability correction

The rendered measurements below were recorded at `a86bae42`. They are retained as historical evidence for that report version. They do not establish rendered acceptance of the schematic changed at `a096a582`.

- Scope: retain all analytical inputs, calibration records and 13 CSV exports. Separate the illustrative response in Sections 5.1–5.3 from archived comparisons in Section 5.4. Define alpha at first use, identify the illustrative screening prediction and retain the unestablished physical benefit. Clarify terms, damping conventions and assumed comparison inputs. Place revision history directly after the summary and keep source locators in Section 9.2.
- Sequence: follow the implementation plan above. Preserve the earlier revision rows individually; redact only the internal public-reference identifiers in the reporting-conventions entry. Keep the following revision verbatim and append a new row. Appendix A links to Section 7 instead of repeating its qualification paragraph. The qualification sentence occurs only in the executive summary and Section 7.
- Rendered layout: Chromium 151.0.7922.34 on Linux aarch64, outer viewport 1440 × 1000 px, zero-border iframes at exactly 390 px, 600 px and 1280 px. All disclosure sections are expanded. At each width, iframe, document client, document scroll and body scroll widths equal the requested width; no sideways scrolling occurs. All 14 SVGs have text bounding boxes inside their rendered bounds. Minimum rendered SVG text is 13.1 px at 390 px and 15.9 px at 600 px and desktop; schematic text is 16.6 px and 19.6 px respectively. Light and dark themes pass at 390 px, as does scripts-disabled rendering. Reference paths wrap in screen and print layouts. These measurements supersede the earlier measurements and arithmetic-only acceptance statement.
- Print and interaction: A4 print media with 16 mm margins produces a 23-page PDF containing the numbered sections, appendices and all 14 figure captions. Figures and tables fit the printable area and retain break avoidance. Minimum rendered SVG text is 12.3 px; schematic text is 15.1 px, with no text bounds outside an SVG. Each of the 13 charts passes pointer hover, keyboard focus and Tab traversal, emulated touch, Escape dismissal and byte-identical CSV download checks. Pointer events during scrolling retain the focused point's readout.
- Reporting: use alpha for the normalized tank stiffness scale; the 10.0 to 5.7 response reduction is the illustrative screening prediction. Preserve the 0.2964–0.3264 frequency-error record and display 0.30–0.33 % so its bounds remain distinguishable. Display roll moments in MN m while retaining raw evidence values and exports. Explain the over-critical equivalent damping and the percent-to-ratio conversion.
- Limits: the automated tests use a local headless browser; no solver is started. PDF text extraction and rendered bounds do not establish physical printing quality or every pagination preference. Other browser engines and physical touch devices remain unmeasured. Physical qualification, continuous fill coverage and calibration transfer remain unestablished. Visual review remains with the maintainer.

## 10. Current report verification record

The report at `a096a58209e33af5806a8e84472e2f0207e60b45` has SHA-256 `592721f644e8727640638f0b3001fa9c93f82fa1670470109e027162b6ef6412`. Appendix C is a visible four-column revision-history table after Appendix B, with ten retained entries. It is outside a disclosure element; Section 1 has no revision-history link.

The focused builder, evidence and model suites collected 137 tests on 2026-10-26: 130 passed and seven errored during browser setup because the Playwright Chromium executable was absent. The passing tests include byte-identical report and CSV regeneration, evidence bindings, plotted values, numerical equations, schematic geometry and revision-history preservation. Browser setup errors supply no current rendered measurements. Screen widths, font sizes, print pagination and interactions from Section 9 remain historical; those historical measurements must not be attributed to the changed schematic, whose rendered acceptance requires a separately identified successful run.

The pull-request description already contained the current checksum, visible Appendix C placement and a separately attributed rendered-check record when inspected. That separate record was not reproduced by this local run. The report, builder, model, evidence and CSV files are unchanged by this verification-record correction.
