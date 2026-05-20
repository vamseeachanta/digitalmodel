# OCIMF Coefficient Lookup — Cross-Project Prompt Template

Paste the block below into a new chat session (Claude / Codex / Gemini / Hermes) when you need OCIMF MEG3/MEG4 wind or current coefficient values for a specific hull and condition.

Two variants are provided:
- **Variant A — Same-machine** (the other project runs on the same workstation that has `/mnt/ace/acma-codes/`)
- **Variant B — Different-machine** (the other project doesn't have local access to the corpus)

---

## Variant A — Same-machine prompt

```
I need OCIMF MEG3/MEG4 coefficient values for the following vessel and condition. Compute the right lookup keys, retrieve the values from the digitized corpus, interpolate where needed, and flag any coverage gaps.

INPUTS
- Vessel class:           [tanker | gas-carrier-LNG | gas-carrier-LPG | other]
- Loading state:          [loaded | ballast-40-percent-T | other-percent]
- Hull bow shape:         [cylindrical | conventional | n/a-for-gas]
- Tank shape (gas only):  [prismatic | spherical-Moss | n/a-for-tanker]
- Vessel LBP:             [meters]
- Vessel draft T:         [meters]
- Water depth:            [meters]
- Free-stream current:    [m/s] (or n/a if no current)
- Wind speed at 10m:      [m/s] (or n/a if no wind)
- Heading angles wanted:  [list of degrees, 0=bow-on, 180=stern-on]
- % full-load condition:  [0-100, for K-factor correction]

CORPUS LOCATIONS (read-only)
- Digitized lookup tables (master):
  /mnt/ace/acma-codes/OCIMF/OCIMF Coef.xlsx
  (17 sheets, 3 data sheets: "Data 5a-9a", "Data 10a-14a", "Data 16a-19a")
- Interactive coefficient explorer (open in browser to verify visually):
  /mnt/local-analysis/digitalmodel/docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html
- Full MEG3 (2008) standard PDF:
  /mnt/ace/acma-codes/OCIMF/OCIMF - 2008 - Mooring Equipment Guidelines.pdf
- Full MEG4 (2018) standard PDF:
  /mnt/ace/acma-codes/OCIMF (MEG 4)/Mooring Equipment Guidelines - MEG4.pdf
- Per-figure PDF extracts:
  /mnt/ace/acma-codes/OCIMF/Figures/
- Alternative WebPlotDigitizer extraction (A5-A9 only, for cross-validation):
  /mnt/ace/acma-codes/OCIMF 3rd ed/Digitizer/

CORPUS SCHEMA
- Secondary axis is WD/T ratio (water-depth/draft), NOT displacement.
  Digitized WD/T values: 1.05, 1.1, 1.2, 1.5, 3, >4.4, >6
- Tanker tables (Figures A5-A14) carry CURRENT coefficients only.
- Gas carrier tables (Figures A17-A19) carry WIND coefficients only.
- Figure A15 is ABSENT from the digitization (gap).
- Figure A16 gives the current velocity correction K factor.
- Tanker WIND and gas-carrier CURRENT are NOT in this corpus.

FIGURE INDEX
| Figure | Coefficient | Vessel class      | Condition         | Secondary axis            |
|--------|-------------|-------------------|-------------------|---------------------------|
| A5-A9  | Cxc         | Tanker            | Loaded            | WD/T = 1.1, 1.2, 1.5, 3.0, >4.4; bow shape (Cyl/Conv) |
| A10    | Cyc         | Tanker            | Loaded            | WD/T = 1.05, 1.1, 1.2, 1.5, 3, >6     |
| A11    | Cxyc        | Tanker            | Loaded            | WD/T as A10                            |
| A12    | Cxc         | Tanker            | Ballast 40% T     | Stat envelope (max/mean/min) + 4.4 Cyl ref |
| A13    | Cyc         | Tanker            | Ballast 40% T     | WD/T (1.1, 1.2 Conv; 1.1, 1.5, 4.4 Cyl) |
| A14    | Cxyc        | Tanker            | Ballast 40% T     | WD/T as A13                            |
| A16    | K factor    | Any               | Any               | % full-load (0-100) x WD/T            |
| A17    | Cxw         | Gas carrier       | n/a               | Tank type (Prismatic, Spherical)      |
| A18    | Cyw         | Gas carrier       | n/a               | Tank type                              |
| A19    | Cxyw        | Gas carrier       | n/a               | Tank type                              |

CONVENTIONS
- Heading angle theta: 0 deg = bow-on flow, 90 deg = beam-on, 180 deg = stern-on
- Symmetric vessel: tables span 0-180 deg only; mirror for 180-360 deg
- Sign convention: vessel-fixed frame
  - Cxw/Cxc > 0 means force toward stern (when flow is ahead of beam)
  - Cyw/Cyc > 0 means force toward starboard (transverse wind/current from port)
  - Cxyw/Cxyc > 0 means bow-to-starboard yaw moment

REQUIRED OUTPUT
For each requested heading, return a structured record:

{
  "heading_deg": <float>,
  "wdt_ratio_computed": <float>,
  "wdt_keys_used": [<bracketing values, e.g., 1.5 and 3>],
  "interpolation_method": "linear" | "exact-match" | "extrapolation-warning",
  "figures_used": ["A10", "A11", ...],
  "wind": {
    "Cxw": <float|null>,
    "Cyw": <float|null>,
    "Cxyw": <float|null>,
    "reference_area_T_m2": "<caller supplies>",
    "reference_area_L_m2": "<caller supplies>",
    "Fxw_N": <float|null>,    // only if reference areas + wind speed supplied
    "Fyw_N": <float|null>,
    "Mxyw_Nm": <float|null>
  } | null,
  "current": {
    "Cxc": <float|null>,
    "Cyc": <float|null>,
    "Cxyc": <float|null>,
    "K_factor": <float|null>,
    "Vc_design_m_s": <float|null>,   // free-stream * K
    "Fxc_N": <float|null>,
    "Fyc_N": <float|null>,
    "Mxyc_Nm": <float|null>
  } | null,
  "coverage_gaps": [<list of strings, e.g., "tanker-wind-not-in-corpus">],
  "citation": {
    "code_id": "OCIMF-MEG4",
    "publisher": "Oil Companies International Marine Forum",
    "revision": "2018",
    "wiki_path": "knowledge/wikis/engineering/wiki/standards/ocimf-meg4.md",
    "figure_refs": [<list of A-numbers>]
  }
}

RULES
1. If WD/T falls between digitized points, linearly interpolate the coefficient between the bracketing WD/T curves at the same heading. Report which keys were used.
2. If heading is between digitized angle steps (5 deg for tanker current, 10 deg for gas carrier wind), linearly interpolate.
3. If the request is for tanker WIND or gas-carrier CURRENT, return null for those coefficients and add the gap string to coverage_gaps with the alternative source name.
4. If the request is for a non-tanker, non-gas-carrier vessel (FPSO, FSO, FSRU, container, bulker), return null for all coefficients and add the gap string "vessel-class-outside-OCIMF-scope".
5. If a calc module is going to consume this output, also emit the citation block so the downstream `Citation` sidecar can be built per .claude/rules/calc-citation-contract.md.
6. Do NOT extrapolate beyond the digitized WD/T or heading bounds without an explicit "extrapolation-warning" flag.
```

---

## Variant B — Different-machine prompt (no local corpus access)

```
I need OCIMF MEG3/MEG4 coefficient values for [vessel description as above]. I do NOT have local access to the digitized corpus.

Please:
1. Tell me whether the OCIMF coefficient for this case is publicly tabulated or only available in the OCIMF Mooring Equipment Guidelines 3rd or 4th edition (vendor-licensed).
2. If publicly approximable, give me a typical curve shape or representative values from open literature (Owens & Palo 1982, Kitamura 1997, RP-2SK Annex, etc.), with explicit warning that these may differ from the published OCIMF curves.
3. If only available via the OCIMF standard, return the gap immediately and recommend that I either:
   - Acquire the OCIMF MEG4 PDF through legitimate purchase (OCIMF website)
   - Request the digitized OCIMF Coef.xlsx from a colleague with a licensed copy
   - Use OCIMF 1994 "Prediction of Wind and Current Loads on VLCCs" if available
4. Do NOT fabricate coefficient values. Better to return a gap than to invent numbers.

Context: this query is for [mooring screening / detailed mooring design / academic study / other], so the precision requirement is [low / medium / high].
```

---

## Notes on usage

1. **Pre-flight check**: before pasting the prompt, run `ls /mnt/ace/acma-codes/OCIMF/` from the target machine to confirm the corpus is mounted.
2. **Citation footprint**: if the downstream code emits a `Citation` sidecar per the calc-citation contract, the AI's response should include the `citation` block verbatim — those fields map 1:1 to `digitalmodel.citations.schema.Citation`.
3. **Reference areas are caller-supplied**: the corpus contains coefficients only, not vessel-specific projected areas. The prompt asks for vessel `LBP` and `T` but transverse `A_T` and lateral `A_L` projected areas must come from the caller's vessel database — they vary with above-water structure and can change between the same hull on different sites.
4. **Verify visually**: open `ocimf_coefficient_explorer.html` in a browser to confirm the curve the AI quoted matches the published figure shape. The explorer's interactive hover lets you read off exact values for cross-checking.
5. **Hermes routing** (workspace-hub-specific): if dispatching to Hermes for execution-heavy use, route via the brain/hands model per `.claude/rules/goal-invocation.md` — Hermes can read both `/mnt/ace/acma-codes/` and `/mnt/local-analysis/` and emit Citation sidecars directly.

## Related
- [`ocimf_coefficient_explorer.html`](ocimf_coefficient_explorer.html) — interactive visualization of the same corpus
- [`ocimf_module_readme.md`](../../mooring/ocimf_module_readme.md) — digitalmodel OCIMF module user guide
- [workspace-hub#2284](https://github.com/vamseeachanta/workspace-hub/issues/2284) — wiki promotion of MEG3/MEG4 (citation wiki targets)
- [digitalmodel#556](https://github.com/vamseeachanta/digitalmodel/issues/556) — interpolation fix
- [digitalmodel#563](https://github.com/vamseeachanta/digitalmodel/issues/563) — OCIMFExcelAdapter scope
