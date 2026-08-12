# KCS validation referent resolution for #1173

**Date:** 2026-08-11
**Branch:** `plan/1173-calm-water-hull-resistance`
**Scope:** evidence only. This document does not revise the plan, does not change any gate, and does not
propose an implementation. It pins the reference condition that #1173's V1 criterion would gate against,
from primary sources.

> **Revision note.** The first version of this document (commit `a663802f`) concluded that the referent's
> attitude could not be pinned, because Hino (2005) is print-only. That conclusion was **wrong, and is
> withdrawn**. It rested on looking for the *printed proceedings* rather than for the *workshop's own
> published case definition*, which is online and states the condition explicitly. §2, §3 and §5 are
> rewritten; the recommendation moves from "4 %, attitude inferred" to "3 %, referent pinned". The
> corrections in §4 survive, with §4.7 revised and §4.10 withdrawn. Superseded claims are marked.

**Why it exists.** Two adversarial rounds rejected the plan for #1173, and both rejections trace to the same
root: reference-condition provenance. Revision 1 gated a fixed body against the free-to-sink-and-trim value.
Revision 2 corrected that, then re-scored Wu (2025)'s with-rudder grids against a bare-hull referent inside
the table rebutting the finding. Reviewer r2 marked every Shen-derived number UNTESTED because that paper had
never been retrieved.

**The rule this document applies.** A bare Ct is worthless. The referent is the tuple
(attitude, appendage, S used for normalisation, nu/Re). A Ct quoted without its tuple cannot be gated against
at any tolerance.

---

## 0. Retrieval log — what was actually opened

Every workshop page below was fetched and tag-stripped **by me directly**, not accepted from a delegated
reading. Two of the load-bearing pages were originally located by a delegated search; both were then
re-fetched and verified independently before being relied on here.

| Source | Retrieved? | Notes |
|---|---|---|
| **CFD Workshop Tokyo 2005, Test Case 1.1 (KCS towed)** — `nmri.go.jp/archives/.../cfdws05/Detail/a1-1/index.html` | **YES** | **The decisive document.** This is the case Shen cites as "Hino, 2005". States attitude, appendage, Fr and Re. |
| **CFD Workshop Tokyo 2005, test matrix** — `.../cfdws05/data.htm` | **YES** | Shows the Fixed/Free distinction is deliberate. |
| **Tokyo 2005 EFD table** — `.../cfdws05/KCS/kcs_integral_variables_table.htm` | **YES** | Ct, S/L², U_D, and the bare-hull normalisation note. |
| **Gothenburg 2000 KCS description** — `.../cfdws05/gothenburg2000/KCS/container.html` | **YES** | "bare hull and fixed model"; names the two test sites. |
| **Gothenburg 2000 KCS geometry and conditions** — `.../gothenburg2000/KCS/kcs_g&c.htm` | **YES** | States Re, Lpp, **S_DWL**, V_m, and the fixed orientation. |
| **Gothenburg 2000 EFD comparison table** — `.../gothenburg2000/KCS/kcs_comparison_table.htm` | **YES** | Ct, C_F0, C_R, S/L² and the "hull only (no rudder)" note. |
| Shen, Wan & Carrica (2015), *Ocean Engineering* 108:287–306 | **YES — full text** | PDF from the corresponding author's SJTU page, `pdftotext -layout`. **First retrieval in this effort** — closes r2's UNTESTED finding. |
| Wu, P.-C. (2025), *Mathematics* 13(11):1788 | **YES — full text** | `mdpi-res.com` PDF path (the `mdpi.com` HTML path 403s to automated fetch). |
| **T2015 Case 2.1 instructions** — `t2015.nmri.go.jp/Instructions_KCS/Case_2.1/Case_2-1.html` | **YES** | Carries the with-rudder normalisation statement, rho, nu, six speeds. |
| **T2015 Case 2.1 EFD data file** — `.../Case_2.1/vary_Fr_2-1.xls` | **YES — decoded** | Downloaded and parsed with `xlrd`. Previously unopened by anyone in this effort. |
| T2015 geometry sheet; T2015 results presentation (Kim, KRISO) | **YES** | Model 1 particulars; slide-5 EFD benchmark. |
| Hino, T. (ed.) (2005), *Proceedings of CFD Workshop Tokyo 2005*, printed volume | **NO** | Print-only. **No longer blocking** — the workshop's own case definition is online and is what the volume documents. |
| Kim, Van & Kim (2001), *Exp. Fluids* 31:567–578 | **NO** | Closed access. The experimenters' own paper; see §3. |
| Springer, T2015 assessment volume Ch. 3 (Kim, KRISO) | **NO** | Paywalled, and covers the T2015 lineage, not Tokyo 2005. |
| 24th ITTC Resistance Committee report | located by delegated search; **not independently verified by me** | Reported to tabulate "1.1 KCS — Fixed". Cited below only as corroboration, explicitly flagged. |

---

## 1. The referent card

| # | Ct | Attitude | Appendage | S used for normalisation | nu and Re | Primary source | Retrieved? | Confidence |
|---|---|---|---|---|---|---|---|---|
| **A/B** | **3.56e-3** (Shen transcribes it as 3.55e-3) | **fixed, even keel — STATED** | **without rudder / bare hull — STATED** | **S_DWL = 9.4379 m², S/L² = 0.1781 — STATED**, "hull only (no rudder) … static orientation without waves" | **Re = 1.4e7 — STATED** (both G2000 and T2005). V_m = 2.1962 m/s stated ⇒ nu = 1.1418e-6. Water temperature not stated | CFD Workshop Tokyo 2005 Case 1.1; G2000 KCS description, geometry-and-conditions, and EFD table | **YES — all fields** | **HIGH on every field** |
| **C** | **3.711e-3** | **free to heave and pitch** (`FR_Zθ`); measured σ = −1.394e-2 m, τ = −0.169° | **with rudder** | **S₀/Lpp² = 0.1803 with rudder** = 9.5531 m² — STATED verbatim | **nu = 1.27e-6, rho = 999.5, Re = 1.26e7** — all stated | T2015 Case 2.1 instructions; `vary_Fr_2-1.xls`; results presentation | **YES — all fields** | **HIGH on every field** |
| **D** | 3.557e-3 | — | — | — | — | provenance **not established** | **NO** | **none — do not use** |
| **E** | 3.711e-3 *as used by Wu for a fixed case* | Wu's run: fixed even-keel | Wu's KCS: **with rudder** | **not stated by Wu** | **Re = 1.46e7** (verbatim caption) | Wu (2025) §2.3, §3.1, Table 5 | **YES** | high — forces §4.4 |

### 1.1 The decisive quotes

**CFD Workshop Tokyo 2005, Test Case 1.1 — verbatim.** This is the case Shen et al. cite as "Hino, 2005",
and it states the condition outright:

> "Test Case 1.1 — KCS
> **Conditions**
> Towing condition in still water
> **Fixed(even keel)**
> **Without rudder**
> Froude number (Fn) 0.26 | Reynolds number (Rn) 1.4×10⁷"

and, for the reference data:

> "Experimental results are available — Table (CFD Workshop 2000), U_D = 1.0 [%C_T]"

Note the EFD pointer: **Tokyo 2005 did not run a new KCS resistance test.** It re-served the Gothenburg 2000
(KRISO) table. That single line closes the "are 3.55e-3 and 3.56e-3 the same measurement" question.

**Tokyo 2005 test matrix — verbatim.** The fixed/free distinction is deliberate, and KCS is only ever fixed:

| Test case | Condition |
|---|---|
| KCS | **Towed (Fixed)** |
| DTMB5415 | Towed (Fixed) |
| DTMB5415 | **Towed (Free)** |
| KVLCC2M | Towed (Fixed) |
| KCS | Self propelled |
| KVLCC2M | Obliquely towed |

The workshop ran a free-to-sink-and-trim variant — on DTMB 5415, not on KCS.

**Gothenburg 2000, KCS description — verbatim:**

> "The KCS was conceived to provide data for both explication of flow physics and CFD validation for a modern
> container ship with bulb bow and stern (i.e., ca. 1997). **The conditions include bare hull and fixed
> model.** No full-scale ship exists."
>
> "Korea Research Institute for Ships and Ocean Engineering performed towing-tank experiments to obtain
> resistance and wave field. Ship Research Institute of Japan performed towing-tank experiments to obtain hull
> surface pressure and velocity field downstream of propeller plane…"

**Gothenburg 2000, KCS geometry and conditions — verbatim:**

> "Re=1.4x10⁷, Fr=0.26
> Model length between perpendiculars, L_pp = 7.2786 m
> **Wetted Surface Area, S_DWL = 9.4379 m²**
> Model speed, V_m = 2.1962 m/s
> **Full-scale bare-hull geometry in fixed static orientation**"

**Tokyo 2005 EFD table, "Data and Data Uncertainty for Unpropelled KCS Integral Variables" — verbatim:**

> `S/L² = 0.1781*` · `C_T = 3.56x10⁻³` · `U_D % = 0.64`
> "* EFD wetted surface area is for hull only (no rudder as per KRISO web site) and for static orientation
> without waves, i.e., S_DWL"

The Gothenburg 2000 copy of the same table adds `C_F0 = 2.83x10⁻³`, `C_R = 0.731x10⁻³`, and
"& EFD C_R is defined as C_R = C_T − C_F0".

**Shen et al. (2015), §4.2, p. 294 — verbatim:**

> "The ship model is ﬁxed at even-keel condition with a service speed of 2.196 m/s, corresponding to Fr = 0.26
> and Re = 1.4 × 10⁷."

This sentence describes *Shen's simulation*. It happens to coincide with the experiment's condition — but
that is because Shen matched the workshop case, not because the sentence documents the experiment. The plan's
Blocker-1 disposition leaned on it as evidence of the experiment's condition; the workshop pages above are
the actual evidence, and they say the same thing.

**Shen et al. (2015), Table 5 — verbatim:**

| | Experiment | Present Work | % Error | CFDShip-Iowa (DES) |
|---|---|---|---|---|
| C_T | 3.55 × 10⁻³ | 3.52 × 10⁻³ | −0.958 | 3.58 × 10⁻³ |
| C_P | 7.18 × 10⁻⁴ ᵃ | 6.99 × 10⁻⁴ | −2.674 | 7.37 × 10⁻⁴ |
| C_F | 2.83 × 10⁻³ ᵇ | 2.82 × 10⁻³ | −0.530 | 2.84 × 10⁻³ |
| W_n | 0.686 | 0.742 | 8.120 | 0.723 |

> ᵃ Computed by C_P = C_T − C_F.  ᵇ By ITTC 1957 friction line C_F = 0.075/(log₁₀ Re − 2)².

**T2015 Case 2.1 — verbatim** (row C):

> "Resistance coefficients are based on wetted surface area ( S₀/L_PP² = 0.1803 ) **with rudder** for static
> orientation in calm water."
>
> "Same with G2010 case2.2b" · "With rudder" · \(FR_{Z\theta}\) · rho = 999.5 · nu = 1.27×10⁻⁶

**T2015 Case 2.1 EFD data, decoded from `vary_Fr_2-1.xls`:**

| Fr | CT×10³ | σ×10² (m) | τ° |
|---|---|---|---|
| 0.108 | 3.796 | −0.09 | −0.017 |
| 0.152 | 3.641 | −0.275 | −0.053 |
| 0.195 | 3.475 | −0.599 | −0.097 |
| 0.227 | 3.467 | −0.944 | −0.127 |
| **0.260** | **3.711** | **−1.394** | **−0.169** |
| 0.282 | 4.501 | −1.702 | −0.159 |

**Wu (2025) — verbatim** (row E):

> §2.3: "Except for the KCS hull appended with a rudder, KVLCC2 and JBC were both in bare hull condition."
> §3.1: "First, neglecting the ship vertical motion, i.e., the fixed ship attitude… all in the even-keel condition."
> Table 5 caption: "…KCS (static mesh, **Re =1.46 × 10⁷**, Fr = 0.26)."

### 1.2 Two lineages, cleanly separated

The apparently inconsistent literature is **two distinct campaigns**, each internally consistent:

| | Gothenburg 2000 → Tokyo 2005 Case 1.1 | Gothenburg 2010 2.2b → T2015 Case 2.1 |
|---|---|---|
| Ct at Fr 0.26 | **3.56e-3** | **3.711e-3** |
| Attitude | **fixed, even keel** | **free to heave and pitch** |
| Appendage | **without rudder** | **with rudder** |
| S / Lpp² | **0.1781** (S_DWL = 9.4379 m²) | **0.1803** (9.5531 m²) |
| Re | **1.4e7** | **1.26e7** (nu = 1.27e-6, rho = 999.5) |
| Speeds | single (2.1962 m/s) | six (0.915 … 2.379 m/s) |
| Sinkage/trim published | **none** — consistent with a restrained model | yes, measured |

The T2015 page states the linkage itself: "Same with G2010 case2.2b".

**Internal consistency checks, all passing:**

```
S_DWL/Lpp^2      = 9.4379 / 52.9780 = 0.17815   vs stated 0.1781      OK
Cf_ITTC57(1.4e7) = 2.8320e-3                    vs stated C_F0 2.83e-3 OK
C_T - C_F0       = 3.56 - 2.83 = 0.730e-3       vs stated C_R 0.731e-3 OK
nu = V_m*Lpp/Re  = 2.1962 x 7.2786 / 1.4e7 = 1.1418e-6
Fr = V_m/sqrt(g*Lpp) = 0.25990                  vs stated 0.26        OK
```

The workshop reduced its EFD at exactly Re = 1.4e7 using the ITTC-57 line. So Re = 1.4e7 is not a loose
round number attached to the data after the fact — it is the case's defining condition and the basis of its
own data reduction.

For row C, the with-rudder normalisation is confirmed a second way, arithmetically. The measured resistance
at Fr 0.26 is 85.44 N:

```
85.44 / (0.5 x 999.5 x 9.5531 x 2.196^2)  =  3.7111e-3   <- matches the published EFD exactly
85.44 / (0.5 x 999.5 x 9.4379 x 2.196^2)  =  3.7564e-3   <- does not
```

### 1.3 A physical point neither review nor either plan revision made

The 1.22 % bare-versus-appended difference is a **bookkeeping-consistency** hazard, not a physical resistance
difference. If the rudder's drag per unit wetted area equalled the hull's mean, fitting the rudder would leave
Ct unchanged — force and reference area scale together. It bites only in the mismatch case: a with-rudder
force divided by a bare-hull area, or the reverse. Since both lineages are now shown to be internally
consistent, **the hazard is fully discharged for the referent** and survives only as a rule for future rows.

---

## 2. Recommendation

### 2.1 The tuple #1173 should gate against — all fields STATED

```
Ct       = 3.56e-3                              [STATED - Tokyo 2005 / Gothenburg 2000 EFD table]
attitude = fixed, even keel                     [STATED - T2005 Case 1.1; G2000 description]
appendage= without rudder (bare hull)           [STATED - T2005 Case 1.1; G2000 description; EFD note]
S        = 9.4379 m^2  (S_DWL, S/Lpp^2 = 0.1781)[STATED - G2000 geometry and conditions]
Re       = 1.4e7                                [STATED - both workshops]
V_m      = 2.1962 m/s, Lpp = 7.2786 m           [STATED - G2000 geometry and conditions]
  => nu  = 1.1418e-6 m^2/s                      [derived from the three stated quantities]
U_D      = 0.64 % (T2005 EFD table) or 1.0 % (T2005 Case 1.1 page) - the workshop states both; use 1.0 %
```

**The referent is pinned. No field is an inference.**

### 2.2 The referent value should be 3.56e-3, not 3.55e-3

The plan gates on 3.55e-3, taken from Shen's Table 5. The workshop that produced the data publishes
**3.56e-3**, in two identical tables, with a self-consistent reduction (`C_R = C_T − C_F0` gives 0.730 against
the published 0.731). Shen's 3.55e-3 is a transcription rounding.

The plan's own Stage-0 rule settles this: *"If the primary source disagrees with 3.55e-3, the primary source
wins and V1/V2a/V2b are recomputed before any solving."* It disagrees, by 0.28 %. Recomputed:

```
V1  centre:  Ct_ref  = 3.56e-3
V2a centre:  Cp_ref  = 3.56e-3 - Cf_ITTC57(1.4e7) = 0.7280e-3   (plan has 0.7180e-3)
             - or the workshop's own published C_R = 0.731e-3
V2b centre:  Cf_ITTC57(1.4e7) = 2.8320e-3                       (unchanged)
```

### 2.3 Tolerance: 3 % is now properly founded

`U_S` (1.22 %) and the `U_Re` term proposed in this document's first version are **both resolved and both
leave the budget** — S and Re are stated, not inferred. What remains:

```
RSS(U_D 1.00, U_SN 1.39, U_i 0.24)  =  1.73 %      (U_D = 1.0 %, the workshop's Case-1.1 figure)
RSS(U_D 0.64, U_SN 1.39, U_i 0.24)  =  1.55 %      (U_D = 0.64 %, the workshop's EFD-table figure)
```

**Recommend keeping V1 at 3 %.** It now rests on a pinned referent rather than on a fork carried as a
systematic, and it leaves ~1.7× margin over the budget. The plan's own conditional — *"if a reviewer can pin
Hino (2005), the tolerance derivation tightens to 2.5 %"* — is technically triggered, and 2.5 % is defensible.
I do not recommend it: `U_SN = 1.39 %` is still borrowed from Wu's grid rather than measured on ours, and
tightening a gate on a borrowed uncertainty buys nothing.

**This document's first version recommended 4 %. That recommendation is withdrawn.**

### 2.4 What the fixture should record

- Row A/B as above, every field `provenance: stated` with its workshop citation.
- **Row C as a complete free-condition series** — the six (Fr, Ct, σ, τ) points of §1.1, with
  `attitude: free_heave_pitch`, `appendages: rudder`, `S: 9.5531`, `nu: 1.27e-6`, `rho: 999.5`. It costs
  nothing and it is what a future free-to-sink-and-trim V3 would gate against.
- The `provenance` marker per field should be kept even though every field is now `stated` — it is what stops
  the next reader from re-introducing an inference silently.

---

## 3. What remains unresolved

Materially reduced from this document's first version. Nothing below blocks #1173.

| # | Item | Impact | What would close it |
|---|---|---|---|
| **U1** | **The experimenters' own report.** Every statement of the condition comes from the *workshop's case specification* (Gothenburg 2000 / Tokyo 2005 organisers) or the ITTC's record of it — not from KRISO's own write-up. Kim, Van & Kim (2001) *Exp. Fluids* 31:567–578 is closed access; the KRISO "Experiment Conditions and Present Status" document that the EFD note refers to ("as per KRISO web site") was never captured by the Wayback Machine. | **Low.** The workshop specification is the document the EFD was published under and is what every downstream user has validated against. Corroborated by the deliberate Fixed/Free split in the test matrix, and by the absence of any published KCS sinkage or trim datum. | Kim, Van & Kim (2001) via institutional access; or the KRISO description archive. |
| **U2** | **Water temperature / exact nu.** Re = 1.4e7 and V_m = 2.1962 m/s are stated, so nu = 1.1418e-6 follows — but no source states the tank temperature, so whether Re = 1.4e7 is nominal-rounded or exact is unknown. | **Very low.** The workshop reduced its own EFD at Re = 1.4e7 using ITTC-57, so matching Re = 1.4e7 reproduces the reference reduction by construction. | Hino (2005) printed condition sheet; SRI/NMRI test report. |
| **U3** | **U_D is stated twice, differently** — 0.64 % in the EFD table, 1.0 % on the Case 1.1 page. | **Low.** Using 1.0 % is the conservative choice and is what the plan already does. | Hino (2005). |
| **U4** | **Provenance of 3.557e-3.** Not established. A third rounding of the same measurement is the likely explanation, but it is not demonstrated. | **Low**, now that the primary value 3.56e-3 is pinned — but the number should not be used. | A workshop or ITTC document tabulating it with its condition. |
| **U5** | **Wu (2025) never states his reference area.** | **Low**, and it only affects how Wu's grids are re-scored, not the referent. Strongly inferable: Wu computes `E%D` against D = 3.711e-3, which T2015 defines on the with-rudder area, so Wu must be using 9.5531 m². | Correspondence with the author. |

---

## 4. Corrections forced by this retrieval

### 4.1 The referent value is 3.56e-3, not 3.55e-3

See §2.2. The plan's 3.55e-3 is Shen's rounding; the workshop publishes 3.56e-3 with a self-consistent
reduction. 0.28 % — small, but it is the gate's centre, and the plan's own rule says the primary source wins.
`Cp_ref` moves from 0.7180e-3 to 0.7280e-3, a **1.4 % shift in V2a's centre**, which matters more than the
0.28 % on V1 because the residuary is a small difference of large numbers.

### 4.2 Shen's Table 14 rows are transposed in the plan — and the conclusion drawn from them is backwards

**Plan asserts:** Shen S3 coarse 1.68 M = 3.528 (+0.62 %); S2 medium 4.26 M = 3.526 (+0.68 %); S1 fine
10.58 M = 3.516 (+0.96 %).

**Shen Table 14 actually reads:**

| Grid | ID | Mesh (M) | C_p (10⁻³) | C_v (10⁻³) | C_t (10⁻³) | Error (%) |
|---|---|---|---|---|---|---|
| EFD | | | | | 3.55 | |
| Fine | S1 | 10.58 | 0.6611 | 2.865 | **3.526** | −0.663 |
| Medium | S2 | 4.26 | 0.6684 | 2.859 | **3.528** | −0.631 |
| Coarse | S3 | 1.68 | 0.6988 | 2.817 | **3.516** | −0.959 |

The plan's values are the right set **rotated by one grid level**. Consequence: the plan's Blocker-4
disposition claims "against 3.55e-3 its error grows from 0.62% to 0.96% across 1.68→10.58 M". This is
**backwards** — 0.959 % is the *coarse* grid; the fine grid is at 0.663 %. Shen's error *shrinks* with
refinement. The plan manufactured a counterweight against its own correct rebuttal out of a row rotation.

Also corrected: "1.68 → 10.58 M moves it 0.34 percentage points" — the actual move is 3.516 → 3.526, i.e.
**0.28 points**.

Shen's own classification, verbatim: "The total resistance coefﬁcients (C t ) presents oscillatory convergence
with R G = −0.1667"; "The grid uncertainty of C t is only 0.1701%".

Correct in the plan: "Shen production run 1.675 M → 3.52e-3", and the Cp span quoted for V2a.

### 4.3 Shen's "Experiment" column contains exactly one measured quantity — the plan's "independent corroboration" is circular

Shen's footnotes make C_P and C_F **derived**: `C_P = C_T − C_F`, `C_F` by ITTC-57. The only measured
resistance datum is C_T.

**Plan asserts:** *"the value is corroborated independently — Shen's tabulated CP = 7.18e-4 reproduces
3.55e-3 − Cf_ITTC57(1.4e7) to three significant figures … Two sources, one arithmetic identity."*

**That corroboration does not exist.** C_P reproduces the difference because Shen defined it as the
difference. The same circularity applies to the workshop's own `C_R = C_T − C_F0`, and to the plan's
`Cr = Ct - Cf = 7.17955e-4 <-- Shen tabulates 7.18e-4. Exact to 3 s.f.`, which is a tautology presented as a
verification.

**Consequence for V2a.** There is **no published experimental KCS pressure or residuary coefficient** — the
V2a reference is the gated EFD number minus a correlation line. V2a imports no independent measurement. The
plan's synthetic-vector demonstration that V2a can fail while V1 passes remains **mathematically valid**, and
V2a still catches compensating errors; what is false is the *provenance* claim that it is checked against
published experimental data.

*(Genuine corroboration of the referent does now exist — the workshop's 3.56e-3 and Shen's 3.55e-3 are the
same measurement, so they are not independent either. The referent rests on one KRISO measurement, well
documented.)*

### 4.4 Wu's KCS is with-rudder and fixed even-keel — r2 was right, now sourced

Wu §2.3 and §3.1 (quoted in §1.1) establish it. The plan's re-scored table treats Wu's grids as
bare-hull-comparable; they are not.

### 4.5 NEW — Wu's static KCS runs at Re = 1.46e7, not 1.4e7. Neither review nor either plan revision noticed.

```
Cf(1.46e7) = 2.8121e-3      Cf(1.40e7) = 2.8320e-3      difference = 0.0199e-3 = 0.56 % of Ct
```

Wu's Ct values are biased **low by ~0.56 %** relative to the referent's 1.4e7. So Wu's grids differ from the
referent on **three** axes at once — rudder, Reynolds, and normalisation area. Re-scored against the pinned
3.56e-3, with the Reynolds adjustment applied but the rudder and area still uncorrected:

| Wu grid | published (Re 1.46e7) | Re-adjusted to 1.40e7 | plan's claimed error | error vs pinned 3.56e-3 |
|---|---|---|---|---|
| S3 coarse 0.208 M | 3.755 | 3.775 | −5.775 % | **+6.04 %** |
| S2 medium 0.572 M | 3.555 | 3.575 | −0.141 % | **+0.42 %** |
| S1 fine 1.640 M | 3.527 | 3.547 | +0.648 % | **−0.37 %** |

**The plan's "six of seven published fixed-condition results land inside 1%" should be withdrawn.** It is the
stated justification for tightening below revision 1's 5 %, and it mixes condition-matched with
condition-mismatched grids. The honest statement is narrower and still supportive:

> The four **condition-matched** results in the literature — Shen's three grids plus his production run, all
> bare-hull, fixed even-keel, at Re 1.4e7 — land at −0.90 %, −0.96 %, −1.12 % and −1.24 % against the pinned
> 3.56e-3. All four sit inside 1.3 %, consistently low.

That is a better argument for a 3 % gate than the withdrawn one, because every point in it is
condition-matched.

### 4.6 NEW — Wu scores his *fixed* case against D = 3.711e-3, and his own text explains why that is a free-condition datum

Wu uses D = 3.711 for both his static and dynamic cases. His text settles the reading:

> "…all C_T is increased by the ship's vertical motions and becomes closer to D = 3.711 (Table 5). For S1 in
> Tables 5 and 10, the under-predicted C_T error is reduced to less than 1% from approximately 5%."

Combined with the `FR_Zθ` attitude code and the measured −13.94 mm sinkage, Blocker 1's direction is confirmed
three independent ways. **But the magnitude is over-attributed.** Wu's static and dynamic runs are at
different Reynolds numbers (1.46e7 vs 1.26e7). Of the 0.1668e-3 gap between his static S1 (3.527) and dynamic
S1 (3.69383), `Cf(1.26e7) − Cf(1.46e7) = 0.0710e-3` — **42.6 %** — is pure friction. The attitude effect is
~2.6 % of D, **not ~5 %**.

### 4.7 NEW — the 4.54 % "condition offset" is one-third Reynolds, not all attitude

```
total gap (3.711e-3 @ Re 1.26e7  vs  3.55e-3 @ Re 1.4e7)  = 0.1610e-3 = +4.54 %
pure ITTC-57 friction (Re 1.26e7 vs 1.40e7)               = 0.0510e-3 = +1.44 %  (31.7 % of the gap)
residual: attitude + appendage + normalisation            = 0.1100e-3 = +3.10 %
```

The plan calls the mismatch "threefold, not twofold", so the structure is acknowledged — but the 1.44 %
Reynolds share is never quantified and the whole 4.54 % is repeatedly described as though attitude drove it.
The residual 3.10 % agrees with the ~2.6 % attitude effect measured independently in §4.6 plus a small
appendage term — two independent estimates converging.

### 4.8 REVISED — the plan's `U_S = 1.22 %` should be removed from the budget entirely

*(This supersedes the first version of this document, which recommended carrying U_S as a declared one-sided
bias.)* S is now **stated**: `S_DWL = 9.4379 m²`, `S/L² = 0.1781`, "hull only (no rudder)". There is no fork.
U_S leaves the RSS. The `U_Re = 0.96 %` term proposed in this document's first version also leaves — Re is
stated and is the basis of the workshop's own data reduction. Budget becomes
`RSS(1.00, 1.39, 0.24) = 1.73 %`.

The plan's flagged "single largest open item" — *"if the with-rudder normalisation turns out to be correct,
the referent itself moves to 3.593e-3"* — is **resolved in the plan's favour and can be struck.**

### 4.9 Minor — the plan's with-rudder wetted surface, 9.5527 m², matches nothing

```
0.1803 x 52.9780 = 9.5519 m^2        9.4379 + 0.1152 = 9.5531 m^2   <- organiser's particulars
```

A transcription slip; immaterial (0.01 %), but the fixture should carry 9.5531 m².

### 4.10 Minor — Shen's wetted area is 9.4376 m², the workshop's is 9.4379 m²

Shen Table 3 lists `AW = 9.4376`. Use the workshop's **9.4379 m²**, which is the case specification.

### 4.11 WITHDRAWN — this document's first version wrongly criticised the plan's viscosity note

The first version argued that the plan's *"nu set to reproduce Re = 1.4e7 … matching the reference condition
exactly"* overstated the match, because Re = 1.4e7 was thought to be Shen's simulation condition rather than
the experiment's. **That criticism is wrong and is withdrawn.** Re = 1.4e7 is stated by both workshops as the
case condition, and the workshop reduced its own EFD at that Re. The plan's `nu = 1.1416e-6` reproduces it:
the workshop's stated `V_m = 2.1962 m/s` gives `nu = 1.1418e-6`, a 0.02 % difference arising only from the
plan's use of the rounded `U = 2.196 m/s`. The plan's wording is defensible; the fixture should record which
U it used.

### 4.12 What survives unchanged — verified, not merely unchallenged

- Revision 2's central move — abandoning 3.711e-3 as the referent for a fixed-body run — is **correct**, and
  is now confirmed from the workshop's own case definitions.
- **The plan's assumed tuple was right on every field it guessed** — fixed even keel, bare hull,
  S = 9.4379 m², nu ≈ 1.1416e-6, Re = 1.4e7. What was missing was the evidence, not the values. Only the Ct
  itself moves, by 0.28 % (§4.1).
- The plan's quotation of the T2015 normalisation statement is **verbatim-accurate**.
- The 1.22 % arithmetic is **correct**; the plan was right to refuse to assert it resolved on the evidence it
  then had. It is now resolved.
- The plan's rebuttal of r1's Blocker 4 is **correct**, and was under-claimed (§4.2).
- The plan's V3 withdrawal rationale is **confirmed at full precision** from the primary data file:
  Ct(0.282)/Ct(0.260) = 4.501/3.711 = **1.2129** (plan: 1.213); Ct(0.260)/Ct(0.227) = 3.711/3.467 =
  **1.0704** (plan: 1.069). The series is indeed free-to-sink-and-trim with a rudder.
- The degenerate-case check is **correct**, and slightly stronger against the pinned referent:
  Ct = 2.832e-3 ⇒ **−20.45 %**.
- Shen's production-run figures (1,675,465 cells; 24 processors; 6.9 h wall clock; Ct = 3.52e-3) are
  **confirmed verbatim**.
- Wu's iteration counts and Table 14 timing data were **not re-checked** — outside this exercise's scope.

---

## 5. Bottom line for the owner

**The referent is pinned, on every field, from the workshops that published the data.**

```
Ct = 3.56e-3 · fixed even keel · without rudder · S_DWL = 9.4379 m^2 (S/Lpp^2 = 0.1781)
             · Re = 1.4e7 · V_m = 2.1962 m/s · Lpp = 7.2786 m  =>  nu = 1.1418e-6
```

The document that unblocked this was not Hino's printed proceedings — it was the Tokyo 2005 workshop's own
Case 1.1 page, which states "Towing condition in still water / Fixed(even keel) / Without rudder" at
Fn 0.26 and Rn 1.4×10⁷, and points its reference data at the Gothenburg 2000 table. **This document's first
version was wrong to report the referent unpinnable**; it searched for the printed volume instead of the
workshop's published case specification.

What this changes for #1173:

1. **V1's centre moves from 3.55e-3 to 3.56e-3**, and V2a's from 0.7180e-3 to 0.7280e-3 (a 1.4 % shift).
2. **Tolerance stays at 3 %**, now founded on `RSS(U_D, U_SN, U_i) = 1.73 %` with no unresolved fork. The
   first version's 4 % recommendation is withdrawn.
3. **`U_S = 1.22 %` leaves the budget.** The plan's self-declared "single largest open item" is resolved, and
   resolved the way the plan hoped.
4. **The "six of seven inside 1 %" claim should be withdrawn** and replaced with the four condition-matched
   Shen results at −0.90 % to −1.24 % — a narrower but genuinely like-for-like argument for the same gate.
5. **Stage 0 is largely done by this document.** What remains is transcribing the tuple into the fixture with
   citations, not retrieving anything.
6. `test_reference_row_is_fixed_even_keel_bare_hull` now asserts something the primary source states. It was
   the right test to call the most important one.

Committing ~19 days of compute against this referent is defensible.
