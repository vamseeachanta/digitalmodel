# KCS validation referent resolution for #1173

**Date:** 2026-08-11
**Branch:** `plan/1173-calm-water-hull-resistance`
**Scope:** evidence only. This document does not revise the plan, does not change any gate, and does not
propose an implementation. It pins — or fails to pin — the reference condition that #1173's V1 criterion
would gate against, from primary sources.

**Why it exists.** Two adversarial rounds rejected the plan for #1173, and both rejections trace to the same
root: reference-condition provenance. Revision 1 gated a fixed body against the free-to-sink-and-trim value.
Revision 2 corrected that, then re-scored Wu (2025)'s with-rudder grids against a bare-hull referent inside
the table rebutting the finding. Reviewer r2 marked every Shen-derived number UNTESTED because that paper had
never been retrieved. This document retrieves the papers.

**The rule this document applies.** A bare Ct is worthless. The referent is the tuple
(attitude, appendage, S used for normalisation, nu/Re). A Ct quoted without its tuple cannot be gated against
at any tolerance.

---

## 0. Retrieval log — what was actually opened

| Source | Retrieved? | How | Notes |
|---|---|---|---|
| Shen, Z.; Wan, D.; Carrica, P.M. (2015), *Ocean Engineering* 108:287–306 | **YES — full text** | PDF from the corresponding author's institutional page at Shanghai Jiao Tong University (`dcwan.sjtu.edu.cn/userfiles/22.pdf`), text-extracted with `pdftotext -layout` | Tables 3, 4, 5, 13, 14 and the §4.2 condition paragraph read directly. **First retrieval of this paper in the #1173 effort** — it closes r2's UNTESTED finding. |
| Wu, P.-C. (2025), *Mathematics* 13(11):1788 | **YES — full text** | `mdpi-res.com` PDF path, text-extracted | §2.3, Table 1, Tables 4–6, Tables 10–11 read directly. The `mdpi.com` HTML path 403s to automated fetch; the `mdpi-res.com` path does not. |
| **T2015 Case 2.1 instructions to participants** (`t2015.nmri.go.jp/Instructions_KCS/Case_2.1/Case_2-1.html`) | **YES — raw HTML, verified twice** | Direct fetch; independently re-fetched and tag-stripped rather than trusted from a delegated read | Organiser-published. Carries the normalisation statement, rho, nu, and the six-speed table. |
| **T2015 Case 2.1 EFD data file** (`.../Case_2.1/vary_Fr_2-1.xls`) | **YES — decoded** | Downloaded and parsed with `xlrd` | Organiser-published binary. Gives the complete six-speed EFD series at full precision. Previously unopened by anyone in this effort. |
| T2015 workshop results presentation, Jin Kim (KRISO), `Presentations/Day2-AM2-KCS-Resistance_SP-Kim.pdf` | **YES** | Direct fetch | Organiser-published. Slide 5 tabulates the EFD benchmark `D` at Fr 0.26. |
| T2015 geometry and conditions (`t2015.nmri.go.jp/kcs_gc.html`) | **YES** | Direct fetch | Organiser-published Model 1 particulars and attitude key. |
| NMRI archive, "Comparison Table for Unpropelled KCS Integral Variables" (`nmri.go.jp/archives/.../cfdws05/gothenburg2000/KCS/kcs_comparison_table.htm`) | **YES — raw HTML** | Direct fetch, tags stripped | Organiser-published. Carries the bare-hull normalisation note for the Gothenburg-2000-lineage EFD. |
| **Hino, T. (ed.) (2005), *Proceedings of CFD Workshop Tokyo 2005*, NMRI** | **NO** | — | Print proceedings; no online full text located. **Every 3.55e-3 in this effort remains a transcription from Shen.** See §3, U1. |
| Springer, *Numerical Ship Hydrodynamics: An Assessment of the Tokyo 2015 Workshop*, Ch. 3 (Kim, KRISO), `10.1007/978-3-030-47572-7_3` | **NO** | — | **Paywalled** (per-chapter purchase). Title, author and abstract only. Would be the authoritative EFD write-up. |

Nothing below is quoted from a secondary source and presented as primary. Where a number is second-hand, the
card says so.

---

## 1. The referent card

Every candidate Ct that has appeared in the issue, the plan, or either review, with its tuple.

| # | Ct | Attitude | Appendage | S used for normalisation | nu and Re | Primary source, table/page | Retrieved? | Confidence |
|---|---|---|---|---|---|---|---|---|
| **A** | **3.56e-3** | **NOT STATED** | **hull only, no rudder** (stated verbatim) | **S/Lpp² = 0.1781**, i.e. 9.4354 m², "static orientation without waves, i.e. S_DWL" (stated) | **not stated** | NMRI archive, Comparison Table for Unpropelled KCS Integral Variables (Gothenburg-2000 lineage, hosted under the Tokyo-2005 workshop archive) | **YES** | **S: high** (verbatim). **Attitude: none.** **Re: none.** |
| **B** | **3.55e-3** | Shen's *own model* is "fixed at even-keel"; the **experiment's** attitude is **NOT STATED** | Shen's own case is "the bare hull KCS"; the **experiment's** appendage state is **NOT STATED** | **not stated for the EFD value.** Shen's own Table 3 lists AW = 9.4376 m² "Wetted area without rudder" | Shen states **Re = 1.4e7** for *his simulation*. No nu, no temperature, and no Re given for the experiment | Shen et al. (2015), Table 5 ("Experiment" column) and §4.2, p. 294 | **YES** (Shen). **NO** (Hino 2005, the actual origin) | **Value: high.** **Attitude and appendage: INFERRED, not stated.** |
| **C** | **3.711e-3** | **free to heave and pitch** — attitude code `FR_Zθ`, defined by the organiser as "free to heave and pitch". Measured sinkage σ = −1.394e-2 m and trim τ = −0.169° at this speed | **with rudder** — stated three independent times in organiser documents | **S₀/Lpp² = 0.1803 with rudder**, i.e. 9.5531 m² — **stated verbatim** in the instructions to participants | **nu = 1.27e-6 m²/s, rho = 999.5 kg/m³, Re = 1.26e7** — all three stated verbatim | T2015 Case 2.1 instructions page; `vary_Fr_2-1.xls`; results presentation slide 5 | **YES — fully** | **HIGH on every field.** This is the only fully pinned tuple. |
| **D** | **3.557e-3** | — | — | — | — | provenance **not established** by this retrieval | **NO** | **none — do not use** |
| **E** | 3.711e-3 *as used by Wu for a **fixed** case* | Wu's run: **fixed even-keel** (stated) | Wu's KCS: **with rudder** (stated) | **not stated by Wu** | **Re = 1.46e7** (verbatim in the Table 5 caption) | Wu (2025), §2.3, §3.1, Table 5 | **YES** | high — and it forces correction §4.4 |

### 1.1 The load-bearing quotes

**T2015 Case 2.1, instructions to participants — verbatim** (row C). This is the single most consequential
sentence for the wetted-surface question, and I re-fetched and tag-stripped the page myself rather than
accept a delegated reading of it:

> "Resistance coefficients are based on wetted surface area ( \(S_0/{L_{PP}}^2=0.1803\) ) **with rudder** for
> static orientation in calm water."

> "Comparison Error, \(E\%D=(D-S)/D \times 100\), where \(D\) is the EFD value, and \(S\) is the simulation
> value."

and, from the conditions block on the same page:

> "Same with G2010 case2.2b" · "With rudder" · "Calm water condition" · \(FR_{Z\theta}\) ·
> \(L_{PP} = 7.2786\) [m] · \(g = 9.81\) [m/s²] · \(\rho = 999.5\) [kg/m³] ·
> \(\nu = 1.27 \times 10^{-6}\) [m²/s]

**T2015 Case 2.1 EFD data, `vary_Fr_2-1.xls`, sheet "KCS Case2.1_EFD data" — decoded verbatim** (row C).
This file had not been opened by anyone in this effort; the delegated researcher could not decode it. It is
the authoritative six-speed series:

| Fr | CT×10³ | Sf×10² (m) | Sa×10² (m) | σ×10² (m) | τ° |
|---|---|---|---|---|---|
| 0.108 | 3.796 | −0.195 | 0.015 | −0.09 | −0.017 |
| 0.152 | 3.641 | −0.613 | 0.063 | −0.275 | −0.053 |
| 0.195 | 3.475 | −1.213 | 0.015 | −0.599 | −0.097 |
| 0.227 | 3.467 | −1.75 | −0.138 | −0.944 | −0.127 |
| **0.260** | **3.711** | −2.465 | −0.322 | **−1.394** | **−0.169** |
| 0.282 | 4.501 | −2.71 | −0.695 | −1.702 | −0.159 |

> "σ: mean sinkage  σ=(Sf+Sa)/2" · "τ°: trim  τ°=arctan((Sf−Sa)/Lm)*180/pi" ·
> "a positive (+) sinkage value is defined upwards and a positive (+) trim value is defined bow up."

The Fr = 0.260 row is corroborated independently by the workshop results presentation (Jin Kim, KRISO,
"Report of the Results for KCS Resistance & Self-Propulsion (Case 2-1, 2-5, and 2-7)", 2015-12-03 at NMRI),
slide 5, whose `EFD(KRISO)` benchmark row reads `D`: CT 3.711, sinkage σ×10² −1.394, trim τ −0.169.

**Arithmetic proof that 3.711e-3 is normalised on the with-rudder area.** The measured total resistance at
Fr 0.26 is 85.44 N. With the stated rho = 999.5 and U = 2.196:

```
85.44 / (0.5 x 999.5 x 9.5531 x 2.196^2)  =  3.7111e-3   <- matches the published EFD exactly
85.44 / (0.5 x 999.5 x 9.4379 x 2.196^2)  =  3.7564e-3   <- does not
```

The coefficient is reproducible only on S₀ = 9.5531 m² (hull + rudder). This closes the question for row C
by two independent routes: the organiser's own statement, and the force-to-coefficient arithmetic.

**NMRI archive comparison table, notes — verbatim** (row A):

> `*  EFD wetted surface area is for hull only (no rudder as per KRISO web site) and for static orientation without waves , i.e., S DWL`
>
> `&  EFD C R is defined as C R =C T -C F0`

with the tabulated EFD row `S/L² = 0.1781*`, `C_T = 3.56x10⁻³`, `C_F0 = 2.83x10⁻³`, `C_R = 0.731x10⁻³`.

**Shen et al. (2015), §4.2, p. 294 — verbatim** (row B):

> "The ship model is ﬁxed at even-keel condition with a service speed of 2.196 m/s, corresponding to Fr = 0.26
> and Re = 1.4 × 10⁷. This was one of the benchmark cases in the CFD Workshops of Tokyo 2005 and Gothenburg
> 2010, and high-quality data is available for comparison."

Read this sentence carefully: **it describes Shen's simulation**, not the towing-tank test. Shen nowhere
states the attitude, appendage state, wetted surface or Reynolds number *of the experiment*. The plan's
Blocker-1 disposition treats this sentence as establishing the experiment's condition. It does not.

**Shen et al. (2015), Table 5 — verbatim** (row B):

| | Experiment | Present Work | % Error | CFDShip-Iowa (DES) |
|---|---|---|---|---|
| C_T | 3.55 × 10⁻³ | 3.52 × 10⁻³ | −0.958 | 3.58 × 10⁻³ |
| C_P | 7.18 × 10⁻⁴ ᵃ | 6.99 × 10⁻⁴ | −2.674 | 7.37 × 10⁻⁴ |
| C_F | 2.83 × 10⁻³ ᵇ | 2.82 × 10⁻³ | −0.530 | 2.84 × 10⁻³ |
| W_n | 0.686 | 0.742 | 8.120 | 0.723 |

> ᵃ Computed by C_P = C_T − C_F.
> ᵇ By ITTC 1957 friction line C_F = 0.075/(log₁₀ Re − 2)².

**Those two footnotes are the most consequential thing retrieved from Shen.** See correction §4.2.

**Wu (2025) — verbatim** (row E):

> §2.3: "Except for the KCS hull appended with a rudder, KVLCC2 and JBC were both in bare hull condition."

> §3.1: "First, neglecting the ship vertical motion, i.e., the fixed ship attitude, the CFD resistance test
> was simulated using static mesh. The three hull forms were all in the even-keel condition."

> Table 5 caption: "Total resistance coefficient and V & V result for KCS (static mesh, **Re =1.46 × 10⁷**,
> Fr = 0.26)."

### 1.2 The two lineages, and why the wetted surfaces differ

The retrieval resolves what looked like one inconsistent literature into **two distinct benchmark datasets
with two different, each internally consistent, normalisation conventions**:

| Lineage | Ct | Attitude | Appendage | S/Lpp² | S | Water |
|---|---|---|---|---|---|---|
| Gothenburg 2000 → Tokyo 2005 (rows A, B) | 3.56e-3 / 3.55e-3 | **unknown** | **hull only, no rudder** | **0.1781** | 9.4354–9.4379 m² | unknown |
| Gothenburg 2010 case 2.2b → Tokyo 2015 Case 2.1 (row C) | 3.711e-3 | **free to heave and pitch** | **with rudder** | **0.1803** | 9.5531 m² | nu 1.27e-6, rho 999.5, Re 1.26e7 |

The T2015 instructions page states the linkage itself: "Same with G2010 case2.2b".

The geometry arithmetic, from the organiser's own Model 1 particulars (hull 9.4379 m², rudder 0.1152 m²):

```
Lpp            = 7.2786 m          Lpp^2 = 52.9780 m^2
hull only      = 9.4379 m^2   ->  S/Lpp^2 = 0.17815
hull + rudder  = 9.5531 m^2   ->  S/Lpp^2 = 0.18032
ratio          = 1.01221      ->  1.221 %
```

So **0.1781 is the bare hull and 0.1803 is hull + rudder**, and the 1.22 % figure the plan carries is
arithmetically correct.

**This substantially resolves the plan's "single largest open item."** The plan feared that row B's 3.55e-3
might secretly be normalised on the with-rudder area, which would move the bare-normalised referent to
3.593e-3. The evidence says otherwise: the with-rudder convention belongs to the *other* lineage (G2010 /
T2015), and the lineage that row B belongs to publishes its EFD against `S/L² = 0.1781` with an explicit
"hull only (no rudder)" note. The residual risk is no longer "which S did they use" but the narrower "is
Shen's 3.55e-3 the same measurement as the NMRI table's 3.56e-3" — see §3, U2.

### 1.3 A physical point neither review nor either plan revision made

The 1.22 % is a **bookkeeping-consistency** hazard, not a physical resistance difference. If the rudder's drag
per unit wetted area equalled the hull's mean, fitting the rudder would leave Ct *unchanged* — force and
reference area scale together. The 1.22 % bites only in the mismatch case: a with-rudder force divided by a
bare-hull area, or the reverse. It is therefore a **discrete either/or provenance error**, not a random
uncertainty. This matters for the plan's budget — see correction §4.7.

---

## 2. Recommendation

### 2.1 The tuple #1173 should gate against

```
Ct       = 3.55e-3        (Shen et al. 2015 Table 5, "Experiment" column,
                           attributed there to NMRI EFD via Hino 2005)
attitude = fixed, even keel                     [INFERRED - not stated by any retrieved source]
appendage= bare hull, no rudder                 [STRONG - stated for the same lineage's 3.56e-3]
S        = 9.4379 m^2  (S/Lpp^2 = 0.17815)      [STRONG - organiser geometry sheet, and the
                                                 lineage's EFD is published on 0.1781]
Re       = 1.4e7  =>  nu = 1.1422e-6 at U = 2.1970 m/s, Lpp = 7.2786 m
                                                [Shen's SIMULATION condition, NOT the experiment's]
```

**Recommend gating on this tuple, with the tolerance widened to 4 %, and with `attitude` and the water
condition declared `provenance: inferred` in the fixture rather than asserted as pinned.**

Reasons:

1. Its appendage state and normalisation are now corroborated by an organiser-published source — the NMRI
   table's explicit "hull only (no rudder)" note at the bare-hull ratio 0.1781 (§1.2).
2. Row C (3.711e-3) is now **positively excluded** for a fixed-body run: T2015 Case 2.1 is `FR_Zθ`, "free to
   heave and pitch", with a *measured* sinkage of −13.94 mm and trim of −0.169° at Fr 0.26. Revision 2's
   re-anchoring away from 3.711e-3 was correct, and this retrieval strengthens it decisively.
3. Wu (2025) supplies independent physical support for the direction of the offset, from one paper and one
   solver: his fixed even-keel KCS lands ~4.5 % below D = 3.711e-3, and releasing heave and pitch closes the
   gap. Wu's own words: "all C_T is increased by the ship's vertical motions and becomes closer to D = 3.711".
   A fixed body belongs below 3.711e-3, which is where 3.55e-3 sits.

### 2.2 But the tolerance should be 4 %, not 3 %

The plan's 3 % comes from `RSS(U_S 1.22, U_D 1.00, U_SN 1.39, U_i 0.24) = 2.12 %`, rounded up. Two inputs do
not survive retrieval:

- **U_S must not be RSS'd.** It is a discrete either/or (§1.3), not a random term. And it is now mostly
  *resolved* in favour of the bare hull (§1.2), so carrying 1.22 % as a random uncertainty is doubly wrong.
- **A new and larger term is required.** "Re = 1.4e7" is a two-significant-figure round number and is *Shen's
  simulation* condition. The experiment's water temperature is unknown — the T2015 lineage states nu
  explicitly, the Tokyo-2005 lineage does not. Over a plausible band Re ∈ [1.35e7, 1.45e7], ITTC-57 Cf spans
  2.8495e-3 to 2.8153e-3 — a **0.96 % of Ct** spread in the friction component alone, wholly unconstrained by
  anything retrieved. Call it U_Re.

Recomputed, treating U_S as a declared bias rather than a random term:

```
RSS(U_Re 0.96, U_D 1.00, U_SN 1.39, U_i 0.24)  =  1.98 %
plus, if U2 (below) resolves against us, a one-sided systematic of +1.22 %
```

**Recommended: V1 tolerance 4 %.** If Hino (2005) is retrieved and pins row B's attitude and water condition,
2.5 % becomes defensible and the plan's own argument for tightening applies in full.

### 2.3 What the fixture must record

Each field needs a `provenance` marker of `stated` or `inferred`, not merely a value. Row B's `attitude` and
water condition are **inferred**. A schema that requires the field but not its provenance lets the next reader
mistake an inference for a retrieval — which is exactly the failure mode that produced two rejections.

The fixture should additionally carry **row C as a complete, fully-pinned free-condition series** (the six
(Fr, Ct, σ, τ) points in §1.1, with `attitude: free_heave_pitch`, `appendages: rudder`, `S: 9.5531`,
`nu: 1.27e-6`, `rho: 999.5`). It costs nothing, it is the best-attested KCS data in existence, and it is what
a future free-to-sink-and-trim V3 would gate against.

---

## 3. What remains unresolved

| # | Unresolved item | Why it matters | What would close it |
|---|---|---|---|
| **U1** | **The attitude of the experiment behind 3.55e-3 / 3.56e-3.** No retrieved source states whether the towing-tank model was restrained in heave and pitch or free to sink and trim. Shen's "fixed at even-keel" describes his own computation. | This is the *entire* subject of Blocker 1. #1173 proposes to gate a fixed body. If the EFD was free, revision 2 has re-anchored onto a second mismatched referent — a different one from revision 1's, but still mismatched. | Hino (2005) proceedings, KCS case description; or Kim, Van & Kim (2001) *Exp. Fluids* 31:567–578, the original KRISO experiment; or the Gothenburg 2000 workshop KCS case sheet. |
| **U2** | **Whether row A (3.56e-3) and row B (3.55e-3) are the same measurement.** They differ by 0.28 %. Their residuaries differ more: row A publishes C_R = 0.731e-3; row B implies C_P = 0.718e-3, a 1.8 % gap. | If they are the same dataset, row A's explicit "hull only, no rudder" note transfers to row B and the wetted-surface fork closes completely. If not, row B's normalisation is still formally open. | Hino (2005); or a workshop document tabulating both. |
| **U3** | **The experiment's water condition (nu, temperature, Re) for the Tokyo-2005 lineage.** Nothing retrieved states it. | Drives U_Re = 0.96 %, now the largest quantified term in the budget. | Hino (2005) condition sheet; the KRISO/SRI test report. |
| **U4** | **Provenance of 3.557e-3.** It appears in the issue and in the literature; this retrieval did not establish where it comes from or what tuple it carries. | Until pinned it is a fourth unlabelled number in circulation. | A workshop or ITTC document tabulating it with its condition. |
| **U5** | **Wu (2025) never states the reference area for his C_T.** Table 1 lists both 9.4379 (hull) and 0.1152 (rudder) for KCS, and §2.3 says the KCS is appended with a rudder. | The plan re-scores Wu's grids against a bare-hull referent. | Partly closed by inference: Wu computes `E%D` against D = 3.711e-3, which is defined by T2015 on the **with-rudder** area, so Wu must be using 9.5531 m² for the comparison to be meaningful. Treat as **strongly inferred, not stated**. |

**The blocking item is U1, and it is blocked on a single document: Hino (2005).** These are printed workshop
proceedings from the National Maritime Research Institute; no online full text was located. The Springer
chapter that would be the modern authority (Ch. 3 of the Tokyo 2015 assessment volume, by Jin Kim of KRISO)
is **paywalled** and covers the T2015 lineage in any case, not Tokyo 2005.

Until someone obtains Hino (2005) — interlibrary loan, an NMRI request, or a co-author's reprint — **the
attitude of the 3.55e-3 referent is an inference, not a fact**, and #1173's self-declared "most important test
in the list" (`test_reference_row_is_fixed_even_keel_bare_hull`) would be asserting something no retrieved
source states.

That is the honest status. It does **not** mean #1173 cannot proceed. It means the fixture must record
`attitude: fixed_even_keel` with `provenance: inferred`, the tolerance must carry the consequence, and Stage 0
must be re-scoped from "pin the reference from the primary source" — which this exercise attempted and could
not complete — to "obtain Hino (2005), or accept and declare the inference".

---

## 4. Corrections forced by this retrieval

Every item below contradicts something currently asserted in the plan (revision 1 or 2) or in a review.

### 4.1 Shen's Table 14 rows are transposed in the plan — and the conclusion drawn from them is backwards

**Plan asserts** (re-scored literature table, and again in the Blocker 4 disposition):

| plan's row | plan's Ct ×10³ | plan's error |
|---|---|---|
| Shen S3 coarse 1.68 M | 3.528 | +0.62 % |
| Shen S2 medium 4.26 M | 3.526 | +0.68 % |
| Shen S1 fine 10.58 M | 3.516 | +0.96 % |

**Shen Table 14 actually reads:**

| Grid name | ID | Mesh size (M) | C_p (10⁻³) | C_v (10⁻³) | C_t (10⁻³) | Error (%) |
|---|---|---|---|---|---|---|
| EFD | | | | | 3.55 | |
| Fine | S1 | 10.58 | 0.6611 | 2.865 | **3.526** | −0.663 |
| Medium | S2 | 4.26 | 0.6684 | 2.859 | **3.528** | −0.631 |
| Coarse | S3 | 1.68 | 0.6988 | 2.817 | **3.516** | −0.959 |

The plan's three Ct values are the right set of numbers **rotated by one grid level**. The magnitudes
0.62/0.68/0.96 are individually correct; they are attached to the wrong grids.

**Consequence, and it is not cosmetic.** The plan states, in the Blocker 4 disposition:

> "Shen's Table 14 is a mild counterweight — against 3.55e-3 its error grows from 0.62% to 0.96% across
> 1.68→10.58 M, and Shen's own V&V classes Ct convergence as oscillatory"

This is **backwards**. The 0.959 % error belongs to the **coarse** 1.68 M grid; the fine 10.58 M grid sits at
0.663 %. Shen's error *shrinks* with refinement, from 0.96 % to 0.66 %. The plan manufactured a counterweight
against its own (correct) rebuttal of r1's Blocker 4 out of a row rotation. Corrected, Shen's data **supports**
the rebuttal instead of qualifying it.

Also corrected: the plan's "Shen: 1.68 → 10.58 M moves it 0.34 percentage points" — the actual move is
3.516 → 3.526, i.e. **0.28 percentage points**.

Shen's own classification, verbatim: "The total resistance coefﬁcients (C t ) presents oscillatory convergence
with R G = −0.1667" and "The grid uncertainty of C t is only 0.1701%, suggesting that the grid density has
limited effect on C t in the selected range of grid size."

The plan's "Shen production run 1.675 M → 3.52e-3" is **correct** (Table 5 "Present Work", on the
1,675,465-cell grid of Table 4), as is the Cp span quoted for V2a (Table 14 gives 0.6611/0.6684/0.6988;
Table 5 gives 0.699 present work, 0.737 CFDShip-Iowa).

### 4.2 Shen's "Experiment" column contains exactly one measured resistance quantity — the plan's "independent corroboration" is circular

Shen Table 5 footnotes, verbatim: `ᵃ Computed by C_P = C_T − C_F.` and `ᵇ By ITTC 1957 friction line
C_F = 0.075/(log₁₀ Re − 2)².`

So in the "Experiment" column, **C_P = 7.18e-4 and C_F = 2.83e-3 are derived, not measured**. The only
measured resistance datum is C_T = 3.55e-3.

**Plan asserts** (risk table, "The reference value is second-hand" row):

> "Mitigating: the value is corroborated independently — Shen's tabulated CP = 7.18e-4 reproduces 3.55e-3 −
> Cf_ITTC57(1.4e7) to three significant figures, and Wu's independent fine-grid computation lands at
> 3.527e-3. Two sources, one arithmetic identity."

**This corroboration does not exist.** C_P = 7.18e-4 reproduces 3.55e-3 − C_F because Shen *defined* it that
way. The identity the plan checks is the identity Shen used to fill the cell. It carries zero information
about whether 3.55e-3 is right. The same applies to the plan's derivation block:

> `Cr   = Ct - Cf = 7.17955e-4     <--  Shen tabulates 7.18e-4.  Exact to 3 s.f.`

Presented as a verification, it is a tautology. (The arithmetic is right: 3.55e-3 − 2.8320e-3 = 0.71796e-3.)

Note also that the *same* circularity appears in row A: the NMRI table's note `& EFD C_R is defined as
C_R = C_T − C_F0` means its C_R = 0.731e-3 is likewise derived, not measured.

**Consequence for V2a.** The plan sets `Cp_ref = 3.55e-3 − Cf_ITTC57(1.4e7) = 7.180e-4` and describes V2a as
gating the computed pressure coefficient against a "reference residuary". There is **no published
experimental KCS pressure or residuary coefficient** in Shen — the reference is the single gated EFD number
minus a correlation line. V2a therefore imports no independent measurement; it re-uses V1's datum.

To be precise about what this does and does not break: the plan's synthetic-vector demonstration that V2a can
fail while V1 passes remains **mathematically valid**, because the tolerances differ and the Cp/Cv split is a
genuine property of the solution. V2a still catches compensating errors, which is what it was built for. What
is false is the *provenance* claim — that V2a is checked against published experimental data. It is checked
against an algebraic construction, and the plan should say so.

### 4.3 Wu's KCS is with-rudder and fixed even-keel — r2 was right, with the primary quotes now in hand

Wu §2.3 and §3.1, quoted in §1.1 above, establish that Wu's static-mesh KCS is **fixed even keel, with
rudder**. r2's finding stands and is now sourced. The plan's re-scored table treats Wu's grids as
bare-hull-comparable; they are not.

### 4.4 NEW — Wu's static KCS runs at Re = 1.46e7, not 1.4e7. Neither review nor either plan revision noticed.

Wu Table 5 caption, verbatim: "...(static mesh, **Re =1.46 × 10⁷**, Fr = 0.26)". Wu's *dynamic* KCS case is a
different Reynolds number again — Tables 10 and 11 read "Re = 1.26 × 10⁷, Fr = 0.260".

The plan's re-scored table compares Wu's three grids to a Re = 1.4e7 referent with no friction adjustment. By
ITTC-57:

```
Cf(1.46e7) = 2.8121e-3      Cf(1.40e7) = 2.8320e-3      difference = 0.0199e-3 = 0.56 % of Ct
```

Wu's Ct values are biased **low by ~0.56 %** relative to a 1.4e7 condition. Adjusting for Reynolds alone —
still leaving the rudder and the normalisation uncorrected:

| Wu grid | as published (Re 1.46e7) | adjusted to Re 1.40e7 | plan's claimed error vs 3.55e-3 | error after Re adjustment |
|---|---|---|---|---|
| S3 coarse 0.208 M | 3.755 | 3.775 | −5.775 % | **−6.34 %** |
| S2 medium 0.572 M | 3.555 | 3.575 | −0.141 % | **−0.70 %** |
| S1 fine 1.640 M | 3.527 | 3.547 | +0.648 % | **+0.09 %** |

The plan's headline — "the closest configurational analogue ... lands at 0.65 % on 1.64 M cells" — is not a
like-for-like comparison. The sign convention in the plan's table matches Wu's own `E = (D − S_i)%D` and is
internally consistent; the defect is an unadjusted condition mismatch in **three** separate dimensions
(rudder, Reynolds, normalisation), not the arithmetic.

The plan's broader claim that "six of the seven published fixed-condition results land inside 1%" of the
corrected referent should be withdrawn or restated with the adjustments applied. It is doing real work in the
plan — it is the stated justification for tightening the tolerance below revision 1's 5 % — and it does not
survive as written.

### 4.5 NEW — Wu scores his *fixed* case against D = 3.711e-3, and his own text explains why that is still a free-condition datum

Wu's Table 5 (static mesh, fixed even keel) uses **D = 3.711** — the same D as his Tables 10/11 for the
free-to-sink-and-trim case. Taken alone that could be read as evidence that 3.711e-3 is not attitude-specific,
which would undercut Blocker 1. Wu's own text settles it the other way, verbatim:

> "With the correction, the |E%D| of C_T is clearly improved for all grids because all C_T is increased by the
> ship's vertical motions and becomes closer to D = 3.711 (Table 5). For S1 in Tables 5 and 10, the
> under-predicted C_T error is reduced to less than 1% from approximately 5%."

Wu observes that his fixed case sits ~5 % below D and that releasing heave and pitch closes the gap. Combined
with the organiser's `FR_Zθ` attitude code and the *measured* sinkage of −13.94 mm at Fr 0.26, Blocker 1's
direction is confirmed from three independent angles: **3.711e-3 is a free-to-sink-and-trim datum, and a
fixed body belongs below it.**

**But the magnitude is over-attributed, in Wu's paper and in the plan.** Wu's static and dynamic runs are at
different Reynolds numbers (1.46e7 vs 1.26e7). Of the 0.1668e-3 gap between Wu's static S1 (3.527) and dynamic
S1 (3.69383):

```
Cf(1.26e7) - Cf(1.46e7) = 0.0710e-3   ->  42.6 % of the gap is pure ITTC-57 friction
remainder                = 0.0958e-3   ->  ~2.6 % of D, attributable to attitude
```

So the sinkage-and-trim effect on Ct is roughly **2.6 %, not 5 %**.

### 4.6 NEW — the 4.54 % "condition offset" is one-third Reynolds, not all attitude

The plan states revision 1 "was spending 4.54% of a ±5% budget on a condition offset" and frames it as the
fixed-versus-free error. Decomposing the 3.711e-3 (Re 1.26e7) versus 3.55e-3 (Re 1.4e7) gap:

```
total gap                                        = 0.1610e-3  = +4.54 % of 3.55e-3
pure ITTC-57 friction (Re 1.26e7 vs 1.40e7)      = 0.0510e-3  = +1.44 %   (31.7 % of the gap)
residual: attitude + appendage + normalisation   = 0.1100e-3  = +3.10 %
```

The plan does say the mismatch is "threefold, not twofold", so the *structure* is acknowledged — but the
1.44 % Reynolds share is never quantified, and the whole 4.54 % is repeatedly described as though attitude
drove it. Usefully, the residual 3.10 % is consistent with the ~2.6 % attitude effect measured independently
in §4.5 plus a small appendage/normalisation term — two independent estimates that agree.

### 4.7 The plan's uncertainty budget mixes a discrete bias into an RSS of random terms

The plan builds `U_S = 1.22 % (UNRESOLVED)` into `RSS(U_S, U_D, U_SN, U_i) = 2.12 %`. Per §1.3, U_S is not a
random uncertainty — it is a fork with two branches and exactly one is true. Root-sum-squaring it both
understates the bad branch and pretends the good branch costs something. Per §1.2 it is now largely resolved
in favour of the bare hull, so it should mostly leave the budget altogether. It should be carried as a
declared one-sided bias, with the gate's centre stated for each branch:

```
if row B is normalised on bare-hull S    ->  referent 3.550e-3   [strongly indicated]
if row B is normalised on with-rudder S  ->  referent 3.593e-3   (= 3.55e-3 x 9.5531/9.4379)
```

And U_Re = 0.96 % (§2.2) belongs in the RSS and is currently absent entirely.

### 4.8 Minor — the plan's with-rudder wetted surface, 9.5527 m², matches nothing

The plan writes "S0/Lpp² = 0.1803 ⇒ 9.5527 m² (with rudder)". Neither route gives that:

```
0.1803 x 52.9780        = 9.5519 m^2
9.4379 + 0.1152         = 9.5531 m^2   <- the organiser's own particulars
```

The plan's own text elsewhere gets 9.5531 right. 9.5527 appears to be a transcription slip. Immaterial to any
gate (0.01 %), but the fixture should carry 9.5531 m² from the T2015 geometry sheet.

### 4.9 Minor — Shen's own wetted area is 9.4376 m², not 9.4379 m²

Shen Table 3 lists `Wetted area without rudder  AW (m²)  9.4376` (model scale; 9424 full scale). The T2015
geometry sheet and Wu Table 1 both give 9.4379 m². The difference is 0.003 % and immaterial, but the fixture
should cite one source rather than blend them. Recommend the T2015 value 9.4379 m², as organiser-published
geometry.

### 4.10 Minor — "matching the reference condition exactly" matches Shen, not the experiment

The plan's implementation table sets `nu = 1.1416e-6` with the note "nu set to reproduce Re = 1.4e7 at
U = 2.196 m/s, Lpp = 7.2786 m ⇒ nu = 1.1416e-6, **matching the reference condition exactly**". Per §1.1, the
Re = 1.4e7 is Shen's simulation condition; no retrieved source gives the Tokyo-2005 experiment's water
condition. The word "exactly" should be struck. (At U = 2.1970 m/s computed from Fr = 0.26 and Lpp = 7.2786,
Re = 1.4e7 implies nu = 1.1422e-6; the plan's 1.1416e-6 corresponds to the rounded U = 2.196 m/s. Immaterial,
but the fixture should record which U it used.)

### 4.11 What survives unchanged — verified, not merely unchallenged

A corrections list that only subtracts is as misleading as one that only adds.

- Revision 2's central move — abandoning 3.711e-3 as the referent for a fixed-body run — is **correct**, and
  this retrieval strengthens it from three directions (§2.1, §4.5).
- The plan's quotation of the T2015 normalisation statement is **verbatim-accurate**; it drops two qualifiers
  ("for static orientation in calm water", and the separate at-rest instruction) but nothing material.
- The 1.22 % wetted-surface figure is **arithmetically correct**, and the plan was right to refuse to assert
  it resolved (§1.2).
- The plan's rebuttal of r1's Blocker 4 is **correct**, and was in fact under-claimed (§4.1).
- The plan's V3 withdrawal rationale is **confirmed at full precision**. The EFD ratios it cites reproduce
  from the primary data file: Ct(0.282)/Ct(0.260) = 4.501/3.711 = **1.2129** (plan: 1.213) and
  Ct(0.260)/Ct(0.227) = 3.711/3.467 = **1.0704** (plan: 1.069, from rounded inputs). And the series is indeed
  free-to-sink-and-trim with a rudder, so the plan's reason for withdrawing V3 is sound.
- `Cr = 3.55e-3 − Cf_ITTC57(1.4e7) = 0.71796e-3` is **arithmetically correct** (it is just not a
  corroboration — §4.2).
- The degenerate-case check "a solution computing no free-surface deformation returns Ct = 2.832e-3 ⇒
  −20.2 %" is **correct**.
- Shen's production-run figures (1,675,465 cells; 24 processors; 6.9 h wall clock; Ct = 3.52e-3) are
  **confirmed verbatim** from Tables 4 and 5 and the §4.2 text.
- Wu's iteration counts and Table 14 timing data were **not re-checked**; they were outside this exercise's
  scope, which was the referent tuple. They remain as the plan states them, unverified by this document.

---

## 5. Bottom line for the owner

**The referent is 80 % pinned, and the missing 20 % is the part the gate depends on most.**

Resolved from primary sources:

- **The wetted-surface fork is essentially closed.** 0.1781 = bare hull, 0.1803 = hull + rudder, from the
  organiser's geometry. The T2015 lineage states with-rudder normalisation verbatim and its 3.711e-3
  reproduces arithmetically only on 9.5531 m². The Tokyo-2005/Gothenburg-2000 lineage publishes its EFD
  explicitly on the bare hull. The plan's "single largest open item" is largely answered, and answered the way
  the plan hoped.
- **Row C is pinned on every field** — 3.711e-3, free to heave and pitch, with rudder, S = 9.5531 m²,
  nu = 1.27e-6, rho = 999.5, Re = 1.26e7 — plus the complete six-speed series with measured sinkage and trim.
- **Wu's tuple is pinned** — fixed even keel, with rudder, Re = 1.46e7 — and it is not the tuple the plan
  assumed when re-scoring his grids.

Not resolved:

- **The attitude and water condition of the 3.55e-3 referent itself.** Hino (2005) is print-only and was not
  obtained; the Springer chapter that might substitute is paywalled and covers the wrong lineage. Shen's
  "fixed at even-keel" sentence describes Shen's computation, not the towing tank.

**This does not block #1173.** The recommendation is: gate against the §2.1 tuple at **4 %** rather than 3 %,
mark `attitude` and the water condition `provenance: inferred` in the fixture, re-scope Stage 0 from "pin the
reference" to "obtain Hino (2005) or declare the inference", and withdraw the "six of seven land inside 1 %"
claim (§4.4) that currently justifies the tighter tolerance.

Spending ~19 days of compute against a referent whose attitude is a declared inference is defensible at 4 %.
It is not defensible at 3 % with the inference described as pinned.
