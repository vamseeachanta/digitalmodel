# Plan for #2094: Two of three committed ANSYS example decks are wrong

> **Status:** plan-review
> **Complexity:** T2
> **Date:** 2026-09-11
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2094
> **Client:** N/A
> **Lane:** lane:claude
> **Review artifacts:** (pending)

---

## Resource Intelligence Summary

### Existing repo code

- Found: `src/digitalmodel/ansys/pressure_vessel.py:865-956` — emits the APDL deck including
  the defective restraint block and the `*CFOPEN` digest.
- Found: `src/digitalmodel/ansys/mudmat.py:82-193` — emits `R,1,<t>, , ,<ksub>` intending a
  Winkler foundation on SHELL181.
- Found: `src/digitalmodel/ansys/mudmat.py:144-157` — the load block. **Second mudmat defect,
  found at adversarial review:** `FCUM` is never issued, so the APDL default `FCUM,REPL`
  applies and the edge couple **replaces** rather than adds to the short-edge nodes' share of
  the uniform vertical load. See Evidence.
- Found: `src/digitalmodel/ansys/padeye.py:73-157` — same generator pattern; deck solves.
- Found: `src/digitalmodel/ansys/runner.py:32` — `_RESULT_SUFFIXES` excludes `.csv`, so the
  digest the decks write is never captured as a result file.
- Found: `src/digitalmodel/ansys/results_extractor.py:162-437` — pure-text extraction; no
  method reads the digest format.
- **Found, and previously missed: `src/digitalmodel/geotechnical/mudmat.py:131-141`** —
  Brinch Hansen / DNV-RP-C212 general bearing capacity with the Meyerhof effective-area
  eccentricity correction (`b_eff = width_b_m - 2.0 * eccentricity`), raising when
  `B - 2e <= 0`. A committed surrogate exists at `atlases/mudmat_bearing_capacity/`. This is
  the independent comparator for the mudmat bearing unity check that an earlier revision of
  this plan listed as a gap.
- Gap: no test in `tests/ansys/` asserts any solved value. Solver dependence is removed by
  monkeypatching, not by a marker.
- Gap: **no deck reports a reaction sum.** None of the three issues `FSUM` or any equilibrium
  report. This single omission is what allowed all three defects to ship undetected.

### Standards

Not applicable. The defects are modelling and harness errors, not standards-derived constants.
The allowable stresses already embedded in the decks are unchanged by this plan.

### LLM Wiki pages consulted

- `pages/acma-tool/notes/ansys-apdl-macros.md` — records the in-house macro library and its
  existing validation workbook practice; confirms the firm's own convention is to keep a
  recorded answer beside a model.
- `domains/analysis/pages/validated-model-index.md` — records that the three example decks
  carry a generator-sync guard rather than a solved answer, and names capturing the digest as
  the cheapest available upgrade.

### Documents consulted

- Issue #2094 — this plan's subject.
- `docs/domains/ansys/workflows/aqwa-to-structural-submodel.md` §12A — establishes the
  house pattern of recording a verified restriction with its evidence.
- Related issue #1524 — versioned launcher discovery; confirmed live on the host used for the
  reproduction below, and explains why discovery must be exercised on a real host.
- Related issue #1525 — canonical licensed workflow and integrity manifests; the golden-value
  files this plan creates are candidate manifest inputs.

### Gaps identified

- No elastic foundation exists in the mudmat model. SHELL181 accepts the real constants and
  ignores them; nothing reports this.
- No digest reader exists.
- No golden value exists for any example.
- No independent comparator exists for the padeye result.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-09-11 via `gh issue list`):
- `#2094` — OPEN — bug(ansys): two of three committed example decks are wrong
- `#1524` — OPEN — bug(ansys): detect versioned ansysNNN launchers
- `#1525` — OPEN — feat(ansys): register canonical licensed workflow

**File existence** (2026-09-11):
- EXISTS: `examples/ansys/pressure-vessel/pv.inp`
- EXISTS: `examples/ansys/padeye/padeye.inp`
- EXISTS: `examples/ansys/mudmat/mudmat.inp`
- MISSING (new — this plan creates): `examples/ansys/*/golden/`
- MISSING (new — this plan creates): `tests/ansys/test_example_goldens.py`

**Line excerpts** — `examples/ansys/pressure-vessel/pv.inp:35-38`:

```
! restrain one node radially to remove rigid-body motion
NSEL,S,LOC,X,780.0
NSEL,R,LOC,Y,0
D,ALL,UX,0
```

`examples/ansys/mudmat/mudmat.inp:17-20`:

```
R,1,60.0, , ,0.05
SECTYPE,1,SHELL
SECDATA,60.0,1
SECNUM,1
```

**Reproduction proofs** — all runs `-b -np 1 -smp`, Ansys 2026 R1.01 Build 26.1 UP20260202,
scratch directory, no repository file modified:

```
$ ANSYS261.exe -b -np 1 -smp -i pv.inp -o pvtest.out
exit code : 0        elapsed : 17.4 s
max_seqv_mpa,    981.3400,allowable_mpa,    138.0000,uc,   7.11116
```

Variants, one variable changed each:

```
A_noThermal    rc=0  max_seqv_mpa,    369.2583,  uc,   2.67578
B_noPointUX    rc=0  max_seqv_mpa,    259.5424,  uc,   1.88074
```

Closed-form Lame, ID 1500 mm, t 30 mm, P 10 MPa: hoop 255.10 MPa, von Mises 260.2 MPa.
Variant B agrees to 0.25 percent.

```
$ ANSYS261.exe -b -np 1 -smp -i padeye.inp
exit code : 0   errors=0   18.0 s
max_seqv_mpa,    348.6653,allowable_mpa,    212.5749,uc,   1.64020

$ ANSYS261.exe -b -np 1 -smp -i mudmat.inp
exit code : 1   errors=1   18.4 s   NO RESULT
*** ERROR *** small pivot term ... UZ degree of freedom of node 717
Pivot value of -2.272885044E-08 at node 717 UZ, element 1020 (SHELL181).
```

Mechanism established by reading the solver's own real-constant listing rather than inferred
(`RLIST,1` inserted in a scratch copy):

```
REAL CONSTANT SET          1  ITEMS   1 TO   6
   60.000       0.0000       0.0000      0.50000E-01   0.0000       0.0000
   INPUT SECTION ID NUMBER               1
   Shell Section ID=        1 Number of layers=    1  Total Thickness=    60.000000
```

The subgrade value 0.05 is stored in slot 4 and the section separately defines the thickness.
A variant with the section definition removed fails identically (`M1_noSection rc=1`), which
rules out a section-versus-real-constant conflict. SHELL181 does not consume an elastic
foundation stiffness from that slot; the input is accepted and ignored, leaving every UZ
degree of freedom unrestrained.

- Reproduced at: 2026-09-11
- Failure mode observed matches issue claim: YES

**Mudmat load defect, established at adversarial review by arithmetic on the generated deck.**
For the committed parameters (4000 x 3000, `ESIZE,100` gives 41 x 31 = 1271 nodes, 31 nodes
per short edge), `FCUM,REPL` makes the couple overwrite the edge nodes' uniform-load share:

| Quantity | Intended | Actually applied |
|---|---|---|
| Net vertical resultant, N | 800 000 | 760 976 |
| Overturning moment, N mm | 4.00e8 | 4.00e8 |
| Eccentricity e = M/V, mm | 500.0 | 525.6 |

The deficit fraction is 2/(L/ESIZE + 1) = 2/41 = 4.88 percent — **a function of mesh density,
not of the physics**. Refining the mesh changes the answer while converging on nothing.

A second load error in the same block: `F,ALL,FZ,fz_n` distributes a uniform pressure as equal
nodal forces rather than tributary-area forces. Interior nodes receive 629.4 N against a
correct 666.7 N, edge nodes 629.4 N against 333.3 N, corner nodes 629.4 N against 166.7 N. For
the committed parameters the elastic length L_c = (D/k)^0.25 = 534 mm spans five elements and
the error smears; at `thickness_mm=20` with `subgrade_modulus_n_per_mm3=0.5`, L_c falls to
132 mm and the perimeter over-load becomes a first-order error in `uz_min`, the quantity the
bearing unity check is built on.

Distinct sources: 10.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-09-11-issue-2094-ansys-example-deck-validation.md` |
| Tests | `tests/ansys/test_example_goldens.py`, `tests/ansys/test_results_extractor.py` |
| Implementation | `src/digitalmodel/ansys/{pressure_vessel,mudmat,runner,results_extractor}.py` |
| Golden values | `examples/ansys/<case>/golden/{<case>_result.csv,PROVENANCE.json}` |

---

## Deliverable

Three committed ANSYS example decks that solve, produce values agreeing with an independent
comparator, and carry those values as committed golden files that a test asserts against — in
place of a guard that only proves the generator is deterministic.

---

## Pseudocode

```
parse_result_digest(csv_text) -> dict[str, float]:
    split the single line on commas
    pair alternating label, value tokens
    coerce each value to float; raise on an odd token count or a non-numeric value
    return the mapping

test_committed_golden_is_stable(case):
    skip with a stated reason when no golden file exists
    read the committed golden digest
    read the freshly solved digest when one is present, else skip
    for each quantity: assert relative difference <= 1e-4
```

Restraint correction in `pressure_vessel.py`: delete the single-node radial constraint. An
axisymmetric model has no radial rigid-body mode, so no substitute restraint is required. The
axial restraint on the bottom edge already removes the only rigid-body mode present.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/ansys/pressure_vessel.py` | remove the spurious radial restraint; add `FSUM` reaction reporting to the digest |
| Modify | `src/digitalmodel/ansys/mudmat.py` | **D1:** apply the bearing distribution from `geotechnical/mudmat.py` as a surface pressure; drop the Winkler bed and the FE bearing unity check; fix the `FCUM` load defect; apply the uniform load as a pressure so tributary weighting is the element's job; minimal statically determinate restraint; add `FSUM` |
| Modify | `src/digitalmodel/ansys/padeye.py` | add `FSUM`; record the peak node number and coordinates in the digest; **D2:** re-parameterise to a compliant lug |
| Modify | `examples/ansys/pressure-vessel/build.py` | **D2:** `wall_thickness_mm` 30.0 → 60.0 |
| Modify | `examples/ansys/{padeye,mudmat}/build.py` | **D2:** re-parameterise to compliant designs |
| Read-only dependency | `src/digitalmodel/geotechnical/mudmat.py` | **D1:** supplies the bearing distribution and the bearing check. Not modified by this plan |
| Modify | `src/digitalmodel/ansys/runner.py` | add `.csv` to `_RESULT_SUFFIXES`; scope capture to files the run created |
| Modify | `src/digitalmodel/ansys/results_extractor.py` | add `parse_result_digest` |
| **Modify** | **`tests/ansys/test_mudmat.py`** | **`test_winkler_soil_support_present:36-45` asserts the presence of the defective inert card and its comment records the disproven mechanism. Any correct fix fails it.** Rewrite to assert the chosen mechanism and the absence of the inert card |
| **Modify** | **`src/digitalmodel/usecase_registry/registry.yaml:352-359`** | **`ansys-mudmat` is published `readiness: ready` with a note asserting the disproven Winkler-EFS mechanism. The case has never solved.** Set `partial` until it does; correct the note |
| Modify | `tests/test_usecase_registry.py` | require a committed golden before a solver case may claim `readiness: ready` |
| Modify | `tests/ansys/test_runner.py` | cover `.csv` capture and stale-digest exclusion |
| Modify | `pyproject.toml` | register the `requires_mapdl` marker |
| Modify | `.gitignore` | `examples/ansys/*/results/` — otherwise a capture run leaves untracked solver output in the tree |
| Regenerate | `examples/ansys/{pressure-vessel,mudmat,padeye}/*.inp` | via each `build.py`, keeping the byte-match guards true |
| Create | `examples/ansys/<case>/golden/<case>_result.csv` | the solved digest |
| Create | `examples/ansys/<case>/golden/PROVENANCE.json` | full argv, core count, SMP/DMP, equation solver, platform, MAPDL build string, deck SHA-256, generator commit, comparator class |
| Create | `tests/ansys/test_example_goldens.py` | always-on comparator assertions plus a `requires_mapdl` re-solve test |
| Modify | `tests/ansys/test_results_extractor.py` | cover the digest parser |

---

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_parse_result_digest_nominal` | label/value pairing | the pv digest line | `{max_seqv_mpa: 259.54, allowable_mpa: 138.0, uc: 1.881}` |
| `test_parse_result_digest_odd_tokens` | rejects a malformed line | `"a,1.0,b"` | `ValueError` |
| `test_parse_result_digest_non_numeric` | rejects a non-numeric value | `"a,x"` | `ValueError` |
| `test_runner_captures_csv_digest` | `.csv` reaches `result_files` | a run directory containing a digest | digest listed |
| `test_pressure_vessel_deck_has_no_radial_point_restraint` | the defect cannot return | generated deck text | no single-node `UX` constraint |
| `test_pv_golden_matches_closed_form` | the physics is right | committed pv golden; comparator computed in Python from `build.py` constants, not a literal | von Mises within 1 percent of the computed Lame value |
| `test_pressure_vessel_deck_is_axisymmetric` | binds the restraint-deletion safety argument to its precondition | generated deck text | `KEYOPT,1,3,1` present |
| `test_golden_unity_check_is_internally_consistent` | the digest is self-consistent | each golden | `uc == max_seqv_mpa / allowable_mpa` within tolerance |
| `test_golden_matches_recorded_status` | a silent move across the allowable fails | each golden + `PROVENANCE.json` | `uc` on the side recorded in `expected_status` |
| `test_golden_respects_linear_elastic_validity` | a linear result at yield does not pass silently | each golden | peak below stated yield, or `linear_elastic_limit_exceeded: true` recorded |
| `test_reaction_sum_balances_applied_load` | equilibrium — the check that would have caught all three defects | each golden | reaction resultant equals applied load within 0.1 percent |
| `test_mudmat_applied_load_is_mesh_independent` | the `FCUM` defect cannot return | decks generated at `element_size_mm` 100 and 50 | identical applied-resultant expression |
| `test_committed_golden_is_stable` *(marked `requires_mapdl`)* | a re-solve reproduces the value | golden and fresh digest, run configuration matching `PROVENANCE.json` | relative difference <= 1e-3 |
| `test_mudmat_deck_solves` *(marked `requires_mapdl`)* | the model is not singular | mudmat run on a licensed host | exit 0, digest present |
| `test_mudmat_pressure_patch_balances_applied_load` | D1 equilibrium holds by construction | generated deck | `q_applied * area_eff == V` and patch centroid offset `== M/V`, both exact to float tolerance |
| `test_mudmat_unit_conversion_kn_m_kpa_to_n_mm_mpa` | the factor-of-1000 hazard cannot pass silently | a known capacity result | converted pressure matches a hand-computed value |
| `test_mudmat_reactions_are_zero` *(marked `requires_mapdl`)* | the applied field and the reaction balance | mudmat golden | reaction resultant below a stated absolute floor, not a percentage |

---

## Acceptance Criteria

- [ ] `uv run pytest tests/ansys/ -v` passes, and the always-on golden tests **assert** rather
      than skip on a host with no licence. A suite that skips its way to green does not satisfy
      this criterion.
- [ ] `uv run pytest tests/` shows no regression.
- [ ] The pressure-vessel deck produces peak von Mises within 1 percent of a Lame value
      computed in the test from the generator's own constants.
- [ ] **Reaction resultant equals the applied load to within 0.1 percent, for all three decks.**
      Fixed here, before any run — not a tolerance chosen after the discrepancy is seen.
- [ ] The mudmat deck exits 0, its applied vertical resultant is mesh-independent, and its
      reactions are near zero because the applied load and the bearing distribution balance by
      construction (D1). A non-zero reaction means the distribution does not integrate to the
      applied load and is a hard failure, not a tolerance question.
- [ ] **Every example reports a unity check below 1.0 and a peak stress safely inside the
      linear-elastic range** (D2). An example that exceeds either does not ship as the
      demonstration case.
- [ ] The padeye deck exits 0, its golden records the peak node and coordinates, and a two-mesh
      comparison is recorded. A peak that moves or grows with refinement is a singularity, and
      the reported quantity shall change rather than the tolerance.
- [ ] Every golden carries the full run configuration in provenance. A golden whose comparator
      class is `archived-run` is labelled as such and states the absence of an independent
      comparator.
- [ ] Review artifacts posted.

---

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude (r1, fresh context) | **MAJOR** | Third mudmat defect (`FCUM,REPL` makes the applied load mesh-dependent); no deck reports a reaction sum, which is why all three defects shipped; an existing test and a public registry entry both assert the disproven mechanism and block the fix; the golden test skips on both branches so it is vacuous in CI; the unity-check test is unfalsifiable as written; the mudmat option set was incomplete and the repo already holds the bearing comparator |
| Second provider | not run | See Risks — a T2 scope nominally takes two providers |

**Overall result:** FAIL on first pass; plan revised.

Revisions made after review:

1. Added the `FCUM,REPL` load defect with quantified evidence, and the tributary-weighting
   error alongside it.
2. Added `FSUM` reaction reporting to all three generators and made equilibrium a fixed
   acceptance criterion at 0.1 percent, set before any run rather than after.
3. Added `tests/ansys/test_mudmat.py`, `usecase_registry/registry.yaml`,
   `tests/test_usecase_registry.py`, `tests/ansys/test_runner.py`, `pyproject.toml` and
   `.gitignore` to Files to Change.
4. Split the golden tests into an always-on comparator test and a `requires_mapdl` re-solve
   test; moved the comparator from a literal to a value computed from the generator's own
   constants; split the tolerance by purpose and bound the stability check to a matching run
   configuration.
5. Replaced the unfalsifiable unity assertion with internal-consistency, recorded-status and
   linear-elastic-validity assertions.
6. Expanded the mudmat option set from three to five and named `geotechnical/mudmat.py`.
7. Corrected two line-range citations.

Findings the review verified as correct and this revision preserves: the Lame comparator
(260.24 MPa recomputed independently, with sigma_z shown to be exactly zero rather than
approximately so), the axisymmetric rigid-body-mode argument across the generator's whole
parameter space, the claim that padeye carries no analogue over-constraint, and the claim that
mudmat's in-plane pinning is legitimate — the latter for the right reason, since a single
symmetric layer uncouples membrane from bending so the in-plane restraints carry zero force.

---

## Decisions taken by the owner, 2026-09-11

**D1 — Mudmat: re-scope, splitting the two checks.** The finite element model performs the
plate-bending check only. The bearing check is performed by `src/digitalmodel/geotechnical/
mudmat.py`, which already implements Brinch Hansen / DNV-RP-C212 with the Meyerhof
effective-area correction `b_eff = B - 2e` and raises when `B - 2e <= 0`.

Mechanically: the geotechnical module computes the bearing-pressure distribution under the
applied vertical load and moment; that distribution is applied to the plate underside as a
surface pressure; the applied load and the bearing reaction then balance **by construction**,
so the plate carries only a minimal statically determinate restraint set and its reactions are
near zero. **That near-zero reaction is itself the conservation check** — it is not an
incidental property, it is the acceptance criterion, and it is the test that would have caught
every defect in this issue.

Eccentricity is handled correctly by construction rather than by a guard: the effective-area
method is defined for `e > B/6`, whereas a linear spring bed is not. No kern check is required
because no bed is modelled. The solve stays linear and runs in seconds, and the two methods
cross-check each other — satisfying the comparator requirement rather than deferring it.

Consequence: the FE no longer reports a bearing unity check. `mudmat.py`'s digest carries plate
stress only. The bearing result comes from the geotechnical module and the two are reported
together.

**D1 feasibility, verified before approval.** The geotechnical module returns capacity, not a
pressure field — `BearingCapacityResult` carries `effective_width_m`, `effective_length_m`,
`effective_area_m2`, `q_ult_kpa` and a vertical capacity (`geotechnical/mudmat.py:27-42`,
`:131-141`, `:176-187`). It does not return a distribution. The distribution nonetheless
follows directly from the same idealisation, and this is what makes D1 work:

Meyerhof's effective-area method carries the applied vertical load on a **uniform pressure over
the effective area**, centred on the load. So the pressure applied to the plate is

    q_applied = V / (b_eff * l_eff)

over an offset rectangular patch, with `b_eff = B - 2e` taken from the module rather than
recomputed. Two properties fall out by construction rather than by tolerance:

- **Vertical equilibrium is exact.** `q_applied * area_eff = V` identically.
- **Moment equilibrium is exact.** The patch centroid sits at eccentricity `e = M/V` from the
  mat centre, so its moment about the centre is `V * e = M` identically.

The FE therefore needs only a minimal statically determinate restraint set, and its reactions
are zero to solver precision. The bearing check is then `q_applied` against `q_ult_kpa` with
the governing factor, performed by the module.

Implementation is a pressure on a selected element patch — `SFE,...,PRES` over the elements
inside the effective rectangle — not a spring bed and not a node loop.

**Limitation, to be stated in the module and in any output.** A uniform pressure block is the
bearing-capacity idealisation, not the true contact-pressure field, which is neither uniform
nor generally trapezoidal. Using it as the plate-bending load is a deliberate screening
choice: it makes the strength check and the bearing check share one load path, so the two
cannot disagree about what the soil is doing, at the cost of a bending distribution that is
approximate near the patch edges. It shall not be described as the contact pressure.

**Implementation hazard.** The geotechnical module works in kN, m and kPa; the decks work in
N, mm and MPa. The conversion shall be explicit and unit-tested. An unchecked factor of 1000
here would reproduce the exact failure mode this issue exists to remove — a plausible number
with nothing to contradict it.

**D2 — Examples shall be compliant designs.** Each example is re-parameterised so its peak
stress sits safely inside the linear-elastic range and its unity check is below 1.0. For the
pressure vessel, `wall_thickness_mm = 60.0` gives 135.5 MPa and UC 0.98 against the ASME UG-27
requirement of 56.8 mm. The padeye is re-parameterised on the same basis; its present 348.67
MPa is 98.2 percent of stated yield. A linear-elastic result reported at its own validity
boundary is not an admissible screening answer and shall not ship as the demonstration case.

**D3 — No second review provider at plan stage.** Implementation proceeds against the revised
plan; the resulting code is reviewed instead.

---

## Risks and Open Questions

- ~~**Decision required — the mudmat foundation.**~~ **Resolved, D1.** Retained for the record:
  SHELL181 does not consume a foundation stiffness as a real constant. Five remedies existed:
  **(a)** a `SURF154` overlay carrying the stiffness — compact, but the real-constant slot must
  be confirmed, and a listing will look equally plausible either way; the decisive test is the
  reaction sum, since an inert foundation gives a vertical reaction near zero against an
  applied 800 kN.
  **(b)** explicit `COMBIN14` springs per node from subgrade modulus and tributary area —
  transparent and hand-checkable, **but linear and therefore tension-carrying**. Valid only
  while the bed stays in compression. `MudmatGeometry` accepts any moment against any vertical
  load with no kern check: the committed case has e = 500 mm against B/6 = 667 mm and is
  inside the kern, but `moment_kNm = 600` gives e = 750 mm and a silently **unconservative**
  bearing answer.
  **(c)** a **no-tension bed** — `COMBIN39` compression-only springs, or node-to-ground
  contact. This is the option that actually answers the tension objection. Its cost is that the
  solve becomes nonlinear, with convergence handling and a longer licensed run.
  **(d)** re-scope: the FE does the plate-strength check and **`geotechnical/mudmat.py`**, which
  already implements Brinch Hansen / DNV-RP-C212 with the Meyerhof eccentricity correction,
  does the bearing check. The two cross-check each other.
  **(e)** drop the bearing output from the FE entirely and state the limitation.
  **This is an engineering decision on a screening tool's fidelity and is referred to the owner
  rather than selected here.** Whichever is chosen carries one acceptance: total foundation
  reaction equals the applied vertical load to within 0.1 percent.

- **Decision required — are the shipped examples meant to be compliant designs?** Corrected pv
  gives UC 1.88 and a peak von Mises at **99.8 percent of the stated yield**; padeye gives UC
  1.64 at **98.2 percent of yield**. A linear-elastic idealisation reported at its own validity
  boundary is not an admissible screening answer. By ASME UG-27 the pv section needs
  t = PR/(SE - 0.6P) = 56.8 mm against the 30 mm modelled; `wall_thickness_mm = 60.0` gives
  135.5 MPa and UC 0.98, a realistic just-acceptable vessel. Either the examples are
  re-parameterised to compliant designs, or the overstress is declared deliberate in the example
  directory itself — not only in a plan.
- **Open — the padeye has no independent comparator.** Its deck solves and yields 348.67 MPa,
  but nothing establishes that value is correct. Recording it as a golden makes it a
  regression guard only, not evidence of correctness. A closed-form check for a lug in
  bearing and tension should be derived, or the golden shall be labelled `archived-run` and
  the absence of an independent comparator stated.
- **Risk — the mudmat result may reveal a second defect.** The model has never solved, so no
  statement about its loads, its bearing calculation or its stress output has ever been
  tested. Fixing the foundation may expose further errors. The plan shall not assume one fix
  closes the case.
- **Risk — golden capture requires a licensed host and one seat.** The seat is single and
  shared. Capture shall be serialised and shall not run concurrently with other solver work.
- **Risk — regenerating the decks changes the byte-match guard's expected content.** The
  guard must be re-run after regeneration, or it will fail for the right reason at the wrong
  time.

---

## Complexity: T2

Four source files modified, two decks regenerated, six new golden artifacts, one new test
module. TDD required. No new module and no architectural change, so not T3.
