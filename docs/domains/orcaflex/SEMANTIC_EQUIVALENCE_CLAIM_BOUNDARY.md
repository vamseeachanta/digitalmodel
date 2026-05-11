# OrcaFlex YAML Semantic-Equivalence Claim Boundary

**Date:** 2026-05-11
**Issue:** [#515](https://github.com/vamseeachanta/digitalmodel/issues/515)
**Approach:** A (broad — taxonomy ratification + OQ-1..OQ-4 closure in-scope)
**Companion doc:** `SEMANTIC_DIFF_TAXONOMY.md` (categories C1..C6, levels L1..L3)
**Cross-solver contract:** `knowledge/wikis/engineering/wiki/concepts/semantic-equivalence-contract.md` (workspace-hub #2476)
**Registry:** `MODEL_CLAIM_REGISTRY.yaml` (this directory)

---

## Purpose

This document is the single authoritative statement of **what the repo is allowed to claim** about semantic equivalence between:

- `spec.yml` (human-authored LLM-friendly YAML) — the engineering-intent source of truth
- modular intermediate YAML files (`*.yml` produced by the modular generator)
- single strict OrcaFlex YAML (`.dat` / monolithic `.yml`, loadable by OrcaFlex / OrcFxAPI)
- reverse-extracted `spec.yml` (output of `format_converter/{modular,single}_to_spec.py`)

Without this contract, downstream users, plans, wiki pages, and marketing copy can over-claim equivalence and either ship false guarantees or strangle legitimate engineering use. This document is the diff between *what is true* and *what we are allowed to write down*.

---

## 1. What the repo IS allowed to claim

Claims are scoped by **(model family, builder track, L-level)** triples. A claim is legitimate only if it appears in `MODEL_CLAIM_REGISTRY.yaml` and is backed by a resolvable `test_enforcing` path.

### 1.1 L-level definitions (recap)

| Level | Name | What it guarantees |
|---|---|---|
| **L1** | **Loadable** | The generated strict YAML loads without error in OrcaFlex / OrcFxAPI. The model can be instantiated. |
| **L2** | **Behaviorally equivalent** | All L1 guarantees + statics/dynamics results match the monolithic original within the benchmark tolerance defined in `SEMANTIC_DIFF_TAXONOMY.md` §C6 (tension: 5 % rel OR 10 kN abs; bending: 15 % rel OR 5 kN·m abs). |
| **L3** | **Semantically identical** | All L2 guarantees + **no diffs except C1 (cosmetic) and C2 (normalization)**. No C3 (intentional omission), no C4, no C5, no C6. |

### 1.2 Legitimate claim patterns

You MAY write the following claims **only if** the corresponding registry entry exists and its test passes:

- "Model `<name>` (family `<f>`, generic track) is **L1-loadable** under spec.yml round-trip."
- "Model `<name>` is **L2 behaviorally equivalent** to its monolithic baseline at benchmark tolerance."
- "Family `<f>` (generic track) supports **L1 forward generation** from spec.yml. C3 diffs are documented in the registry's `known_diffs` field."
- "Reverse extraction (`single_to_spec.py`) recovers a `spec.yml` with confidence ≥ 0.85 for model `<name>`."

### 1.3 Family-level L-coverage at the time of writing

Bootstrapped from the four currently-landed per-family proof plans referenced in #515:

| Family | Builder track | Highest legitimately-claimable L-level | Representative model | Notes |
|---|---|---|---|---|
| Catenary riser | generic | **L1** | `a01_catenary_riser` | C3 diffs in `General` (view keys), `Environment` (bool/numeric normalization → C2), `Groups` (generic-track omission). |
| Turret-moored FPSO | generic | **L1** | `c03_fpso` | Per workspace-hub #2454 proof plan; L2 not yet attested. |
| Rigid jumper (PLET-PLEM) | generic | **L1** | per workspace-hub #2455 | L2 deferred — needs licensed-win-1 statics rerun. |
| Lazy-wave riser | generic | **L1** | per workspace-hub #2456 | Closed; complementary per-family proof. |
| CALM / SPM buoy | generic | (not yet claimed) | per workspace-hub #2472 | Plan exists, proof not yet run. Registry omits the entry. |
| OrcaWave handoff | (separate) | (out of scope here) | — | OrcaWave-side covered by workspace-hub #2457 / #2473; not claimed by this doc. |

**Update procedure:** registry entries land when proofs land. A claim is illegitimate at any moment when the registry doesn't carry it, regardless of what plan docs may say.

---

## 2. What the repo is NOT allowed to claim

The following claims are forbidden in repo docs, code comments, commit messages, PR bodies, wiki pages (engineering / marine-engineering), and marketing copy until and unless they appear in the registry under an enforceable test.

### 2.1 Hard forbidden

- ❌ "spec.yml is 100 % semantically equivalent to OrcaFlex strict YAML in the general case."
- ❌ "Round-trip (spec → strict → spec) is lossless."
- ❌ "L3 semantic identity for generic-track models." **(L3 is unsupported on generic track today; the generator intentionally drops C3 keys to remain loadable.)**
- ❌ "Reverse extraction is lossless." It is explicitly best-effort with confidence scoring per `modular_to_spec.py:EXTRACTION_MAP` (17 fields, hardcoded).
- ❌ "Environment defaults match OrcaFlex's own defaults." (OQ-3 is open. Until the licensed-win-1 verification passes, the 21 hardcoded defaults in `environment_builder.py::_DEFAULTS` are a **silent substitution risk** — see §5.3.)
- ❌ "Groups round-trip cleanly." Generic-track models do not emit `Groups`; pipeline/riser tracks emit a derived `Groups` that may not match user-authored monolithic Groups (OQ-2).

### 2.2 Soft forbidden (needs explicit qualifier)

You MAY discuss the following only with the listed qualifier inline:

- "Behaviorally equivalent" — must be qualified with the benchmark model and tolerance, e.g. "behaviorally equivalent on `a01_catenary_riser` at <5 % tension tolerance per L2 test `<path>`".
- "Validated" — must name the test enforcing the claim, e.g. "validated by `tests/.../test_a01_generic_merge.py`".
- "Production-ready" — must scope to family + L-level, e.g. "production-ready for L1 catenary-riser forward generation".

### 2.3 Why these limits exist

Engineering deliverables that ride on spec.yml → strict YAML translation include statics, dynamics, fatigue, and certification submittals. False equivalence claims propagate into client decisions. The plan-#515 evidence (`a01_catenary_riser` ships at 0.88 reverse-confidence and three diverging sections) demonstrates the gap is real, not theoretical.

---

## 3. How claims are enforced

The claim boundary is enforced by **four mechanisms**, each in tension with the others — when they disagree, the highest-precedence wins.

| # | Mechanism | Precedence | Source of truth | Enforcement gradient |
|---|---|---|---|---|
| 1 | **Per-model proof test** | highest | `tests/solvers/orcaflex/test_*_semantic.py` (and per-family `test_<model>_*.py`) | Level 2 (script — pytest) |
| 2 | **Skip-list reconciliation test** | high | `tests/solvers/orcaflex/test_skip_list_reconciliation.py` | Level 2 (script — pytest) |
| 3 | **Claim registry** | medium | `MODEL_CLAIM_REGISTRY.yaml` | Level 1 (machine-readable doc, asserted by mechanism 2) |
| 4 | **Taxonomy doc** | medium | `SEMANTIC_DIFF_TAXONOMY.md` | Level 0 (prose, asserted by mechanism 2) |

### 3.1 Reconciliation test contract

`test_skip_list_reconciliation.py` (introduced by #515 iteration 3) enforces that:

- Every entry in `generic_builder._SKIP_GENERAL_KEYS` (28 keys at branch time of writing) is documented in `SEMANTIC_DIFF_TAXONOMY.md` §C3.
- Every entry in `generic_builder._SKIP_OBJECT_KEYS` (2 keys) is documented in §C3.
- Every entry in `environment_builder._WIND_SPEED_DORMANT` (1 key, `Full field`) is documented in §C3.
- Every property in `semantic_validate.ALLOWED_DIFF_PROPS` (50+ at branch time) is either classified C1 (cosmetic) in §C1 or is a documented C3 skip-list entry.
- Every entry in `MODEL_CLAIM_REGISTRY.yaml` resolves to an existing `test_enforcing` path.

When any of these reconciliations fail, the test emits a human-readable diff. The fix is **either**: (a) add the missing entry to the taxonomy doc with a documented reason, **or** (b) remove the unjustified entry from the skip list. **Silently deleting the diff is forbidden.**

### 3.2 Claim-level gating

| L-level | Where it runs | Marker |
|---|---|---|
| L1 (loadable) | dev-primary CI; gated by `uv run pytest tests/solvers/orcaflex/` | none — runs by default |
| L2 (behavioral) | dev-primary if no OrcFxAPI dependency in the assertion path; otherwise licensed-win-1 | `@pytest.mark.licensed_win_1` (when OrcFxAPI statics required) |
| L3 (semantic identity) | not currently attainable; no enforcement | n/a |

### 3.3 Cross-references

- All claims in this doc that name a property family must resolve back to a row in `SEMANTIC_DIFF_TAXONOMY.md` §C1–C6.
- All claims that name a model must resolve back to a row in `MODEL_CLAIM_REGISTRY.yaml`.
- This document MUST be cross-linked from `knowledge/wikis/engineering/wiki/concepts/semantic-equivalence-contract.md` once that wiki page lands (per workspace-hub #2476). The wiki page is the cross-solver substrate; this doc is the OrcaFlex-specific instantiation.

---

## 4. How to raise a claim level

When a contributor wants to upgrade a model's claimable level (e.g. add a new family at L1, or promote an existing family from L1 to L2), the procedure is:

### 4.1 New L1 claim (add a family or model)

1. Add a fixture under `tests/solvers/orcaflex/fixtures/` with a `spec.yml` and the monolithic `.dat` baseline.
2. Run forward generation: `spec → modular → strict`. Verify validator-clean.
3. Run `semantic_validate.py` to enumerate diffs. Classify each diff per `SEMANTIC_DIFF_TAXONOMY.md`:
   - C1 / C2 diffs: allowed, no action.
   - C3 diffs: confirm each property is in a documented skip list; if not, add to the appropriate skip list and document in the taxonomy doc §5.
   - C4: must resolve correctly; otherwise blocks the claim.
   - C5: blocks the claim until fixed in generator.
   - C6: blocks the claim until fixed in generator OR explicitly classified as documented divergence in `known_diffs`.
4. Add a `test_enforcing` test that asserts the model loads (`OrcFxAPI.Model().LoadData(...)`) without error.
5. Add a registry entry under `MODEL_CLAIM_REGISTRY.yaml` with:
   - `name`, `family`, `builder_track`, `highest_validated_level: L1`
   - `test_enforcing: <path>`
   - `known_diffs: [<list of C1/C2/C3 diffs with property names>]`
6. PR review: confirm `test_skip_list_reconciliation.py` still passes; confirm the test runs in dev-primary CI.

### 4.2 Promote L1 → L2

1. Add a statics (and optionally dynamics) parity test. The test runs the same load case on both the monolithic and the generated model via OrcFxAPI and asserts results match within the tolerance in `SEMANTIC_DIFF_TAXONOMY.md` §C6.
2. Mark the test `@pytest.mark.licensed_win_1` (statics needs OrcFxAPI license; runs only on licensed-win-1).
3. Update the registry entry: `highest_validated_level: L2`.
4. Cross-review (route:B) — Codex + Gemini + Claude.
5. PR review: confirm prior #2454/#2455/#2456/#2457 per-family proofs still pass.

### 4.3 L2 → L3 (currently unreachable on generic track)

L3 requires zero C3/C5/C6 diffs. The generic builder's intentional-omission policy makes this unreachable today. Promotion is **out of scope** for the generic track. A future builder track (e.g. a "round-trip-preserving" track) could attempt L3; design is deferred and not tracked by this contract.

---

## 5. In-scope work for #515 (Approach A) and its child issues

Approach A means #515 closes the four Open Questions from `SEMANTIC_DIFF_TAXONOMY.md` §7 inline, rather than deferring to child issues. The taxonomy doc is **ratified by #515** as the authoritative classification policy (see the adoption header added at the top of that file).

### 5.1 Open Questions, plain-language impact, and disposition under Approach A

| OQ | What it is | What it breaks today | Disposition |
|---|---|---|---|
| **OQ-1** | `VerticalWindVariationFactor` is present in some monolithic exports, absent in generated. | A user comparing the two YAMLs sees a SIGNIFICANT diff and may believe wind loading differs. In reality the property is only emitted under specific `WindType` values (see `environment_builder._WIND_TYPE_PROPS`). | **Classify as C3 with explicit `WindType`-conditional reason in taxonomy §C3.** Iteration 5. |
| **OQ-2** | `Groups` section present in monolithic, absent in generated for generic-track models. | Users with curated UI group sets see them silently dropped. Pipeline / riser tracks generate derived Groups that may not match user-authored ones. | **Classify as C3 (generic-track policy) + measure pipeline/riser gap, record in registry `known_diffs`.** Iteration 5. |
| **OQ-3** | 21 hardcoded defaults in `environment_builder._DEFAULTS` may not match OrcaFlex's own defaults. | If a user's monolithic has a different value, the generator silently substitutes the hardcoded default — *silent physics change*. | **Add `test_environment_defaults_vs_orcfxapi.py` marked licensed-win-1.** Iteration 6. |
| **OQ-4** | `values_equal()` treats `Yes ≠ true` and `No ≠ false`, producing false-positive SIGNIFICANT diffs. | Every prior per-family proof that ran against bool-encoded Environment shows inflated diff counts. | **Fix at compare site (`semantic_validate.py:values_equal`), not at dump site. Add bool-normalization test. Re-run #2454/#2455/#2456/#2457 proofs and diff verdicts BEFORE landing the fix.** Iteration 4. |

### 5.2 Child-issue dispositions

| Child | Role under Approach A | When it closes |
|---|---|---|
| **#517** Taxonomy | Now ratified inline by #515; the taxonomy doc carries an "Adopted by #515 on 2026-05-11" header. | Close when iteration 1 commits land and this doc + the adoption header are on main. |
| **#518** Regression tests | Continues as the home for executable evidence. The new `test_skip_list_reconciliation.py` from #515 iteration 3 is the first artifact; #518 expands to per-family regression coverage. | Close when comprehensive per-family regression tests land. |
| **#519** General/Environment/Groups fidelity | OQ-1 + OQ-2 close under #515 iteration 5; OQ-3 closes under iteration 6. Residual C6 closures remain on #519. | Close when residual C6 diffs (if any) close. May close concurrently with #515 if no residual found. |
| **#520** Reverse extraction | Already shipped (commit `63c1cbdd`). No further action under #515. | (closed) |

### 5.3 Silent-substitution risk register

Until OQ-3 closes (iteration 6), the following 21 properties carry **silent-substitution risk**: if a user's monolithic model has a value that differs from the `_DEFAULTS` value, the generator emits the default and the user never sees the substitution. Properties at risk:

```
WaterSurfaceZ, KinematicViscosity, SeaTemperature, ReynoldsNumberCalculation,
HorizontalWaterDensityFactor, VerticalDensityVariation, SeabedType, SeabedOrigin,
NominalDepth, SeabedSlopeDirection, SeabedModel, WaveKinematicsCutoffDepth,
WaveCalculationMethod, WaveCalculationTimeInterval, WaveCalculationSpatialInterval,
MultipleCurrentDataCanBeDefined, CurrentModel, CurrentRamped,
CurrentApplyVerticalStretching, HorizontalCurrentFactor, VerticalCurrentVariationMethod,
IncludeVesselWindLoads, IncludeLineWindLoads, IncludeBuoyWindLoads,
IncludeBuoyWingWindLoads, WindRamping, WindType, AirDensity, AirSpeedOfSound
```

**User-facing warning that should appear in spec-loading docs until OQ-3 closes:** "Environment defaults are hardcoded in the generator and may not match OrcaFlex's own defaults. Audit your `Environment` block explicitly."

---

## 6. References

| Artifact | Path | Role |
|---|---|---|
| Parent issue | [#515](https://github.com/vamseeachanta/digitalmodel/issues/515) | Semantic-equivalence claim-boundary contract |
| Plan | `workspace-hub/docs/plans/2026-04-24-issue-515-semantic-equivalence-yaml-strict.md` | Wave 3 adversarial-reviewed, user-approved 2026-04-24 |
| Taxonomy | `SEMANTIC_DIFF_TAXONOMY.md` (this directory) | Classification policy C1..C6, levels L1..L3, OQ-1..OQ-4 |
| Registry | `MODEL_CLAIM_REGISTRY.yaml` (this directory) | Per-model claim attestation (iteration 2) |
| Reconciliation test | `tests/solvers/orcaflex/test_skip_list_reconciliation.py` | Enforces taxonomy ↔ code skip-list coupling (iteration 3) |
| Section fidelity | `SECTION_FIDELITY_ANALYSIS.md` (this directory) | Builder coverage matrix per family |
| Generator methods | `MODULAR_GENERATOR_METHODS.md` (this directory) | Architecture and data flow |
| Comparison engine | `scripts/semantic_validate.py` | Implements taxonomy enforcement (2108 lines) |
| Cross-solver contract | `knowledge/wikis/engineering/wiki/concepts/semantic-equivalence-contract.md` (workspace-hub) | Wiki landing for the cross-solver story (#2476) — link bidirectionally once landed |
| Per-family proofs | `workspace-hub/docs/plans/2026-04-23-issue-{2454,2455,2456,2457}-*.md` | Source evidence for the registry's L1 entries |

---

## 7. Versioning

This document follows the repo's docs (not code). Material changes — adding a new L-level definition, changing what "L2" requires, opening a new forbidden-claim row, changing the reconciliation test contract — require:

1. PR review by a domain-aware reviewer.
2. Update to the corresponding wiki page (#2476) if the change is cross-solver.
3. Re-run of all per-family proofs cited in §1.3 if the change touches L1/L2 definitions.

Cosmetic changes (typos, link fixes, table reformatting) do not require the above.
