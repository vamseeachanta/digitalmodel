# Actual Claude plan review r2

Requested model sonnet; inline tools-disabled review. Exit 0. Plan SHA256 2241b5fc47806631b0d2ed93c913e0d2026940eda2a8257494fa47c048b00b3d

# Plan review r2 (adversarial, no tool execution)

## Verdict: MINOR (not a full APPROVE, but no remaining architectural blockers)

All six r1 items got concrete mechanisms in the revision, not restated intent. Two small implementation-level gaps remain — neither warrants blocking approval.

## Disposition of r1 items

**1. Staged authorization not architecturally bound — RESOLVED.** The new "Exact-packet authorization boundary" section gives the actual mechanism r1 asked for: `packet_sha256` over canonical JSON of the frozen fields, a receipt with digest + expiry + single-use nonce, and — critically — the runner *recomputes* the packet/profile digest from the **actual staged files at launch time** and requires equality with the receipt. That's a content-addressed check, not a name-addressed one, so "approve A, run B" is caught: if B's geometry/runfile/argv differ from what was reviewed, the recomputed hash won't match the receipt and launch is refused. Combined with mutation tests ("swap each bound component after preparation and assert no process start"), this is a real enforcement point. Within the stated threat model (trusted local operator, not an adversarial remote actor), I don't think this needs cryptographic signing or a new auth platform — a local hash-compare-at-launch is proportionate.

**2. Termination/license-lock cleanup underspecified — RESOLVED.** Windows Job Object with kill-on-close, suspended-start-then-join-job, breakaway denial, and a sentinel test that checks both PID *and* process creation time disappear (correctly avoiding PID-reuse false negatives) directly answers "no test methodology for proving the process tree is reaped." The license-lock uncertainty is handled honestly rather than assumed away: a persistent "run guard" blocks further live approval until the operator verifies seat/lease state via the vendor's own admin tooling, and retry-as-probe is explicitly forbidden.

**3. Manual-derived commands not uniformly gated — RESOLVED.** "Every command token... will receive a version-matched manual citation and independent review in the capture packet... Milestone 1 command doubles will remain provisional non-executable examples until that packet is approved." This extends the handler-disposition skepticism to `END`, `HS`, `REPORT OFF` as requested.

**4. Single-capture format qualification too strong a claim — RESOLVED.** Plan took the scope-down option: qualification is now explicitly limited to "the exact observed version, encoding, locale, single-page three-depth table," unobserved layouts fail closed, and any expansion requires new independently captured evidence. This is honest rather than requiring a second real capture, which is an acceptable choice given r1 offered both options.

**5. No sanitization step for public fixtures — RESOLVED.** Named step (operator inspects decoded text/headers/metadata for license-holder/machine identifiers), named role (designated human reviewer), and an artifact (promotion record with hashes/redactions/permissions) before anything reaches the repo. Fallback to external-only evidence if clearance can't be obtained.

**6. Frame convention and KM/BM ambiguity — RESOLVED.** Frame section now states the actual sign convention (x forward from aft, y port, z up), matching the earlier "positive transverse coordinate port" line. The KM-vs-BM ambiguity is resolved architecturally: `comparison.py` is scoped to consume a normalized physical contract (BMT/BML etc.), and any KM−KB derivation is explicitly gated behind qualifying native KM/KB conventions with retained transformation lineage — so the parser/qualification layer absorbs the ambiguity rather than `comparison.py` needing rework later.

## Remaining concrete (non-blocking) gaps

- **Receipt issuance provenance is still implicit.** The plan specifies exactly what's *in* the receipt and how it's *checked*, but never says what creates it — is it a distinct reviewed tool the operator runs after reading the packet, or manual JSON editing? Manual editing risks transcription errors producing spurious rejects (safe direction) but is still worth naming explicitly (e.g., a small `ghs approve <packet>` helper that reads the already-computed packet hash and writes the receipt) so the human-review step has an actual interface rather than an undefined hand-off.
- **JSON numeric canonicalization isn't fully pinned down.** "Canonical UTF-8 JSON with sorted keys" doesn't by itself guarantee reproducible hashing for floats like `depths_m`/`density_kg_m3` (e.g., `1000` vs `1000.0` vs `1e3`) if prepare-time and launch-time recomputation ever use different serializers. Worth specifying a fixed numeric string representation. Failure mode is fail-safe (spurious reject, not a false accept), so this is a robustness nit, not a security gap.

## Recommendation

Proceed to Milestone 1 implementation; fold in the two nits above as small spec clarifications rather than treating them as gating findings.

★ Insight ─────────────────────────────────────
The authorization fix here is a good small example of "commit-then-verify" instead of "trust-then-execute": instead of a boolean `local_execution_authority` flag (which says nothing about *which* packet was approved), the runner recomputes a hash from the actual bytes it's about to use and compares it against what a human signed off on earlier. This is the same principle behind lockfile hashes in package managers — the point isn't cryptographic unforgeability, it's making an accidental or stale substitution *detectable* rather than silent.
─────────────────────────────────────────────────

## stderr
