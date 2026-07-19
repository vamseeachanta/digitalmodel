# Issue 1602 excitation amendment — solver-neutrality review — round 1

Reviewed commit: `6e68f58cc09ba4b54d4851290b56d277bceec1a2`

Verdict: **MAJOR**

- Component availability was not committed.
- Canonical impedance/sign semantics were undefined.
- Same-run payload lineage was prose rather than equality of shared commitments in every payload preimage.
- Heading direction was ambiguous for backend adapters.
- The value-row set was omitted from release `exact_sets`.
- Confidentiality rules named private RAOs but not private excitation.

Verified: YAML parsing and all immutable plan/component SHA bindings.

Disposition: draft remains blocked; apply findings and rerun adversarial review.
