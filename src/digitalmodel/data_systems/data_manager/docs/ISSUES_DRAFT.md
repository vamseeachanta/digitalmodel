# Data manager issue drafts

Draft for maintainer review. Entries 1–5 resolve the numbered markers in
[agent-pack.md](agent-pack.md); no issue has been filed by this document.
The observed behavior is pinned to the code revision declared in the pack.

## Draft 1: Make YAML, plot and extraction outcomes observable

Scope: sweep rows S01–S04, S06–S08, S10–S11, S21–S26 and S29–S30.
Empty/false YAML, rejected overlays, duplicate keys, template errors and skipped
or partial extraction rows can be indistinguishable from complete results.
Non-mapping overlays, extra positional format fields and scalar extraction
descent propagate exceptions outside the documented fallback handlers.

Acceptance criteria: each fallback or propagated exception will have a focused
test and an explicit outcome policy; extraction will distinguish attempted,
returned and rejected records; duplicate keys and incomplete output will not be
silently promoted to valid input. Plot formatting will have an explicit filename
extension policy. Missing detection checks will be resolved or remain visible in
section 6 of the pack.

## Draft 2: Establish numeric, unit, geometry and default contracts

Scope: S05, S09, S12–S20, S27–S28 and S31–S33. Missing, non-finite or text values
can pass presence checks, raise validation errors or concatenate into a text
diameter. Geometry and method corrosion allowances are independent and default
separately to zero; no consistency check reconciles the routes.

Acceptance criteria: input screening will reject or explicitly escalate missing,
non-numeric, non-finite and non-applicable values before use; exponent-form YAML
and mixed-type geometry will have regression tests; length/stress units, geometry
completeness, tolerance, index bounds and every default will have explicit
policies. Corrosion-allowance precedence or reconciliation will be stated and
tested in both directions. Missing WT derivation will be implemented or its
contract will be corrected.

## Draft 3: Establish executable examples, tests and analyzer integration

The dedicated example is a placeholder and the migration tests skip before their
definitions. The separate analyzer package raises `ModuleNotFoundError` for its
absent legacy import route at the code pin. Passing surrounding contract tests
does not establish working analyzer integration or validate the skipped methods.

Acceptance criteria: module tests and a synthetic example will collect and run
with independent expected values; analyzer import and integration will be tested
after its route is repaired; migration documentation claims will be supported or
corrected. Broader local aggregate coverage, deputy assignment and a shared
lessons-log reference will be recorded separately from CI results.

## Draft 4: Resolve standards provenance and applicability

API STD 2RD is named by the package, but its edition and applicable clause are
not established. No standards compliance claim is supported by that association.

Acceptance criteria: the governing edition and applicable clause titles will be
verified from an authoritative source; applicable standards-derived calculations
will carry the required citation sidecars. Public documentation will cite the
basis without reproducing licensed source sections or tables.

## Draft 5: Classify methods and alternatives in the implementation map

The pinned implementation map counts no in-package family. YAML overlay,
diameter completion/consistency, nested extraction, plot metadata, reset and
persistence have not been assigned method families; the pack records concrete
outside-package alternatives without asserting equivalence.

Acceptance criteria: each method will be classified against the pinned map;
repository-wide discovery will include scripts, and inspected alternatives will
have an explicit equivalence or exclusion basis before any family count changes.
