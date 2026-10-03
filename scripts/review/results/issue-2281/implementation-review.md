# Issue 2281 implementation and partial-artifact review

Date: 2026-10-03. Scope: eleven public regression packs plus explicit A–E
blockers. Full portfolio completion is **not approved or claimed**.

## Findings and inline disposition

Sol r1 returned MAJOR: unused release allowlist; children inferred only from
available outputs; destructive reruns. The implementation now uses exact-output
release digests, independent frozen S09/S10 input/result contracts, new candidate
directories, and transactional publication that refuses modified destinations.
Negative integration tests exercise private-vector injection and unlisted review
files before any destination changes.

Sol r2 confirmed the fixed child checks and pack receipts but returned MAJOR for
**final** publication while five benchmarks remain blocked. Main-session disposition:
this is an explicitly incomplete draft under the owner's constructive-work
instruction, not final portfolio acceptance. The publisher rejects incomplete
coverage unless `--partial-review` is explicit; the index states incomplete and
the goal remains unachieved. No scope reduction or benchmark substitution occurs.

The r2 backup concern is fixed inline: old generations remain outside temporary
cleanup. A failed replacement cannot cause temporary cleanup to erase the old
portfolio. Tests preserve the prior generation and check a failed copy leaves
the existing destination intact. No additional review cycle was dispatched.

## Artifact evidence and limits

- Presentation agent audited all eleven candidate reports statically: ten review
  sections, stable source/result anchors, no duplicate IDs/dead internal anchors,
  full-file sidecar hashes and no externally fetched scripts/styles.
- Main independently regenerated all eleven route results and compared every
  source HTML table and actual figure-creation payload against the report. All
  matched; table counts include document-control/input-echo tables, not only
  engineering tables. Jacket results exactly match the preserved original draft.
- Two clean candidate rebuilds matched all 129 artifact hashes. The final candidate
  differs only by correcting “Engineer-of-record” in change records and updating
  their receipts, recording build dependencies, and normalizing the two new YAML
  fixtures to LF before freezing their source hashes; reports/results/sidecars are
  unchanged. Final release contains 130 bound artifacts plus the tree receipt.
- Identifier scanner inspected all 129 files, with zero findings/uninspectable
  files. Privacy review traced inputs to frozen existing repository fixtures and
  two synthetic test-derived extensions; generated results exactly match those
  inputs. No private benchmark vectors, source files, mappings, hashes, OCR or
  client metadata enter these artifacts. Embedded plotting-library bytes and
  numerical payloads come from the same unchanged report renderer. A string scan
  alone is not treated as privacy certification.
- Git-index bytes, not only working-tree files, matched the release hashes for all
  130 artifacts and all eleven frozen inputs. Scoped Git attributes preserve LF
  across Windows/Linux checkouts, retaining exact-file comment bindings.
- `portfolio-release.json` binds the exact reviewed candidate bytes for partial
  repository inclusion. It is a review receipt, not a cryptographic identity or
  engineering signature. The publisher checks it before writing.
- 45 new tests passed, including actual Node execution of comment binding,
  stale/repeated exports, deferred dispositions, decision-conflict retention,
  Save read-back and Load round-trip. Ruff/mypy passed on all touched Python.
- Existing scoped checks: 178 reporting tests passed; the remaining skeleton
  test passed with UTF-8 enabled. A further 103 scoped adapter/zone/phase checks
  passed, including that skeleton retest. Local router collection and the
  engine-dispatch integration are limited by pre-existing missing bs4 and
  pyarrow/NumPy incompatibility; CI remains authoritative for them.

Main disposition: **accept partial regression-only draft inclusion** with the
recorded limits. This is a Codex/Sol fallback review, not cross-provider consensus.
Claude/Gemini availability is recorded separately. Human desktop/mobile/print/file
control review remains deferred, and EOR acceptance remains pending.

## Private benchmark review

A separate agent independently reviewed the private source-backed diagnostics.
A's conditional component-demand comparison is supported for its represented
source subset only. Its families remain local; recommended counts do not establish
historical installed adequacy. Omitted components and temperature assumptions
remain limitations, and full-case validation remains false.
D's concrete-demand diagnostic is supported but does not establish buried-family
layout adequacy. E's unresolved infill correctly fails closed. A design continuity
requirement is not field verification; B's excluded assemblies must stay outside
the protected network. All precise evidence remains private. Further source-backed
mapping is constructive work; private-data release and unsupported engineering
assumptions remain critical decisions, not administrative permission gates.

The shared B401 edition-citation defect is tracked in
[#2284](https://github.com/vamseeachanta/digitalmodel/issues/2284).
