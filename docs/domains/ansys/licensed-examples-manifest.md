# MAPDL example preparation manifest

## Summary

The [manifest](../../../examples/ansys/licensed-examples-manifest.json) reserves
`ansys-mechanical-solve` as a preparation identifier for three generic examples.
It does not register a workflow, authorize execution or establish engineering
qualification. The six existing files are unchanged.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-11-16 | Added canonical byte pins and a static effects record before runtime enforcement. |

## Schema and byte convention

Schema version 1 has `workflow_id`, `status`, `digest_convention`,
`source_revision` and `cases`. The status is `preparation-only`; the digest
convention is `sha256-raw-git-blob`. Each unique `case_id` has exactly two files:
one `input` and one `companion`, each recording `role`, `path`, `bytes` and
lowercase hexadecimal `sha256`. Paths are exact repository-relative POSIX paths.
Absolute paths, drive or UNC forms, backslashes, empty or dot segments, parent
traversal, duplicates and unlisted companions are rejected by the offline tests.

The source revision records where the bytes were captured; it is not an approval
receipt. SHA-256 and byte counts cover raw Git blob bytes without text decoding or
newline conversion. Tests compare all six pins with `git cat-file blob HEAD:<path>`
in the owning repository, clearing inherited Git environment bindings. They work
in a shallow checkout without the historical source revision. Later changes to
any of the six blobs require manifest review and updated pins.

Canonical blob integrity differs from execution-byte integrity. Existing
attributes preserve raw bytes for `pv.inp` and `mudmat.inp`; both blobs contain
CRLF line endings. A checkout may transform other files. Before future dispatch,
all six execution files require raw-byte readback against the reviewed pins.
Newline normalization cannot satisfy a byte pin; the mutation test proves refusal.

## Static effects review

The complete six files were inspected at the manifest's source revision.
They contain generic geometry and material descriptions; no client identifiers
were observed. No external source or companion beyond the listed decks is
referenced by the YAML. All three inputs specify a relative script, relative
`results` output directory and `dry_run: false`; their comments do not constitute
execution authority.

| Deck | Observed file, process and output effects |
| --- | --- |
| `padeye.inp` | `FINISH`, `/CLEAR,NOSTART`; `/SOLU`, `ANTYPE,STATIC`, `SOLVE`; `/POST1`, `SET,LAST`; `PRNSOL`, `PRRSOL`, `FSUM`, `/COM`, `*MSG`; `*CFOPEN,padeye_result,csv`, `*VWRITE`, `*CFCLOS`. A `*DO` loop accumulates nodal reactions. |
| `pv.inp` | `FINISH`, `/CLEAR,NOSTART`; `/SOLU`, `ANTYPE,STATIC`, `SOLVE`; `/POST1`, `SET,LAST`; `FSUM`, `/COM`, `*MSG`; `*CFOPEN,pv_result,csv`, `*VWRITE`, `*CFCLOS`. |
| `mudmat.inp` | `FINISH`, `/CLEAR,NOSTART`; `/SOLU`, `ANTYPE,STATIC`, `SOLVE`; `/POST1`, `SET,LAST`; `FSUM`, `/COM`, `*MSG`; `*CFOPEN,mudmat_result,csv`, `*VWRITE`, `*CFCLOS`. |

`*CFOPEN` destinations are relative to the solver working directory. `SOLVE`
produces native solver artifacts and `SET,LAST` reads the result set. Listing and
message commands write to the solver output stream. No directory changes, macro
includes, explicit external-program launches or other explicit file destinations
were observed. This is a static record of the pinned decks, not an APDL sandbox.
There are no observed unresolved companion references; implicit native solver
files still require execution containment.

## Checks and remaining limits

[Offline tests](../../../tests/ansys/test_licensed_examples_manifest.py) reuse
`load_bounded_json` and `verify_payload_inventory`. They enforce the versioned
schema and independent six-path inventory, then refuse changed bytes, missing or
extra payloads, invalid or mismatched digests and incorrect byte counts. Copied
payload mutations stay in memory and are never submitted to a solver.

The decks use fixed inputs and static solve sequences, with no observed random,
clock or external-data inputs. Numerical determinism across solver versions,
convergence and design suitability are not established. Deck headers state mm,
N and MPa; these are declared assumptions, not independently checked units.
Integrity receipts retain `source_rights: NOT_EVALUATED` and
`engineering_qualification: NOT_EVALUATED`.

Before registry activation, the subsequent work must enforce every symlink path
component, script containment, dedicated results containment, a trusted launcher,
timeout bounds and execution-byte verification. Positive completion remains
dependent on #1524. This preparation slice leaves #1525 open, performs no licensed
solve and makes no dispatch-readiness claim. Second-provider artifact review is
required before implementation closeout.
