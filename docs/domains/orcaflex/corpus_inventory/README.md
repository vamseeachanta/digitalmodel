# OrcaFlex corpus inventory

This is a local syntax census for issue #2078. The committed table samples
public library models with riser, mooring and distributed buoy/line objects.
It records relative paths, source byte digests, export versions, local named
record counts and unclassified sections. It contains no model parameter values.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Add a reproducible local census before measuring extraction coverage. |

## Regeneration

From the repository root, with PyYAML available:

```sh
python src/digitalmodel/solvers/orcaflex/corpus_inventory.py . \
  --select 'docs/domains/orcaflex/library/model_library/a01_pliant_wave_riser/monolithic/*.yml' \
  --select 'docs/domains/orcaflex/library/model_library/c06_calm_buoy/monolithic/*.yml' \
  --select 'docs/domains/orcaflex/library/model_library/c08_fish_farm/monolithic/*.yml' \
  --output docs/domains/orcaflex/corpus_inventory
```

Omit `--select` to inventory all `.yml`, `.yaml`, `.dat` and `.sim` candidates
under an explicit input root. Choose an output directory outside that root for
private inputs. Review relative paths and section names before publishing any
inventory; omitting object names does not anonymise an arbitrary input tree.

`inventory.jsonl` is queryable one file per line. `manifest.json` fingerprints
the table bytes with SHA-256; each row fingerprints its source bytes. The
regression test regenerates both artifacts and compares their exact bytes.
No absolute root, timestamp or raw header is included, so regeneration is stable.
The manifest covers the selected files only; it does not attest the wider corpus.

## Meaning of the fields

`local_census` means an unambiguous mapping contains General, Environment or a
nonempty named-record list. Object counts are lengths of top-level lists in
which every entry defines a nonempty scalar `Name`. Future list types can be
counted without a hardcoded registry. This is structural recognition and does
not prove a file is a complete or valid OrcaFlex model. Singleton sections are
not counted as named objects. Empty, mixed or non-list sections remain listed
as unclassified, except General and Environment mappings.

`signature_candidate` means a binary extension has the OrcaFlex marker at the
offset described by issue #2078, inspected within its specified header window.
Binary versions and counts are null; conversion remains necessary. A candidate
signature is not a validated binary format or a successful model load.

`NOT_EVALUATED` identifies unresolved BaseFile, IncludeFile or RestartingFrom
references, symlinks, read failures, duplicate keys, multiple documents,
recursive aliases, excessive nesting, merge keys or unsupported tags. Dependencies are never
opened. Where syntax permits, counts describe only local declarations and
exclude dependencies. Ambiguous syntax produces null counts. Error text and
dependency target paths are withheld. `not_recognised` preserves other candidate
files in the table rather than dropping them. Missing versions remain null.

Every row has `round_trip: NOT_EVALUATED`. No parameter preservation percentage,
ProjectInputSpec extraction, expression evaluation or solver loading is claimed.
The existing extractors and converters are unchanged. Loss-accounted extraction,
dependency composition and semantic round-trip checks remain later slices.

## Sources and test basis

[Orcina, Text data files](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm),
online edition retrieved 2026-10-19, describes named lists, grouped data,
export headers, includes, ordered updates and multiple documents. Those features
explain why this census reports unsupported composition explicitly.
Issue #2078, Phase 1 and Notes, supplies the binary header window and marker offset.
There are no engineering coefficients, acceptance limits or equations here.

Tests enumerate synthetic records by hand, independently count the public riser
and mooring example records, and compare source and table byte digests. These
tests establish the local syntax contract, without asserting solver equivalence.
