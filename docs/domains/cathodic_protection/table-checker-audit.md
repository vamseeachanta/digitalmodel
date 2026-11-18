# Table checker audit

The B401, F103 and ABS CSV checks now compare quantity metadata before numeric
values. They use the existing `verify_table_value` interface: exact nonempty
unit labels, equality of every citation field, live resolution against the
repository identity fixture, then exact finite numeric equality. Explicit
exceptions preserve these checks under optimized Python. This is a test-checker
change; calculation outputs and standard tables are unchanged.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-21 | Extend the table audit beyond utilisation factors because equal numerals can hide changed units or provenance. |

## Scope and sources

| Table family | Independent numeric reference | Metadata-aware coverage |
|---|---|---|
| B401 June 2017 | Existing Appendix A CSVs, Tables A-1, A-2, A-3, A-4, A-6, A-8 | Initial, mean and final design density, reinforcement, coating constants, ambient capacity and potential, utilisation |
| B401 May 2021 | Existing Section 8 CSVs, Tables 8-1, 8-2, 8-3, 8-4, 8-6, 8-8 | Same families, including temperature-dependent anode rows |
| F103 October 2010 | Existing Table 5-1 and Tables A.1, A.2 CSVs | Mean density and both coating constants; annex column scaling retained |
| F103 September 2019, amended May 2021 | Existing Tables 6-2, 6-3, A-1, A-2 CSVs | Mean density, both coating constants, temperature-dependent capacity and potential |
| ABS December 2017 | Existing Section 2 Tables 1, 3, 4, 5 and Section 3 Tables 2, 4 CSVs | Supported protection criteria, current ranges, coating deterioration and alloy properties |

The dataset [provenance record](../../../tests/fixtures/test_vectors/cathodic_protection/datasets/PROVENANCE.md)
records the B401/F103 transcription basis. The
[ABS provenance record](../../../tests/fixtures/test_vectors/cathodic_protection/datasets/abs-gn-ships/2017-12/PROVENANCE.md)
records that dataset's basis. Existing citation pages under
`tests/citations/fixtures/knowledge/wikis/engineering-standards/wiki/standards/`
provide source-identity frontmatter. These are repository references; no new
comparison with a licensed publication is claimed. No numerical engineering
constant, tolerance or acceptance limit is introduced. The F103 October 2010
scaling divisor comes from the x100 headings in Tables A.1 and A.2.

Expected units, source edition, table locator and quantity note are constructed
independently of the actual returned citation and production source map. CSV
parsers provide the expected numbers. Existing enum values identify requested
rows; this does not independently qualify enum-to-row mappings or CSV headers.

## Regression method

Metadata probes replace the production lookup result with an unchanged numeral
carrying a different unit, edition, source path or quantity note. Paired values
also receive a mutation confined to the last member, so a checker that validates
only the first member fails the probe. Each checker is exercised directly and
in a real `python -O` subprocess, with matching positive controls. The subprocess
requires explicit rejection and does not rely on an `assert` statement.

The shared citation-checker tests separately reject nonfinite values, missing
sources and altered frontmatter, and verify metadata-before-number ordering.
Existing numeric assertions remain in the domain tests as additional checks.

## Worked example

Inputs are synthetic: two segments of three metres and an independent expected
length of six metres. The hand calculation is `3 m + 3 m = 6 m`; no standard or
engineering coefficient is involved. `reference_root` is a caller-supplied
temporary directory. The example creates only a synthetic identity fixture there.

```python
from dataclasses import replace
from digitalmodel.citations import Citation, CitedValue
from digitalmodel.citations.table_verification import (
    TableVerificationError, verify_table_value,
)

page = reference_root / "wikis/synthetic/length.md"
page.parent.mkdir(parents=True, exist_ok=True)
page.write_text(
    "---\ncode_id: synthetic-length\npublisher: synthetic\n"
    "revision: example\n---\nSynthetic arithmetic only.\n",
    encoding="utf-8",
)
citation = Citation(
    code_id="synthetic-length", publisher="synthetic", revision="example",
    section="Example length", wiki_path="wikis/synthetic/length.md",
    note="two segments of three metres",
)
# Independent hand calculation above supplies the reference numeral.
expected = CitedValue(value=6, units="m", citation=citation)
actual = CitedValue(value=3 + 3, units="m", citation=citation)
variants = (
    actual,
    replace(actual, units="mm"),
    replace(actual, citation=replace(citation, section="Different example")),
)
outcomes = []
for quantity in variants:
    try:
        verify_table_value(quantity, expected, repo_root=reference_root)
    except TableVerificationError:
        outcomes.append("REJECTED")
    else:
        outcomes.append("PASS")
```

Answer: `outcomes` is `['PASS', 'REJECTED', 'REJECTED']`. Six millimetres and
the differently attributed six metres fail despite unchanged numerals. The
regression suite executes this example against a temporary source root.

## Remaining qualification

| Area inspected | Status and way forward |
|---|---|
| B401 earlier editions and F103 quantities deferred to them | Source resolution is NOT_EVALUATED here: the earlier B401 identity fixture is absent. Existing numeric tests remain. Supply an independently reviewed source fixture before extending resolution checks. |
| ABS unsupported alloy rows and magnesium table | NOT_EVALUATED: no corresponding lookup is implemented. Existing catalogue coverage is unchanged. |
| Fatigue C203 table records | NOT_EVALUATED by this change: records are plain numeric tuples, without per-quantity citation/unit objects. Establish a separately reviewed metadata contract before adapting comparison checks. |
| E305 external tables | The loader already validates units and resolves citations with explicit exceptions. No external table bytes were supplied or compared in this audit. |
| BHA ISO table loader | Separate implementation already validates units, source keys, edition, locator and basis through explicit exceptions; not modified here. |

Exact labels provide the bounded dimension contract; unit conversion and aliases
are not inferred. Matching page frontmatter establishes source identity only.
Source-byte authenticity, the presence or correctness of a cited section, CSV
transcription accuracy and current-edition design qualification remain outside
these checks. Independent review remains required before merging.
