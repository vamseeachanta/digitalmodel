# Python source locators in Markdown

Wrap the complete source locator in an inline-code span, including its square
brackets. This preserves underscores in Python filenames during Markdown
formatting. Apply the convention in paragraphs and table cells alike.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-28 | Define literal source locators and a separate check to preserve formatter output without changing inventory semantics. |
| Update | 2026-10-29 | Check locators before colon prose and provision the pinned formatter for contracts tests in both CI workflows. |

## Convention

Use the form `[path/to/module.py:Lstart-Lend]` with a repository-relative POSIX
path and inclusive, one-based line bounds. For an initializer, keep the literal
`__init__.py` filename inside the span. Do not replace underscores with emphasis
markers or escapes. Both range endpoints must exist, and the first must not
exceed the last.

For example, the standard header occupies the first two physical lines of
`scripts/docs/source_locators.py`, so its locator is
`[scripts/docs/source_locators.py:L1-L2]`.

| Evidence | Meaning |
|---|---|
| `[scripts/docs/source_locators.py:L1-L2]` | The same literal header locator in a table cell. |

## Standalone check

From the repository root, supply the Markdown files to inspect:

```sh
uv run --no-project python scripts/docs/source_locators.py docs/development/source-locators.md
```

More than one document may be supplied. `--root` selects a different repository
root; relative document and source paths resolve against that root. A successful
check returns zero; any document or locator error returns one, with the document
path, Markdown line, literal locator and reason where available.

The check recognizes explicit bracketed Python locators with decimal line
numbers, such as the example above, anywhere in supplied Markdown. This includes
paragraphs outside section two, table cells, bare locators and code examples.
Paths use letters, digits, underscores, dots, hyphens and forward slashes; a
resolved source must stay inside the repository. Source files are inspected as
UTF-8 text, never imported or executed.

Ordinary prose, unbracketed filenames, external URLs, Markdown inline links,
images and reference links are outside the grammar. Other file types, optional
or malformed line-range syntax and formatter-escaped filenames are outside it
as well. The check validates source presence and line bounds; it does not check
the meaning of the cited text, enforce the inline-code convention or repair
documents. It does not discover documents recursively or claim that unrecognized
text is a valid citation.

This helper is independent of the section-two public-symbol inventory and the
bounded knowledge-pack acceptance audit. Existing packs, inventory semantics,
hooks, gates and formatter configuration are unchanged.

## Formatter regression fixture

`tests/docs/test_source_locators.py` creates a disposable package initializer
containing exactly two literal source lines and a Markdown document with a
literal locator for `src/example/__init__.py` covering those lines, inside
inline-code spans in a paragraph and a table. The literal fixture defines the
expected bytes and bounds independently of the helper. The formatter may realign
the table, but both locator occurrences must remain byte-for-byte unchanged.

Set `SOURCE_LOCATORS_PRETTIER` to the executable from the Prettier package version
pinned in `.pre-commit-config.yaml`, or put that executable on `PATH`. The test
checks the installed package version against that pin and reports the CLI
version separately, since the package can delegate to another CLI dependency.
It formats only the temporary fixture and does not obtain packages itself. An
unavailable or mismatched formatter fails the acceptance check; another formatter
is not substituted.

The contracts steps in both the domain and full-matrix CI workflows install the
pinned package and export `SOURCE_LOCATORS_PRETTIER` before running the tests.
Regression checks keep those installations aligned with the hook pin.

```sh
uv run --no-project --with pytest --with pyyaml python -m pytest tests/docs/test_agent_pack_functions.py tests/reporting/test_knowledge_pack_acceptance.py tests/docs/test_source_locators.py -q -rA -p no:cacheprovider
```
