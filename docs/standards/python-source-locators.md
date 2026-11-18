# Python source locators

Enclose local Python source locators in inline code so Markdown formatting
preserves their path and line-range bytes. The existing document-scoped checker
validates those citations independently of the public-symbol inventory.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-11-04 | Define a reusable locator convention and test its formatted example to prevent initializer paths from becoming emphasis. |

## Convention

Use a single inline-code span around the complete bracketed locator:
`[repository/relative.py:Lstart-Lend]`. Replace `start` and `end` with inclusive,
one-based physical source line numbers. Paths resolve from the repository root,
including when the Markdown file is in a package's `docs/` directory.

Apply this form in prose and table cells, including citations outside section
two. Protect the whole token, rather than only its filename or underscores.
Bare `__init__.py` text can become `**init**.py` during Markdown formatting;
inline code preserves the literal underscores. Table padding and alignment may
change without changing a locator's bytes.

Copy this example when adding a citation. Its ranges refer to the current
corrosion initializer; choose the actual source and bounds for each new citation.

```markdown
## 3. Method

Initializer: `[src/digitalmodel/corrosion/__init__.py:L7-L21]`.

| Evidence | Meaning |
|---|---|
| `[src/digitalmodel/corrosion/__init__.py:L7-L21]` | initializer |
```

## Scoped check

Run the existing checker on the documents being changed, from the repository root:

```bash
uv run --no-project python scripts/docs/source_locators.py \
  src/digitalmodel/corrosion/docs/agent-pack.md
```

The checker requires Node.js and the Prettier package pinned in
`.pre-commit-config.yaml`. Set `SOURCE_LOCATORS_PRETTIER` to that package's
executable when it is absent from `PATH`. The regression test checks the package
version before formatting its example.

The check reads Python source without importing it. It reports missing files,
paths resolving outside the repository, reversed ranges and bounds outside
the physical source lines. Explicit bracketed `.py:Lnumber-Lnumber` tokens
are checked in bare text, inline code and code blocks in every section of each
supplied document. Resolved Markdown links, external URLs, escaped prose
brackets and unbracketed prose are outside that grammar. Documents not supplied
to the command are not checked.

Continue to run `scripts/docs/check_agent_pack_functions.py` for section-two
public-symbol inventories. Citation existence and line bounds do not establish
that a cited range supports a claim or identifies the intended public symbol.
The two checks retain separate responsibilities; no hook or gate is changed.

## Existing-pack inventory and limits

At base revision `5269966071f9350f4c4bac5b1c2df45ef4e77271`, a read-only scan
of `src/**/docs/agent-pack.md` on 2026-11-04 found:

| Observation | Count |
|---|---|
| Packs | 112 |
| Packs with explicit local Python locator tokens | 111 |
| Explicit local Python locator tokens | 42,879 |
| Packs with initializer locator tokens | 104 |
| Initializer locator tokens | 851 |
| Initializer tokens immediately enclosed by backticks | 77 |
| Initializer tokens containing the literal `**init**.py` form | 0 |

The scan counts bracketed repository-relative `.py:Lnumber-Lnumber` tokens;
the protected count uses immediately adjacent backticks. It separately searches
for bracketed `**init**.py` tokens with numeric line bounds. These are textual
counts, without Markdown-context classification or validation of every target.
They do not establish formatting stability or citation validity for all packs.

Existing packs remain unchanged. Review their bare locators and source bounds
before a broader migration; do not infer corruption solely from missing
backticks. The focused regression substitutes a synthetic two-line initializer
into the example, formats it with the configured formatter, permits table
alignment changes and checks both retained citations. Replacing its prose
citation with an absent source must then fail the scoped checker.
