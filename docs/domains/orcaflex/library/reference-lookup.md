# Case-insensitive object reference lookup

The post-generation validator resolves object names without case sensitivity.
The [offline worked example](../../../../examples/domains/orcaflex/reference_lookup.py)
shows the input dictionaries, lookup method and expected findings across the
existing line-type, line-connection, winch, link and constraint checks.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-21 | Added a runnable example and diagnostic expectations to explain object-name matching. |

## Basis and method

[Orcina OrcaFlex webhelp, The model](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Themodel.htm),
object names paragraph (unversioned, accessed 2026-10-21), states that object
names are case-insensitive. Its published `Riser`, `riser` and `RISER` example
establishes the expected equivalence. No equations, physical constants or
acceptance limits are needed for this string comparison.

The validator collects names from list-style object definitions. Each existing
reference check compares case-folded lookup keys while retaining the original
input for diagnostics. Line types and the two winch targets must also belong to
their required definition sections. Line connections, links and constraints
use the existing collection of named targets. Matching does not strip spaces
or correct spelling mistakes.

## Inputs and answer

The example defines `ChainType`, `SupportObject`, `BuoyObject` and `GuideObject`
in one file dictionary. Another file dictionary defines `line_a` and references
those names with different case:

| Reference field | Input | Matching definition |
| --- | --- | --- |
| Line segment type | `chaintype` | `ChainType` in LineTypes |
| Line connection at end A | `SUPPORTOBJECT` | `SupportObject` in Vessels |
| Line connection at end B | `buoyobject` | `BuoyObject` in 6DBuoys |
| Winch vessel and line | `supportobject`, `LINE_A` | `SupportObject`, `line_a` |
| Link connections | `guideobject`, `LINE_A` | `GuideObject`, `line_a` |
| Constraint InFrameConnection | `SUPPORTOBJECT` | `SupportObject` |
| Constraint OutFrameConnection | `buoyobject` | `BuoyObject` |
| Constraint Connection | `GUIDEOBJECT` | `GuideObject` |

Hand comparison shows that each reference differs from its definition only in
letter case. The clean result is therefore an empty list. Replacing the winch
line reference with `MissingLine` leaves that name undefined and produces one
error, preserving its spelling. The example copies the input before changing it.

Run from the repository root with the package dependencies available:

```sh
PYTHONPATH=src uv run python examples/domains/orcaflex/reference_lookup.py
```

Expected output:

```text
Clean findings: []
Winch 'WinchObject' references line 'MissingLine' which is not defined
```

`test_reference_case.py` runs this example, checks the literal output and loads
the clean dictionaries through both directory entry points. Its other tests
cover the original issue reproduction, missing names, spaces and section
restrictions, with exact diagnostic comparisons.

## Limits

The example uses partial rows containing only reference names; it establishes
offline lookup behaviour only. It supplies no geometry, material properties or
native model loading evidence. No licensed solver is needed. Mapping-style
definitions and reference fields outside the existing checks remain separate
work. Built-in connection keywords retain their existing handling. If no line
types are defined, the existing partial-model skip still applies.
