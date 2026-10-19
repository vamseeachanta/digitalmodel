# OrcaFlex YAML input preflight

The offline validator rejects source structures that lose assignments during
dictionary construction. A valid result covers the implemented structural
checks; it does not establish native loading, units or physical qualification.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Document source controls and their limits so structural results are not treated as complete input qualification. |

## Source controls

`OrcaFlexYAMLValidator.validate_file` inspects the composed YAML graph before
constructing Python dictionaries. The source inspection rejects:

- Repeated keys in any mapping, including nested objects, fragments and include
  entries. Diagnostics identify the repeated key's source line and the first
  declaration line. Quoted and unquoted spellings of the same string are equal.
- Non-string mapping keys, including implicitly typed booleans, numbers,
  timestamps, nulls and collection keys. This application profile requires
  property names even though general YAML permits other key types.
- Null or comment-only documents, which contain no model or fragment.
- Present `General` and `Environment` sections that contain neither a mapping
  nor a nonempty sequence of include mappings with string file references.

Invalid source controls halt construction. Existing assignment-time timestep
diagnostics are retained. Shared aliases do not repeat the same source finding.
Keys in distinct mappings are independent. Empty section mappings, missing
sections and the existing include-list convention remain supported.

The existing SafeLoader scalar resolution and merge behavior remain in use.
A merge directive and a local override are distinct source keys; two merge
directives in one mapping are rejected. Merge expansion does not establish
resolved assignment context or native compatibility.

## Interpretation and limits

The uniqueness rule follows [YAML 1.2.2 (2021), Nodes and Node
Comparison](https://yaml.org/spec/1.2.2/#3213-node-comparison). The property-name
restriction is an application profile, rather than a restriction on general
YAML. No physical equations, coefficients or acceptance limits are introduced.

[Orcina, OrcaFlex help, Text data files, Indentation and Order of
data](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm)
describes property blocks and order-sensitive assignments. The online page was
read on 2026-10-19 and states no edition. It includes repeated assignments in a
variation file. Such files are intentionally rejected by this preflight profile
because dictionary construction would discard their sequence. Rejection is not
a claim that the native loader rejects every such file.

The include-list convention is documented in the repository's
[format findings](../CRITICAL_YAML_FORMAT_FINDINGS.md), Correct OrcaFlex YAML
Formats, Format 2, dated 2025-11-15. It is retained as an existing structural
convention, not newly qualified against a solver.

Property-level null/default tokens remain unresolved. In particular, acceptance
of `WaterDepth: ~` does not supply a depth, a unit, a design-basis revision or a
physical range. Unit evidence, design-basis ranges, target-version names,
selector order beyond existing timestep controls, transitive reference closure,
starting shapes and result-transform citations require later work. Native
loading and physical qualification remain unverified.

## Test evidence

`tests/solvers/orcaflex/test_yaml_validator_input_contract.py` uses synthetic
source controls. Expected duplicate locations are independently counted from
the literals in the tests, with the uniqueness source cited in the module
docstring. Negative controls cover repeated keys, typed keys, null documents
and malformed sections. Positive controls cover separate object mappings,
quoted text keys, aliases, local merge overrides, section include lists and
property defaults. These fixtures contain no engineering measurements.
