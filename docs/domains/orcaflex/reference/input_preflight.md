# OrcaFlex YAML input preflight

The offline validator offers an opt-in strict input profile that rejects source
structures that lose assignments during dictionary construction. Existing
callers retain the default compatibility checks. A valid result covers only the
enabled checks; it does not establish native loading, units or physical
qualification.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Document source controls and their limits so structural results are not treated as complete input qualification. |
| 1 | 2026-10-19 | Make the strict profile opt-in so existing callers retain support for native numeric names and repeated directives; state the profile's exclusions. |
| 2 | 2026-10-25 | Check effective include-entry mappings so merged references receive the same shape checks as literal entries, while preserving source duplicate diagnostics. |

## Source controls

`OrcaFlexYAMLValidator.validate_file` inspects the composed YAML graph before
constructing Python dictionaries. Enable the strict preflight profile with
`OrcaFlexYAMLValidator(strict_profile=True)` or
`validate_orcaflex_yaml(path, strict_profile=True)`. The option also applies to
every file checked by `validate_directory` or the directory convenience call.
With this option the source inspection rejects:

- Repeated keys in any mapping, including nested objects, fragments and include
  entries. Diagnostics identify the repeated key's source line and the first
  declaration line. Quoted and unquoted spellings of the same string are equal.
- Non-string mapping keys, including implicitly typed booleans, numbers,
  timestamps, nulls and collection keys. This application profile requires
  property names even though general YAML permits other key types.
- Null or comment-only documents, which contain no model or fragment.
- Present `General` and `Environment` sections that contain neither a mapping
  nor a nonempty sequence of include mappings with string file references.

Invalid strict source controls halt construction. Existing assignment-time
timestep diagnostics and graph traversal limits apply in both modes. Shared
aliases do not repeat the same source finding.
Keys in distinct mappings are independent. Empty section mappings, missing
sections and the existing include-list convention remain supported.

The existing SafeLoader scalar resolution and merge behavior remain in use.
A merge directive and a local override are distinct source keys; two merge
directives in one mapping are rejected by the strict profile. Merge expansion
does not establish resolved assignment context or native compatibility.
Section and include-entry shape checks use effective merge values, including
local overrides and merge-list precedence. These checks flatten a copied graph;
the original graph retains duplicate-key and assignment-order evidence.

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

The strict profile also rejects native numeric object names such as `1:` and
`2.5:`, repeated `includefile:` entries in one mapping, and repeated `New:`
actions. These constructs occur in tracked model inputs. They remain accepted
by the default compatibility checks, which use SafeLoader's final dictionary
and do not interpret directive order or prove transitive include closure.
Default acceptance therefore does not establish that every assignment was
retained. The strict profile is intended for preflight inputs whose authors
require unique string keys, rather than as a native model acceptance test.

The comment-only `modular_generator/templates/defaults.yml` is an inactive
placeholder, outside the model and fragment input scope. The generator uses
registered builders or explicitly selected section templates; it does not
automatically load that placeholder. If explicitly submitted as an input, a
comment-only document is rejected by the strict profile. The default validator
retains its null-document warning.

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
source controls with strict preflight enabled. Expected duplicate locations are
independently counted from the literals in the tests, with the uniqueness source
cited in the module docstring. Negative controls cover repeated keys, typed
keys, null documents and malformed sections. Positive controls cover separate object mappings,
quoted text keys, aliases, local merge overrides, section include lists and
property defaults. These fixtures contain no engineering measurements.
Default compatibility controls separately cover numeric object names, repeated
include directives and repeated creation actions. File and directory controls
check that the strict profile must be explicitly requested. Custom-tagged keys
identify the unsupported tag in the strict diagnostic.
