# Generator validation and diagnostic output

`ModularModelGenerator.generate()` and `generate_with_overrides()` validate a
temporary model before publishing files to the requested output directory.
Any error-level `PostGenerationValidator` finding raises `ModelValidationError`.
The exception retains all findings in `.findings`. An existing output tree is
unchanged by a validation or builder failure; a new target is not created.
Warnings alone allow publication. The CLI delegates to the same validated
generator. Publication is a copy after validation, so an I/O failure during that copy is not an atomic filesystem transaction.

The default enforces the existing validator rules. For example, the native
format requires [unique object names](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Datafields.htm)
and [case-insensitive name matching](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Themodel.htm).
See the [object-reference worked example](reference-lookup.md) for inputs,
matching rules and the expected missing-reference diagnostic.
The validator also rejects `WaveType: No waves`, including raw extra trains;
still-water input uses the existing zero-height Airy convention. No engineering
factor or acceptance band changes in this correction.

For a diagnostic run only, either method accepts the keyword-only option
`allow_validation_errors=True`. Error findings remain logged at ERROR level;
`generate_with_overrides()` also retains them in `GenerationResult.warnings`.
This option does not relax collection-key collision checks or schema checks.
Existing valid callers need no changes, and returned paths refer to the final
output directory after temporary storage is removed.

Nine pre-existing diffraction input-generation failures are marked beside their YAML with a
`Generation audit #2711: BLOCKED` comment. These markers document outstanding
mesh-symmetry verification gaps; they do not waive any validator rule. They are
in the separate diffraction input generator, outside the modular generator's
post-validation gate. The narrower interpretation is to mark them rather than
replace unavailable geometry or alter engineering input values. Their existing
input generation continues to fail loudly. No LFS-tracked geometry is changed.
Native loading, statics and engineering clearance are not established by these
offline serialization checks and remain with the maintainer.

The current full audit also reports five pre-existing strict-format findings in
one additional library template. That input is marked BLOCKED too. The separate
YAML-strict validator reports the same findings on main and this correction;
the post-validator reports no errors for that input. The post-validation gate
does not replace the separate YAML-strict audit. No format check is relaxed.

Validation receipt: 775 modular-generator tests pass, with two existing skips.
Two files that import the commercial binding during collection are excluded.
All 29 structure tests pass. Seventeen gate regressions include both generation
paths, diagnostic output, unchanged existing targets, warning-only publication,
custom overrides and CLI rejection; 15 fail on the previous implementation.
The 109-input audit covers 96 time-domain specs (93 generated and post-validator
clean, three validated by alternative schemas) and 13 diffraction specs (four
generated, nine blocked on mesh symmetry). The separate strict audit reports the
five unchanged findings described above. Marker comments preserve parsed inputs.
