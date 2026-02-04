# Contract: Variable Resolution & Section Overrides

> API contract for variable substitution and section override mechanism
> Rev: 2 (post iteration-2 review — SectionRegistry/SectionComposer eliminated)

## Module

`digitalmodel.solvers.orcaflex.modular_generator.sections`

## Class: `VariableResolver`

Resolves `${variable_name}` tokens in template content with optional format specifiers.

```python
class VariableResolver:
    """Resolves ${variable_name} tokens in template content.

    After substitution, parses result as YAML to validate structure.
    Supports format specifiers: ${water_depth:.0f} for integer formatting.
    """

    @staticmethod
    def resolve(template_content: str, variables: dict[str, Any]) -> str:
        """
        Replace ${key} and ${key:format} tokens in template content.

        Args:
            template_content: Raw template string with ${...} tokens.
            variables: Key-value pairs for substitution.

        Returns:
            Resolved string with all tokens replaced.

        Raises:
            ValueError: If any ${...} token remains unresolved.
            ValueError: If post-substitution content is not valid YAML.

        Behavior:
            - Pattern: ${key} or ${key:format_spec}
            - Whole-number floats auto-format as int (e.g., 30.0 → "30")
            - Format specifiers use Python f-string syntax (e.g., :.0f, :.2f)
            - Post-substitution YAML validation via yaml.safe_load()
        """
```

## Variable Substitution Rules

1. Token format: `${variable_name}` or `${variable_name:format_spec}` (dollar + braces)
2. Variable names: lowercase, underscores, alphanumeric only (`\w+` regex)
3. Format specifiers: any valid Python format spec after `:` (e.g., `.0f`, `.2f`, `>10`)
4. Nested resolution not supported (no `${${var}}`)
5. Unresolved tokens after substitution → `ValueError` with token name
6. Post-substitution YAML validation → `ValueError` if structure broken
7. Whole-number float auto-formatting: `30.0` → `"30"` (no format spec needed)
8. Variables from multiple sources merged with precedence:
   - Campaign matrix run parameters (highest)
   - Section-level variables (`InstallationSection.variables`)
   - Base spec computed values (e.g., `water_depth` from `spec.environment.water.depth`)

## Section Override Mechanism

Section overrides are handled directly by `ModularModelGenerator.generate_with_overrides()` — no separate `SectionComposer` class.

### `ModularModelGenerator.generate_with_overrides()`

```python
class ModularModelGenerator:
    @classmethod
    def from_spec(cls, spec: ProjectInputSpec) -> "ModularModelGenerator":
        """Create generator from an in-memory ProjectInputSpec (no YAML file needed)."""

    def generate_with_overrides(
        self,
        output_dir: Path,
        sections: list[InstallationSection] | None = None,
        variables: dict[str, Any] | None = None,
        template_base_dir: Path | None = None,
    ) -> GenerationResult:
        """
        Generate OrcaFlex include files with optional section overrides.

        Args:
            output_dir: Root directory for generated files.
            sections: Optional list of InstallationSection overrides.
                - If a section's template is set, loads template file,
                  resolves ${var} tokens via VariableResolver, writes result.
                - If a section's enabled=False, that builder is skipped entirely.
                - If None, all sections auto-generated from spec (standard behavior).
            variables: Global variables merged into all template resolutions.
            template_base_dir: Base directory for resolving relative template paths.
                Defaults to campaign YAML file's parent directory.

        Returns:
            GenerationResult with paths to generated files.

        Behavior:
            1. Iterates BuilderRegistry entries in order
            2. For each builder, checks if a matching InstallationSection exists
               (matched by builder_file string)
            3. If matched and enabled=False → skip
            4. If matched and template is set → resolve variables, write template output
            5. Otherwise → delegate to standard builder
            6. Write master.yml with includefile entries for all generated sections
        """
```

### `InstallationSection` Model

```python
class InstallationSection(BaseModel):
    """Override for a specific include-file section.

    template_path resolved relative to the campaign YAML file's directory.
    """
    builder_file: str = Field(...,
        description="Builder output filename to override (e.g., '08_buoys.yml'). "
                    "Must match a BuilderRegistry entry.")
    template: str | None = Field(default=None,
        description="Path to template file (resolved relative to template_base_dir)")
    variables: dict[str, Any] = Field(default_factory=dict)
    enabled: bool = Field(default=True)
```

### `GenerationResult` Dataclass

```python
@dataclass
class GenerationResult:
    master_file: Path              # Path to the master YAML
    include_files: list[Path]      # Paths to all generated include files
    variables_resolved: dict[str, Any]  # Variables that were substituted (if any)
    warnings: list[str]            # Non-fatal issues (e.g., missing context for downstream builders)
```

## Master YAML Format

```yaml
# Generated master file — do not edit
# Campaign: 30in_pipeline_campaign
# Run: wd30m_rl5000m_1yr_summer_soft_clay

includefile:
  - includes/01_general.yml
  - includes/02_var_data.yml
  - includes/03_environment.yml
  - includes/05_linetypes.yml
  - includes/08_buoys.yml
  - includes/07_lines.yml
  - includes/09_shapes.yml
  - includes/10_groups.yml
  - includes/13_supports.yml
  # S-lay only:
  - includes/04_vessel_types.yml
  - includes/06_vessels.yml
  - includes/11_winches.yml
```
