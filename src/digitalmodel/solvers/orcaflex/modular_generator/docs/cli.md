# Offline model generation

After installing the package, generate model YAML from a specification:

```sh
modular-generator example.yml --output generated/
```

The specification is positional and `--output` (or `-o`) is required.
Use `modular-generator --help` for usage. The command builds YAML and runs
the generator's post-validation before publishing files. It requires no
licensed solver. A successful command returns zero; an absent or invalid
specification, or a post-validation error, returns a nonzero status.
Validation errors leave an existing output directory unchanged.

Save the following synthetic input as `example.yml`. Its prescribed water
depth is 100 m, chosen as two 50 m layers by hand. It contains no objects
and uses schema defaults for other properties; it is a CLI example rather
than an engineering design.

```yaml
metadata:
  name: synthetic
  description: Synthetic CLI input
  structure: generic
  operation: generic
  project: generic
environment:
  water:
    depth: 100
  seabed:
    stiffness: {}
generic: {}
```

Generation writes `master.yml`, the referenced YAML files under `includes/`,
and `inputs/parameters.yml`. YAML output contains no anchors or aliases.
Default seabed stiffness may produce warnings in this minimal example;
warnings permit publication, while validation errors prevent it.

The existing module commands remain available:

```sh
python -m digitalmodel.solvers.orcaflex.modular_generator generate --input example.yml --output generated/
python -m digitalmodel.solvers.orcaflex.modular_generator validate --input example.yml
python -m digitalmodel.solvers.orcaflex.modular_generator campaign --help
```

Generator retirement and conversion changes are outside this interface change.
