"""Generate modular YAML from a domain spec and explicit installation inputs.

Run with DOMAIN_SPEC INSTALLATION_YAML OUTPUT_DIR; all outputs go to OUTPUT_DIR.
No licensed solver is imported or executed.
"""

from __future__ import annotations

import argparse
from pathlib import Path
from tempfile import TemporaryDirectory

import yaml

from digitalmodel.marine_ops.installation.jumper_to_modular_spec import build_modular_spec
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def main() -> None:
    """Merge the illustrative overlay and emit master.yml with includes."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("domain_spec", type=Path)
    parser.add_argument("installation_yaml", type=Path)
    parser.add_argument("output_dir", type=Path)
    args = parser.parse_args()
    domain = yaml.safe_load(args.domain_spec.read_text())
    overlay = yaml.safe_load(args.installation_yaml.read_text())
    domain["installation"] = overlay["installation"]
    with TemporaryDirectory() as temporary:
        merged = Path(temporary) / "domain.yml"
        merged.write_text(yaml.safe_dump(domain, sort_keys=False))
        data = build_modular_spec(str(merged))
    model = ProjectInputSpec(**data)
    ModularModelGenerator.from_spec(model).generate(args.output_dir)
    print("Generated master.yml and includes; licensed checks NOT_EVALUATED.")


if __name__ == "__main__":
    main()
