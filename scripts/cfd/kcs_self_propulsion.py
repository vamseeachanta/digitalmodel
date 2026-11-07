# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build, smoke or check the public KCS actuator-disk case without extra packages."""

import importlib.util
import sys
from pathlib import Path


def main() -> int:
    """Load the standard-library implementation without optional solver imports."""
    source = (Path(__file__).resolve().parents[2] /
              "src/digitalmodel/solvers/openfoam/kcs_actuator_disk.py")
    spec = importlib.util.spec_from_file_location("_kcs_actuator_disk", source)
    if spec is None or spec.loader is None:
        raise ValueError("actuator-disk module could not be loaded")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module.main()


if __name__ == "__main__":
    raise SystemExit(main())
