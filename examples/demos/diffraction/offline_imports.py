"""Worked example: resolve offline diffraction tools without an interface.

Inputs are module and callable names. The method uses Python module imports
(PEP 562, 2017, Specification). The expected answer is a callable for each
requested tool; no solver is started and no availability flag is evaluated.
"""

from importlib import import_module


TOOLS = {
    "analytical_cylinder": "wavenumbers",
    "reverse_parsers": "OrcaWaveInputParser",
    "mesh_orientation": "orientation_report",
    "solver.report_extractors": "build_report_data_from_solver_results",
}


def main() -> None:
    """Print the resolved offline tools, failing if an export is unavailable."""
    for module_name, attribute in TOOLS.items():
        module = import_module(f"digitalmodel.hydrodynamics.diffraction.{module_name}")
        if not callable(getattr(module, attribute)):
            raise TypeError(f"{module_name}.{attribute} is unavailable")
        print(f"{module_name}.{attribute}: callable")


if __name__ == "__main__":
    main()
