"""Spec-to-solver input converter (WRK-061).

Provides the ``SpecConverter`` class, which is the main entry point for
converting a canonical DiffractionSpec YAML file into solver-specific
input files for AQWA and/or OrcaWave.

Example::

    from digitalmodel.diffraction.spec_converter import SpecConverter

    converter = SpecConverter(Path("analysis.yml"))
    # Single solver
    path = converter.convert(solver="aqwa", format="single", output_dir=Path("out"))
    # All solvers
    results = converter.convert_all(output_dir=Path("out"))
    # Validate only
    issues = converter.validate()
"""

from __future__ import annotations

from pathlib import Path

from digitalmodel.diffraction.aqwa_backend import AQWABackend
from digitalmodel.diffraction.input_schemas import DiffractionSpec
from digitalmodel.diffraction.orcawave_backend import OrcaWaveBackend


class SpecConverter:
    """Main entry point for spec -> solver input conversion.

    Parameters
    ----------
    spec_path : Path
        Path to a YAML file conforming to the DiffractionSpec schema.

    Raises
    ------
    Exception
        If the YAML file cannot be parsed or fails schema validation.
    """

    def __init__(self, spec_path: Path) -> None:
        self.spec_path = Path(spec_path)
        self.spec = DiffractionSpec.from_yaml(self.spec_path)
        self.backends: dict[str, AQWABackend | OrcaWaveBackend] = {
            "aqwa": AQWABackend(),
            "orcawave": OrcaWaveBackend(),
        }

    def convert(
        self,
        solver: str,
        format: str = "single",
        output_dir: Path = Path("output"),
    ) -> Path:
        """Convert spec to solver-specific input files.

        Parameters
        ----------
        solver : str
            Target solver: ``"aqwa"`` or ``"orcawave"``.
        format : str
            Output format: ``"single"`` (one file) or ``"modular"``
            (one file per section/deck).
        output_dir : Path
            Directory where output files are written.

        Returns
        -------
        Path
            Path to the generated file (single mode) or directory/master
            file (modular mode).

        Raises
        ------
        ValueError
            If *solver* or *format* is not recognised.
        """
        solver = solver.lower()
        format = format.lower()

        if solver not in self.backends:
            raise ValueError(
                f"Unknown solver '{solver}'. "
                f"Choose from: {', '.join(self.backends)}"
            )
        if format not in ("single", "modular"):
            raise ValueError(
                f"Unknown format '{format}'. Choose 'single' or 'modular'."
            )

        backend = self.backends[solver]
        output_dir = Path(output_dir)

        if format == "single":
            return backend.generate_single(self.spec, output_dir)
        return backend.generate_modular(self.spec, output_dir)

    def convert_all(
        self,
        format: str = "single",
        output_dir: Path = Path("output"),
    ) -> dict[str, Path]:
        """Generate inputs for all registered solvers.

        Creates a sub-directory per solver under *output_dir*.

        Parameters
        ----------
        format : str
            Output format: ``"single"`` or ``"modular"``.
        output_dir : Path
            Root output directory.

        Returns
        -------
        dict[str, Path]
            Mapping of solver name to generated path.
        """
        output_dir = Path(output_dir)
        results: dict[str, Path] = {}
        for name in self.backends:
            solver_dir = output_dir / name
            results[name] = self.convert(
                solver=name, format=format, output_dir=solver_dir
            )
        return results

    def validate(self) -> list[str]:
        """Validate the loaded spec and return a list of issues.

        An empty list means the spec is valid.

        Returns
        -------
        list[str]
            Human-readable issue descriptions (empty if valid).
        """
        issues: list[str] = []

        # The DiffractionSpec model already enforces most constraints via
        # Pydantic validators. If we got this far, the spec parsed OK.
        # Perform additional semantic checks here.

        # Check bodies have mesh files specified
        for body in self.spec.get_bodies():
            mesh_file = body.vessel.geometry.mesh_file
            if not mesh_file or mesh_file.strip() == "":
                issues.append(
                    f"Body '{body.vessel.name}' has no mesh file specified."
                )

        # Check frequencies are non-empty
        freqs = self.spec.frequencies.to_frequencies_rad_s()
        if len(freqs) == 0:
            issues.append("No frequencies resolved from the spec.")

        # Check headings are non-empty
        headings = self.spec.wave_headings.to_heading_list()
        if len(headings) == 0:
            issues.append("No wave headings resolved from the spec.")

        # Check mass is positive (already enforced by Pydantic, but belt-and-braces)
        for body in self.spec.get_bodies():
            if body.vessel.inertia.mass <= 0:
                issues.append(
                    f"Body '{body.vessel.name}' has non-positive mass."
                )

        return issues
