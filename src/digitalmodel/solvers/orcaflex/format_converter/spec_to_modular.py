"""Convert spec.yml to modular OrcaFlex format.

Thin wrapper around the existing ModularModelGenerator.
"""

from __future__ import annotations

from pathlib import Path

from .protocols import ConversionReport


class SpecToModularConverter:
    """Convert a spec.yml to modular OrcaFlex format.

    Delegates to ModularModelGenerator which handles:
    - Spec validation via Pydantic ProjectInputSpec
    - Builder-based include file generation
    - master.yml and parameters.yml generation
    """

    def __init__(self, spec_path: Path | None = None):
        self.spec_path = Path(spec_path) if spec_path else None

    def convert(
        self, source: Path | None = None, target: Path | None = None
    ) -> ConversionReport:
        """Convert spec to modular format.

        Args:
            source: Path to spec.yml.
            target: Output directory for modular files.
        """
        src = Path(source) if source else self.spec_path
        if src is None:
            raise ValueError("No source path provided")

        tgt = Path(target) if target else src.parent / src.stem

        warnings: list[str] = []

        try:
            from ..modular_generator import ModularModelGenerator

            generator = ModularModelGenerator(src)
            generator.generate(tgt)
        except Exception as e:
            return ConversionReport(
                success=False,
                source_format="spec",
                target_format="modular",
                source_path=src,
                target_path=tgt,
                warnings=[str(e)],
            )

        return ConversionReport(
            success=True,
            source_format="spec",
            target_format="modular",
            source_path=src,
            target_path=tgt,
            warnings=warnings,
        )

    def supported_formats(self) -> tuple[str, str]:
        return ("spec", "modular")
