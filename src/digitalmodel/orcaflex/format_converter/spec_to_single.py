"""Convert spec.yml to single monolithic OrcaFlex YAML.

Chain: spec -> modular (temp dir) -> single.
"""

from __future__ import annotations

import tempfile
from pathlib import Path

from .modular_to_single import ModularToSingleConverter
from .protocols import ConversionReport
from .spec_to_modular import SpecToModularConverter


class SpecToSingleConverter:
    """Convert spec.yml directly to a single OrcaFlex YAML file.

    Chains SpecToModularConverter -> ModularToSingleConverter
    using a temporary directory for the intermediate modular form.
    """

    def __init__(self, spec_path: Path | None = None):
        self.spec_path = Path(spec_path) if spec_path else None

    def convert(
        self, source: Path | None = None, target: Path | None = None
    ) -> ConversionReport:
        """Convert spec to single format.

        Args:
            source: Path to spec.yml.
            target: Path for output single YAML file.
        """
        src = Path(source) if source else self.spec_path
        if src is None:
            raise ValueError("No source path provided")

        tgt = Path(target) if target else src.parent / f"{src.stem}_model.yml"

        all_warnings: list[str] = []

        with tempfile.TemporaryDirectory() as tmp:
            tmp_modular = Path(tmp) / "modular"

            # Step 1: spec -> modular
            s2m = SpecToModularConverter()
            report1 = s2m.convert(source=src, target=tmp_modular)
            if not report1.success:
                return ConversionReport(
                    success=False,
                    source_format="spec",
                    target_format="single",
                    source_path=src,
                    target_path=tgt,
                    warnings=report1.warnings,
                )
            all_warnings.extend(report1.warnings)

            # Step 2: modular -> single
            m2s = ModularToSingleConverter()
            report2 = m2s.convert(
                source=tmp_modular / "master.yml",
                target=tgt,
            )
            if not report2.success:
                return ConversionReport(
                    success=False,
                    source_format="spec",
                    target_format="single",
                    source_path=src,
                    target_path=tgt,
                    warnings=all_warnings + report2.warnings,
                )
            all_warnings.extend(report2.warnings)

        return ConversionReport(
            success=True,
            source_format="spec",
            target_format="single",
            source_path=src,
            target_path=tgt,
            warnings=all_warnings,
        )

    def supported_formats(self) -> tuple[str, str]:
        return ("spec", "single")
