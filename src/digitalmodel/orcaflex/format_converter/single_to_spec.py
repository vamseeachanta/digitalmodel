"""Convert single monolithic OrcaFlex YAML to spec.yml.

Chain: single -> modular (temp dir) -> spec.
"""

from __future__ import annotations

import tempfile
from pathlib import Path

from .modular_to_spec import ModularToSpecConverter
from .protocols import ConversionReport
from .single_to_modular import SingleToModularConverter


class SingleToSpecConverter:
    """Convert single OrcaFlex YAML to spec.yml (best-effort).

    Chains SingleToModularConverter -> ModularToSpecConverter
    using a temporary directory for the intermediate modular form.
    """

    def __init__(self, source_path: Path | None = None):
        self.source_path = Path(source_path) if source_path else None

    def convert(
        self, source: Path | None = None, target: Path | None = None
    ) -> ConversionReport:
        """Convert single format to spec.

        Args:
            source: Path to single OrcaFlex YAML file.
            target: Path for output spec.yml.
        """
        src = Path(source) if source else self.source_path
        if src is None:
            raise ValueError("No source path provided")

        tgt = Path(target) if target else src.parent / f"{src.stem}_spec.yml"

        all_warnings: list[str] = []

        with tempfile.TemporaryDirectory() as tmp:
            tmp_modular = Path(tmp) / "modular"

            # Step 1: single -> modular
            s2m = SingleToModularConverter(src, tmp_modular)
            report1 = s2m.convert()
            if not report1.success:
                return ConversionReport(
                    success=False,
                    source_format="single",
                    target_format="spec",
                    source_path=src,
                    target_path=tgt,
                    warnings=report1.warnings,
                )
            all_warnings.extend(report1.warnings)

            # Step 2: modular -> spec
            m2spec = ModularToSpecConverter()
            report2 = m2spec.convert(
                source=tmp_modular / "master.yml",
                target=tgt,
            )
            if not report2.success:
                return ConversionReport(
                    success=False,
                    source_format="single",
                    target_format="spec",
                    source_path=src,
                    target_path=tgt,
                    warnings=all_warnings + report2.warnings,
                )
            all_warnings.extend(report2.warnings)

        return ConversionReport(
            success=True,
            source_format="single",
            target_format="spec",
            source_path=src,
            target_path=tgt,
            unmapped_sections=report2.unmapped_sections,
            confidence=report2.confidence,
            warnings=all_warnings,
        )

    def supported_formats(self) -> tuple[str, str]:
        return ("single", "spec")
