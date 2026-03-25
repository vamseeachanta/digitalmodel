"""OrcaFlex Three-Way Format Converter.

Bidirectional conversion between:
- spec.yml: High-level engineering specification (Pydantic-validated)
- modular: master.yml + includes/*.yml (section-per-file)
- single: One monolithic OrcaFlex .yml file
"""

from .protocols import ConversionReport, FormatConverter
from .format_detector import FormatType, detect_format
from .single_to_modular import SingleToModularConverter
from .modular_to_single import ModularToSingleConverter
from .spec_to_modular import SpecToModularConverter
from .spec_to_single import SpecToSingleConverter
from .modular_to_spec import ModularToSpecConverter
from .single_to_spec import SingleToSpecConverter

__all__ = [
    "ConversionReport",
    "FormatConverter",
    "FormatType",
    "detect_format",
    "SingleToModularConverter",
    "ModularToSingleConverter",
    "SpecToModularConverter",
    "SpecToSingleConverter",
    "ModularToSpecConverter",
    "SingleToSpecConverter",
]
