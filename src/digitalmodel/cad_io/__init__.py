# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Optional, headless inspection of DWG, DXF and STEP files.

Drawing dependencies and geometry kernels are loaded inside operations so that
importing this package does not require a CAD installation.
"""

from .drawings import (
    dwg_to_dxf,
    dxf_to_json,
    dxf_to_svg,
    read_dwg_version,
    roundtrip_check,
)
from .solids import step_checks

__all__ = [
    "read_dwg_version", "dwg_to_dxf", "dxf_to_json", "dxf_to_svg",
    "roundtrip_check", "step_checks",
]
