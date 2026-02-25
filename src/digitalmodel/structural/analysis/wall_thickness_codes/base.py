# ABOUTME: CodeStrategy Protocol and CODE_REGISTRY for design code strategy pattern
# ABOUTME: New codes register via @register_code decorator, auto-discovered on import

from __future__ import annotations

import logging
from typing import Dict, List, Protocol, Tuple, runtime_checkable

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)

logger = logging.getLogger(__name__)


@runtime_checkable
class CodeStrategy(Protocol):
    """Protocol for a design code check strategy."""

    code_name: str
    check_names: List[str]

    def run_checks(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Dict[str, Tuple[float, Dict[str, float]]]:
        """Run all checks for this code. Returns {check_name: (utilisation, details)}."""
        ...

    def compute_plastic_moment(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        """Compute plastic moment capacity M_p in N*m."""
        ...

    def compute_plastic_tension(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        """Compute plastic axial capacity S_p in N."""
        ...


CODE_REGISTRY: Dict[DesignCode, type] = {}


def register_code(code_enum: DesignCode):
    """Class decorator to register a code strategy in the global registry."""
    def decorator(cls):
        CODE_REGISTRY[code_enum] = cls
        logger.debug("Registered code strategy %s for %s", cls.__name__, code_enum.value)
        return cls
    return decorator
