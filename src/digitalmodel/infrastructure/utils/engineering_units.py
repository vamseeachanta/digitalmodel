# ABOUTME: Re-exports unit-tracking utilities from assetutilities.units.
# ABOUTME: Provides backward-compatible use_unit function via the singleton registry.

"""Engineering units bridge module.

Re-exports the shared pint UnitRegistry, TrackedQuantity, and unit_checked
decorator from assetutilities.units so that digitalmodel code can import
from a single local path.
"""

from assetutilities.units import (
    CalculationAuditLog,
    ProvenanceEntry,
    TrackedQuantity,
    get_registry,
    unit_checked,
)

__all__ = [
    "get_registry",
    "TrackedQuantity",
    "ProvenanceEntry",
    "CalculationAuditLog",
    "unit_checked",
    "use_unit",
]


def use_unit(unit: str):
    """Decorator that attaches a pint unit to a function's return value.

    Uses the singleton registry from assetutilities.units instead of
    creating a new UnitRegistry on every call.
    """
    import functools

    ureg = get_registry()

    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            value = func(*args, **kwargs)
            return value * ureg(unit)

        return wrapper

    return decorator
