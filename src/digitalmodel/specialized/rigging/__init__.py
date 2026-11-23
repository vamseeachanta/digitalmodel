# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rigging calculations with catalogue access loaded only on request."""

__all__ = ["Rigging"]


def __getattr__(name: str) -> type:
    """Resolve the existing catalogue class without loading it for statics.

    Args:
        name: Requested package attribute.

    Returns:
        The existing Rigging class when explicitly requested.

    Raises:
        AttributeError: If the requested attribute is not exported.
    """
    if name == "Rigging":
        from .rigging import Rigging

        return Rigging
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
