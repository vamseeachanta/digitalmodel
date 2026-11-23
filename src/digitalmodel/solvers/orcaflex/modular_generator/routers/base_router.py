"""Abstract base class for domain routers."""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import Any


class BaseRouter(ABC):
    """Base class for domain-specific routers.

    Routers sit upstream of builders. They transform engineering domain
    specs (mooring, vessel hydrodynamics, etc.) into GenericModel-compatible
    dicts that flow through the existing GenericModelBuilder.
    """

    @abstractmethod
    def route(self, spec: Any) -> dict[str, Any]:
        """Transform a domain spec into a GenericModel-compatible dict.

        The returned dict should have keys matching GenericModel field names:
        line_types, lines, winches, vessels, etc. Each value should be a list
        of dicts with at minimum a 'name' key and a 'properties' dict.

        Args:
            spec: Domain-specific specification object.

        Returns:
            Dictionary compatible with GenericModel(**result).
        """
