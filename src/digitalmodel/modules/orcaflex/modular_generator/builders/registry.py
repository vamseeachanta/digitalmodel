"""Builder registry for auto-discovery and ordered execution."""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .base import BaseBuilder


class BuilderRegistry:
    """Registry for builder classes with output file and execution order.

    Builders self-register using the @register decorator:

        @BuilderRegistry.register("03_environment.yml", order=30)
        class EnvironmentBuilder(BaseBuilder):
            ...

    The orchestrator uses get_ordered_builders() to iterate in
    dependency order without maintaining a hardcoded list.
    """

    _registry: dict[str, tuple[type["BaseBuilder"], int]] = {}

    @classmethod
    def register(cls, output_file: str, order: int):
        """Decorator to register a builder class.

        Args:
            output_file: The YAML file this builder generates (e.g., "03_environment.yml")
            order: Execution order (lower runs first). Use multiples of 10
                   for easy insertion of future builders.

        Returns:
            Decorator function.
        """

        def decorator(builder_class: type["BaseBuilder"]) -> type["BaseBuilder"]:
            cls._registry[output_file] = (builder_class, order)
            builder_class._output_file = output_file
            builder_class._order = order
            return builder_class

        return decorator

    @classmethod
    def get_ordered_builders(cls) -> list[tuple[str, type["BaseBuilder"]]]:
        """Get builders in execution order.

        Returns:
            List of (output_file, builder_class) tuples sorted by order.
        """
        sorted_items = sorted(cls._registry.items(), key=lambda x: x[1][1])
        return [(file_name, builder_class) for file_name, (builder_class, _) in sorted_items]

    @classmethod
    def get_include_order(cls) -> list[str]:
        """Get the ordered list of include file names.

        Returns:
            List of output file names in execution order.
        """
        return [file_name for file_name, _ in cls.get_ordered_builders()]

    @classmethod
    def get_builder(cls, output_file: str) -> type["BaseBuilder"] | None:
        """Get a specific builder by its output file.

        Args:
            output_file: The YAML file name.

        Returns:
            Builder class or None if not registered.
        """
        entry = cls._registry.get(output_file)
        return entry[0] if entry else None

    @classmethod
    def clear(cls) -> None:
        """Clear the registry (for testing)."""
        cls._registry.clear()
