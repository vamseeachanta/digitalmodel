"""Typed context for cross-builder entity sharing."""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass
class BuilderContext:
    """Typed context replacing Dict[str, Any] for cross-builder sharing.

    Each builder registers entities into this context for downstream
    builders to consume. Typed fields catch typos at registration time
    and provide IDE support.

    Example:
        ctx = BuilderContext()
        ctx.coating_names = ["coating+CWC120", "coating"]
        # Later builder reads:
        names = ctx.coating_names  # IDE autocomplete works
    """

    # From VarDataBuilder
    coating_names: list[str] = field(default_factory=list)

    # From LineTypeBuilder
    line_type_names: list[str] = field(default_factory=list)

    # From SupportsBuilder
    support_type_names: list[str] = field(default_factory=list)

    # From MorisonBuilder
    morison_type_names: list[str] = field(default_factory=list)

    # From ShapesBuilder
    shape_names: list[str] = field(default_factory=list)

    # From BuoysBuilder
    buoy_names_6d: list[str] = field(default_factory=list)
    buoy_names_3d: list[str] = field(default_factory=list)
    all_buoy_names: list[str] = field(default_factory=list)
    end_buoy_name: str = "6D buoy1"
    bm_buoy_name: str = "BM"

    # From LinesBuilder
    line_names: list[str] = field(default_factory=list)
    main_pipeline_name: str = ""
    additional_line_names: list[str] = field(default_factory=list)

    # From VesselTypeBuilder
    vessel_type_names: list[str] = field(default_factory=list)

    # From VesselBuilder
    vessel_names: list[str] = field(default_factory=list)
    main_vessel_name: str = ""

    # From WinchBuilder
    winch_names: list[str] = field(default_factory=list)

    # From GroupsBuilder
    group_names: list[str] = field(default_factory=list)

    def to_dict(self) -> dict:
        """Convert to dict for backward compatibility with old builder code.

        Returns:
            Dictionary with all non-default context values.
        """
        result = {}
        defaults = BuilderContext()
        for attr in vars(self):
            if not attr.startswith("_"):
                val = getattr(self, attr)
                default_val = getattr(defaults, attr)
                if val != default_val:
                    result[attr] = val
        return result

    def update_from_dict(self, d: dict) -> None:
        """Update context from a dictionary (backward compat).

        Args:
            d: Dictionary with entity keys and values.
        """
        for key, value in d.items():
            if hasattr(self, key):
                setattr(self, key, value)
