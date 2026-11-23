"""Deterministic SVG renderers for subsea cross-section report fixtures."""

from __future__ import annotations

from html import escape
import math

from digitalmodel.subsea.cross_sections.schema import CrossSectionDefinition, PackedComponent, RadialLayer

PALETTE = [
    "#4c78a8",
    "#f58518",
    "#54a24b",
    "#e45756",
    "#72b7b2",
    "#b279a2",
    "#ff9da6",
    "#9d755d",
    "#bab0ac",
]


def render_cross_section_svg(definition: CrossSectionDefinition) -> str:
    """Render a deterministic, self-contained SVG for a fixture.

    Radial-layer fixtures are drawn as concentric circles using the landed fixture
    layer order. Packed-component fixtures are drawn as a deterministic schematic;
    the layout is intentionally not an optimization or manufacturing packing claim.
    """

    if definition.radial_layers:
        return _render_radial_svg(definition)
    return _render_packed_svg(definition)


def _render_radial_svg(definition: CrossSectionDefinition) -> str:
    layers = definition.radial_layers
    max_outer = max((layer.derived_outer_diameter.value for layer in layers if layer.derived_outer_diameter), default=1.0)
    center = 120
    scale = 96 / max_outer
    circles: list[str] = []
    labels: list[str] = []
    for index, layer in reversed(list(enumerate(layers))):
        outer = layer.derived_outer_diameter
        radius = max(2.0, (outer.value if outer else max_outer) * scale / 2)
        color = PALETTE[index % len(PALETTE)]
        circles.append(
            f'<circle cx="{center}" cy="{center}" r="{radius:.2f}" fill="{color}" '
            f'stroke="#243447" stroke-width="1"><title>{escape(layer.name)}</title></circle>'
        )
    for index, layer in enumerate(layers):
        y = 20 + index * 14
        labels.append(
            f'<text x="250" y="{y}" font-size="10"><tspan font-weight="700">{index + 1}.</tspan> '
            f'{escape(layer.name)} ({escape(layer.material)})</text>'
        )
    return _svg(definition.id, "radial layer annulus schematic", circles + labels)


def _render_packed_svg(definition: CrossSectionDefinition) -> str:
    components = sorted(
        definition.packed_components,
        key=lambda item: (item.component_type, item.service_role, item.name),
    )
    center = (120, 120)
    radius = 62
    shapes = [
        '<circle cx="120" cy="120" r="92" fill="#f7f7f7" stroke="#243447" stroke-width="2">'
        '<title>not-to-scale schematic bundle envelope</title></circle>',
        '<text x="32" y="224" font-size="10" font-weight="700">not-to-scale schematic</text>',
    ]
    count = max(1, len(components))
    for index, component in enumerate(components):
        angle = (2 * math.pi * index) / count - math.pi / 2
        x = center[0] + radius * math.cos(angle)
        y = center[1] + radius * math.sin(angle)
        size = _component_radius(component)
        color = PALETTE[index % len(PALETTE)]
        shapes.append(
            f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{size:.2f}" fill="{color}" '
            f'stroke="#243447" stroke-width="1"><title>{escape(component.name)}; count={component.count}</title></circle>'
        )
    for index, component in enumerate(components):
        y = 20 + index * 14
        shapes.append(
            f'<text x="250" y="{y}" font-size="10"><tspan font-weight="700">{component.count}x</tspan> '
            f'{escape(component.name)} ({escape(component.service_role)})</text>'
        )
    return _svg(definition.id, "not-to-scale schematic packed-component bundle", shapes)


def _component_radius(component: PackedComponent) -> float:
    if component.diameter is None:
        return 11.0
    return max(7.0, min(20.0, component.diameter.value / 4))


def _svg(fixture_id: str, description: str, body: list[str]) -> str:
    return (
        f'<svg id="visual-{escape(fixture_id)}" role="img" aria-label="{escape(description)}" '
        'xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 240">'
        '<rect width="520" height="240" fill="white"/>'
        + "".join(body)
        + "</svg>"
    )
