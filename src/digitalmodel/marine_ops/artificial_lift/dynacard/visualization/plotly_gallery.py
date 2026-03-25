# ABOUTME: Optional interactive Plotly HTML gallery for all 18 dynacard failure modes.
# ABOUTME: Generates a self-contained HTML file with 18 subplot cards.

from __future__ import annotations

from pathlib import Path

from ..card_generators import ALL_GENERATORS
from .svg_primitives import MODE_LABELS, MODES_BY_TIER, TIER_INFO


def _check_plotly() -> None:
    """Check that plotly is available."""
    try:
        import plotly  # noqa: F401
    except ImportError:
        raise ImportError(
            "plotly is required for the interactive gallery. "
            "Install with: pip install plotly"
        )


def generate_plotly_gallery(output_path: Path, seed: int = 42) -> Path:
    """Generate an interactive HTML gallery with all 18 failure modes.

    Each card is rendered as a subplot with hover tooltips showing
    position and load values. Cards are organized by tier.

    Args:
        output_path: Path to write the HTML file.
        seed: Random seed for card generation.

    Returns:
        Path to the generated HTML file.
    """
    _check_plotly()

    from plotly.subplots import make_subplots
    import plotly.graph_objects as go

    # Collect all modes in tier order
    ordered_modes: list[str] = []
    for tier_num in [1, 2, 3]:
        ordered_modes.extend(MODES_BY_TIER[tier_num])

    n_modes = len(ordered_modes)
    cols = 3
    rows = (n_modes + cols - 1) // cols

    # Create subplot titles
    subplot_titles = [MODE_LABELS.get(m, m) for m in ordered_modes]

    fig = make_subplots(
        rows=rows,
        cols=cols,
        subplot_titles=subplot_titles,
        horizontal_spacing=0.06,
        vertical_spacing=0.06,
    )

    tier_colors = {
        1: TIER_INFO[1]["color"],
        2: TIER_INFO[2]["color"],
        3: TIER_INFO[3]["color"],
    }

    # Track which tier each mode belongs to for coloring
    mode_tier_map = {}
    for tier_num, modes in MODES_BY_TIER.items():
        for m in modes:
            mode_tier_map[m] = tier_num

    for i, mode_name in enumerate(ordered_modes):
        row = i // cols + 1
        col = i % cols + 1
        tier = mode_tier_map[mode_name]
        color = tier_colors[tier]

        gen = ALL_GENERATORS[mode_name]
        card = gen(seed=seed)

        fig.add_trace(
            go.Scatter(
                x=card.position,
                y=card.load,
                mode="lines",
                line={"color": color, "width": 2},
                fill="toself",
                fillcolor=f"rgba({int(color[1:3], 16)},{int(color[3:5], 16)},{int(color[5:7], 16)},0.1)",
                name=MODE_LABELS.get(mode_name, mode_name),
                showlegend=False,
                hovertemplate="Position: %{x:.1f} in<br>Load: %{y:.0f} lbs<extra></extra>",
            ),
            row=row,
            col=col,
        )

        fig.update_xaxes(title_text="Position (in)", row=row, col=col, title_font_size=9)
        fig.update_yaxes(title_text="Load (lbs)", row=row, col=col, title_font_size=9)

    fig.update_layout(
        title={
            "text": "DynaCard AI — 18 Failure Mode Interactive Gallery",
            "x": 0.5,
            "font": {"size": 20},
        },
        height=rows * 250,
        width=1100,
        template="plotly_white",
    )

    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.write_html(str(output_path), include_plotlyjs="cdn")
    return output_path


__all__ = [
    "generate_plotly_gallery",
]
