"""Spike: render a single force-arrow on a Plotly polar axis.

Technique chosen: two Scatterpolar traces — one for the shaft (line), one for
the arrowhead (triangle-up marker with rotation). Keeps everything in native
polar coords; no Cartesian conversion needed.

Spike scope (per digitalmodel#616 plan §Risks > Plotly arrow rendering):
- Renders one arrow at θ=90° (starboard beam in our convention), r-extent 0→0.5,
  in vessel-fixed +Y direction.
- Confirms arrowhead rotation behaves correctly on a polar projection.
- Validates `direction='clockwise', rotation=90` keeps bow-up.
"""
import plotly.graph_objects as go

fig = go.Figure()

# Arrow shaft — line from origin to (r=0.5, theta=90)
fig.add_trace(go.Scatterpolar(
    r=[0.0, 0.5], theta=[90.0, 90.0],
    mode="lines",
    line=dict(color="#1f77b4", width=3),
    name="force vector (shaft)",
    showlegend=False,
))

# Arrow head — triangle-up at the tip, rotated to point outward
fig.add_trace(go.Scatterpolar(
    r=[0.5], theta=[90.0],
    mode="markers",
    marker=dict(symbol="triangle-up", size=18, color="#1f77b4", angle=90),
    name="force vector (head)",
    showlegend=False,
))

fig.update_layout(
    title="Spike: Plotly polar arrow at θ=90° (starboard beam), pointing +Y body-fixed",
    polar=dict(
        angularaxis=dict(direction="clockwise", rotation=90,
                         tickvals=[0, 90, 180, 270],
                         ticktext=["0° bow", "90° stbd", "180° stern", "270° port"]),
        radialaxis=dict(range=[0, 1.0], gridcolor="#e0e0e0"),
    ),
    width=600, height=600, paper_bgcolor="white",
)

out = __file__.replace("prototype.py", "rendered.html")
fig.write_html(out, include_plotlyjs="cdn", full_html=True)
print(f"wrote {out}")
