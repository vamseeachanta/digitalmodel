import plotly.express as px
import plotly.graph_objects as go
import pandas as pd

def generate_interactive_heatmap(df, x_label, y_label, z_label, title="Engineering Analysis Heatmap"):
    """
    Reimplementation of legacy Bokeh heatmap using Plotly.
    Optimized for engineering exception mapping and metal loss visualization.
    """
    # Custom color palette from legacy implementation
    colors = ["#FAFAFA", "#D7CCC8", "#FFAB91", "#FFCC80", "#E6EE9C", "#80DEEA", "#CE93D8", "#E57373", "#0000CD"]
    
    # Pivot the data if it's in long format (common in legacy FFS logs)
    if not df.index.name == y_label:
        try:
            plot_df = df.pivot(index=y_label, columns=x_label, values=z_label)
        except Exception:
            # Fallback if pivot fails
            plot_df = df
    else:
        plot_df = df

    fig = go.Figure(data=go.Heatmap(
        z=plot_df.values,
        x=plot_df.columns,
        y=plot_df.index,
        colorscale=colors,
        hoverongaps=False,
        hovertemplate=f"{x_label}: %{{x}}<br>{y_label}: %{{y}}<br>{z_label}: %{{z}}<extra></extra>"
    ))

    fig.update_layout(
        title=title,
        xaxis_title=x_label,
        yaxis_title=y_label,
        template="plotly_white",
        xaxis=dict(tickangle=45)
    )

    return fig

def generate_exception_matrix(df, api14, title="Well Exception Matrix"):
    """
    Specialized matrix visualization for wellbore data quality and exceptions.
    """
    fig = px.imshow(df, 
                    labels=dict(x="Dimension", y="Timestamp", color="Exception Level"),
                    color_continuous_scale="RdBu_r",
                    title=f"{title} - API: {api14}")
    
    fig.update_xaxes(side="top")
    return fig
