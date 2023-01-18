import plotly.graph_objects as go

plt_settings_generic = {
    'html_className':
        'eight columns div-for-charts',
    'color_list': [[
        "#5E0DAC", '#FF4F00', '#375CB1', '#FF7400', '#FFF400', '#FF0056'
    ], ["#5E0DAC", '#FF4F00', '#375CB1', '#FF7400', '#FFF400', '#FF0056']]
}


def get_plt_trace(cfg):
    df = cfg['data_source']
    plt_trace = go.Scatter(
        x=df[df[cfg['filter_column']] == cfg['filter_value']][cfg['x']],
        y=df[df[cfg['filter_column']] == cfg['filter_value']][cfg['y']],
        mode='lines',
        opacity=0.7,
        name=cfg['filter_value'],
        textposition='bottom center')

    return plt_trace


def get_plt_go_layout(figure_settings, figure_idx):
    plt_go_layout = []

    title = {'text': 'Daily Change', 'font': {'color': 'black'}, 'x': 0.5}
    title.update(figure_settings[figure_idx]['title'])

    plt_go_layout = go.Layout(
        colorway=plt_settings_generic['color_list'][0],
        template='plotly_white',
        paper_bgcolor='rgba(0, 0, 0, 0)',
        plot_bgcolor='rgba(0, 0, 0, 0)',
        margin={'t': 50},
        height=250,
        hovermode='x',
        autosize=True,
        title=title,
        xaxis={
            'showticklabels': True,
        },
    )

    return plt_go_layout


def get_figure(df, selected_dropdown_value, figure_settings, figure_idx):

    trace = []
    # Draw and append traces for each selected value
    for stock in selected_dropdown_value:
        trace_cfg = figure_settings[figure_idx]['trace'].copy()
        trace_cfg.update({'data_source': df, 'filter_value': stock})
        trace_value = get_plt_trace(trace_cfg)
        trace.append(trace_value)

    traces = [trace]
    data = [val for sublist in traces for val in sublist]

    # Define Figure
    plt_go_layout = get_plt_go_layout(figure_settings, figure_idx)

    figure = {
        'data': data,
        'layout': plt_go_layout,
    }

    return figure
