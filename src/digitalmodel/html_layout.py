import pandas as pd

from dash import html, dcc
from dashhtmlgrid.plt_children import plt_settings_generic
from dashhtmlgrid.dropdown import get_dropdown_div
from dashhtmlgrid.data_table import get_dash_table_from_df


def get_html_layout(layout_settings_custom):

    grid_array = layout_settings_custom.get('grid_array', None)
    text_area_settings = layout_settings_custom.get('text_area', None)
    dropdown_settings = layout_settings_custom.get('dropdown', None)
    table_settings = layout_settings_custom.get('table', None)
    figure_settings = layout_settings_custom.get('figure', None)

    html_children = []
    text_area_idx = 0
    dropdown_idx = 0
    figure_idx = 0
    table_idx = 0
    for grid_element in grid_array:
        if grid_element == 'text_area':
            dash_text_area_children = get_text_area_with_submit_html_div(
                text_area_settings, text_area_idx)
            text_area_idx = text_area_idx + 1
            html_children = html_children + dash_text_area_children
        if grid_element == 'dropdown':
            html_child = get_dropdown_html_div(dropdown_settings, dropdown_idx)
            dropdown_idx = dropdown_idx + 1
            html_children.append(html_child)
        elif grid_element == 'figure':
            html_child = get_figure_html_div(figure_settings, figure_idx)
            figure_idx = figure_idx + 1
            html_children.append(html_child)
        elif grid_element == 'table':
            html_child = get_table_html_div(table_settings, table_idx)
            table_idx = table_idx + 1
            html_children.append(html_child)

    html_layout = html.Div(
        children=[html.Div(className='row', children=html_children)])

    return html_layout


def get_text_area_with_submit_html_div(text_area_settings, text_area_idx):

    dash_text_area = dcc.Textarea(
        id=text_area_settings[text_area_idx]['id'],
        value=text_area_settings[text_area_idx]['value'],
        style={
            'width': '20%',
            'height': 20
        },
    )
    dash_submit = html.Button(text_area_settings[text_area_idx]['submit_text'],
                              id=text_area_settings[text_area_idx]['button_id'],
                              n_clicks=0)

    dash_text_area_children = [dash_text_area, dash_submit]

    return dash_text_area_children


def get_dropdown_html_div(dropdown_settings, dropdown_idx):

    dropdown_div = get_dropdown_div(dropdown_settings)

    dropdown_children = [
        html.H2(dropdown_settings[dropdown_idx]["H2"]),
        html.P(dropdown_settings[dropdown_idx]["p1"]),
        html.P(dropdown_settings[dropdown_idx]["p2"]),
        html.Div(dropdown_div)
    ]

    dropdown_html_div = html.Div(className='four columns div-user-controls',
                                 children=dropdown_children)

    return dropdown_html_div


def get_table_html_div(table_settings, table_idx):
    df = pd.DataFrame()
    table_html_div = html.Div([
        html.H4(table_settings[table_idx]["H2"]),
        html.P(table_settings[table_idx]["p1"]),
        html.P(id=table_settings[table_idx]['html_id']),
        get_dash_table_from_df(df, table_settings, table_idx)
    ])

    return table_html_div


def get_figure_html_div(figure_settings, figure_idx):

    plt_children_html = [
        dcc.Graph(id=figure_settings[figure_idx]['id'],
                  config={'displayModeBar': False},
                  animate=True),
    ]
    figure_html_div = html.Div(className=plt_settings_generic['html_className'],
                               children=plt_children_html)

    return figure_html_div


def get_plt_children_html(figure_settings_custom):
    plt_children_html = [
        dcc.Graph(id='figure_1', config={'displayModeBar': False},
                  animate=True),
        dcc.Graph(id='figure_2', config={'displayModeBar': False},
                  animate=True),
    ]

    return plt_children_html


def get_html_layout_data_table_call_back(drop_down_div, layout_settings_custom):

    div_text = layout_settings_custom['dropdown'][0]
    table_div_text = layout_settings_custom['table'][0]

    html_layout = html.Div(children=[
        html.Div(className='row',
                 children=[
                     html.Div(className='four columns div-user-controls',
                              children=[
                                  html.H2(div_text["H2"]),
                                  html.P(div_text["p1"]),
                                  html.P(div_text["p2"]),
                                  html.Div(drop_down_div)
                              ]),
                     html.Div([
                         html.H4(table_div_text["H2"]),
                         html.P(table_div_text["p1"]),
                         html.P(id='table_1_html'),
                         get_dash_table_from_df(df=pd.DataFrame(),
                                                table_data_id='table_1_data')
                     ]),
                 ])
    ])

    return html_layout
