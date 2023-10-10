""" 
VS Code notes: Run file. "Deug Run file" gives error. 
An example of single selection dropdown menu
"""
import dash
import pandas as pd
import plotly.express as px

import dash_html_components as html
import dash_core_components as dcc
from dash.dependencies import Input, Output

df = pd.read_csv(
    r'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
)

df_state_lvl = df.groupby('Province_State', as_index=False).sum()

df_melt = df_state_lvl.melt(
    id_vars=['Province_State'],
    value_vars=df_state_lvl.columns[(df_state_lvl.columns.str[-2:] == '21') |
                                    (df_state_lvl.columns.str[-2:] == '20')])

ops = df_melt['Province_State'].unique()
labels = [{'label': i, 'value': i} for i in ops]

fig = px.line(df_melt.loc[df_melt['Province_State'] == 'California'],
              x='variable',
              y='value')

app = dash.Dash()
app.layout = html.Div([
    html.Div('Hello world from dash updated.'),
    html.H1('H1 tag here'),
    html.Div(dcc.Dropdown(id='dropdown', options=labels)),
    dcc.Graph(id='fig1', figure=fig)
])


@app.callback(Output('fig1', 'figure'), Input('dropdown', 'value'))
def update_graph(state):
    df_state = df_melt.loc[df_melt['Province_State'] == state]
    fig = px.line(df_state,
                  x='variable',
                  y='value',
                  title=f'{state} cumulative case counts')
    return fig


app.run_server(debug=True)
