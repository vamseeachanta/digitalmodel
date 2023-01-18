from dash import Dash, dcc, html
from dash.dependencies import Input, Output, State

app = Dash(__name__)

app.layout = html.Div([
    dcc.Textarea(
        id='textarea-state-example',
        value='AAPL',
        style={
            'width': '20%',
            'height': 20
        },
    ),
    html.Button('Submit', id='textarea-state-example-button', n_clicks=0),
    html.Div(id='textarea-state-example-output',
             style={'whiteSpace': 'pre-line'})
])


@app.callback(Output('textarea-state-example-output', 'children'),
              Input('textarea-state-example-button', 'n_clicks'),
              State('textarea-state-example', 'value'))
def update_output(n_clicks, value):
    if n_clicks > 0:
        return 'You have entered: \n{}'.format(value)


if __name__ == '__main__':
    app.run_server(debug=True)