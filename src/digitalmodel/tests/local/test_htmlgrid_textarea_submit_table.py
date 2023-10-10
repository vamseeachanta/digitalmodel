from dash import Dash
from dash.dependencies import Input, Output, State
from dash.exceptions import PreventUpdate

from dashhtmlgrid.plt_children import get_figure
from dashhtmlgrid.html_layout import get_html_layout
from dashhtmlgrid.data_table import get_dash_table_from_df

from dashhtmlgrid.tests.example_stock_data import get_input_data, get_stock_data_df, get_drop_down_options
from dashhtmlgrid.tests.example_data_table import get_example_data_table_df

# Dashboard custom settings
grid_array = ['text_area', 'table']

figure_settings = []

stock_df = get_stock_data_df()
text_area_settings = [{
    'id': 'textarea-state',
    'value': 'AAPL',
    'submit_text': 'Submit',
    'button_id': 'textarea-state-button'
}]

table_settings = [{
    "html_id": 'table_1_html',
    "H2": 'Example Table',
    'id': 'table_1_data',
    "p1": 'Additional description for the table',
}]

layout_settings_custom = {
    'grid_array': grid_array,
    'text_area': text_area_settings,
    'table': table_settings,
    'figure': figure_settings,
}

# Initialize the app
app = Dash(__name__)
app.config.suppress_callback_exceptions = False

app.layout = get_html_layout(layout_settings_custom)


# Callback for Table #1 (i.e. Example table)
@app.callback(Output(table_settings[0]['html_id'], 'children'),
              Input(text_area_settings[0]['button_id'], 'n_clicks'),
              State(text_area_settings[0]['id'], 'value'))
def update_table_0(n_clicks, selected_dropdown_value):

    if n_clicks is not None and n_clicks > 0:
        table_1_df = get_example_data_table_df()
        table = get_dash_table_from_df(table_1_df, table_settings, table_idx=0)

        return table
    else:
        raise PreventUpdate


if __name__ == '__main__':
    app.run_server(debug=True)
