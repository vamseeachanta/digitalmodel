from dash import dcc, html


def get_dash_text_area_with_submit(text_area_settings, text_area_idx):
    dash_text_area = dcc.Textarea(
        id=text_area_settings[text_area_idx]['id'],
        value=text_area_settings[text_area_idx]['value'],
        style={
            'width': '100%',
            'height': 300
        },
    )
    dash_submit = html.Button(text_area_settings[text_area_idx]['submit_text'],
                              id=text_area_settings[text_area_idx]['button_id'],
                              n_clicks=0),

    dash_text_area_with_submit = [dash_text_area, dash_submit]

    return dash_text_area_with_submit
