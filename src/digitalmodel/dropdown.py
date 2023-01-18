from dash import html, dcc


def get_full_dropdown_settings(dropdown_settings, dropdown_indx=0):

    start_value = dropdown_settings[dropdown_indx]['start_value']
    start_value = start_value if start_value is not None else dropdown_settings[
        dropdown_indx]['options'][0]['value']

    full_dropdown_settings = {
        'multiple_flag': dropdown_settings[dropdown_indx]['multiple_flag'],
        'html_className': 'div-for-dropdown',
        'id': dropdown_settings[dropdown_indx]['id'],
        'className': dropdown_settings[dropdown_indx]['className'],
        'options': dropdown_settings[dropdown_indx]['options'],
        'start_value': start_value,
        'style': {
            'backgroundColor': '#ffffff'
        },
        'html_style': {
            'color': '#000000'
        }
    }

    return full_dropdown_settings


def get_dropdown_div(dropdown_settings, dropdown_indx=0):

    full_dropdown_settings = get_full_dropdown_settings(dropdown_settings,
                                                        dropdown_indx=0)

    if dropdown_settings[dropdown_indx]['multiple_flag']:
        children = [
            dcc.Dropdown(id=full_dropdown_settings['id'],
                         options=full_dropdown_settings['options'],
                         multi=full_dropdown_settings['multiple_flag'],
                         value=[full_dropdown_settings['start_value']],
                         style=full_dropdown_settings['style'],
                         className=full_dropdown_settings['className']),
        ]
    else:
        children = [
            dcc.Dropdown(
                id=full_dropdown_settings['id'],
                options=full_dropdown_settings['options'],
                className=full_dropdown_settings['className'],
                value=[full_dropdown_settings['start_value']],
            ),
        ]

    drop_down_div = html.Div(className=full_dropdown_settings['html_className'],
                             children=children,
                             style=full_dropdown_settings['html_style'])

    return drop_down_div

