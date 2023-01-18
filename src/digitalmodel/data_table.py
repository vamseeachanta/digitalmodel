from dash import dash_table


def get_dash_table_from_df(df, table_settings, table_idx):
    df_dash_table = dash_table.DataTable(
        id=table_settings[table_idx]['id'],
        data=df.to_dict('records'),
        columns=[{
            'id': c,
            'name': c
        } for c in df.columns],
        tooltip_data=[{
            column: {
                'value': str(value),
                'type': 'markdown'
            } for column, value in row.items()
        } for row in df.to_dict('records')],

    # Overflow into ellipsis
        style_cell={
            'overflow': 'hidden',
            'textOverflow': 'ellipsis',
            'maxWidth': 0,
        },
        tooltip_delay=0,
        tooltip_duration=None)

    return df_dash_table
