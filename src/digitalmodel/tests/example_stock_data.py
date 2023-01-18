import pandas as pd

data_source = {
    'type':
        'csv',
    'filename':
        'https://raw.githubusercontent.com/vamseeachanta/dashhtmlgrid/master/src/dashhtmlgrid/tests/test_data/stockdata.csv'
}
data_definitions = {'x': 'Date', 'y': 'value'}
data_source.update({'data_definitions': data_definitions})


def get_input_data():

    if data_source['type'] == 'csv':
        df = pd.read_csv(data_source['filename'], index_col=0, parse_dates=True)
        df.index = pd.to_datetime(df['Date'])
        data_source['drop_down_options'] = get_drop_down_options(
            df['stock'].unique())
        data_source['df'] = df
    else:
        raise ("Data source is undefined. Custom data reading is required")

    return data_source


def get_stock_data_df():
    if data_source['type'] == 'csv':
        try:
            df = pd.read_csv(data_source['filename'],
                             index_col=0,
                             parse_dates=True)
            df.index = pd.to_datetime(df['Date'])
        except:
            df = pd.DataFrame()
    else:
        raise ("Data source is undefined. Custom data reading is required")

    return df


def get_drop_down_options(list_stocks):
    dict_list = []
    for i in list_stocks:
        dict_list.append({'label': i, 'value': i})

    return dict_list
