import pandas as pd

from dashhtmlgrid.common.visualization import Visualization


def test_basic_visualization():
    viz = Visualization()

    df = pd.DataFrame(
        {
            'num_legs': [2, 4, 8, 0],
            'num_wings': [2, 0, 0, 0],
            'num_specimen_seen': [10, 2, 1, 8]
        },
        index=['falcon', 'dog', 'spider', 'fish'])

    cfg_plot = {
        'data_source': df,
        'type': "scatter",
        'mode': "lines",
        'name': ['legs', 'wings', 'samples'],
        'x': ['index'],
        'y': ['num_legs', 'num_wings', 'num_specimen_seen'],
        'line': {
            'color': None
        }
    }

    plotly_data = viz.get_plotly_data(cfg_plot)
    print(plotly_data)
    print(f"Executing 'test_basic_visualization' ... SUCCESS")


def test_grouped_visualization():

    viz = Visualization()

    df = pd.DataFrame({
        'location': [
            'SF Zoo', 'SF Zoo', 'SF Zoo', 'LA Zoo', 'LA Zoo', 'LA Zoo'
        ],
        'animal': [
            'giraffes', 'orangutans', 'monkeys', 'giraffes', 'orangutans',
            'monkeys'
        ],
        'count': [20, 14, 23, 12, 18, 29]
    })

    cfg_plot = {
        'data_source': df,
        'type': "bar",
        'mode': "lines",
        'name': ['legs', 'wings', 'samples'],
        'name_column_legend_groups': ['location'],
        'x': ['animal'],
        'y': ['count'],
        'line': {
            'color': None
        }
    }

    layout = {
        'title': 'Animal Count | Group Plot',
        'xaxis': {
            'tickangle': -45
        },
        'barmode': 'group'
    }
    cfg_plot.update({'layout': layout})

    plotly_data = viz.get_plotly_data(cfg_plot)
    print(plotly_data)
    print(f"Executing 'test_grouped_visualization' ... SUCCESS")


if __name__ == '__main__':
    test_basic_visualization()
    test_grouped_visualization()
