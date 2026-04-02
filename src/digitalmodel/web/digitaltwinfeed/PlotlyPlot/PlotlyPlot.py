"""Plotly Plot blueprint demonstrating interactive chart rendering.

Provides Flask routes that generate and serve Plotly.js charts
embedded in HTML templates.
"""

import json
import os

import numpy as np
import pandas as pd
import plotly
import plotly.graph_objs as go
from flask import (Blueprint, abort, jsonify, make_response, render_template,
                   request, url_for)
from flask_httpauth import HTTPBasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with,
                           reqparse)

auth = HTTPBasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates')
api = Api(AppBlueprint)


# @auth.get_password
def get_password(username):
    """Retrieve the password for HTTP basic authentication.

    Args:
        username: The username to authenticate.

    Returns:
        The password string if username is valid, None otherwise.
    """
    if username == 'dtf_beta':
        return 'dtf_gamma'
    return None


# @auth.error_handler
def unauthorized():
    """Handle unauthorized access attempts.

    Returns:
        JSON error response with HTTP 403 status code.
    """
    return make_response(jsonify({'error': 'Unauthorized access'}), 403)


def create_plot():
    """Create a sample Plotly bar chart with random data.

    Returns:
        str: JSON string containing Plotly chart data.
    """

    N = 40
    x = np.linspace(0, 1, N)
    y = np.random.randn(N)
    df = pd.DataFrame({'x': x, 'y': y})  # creating a sample dataframe

    data = [
        go.Bar(
            x=df['x'],  # assign x as the dataframe column 'x'
            y=df['y'])
    ]

    graphJSON = json.dumps(data, cls=plotly.utils.PlotlyJSONEncoder)

    return graphJSON


@AppBlueprint.route('/', methods=['GET'])
def index():
    """Render the PlotlyPlot index page with a sample bar chart.

    Returns:
        Rendered HTML template with embedded Plotly chart.
    """

    bar = create_plot()
    url_path = AppName + '/index.html'
    return render_template(url_path, plot=bar)


# class AppIndex(Resource):
#     # decorators = [auth.login_required]
#
#     def get(self):
#         bar = create_plot()
#         return render_template('PlotlyPlot/index.html', plot=bar)
#
#
# api.add_resource(AppIndex, '/', endpoint='AppRoot')
