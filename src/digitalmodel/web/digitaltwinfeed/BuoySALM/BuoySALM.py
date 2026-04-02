"""Buoy SALM (Single Anchor Leg Mooring) blueprint with data and OrcaFlex endpoints.

Provides RESTful API endpoints for SALM buoy data retrieval
and OrcaFlex model rendering within the digitaltwinfeed application.
"""

import os

from flask import (Blueprint, Response, abort, jsonify, make_response,
                   render_template, request, url_for)
from flask_httpauth import HTTPBasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with,
                           reqparse)

auth = HTTPBasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates', static_folder='static')
api = Api(AppBlueprint)


@auth.get_password
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


@auth.error_handler
def unauthorized():
    """Handle unauthorized access attempts.

    Returns:
        JSON error response with HTTP 403 status code.
    """
    return make_response(jsonify({'error': 'Unauthorized access'}), 403)


class AppIndex(Resource):
    """Root API resource that returns YAML configuration data.

    Reads default configuration from a YAML file and returns it
    with an additional OrcaFlex service URL.
    """
    # decorators = [auth.login_required]

    def get(self):
        """Retrieve YAML configuration data for the BuoySALM service.

        Returns:
            dict: Configuration data loaded from YAML with OrcaFlex URL added.
        """
        import os

        from common.data import ReadDataFromSystemFiles
        read_data = ReadDataFromSystemFiles()
        filename = 'default.yml'
        file_url = os.path.join('services', AppName, 'static', AppName, 'data', filename)

        data = {'error': 'Unauthorized access'}
        if 'yml' in filename:
            data = read_data.get_data_from_yaml(file_url)
            data['OrcaFlex'] = 'services/{0}/OrcaFlex'.format(AppName)
        return data


class OrcaFlex(Resource):
    """OrcaFlex model resource for rendering mooring analysis models.

    Reads OrcaFlex configuration from YAML and renders it as an
    HTML template.
    """
    # decorators = [auth.login_required]

    def get(self):
        """Retrieve and render the OrcaFlex model configuration.

        Returns:
            Response: HTML-rendered OrcaFlex model template.
        """
        import os

        import yaml

        from common.data import ReadDataFromSystemFiles
        read_data = ReadDataFromSystemFiles()
        filename = 'default.yml'
        file_url = os.path.join('services', AppName, 'static', AppName, 'data', filename)

        data = {'error': 'Unauthorized access'}
        if 'yml' in filename:
            data = read_data.get_data_from_yaml(file_url)
            if data is not None:
                data = data.get('OrcaFlex', None)

        yaml = yaml.dump(data, default_flow_style=False)
        headers = {'Content-Type': 'text/html'}
        return make_response(render_template('OrcaflexModel.html', yaml=yaml), 200, headers)


api.add_resource(AppIndex, '/', endpoint='AppRoot')
api.add_resource(OrcaFlex, '/OrcaFlex', endpoint='OrcaFlex')
