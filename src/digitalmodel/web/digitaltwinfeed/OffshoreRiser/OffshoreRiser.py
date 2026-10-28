"""OffshoreRiser blueprint with data and OrcaFlex endpoints.

Provides RESTful API endpoints for OffshoreRiser data retrieval
and OrcaFlex model rendering within the digitaltwinfeed application.
"""

from digitalmodel.web.digitaltwinfeed.basic_auth import basic_auth_verify_password
import os

from flask import (Blueprint, Response, abort, jsonify, make_response,
                   render_template, request, url_for)
from digitalmodel.web.authentication import UTF8BasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with,
                           reqparse)

auth = UTF8BasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates', static_folder='static')
api = Api(AppBlueprint)


@auth.verify_password
def verify_password(username, password):
    """Verify live environment credentials using UTF-8 byte comparisons."""
    return basic_auth_verify_password(username, password)


@auth.error_handler
def unauthorized():
    """Handle unauthorized access attempts.

    Returns:
        JSON error response with HTTP 403 status code.
    """
    return make_response(jsonify({'error': 'Unauthorized access'}), 403)


class AppIndex(Resource):
    """Root API resource that returns YAML configuration data for OffshoreRiser."""
    # decorators = [auth.login_required]

    def get(self):
        """Retrieve YAML configuration data for the OffshoreRiser service.

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
    """OrcaFlex model resource for rendering OffshoreRiser analysis models."""
    decorators = [auth.login_required]

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
