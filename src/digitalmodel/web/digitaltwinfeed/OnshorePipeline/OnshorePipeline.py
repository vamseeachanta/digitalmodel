"""Onshore Pipeline RESTful API blueprint for pipeline analysis.

Provides authenticated API endpoints for onshore pipeline data
within the digitaltwinfeed application.
"""

import os

from flask import Blueprint, abort, jsonify, make_response, request, url_for
from flask_httpauth import HTTPBasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with,
                           reqparse)

auth = HTTPBasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates')
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
    """Root API resource for the OnshorePipeline service requiring authentication."""
    decorators = [auth.login_required]

    def get(self):
        """Handle GET requests to the OnshorePipeline root endpoint.

        Returns:
            dict: A dictionary with tasks set to None.
        """
        return {'tasks': None}


api.add_resource(AppIndex, '/', endpoint='AppRoot')
