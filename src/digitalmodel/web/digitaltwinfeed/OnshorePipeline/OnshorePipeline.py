"""Onshore Pipeline RESTful API blueprint for pipeline analysis.

Provides authenticated API endpoints for onshore pipeline data
within the digitaltwinfeed application.
"""

from digitalmodel.web.digitaltwinfeed.basic_auth import basic_auth_verify_password
import os

from flask import Blueprint, abort, jsonify, make_response, request, url_for
from digitalmodel.web.authentication import UTF8BasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with,
                           reqparse)

auth = UTF8BasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates')
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
    """Root API resource for the OnshorePipeline service requiring authentication."""
    decorators = [auth.login_required]

    def get(self):
        """Handle GET requests to the OnshorePipeline root endpoint.

        Returns:
            dict: A dictionary with tasks set to None.
        """
        return {'tasks': None}


api.add_resource(AppIndex, '/', endpoint='AppRoot')
