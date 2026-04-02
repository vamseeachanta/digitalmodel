"""Template code RESTful API blueprint with HTTP basic authentication.

Provides a minimal Flask-RESTful API blueprint with authentication
that can be used as a starting template for new service endpoints.
"""

import os

from flask import Blueprint, jsonify, make_response
from flask_httpauth import HTTPBasicAuth
from flask_restful import Api, Resource

auth = HTTPBasicAuth()

AppName = os.path.basename(__file__).split(".")[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder="templates")
api = Api(AppBlueprint)


@auth.get_password
def get_password(username):
    """Retrieve the password for HTTP basic authentication.

    Args:
        username: The username to authenticate.

    Returns:
        The password string if username is valid, None otherwise.
    """
    if username == "dtf_beta":
        return "dtf_gamma"
    return None


@auth.error_handler
def unauthorized():
    """Handle unauthorized access attempts.

    Returns:
        JSON error response with HTTP 403 status code.
    """
    return make_response(jsonify({"error": "Unauthorized access"}), 403)


class AppIndex(Resource):
    """Root API resource requiring authentication.

    Returns a placeholder response for the application root endpoint.
    """
    decorators = [auth.login_required]

    def get(self):
        """Handle GET requests to the application root.

        Returns:
            dict: A dictionary with tasks set to None.
        """
        return {"tasks": None}


api.add_resource(AppIndex, "/", endpoint="AppRoot")
