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


def _basic_auth_password_for(username):
    """Use TEMPLATE_CODE_BASIC_AUTH_USER and TEMPLATE_CODE_BASIC_AUTH_PASSWORD.

    Missing or blank variables refuse every authentication request.
    """
    # Keep this import local so tests can compile the function on its own.
    import os

    configured = os.environ.get('TEMPLATE_CODE_BASIC_AUTH_USER')
    value = os.environ.get('TEMPLATE_CODE_BASIC_AUTH_PASSWORD')
    if (configured is None or value is None or not configured.strip()
            or not value.strip() or not username):
        return None
    return value if username == configured else None


@auth.get_password
def get_password(username):
    """Retrieve the password for HTTP basic authentication.

    Args:
        username: The username to authenticate.

    Returns:
        The password string if username is valid, None otherwise.
    """
    return _basic_auth_password_for(username)


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
