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
    if username == 'dtf_beta':
        return 'dtf_gamma'
    return None


@auth.error_handler
def unauthorized():
    return make_response(jsonify({'error': 'Unauthorized access'}), 403)


class AppIndex(Resource):
    decorators = [auth.login_required]

    def get(self):
        return {'tasks': None}


api.add_resource(AppIndex, '/', endpoint='AppRoot')
