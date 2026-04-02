"""RESTful TODO API blueprint using Flask-RESTful with CRUD task resources.

Provides a Flask-RESTful API with authentication for managing
a TODO task list including create, read, update, and delete operations.
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


tasks = [{
    'id': 1,
    'title': u'Buy groceries',
    'description': u'Milk, Cheese, Pizza, Fruit, Tylenol',
    'done': False
}, {
    'id': 2,
    'title': u'Learn Python',
    'description': u'Need to find a good Python tutorial on the web',
    'done': False
}]

task_fields = {
    'title': fields.String,
    'description': fields.String,
    'done': fields.Boolean,
    'uri': fields.Url(AppName + '.task')
}


class AppIndex(Resource):
    """Root API resource requiring authentication."""
    decorators = [auth.login_required]

    def get(self):
        """Handle GET requests to the application root.

        Returns:
            dict: A dictionary with tasks set to None.
        """
        return {'tasks': None}


class TaskListAPI(Resource):
    """API resource for listing and creating tasks.

    Supports GET to list all tasks and POST to create new tasks.
    Requires HTTP basic authentication for all operations.
    """
    decorators = [auth.login_required]

    def __init__(self):
        self.reqparse = reqparse.RequestParser()
        self.reqparse.add_argument('title', type=str, required=True, help='No task title provided', location='json')
        self.reqparse.add_argument('description', type=str, default="", location='json')
        super(TaskListAPI, self).__init__()

    # @api.marshal_with(task_fields, as_list=True)
    def get(self):
        """Retrieve all tasks as a marshalled list.

        Returns:
            dict: Dictionary containing a list of all tasks.
        """
        # return {'tasks': [task for task in tasks]}
        # return {'tasks': [make_public_task(task) for task in tasks]}
        return {'tasks': [marshal(task, task_fields) for task in tasks]}

    def post(self):
        """Create a new task from the JSON request body.

        Returns:
            tuple: Dictionary with the created task and HTTP 201 status.

        Raises:
            400: If request body is missing or lacks a 'title' field.
        """
        if not request.json or not 'title' in request.json:
            abort(400)
        task = {
            'id': tasks[-1]['id'] + 1,
            'title': request.json['title'],
            'description': request.json.get('description', ""),
            'done': request.json.get('done', False)
        }
        tasks.append(task)
        return {'task': marshal(task, task_fields)}, 201


class TaskAPI(Resource):
    """API resource for individual task operations.

    Supports GET, PUT, and DELETE for a single task identified by ID.
    Requires HTTP basic authentication for all operations.
    """
    decorators = [auth.login_required]

    def __init__(self):
        self.reqparse = reqparse.RequestParser()
        self.reqparse.add_argument('title', type=str, location='json')
        self.reqparse.add_argument('description', type=str, location='json')
        self.reqparse.add_argument('done', type=bool, location='json')
        super(TaskAPI, self).__init__()

    def get(self, id):
        """Retrieve a single task by its ID.

        Args:
            id: Integer ID of the task to retrieve.

        Returns:
            dict: Dictionary containing the marshalled task data.

        Raises:
            404: If no task with the given ID exists.
        """
        task = [task for task in tasks if task['id'] == id]
        if len(task) == 0:
            abort(404)
        return {'task': marshal(task[0], task_fields)}

    def put(self, id):
        """Update an existing task by its ID.

        Args:
            id: Integer ID of the task to update.

        Returns:
            dict: Dictionary containing the updated marshalled task.

        Raises:
            404: If no task with the given ID exists.
        """
        task = list(filter(lambda t: t['id'] == id, tasks))
        if len(task) == 0:
            abort(404)
        task = task[0]
        args = self.reqparse.parse_args()
        for k, v in args.items():
            if v != None:
                task[k] = v
        return {'task': marshal(task, task_fields)}

    def delete(self, id):
        """Delete a task by its ID.

        Args:
            id: Integer ID of the task to delete.

        Returns:
            dict: Dictionary with result set to True on success.

        Raises:
            404: If no task with the given ID exists.
        """
        task = [task for task in tasks if task['id'] == id]
        if len(task) == 0:
            abort(404)
        tasks.remove(task[0])
        return {'result': True}


api.add_resource(AppIndex, '/', endpoint='AppRoot')
api.add_resource(TaskListAPI, '/tasks', endpoint='tasks')
api.add_resource(TaskAPI, '/tasks/<int:id>', endpoint='task')
