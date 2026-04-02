"""Basic TODO API blueprint with HTTP authentication and CRUD operations.

Provides a simple RESTful-style TODO list API using Flask blueprint
routing with HTTP basic authentication for all task endpoints.
"""

from flask import Blueprint, abort, jsonify, make_response, request, url_for
from flask_httpauth import HTTPBasicAuth

auth = HTTPBasicAuth()

todobasic = Blueprint('todobasic', __name__, template_folder='templates')


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


@todobasic.route('/tasks', methods=['GET'])
@auth.login_required
def get_tasks():
    """Retrieve all tasks as a JSON list.

    Returns:
        JSON response containing all tasks with public URIs.
    """
    return jsonify({'tasks': [make_public_task(task) for task in tasks]})


@todobasic.route('/tasks/<int:task_id>', methods=['GET'])
@auth.login_required
def get_task(task_id):
    """Retrieve a single task by its ID.

    Args:
        task_id: Integer ID of the task to retrieve.

    Returns:
        JSON response containing the requested task.

    Raises:
        404: If no task with the given ID exists.
    """
    task = [task for task in tasks if task['id'] == task_id]
    if len(task) == 0:
        abort(404)
    return jsonify({'task': make_public_task(task[0])})


@todobasic.errorhandler(404)
def not_found(error):
    """Handle 404 errors with a JSON response.

    Args:
        error: The error object from Flask's error handler.

    Returns:
        JSON error response with HTTP 404 status code.
    """
    return make_response(jsonify({'error': 'Not found'}), 404)


@todobasic.route('/tasks', methods=['POST'])
@auth.login_required
def create_task():
    """Create a new task from JSON request body.

    Returns:
        JSON response containing the created task with HTTP 201 status.

    Raises:
        400: If request body is missing or lacks a 'title' field.
    """
    if not request.json or not 'title' in request.json:
        abort(400)
    task = {
        'id': tasks[-1]['id'] + 1,
        'title': request.json['title'],
        'description': request.json.get('description', ""),
        'done': False
    }
    tasks.append(task)
    return jsonify({'task': make_public_task(task)}), 201


@todobasic.route('/tasks/<int:task_id>', methods=['PUT'])
@auth.login_required
def update_task(task_id):
    """Update an existing task by its ID.

    Args:
        task_id: Integer ID of the task to update.

    Returns:
        JSON response containing the updated task.

    Raises:
        400: If request body is invalid or contains wrong field types.
        404: If no task with the given ID exists.
    """
    task = [task for task in tasks if task['id'] == task_id]
    if len(task) == 0:
        abort(404)
    if not request.json:
        abort(400)
    if 'title' in request.json and not isinstance(request.json['title'], str):
        abort(400)
    if 'description' in request.json and not isinstance(request.json['description'], str):
        abort(400)
    if 'done' in request.json and type(request.json['done']) is not bool:
        abort(400)
    task[0]['title'] = request.json.get('title', task[0]['title'])
    task[0]['description'] = request.json.get('description', task[0]['description'])
    task[0]['done'] = request.json.get('done', task[0]['done'])
    return jsonify({'task': make_public_task(task[0])})


@todobasic.route('/tasks/<int:task_id>', methods=['DELETE'])
@auth.login_required
def delete_task(task_id):
    """Delete a task by its ID.

    Args:
        task_id: Integer ID of the task to delete.

    Returns:
        JSON response with result set to True on success.

    Raises:
        404: If no task with the given ID exists.
    """
    task = [task for task in tasks if task['id'] == task_id]
    if len(task) == 0:
        abort(404)
    tasks.remove(task[0])
    return jsonify({'result': True})


def make_public_task(task):
    """Convert a task dict to its public representation with URI instead of ID.

    Args:
        task: Dictionary containing task data with an 'id' field.

    Returns:
        dict: New dictionary with 'id' replaced by a public 'uri' field.
    """
    new_task = {}
    for field in task:
        if field == 'id':
            new_task['uri'] = url_for('todobasic.get_task', task_id=task['id'], _external=True)
        else:
            new_task[field] = task[field]
    return new_task
