"""Example blueprint demonstrating page-based routing with Jinja2 templates.

Provides a simple Flask blueprint that serves HTML templates
based on the URL path segment within the digitaltwinfeed application.
"""

from flask import Blueprint, abort, render_template
from jinja2 import TemplateNotFound

example_blueprint = Blueprint('example_blueprint', __name__, template_folder='templates')

# @example_blueprint.route('/')
# def index():
#     return "This is an example app"


@example_blueprint.route('/', defaults={'page': 'index'})
@example_blueprint.route('/<page>')
def show(page):
    """Render a template page by name, returning 404 if not found.

    Args:
        page: Name of the template page to render. Defaults to 'index'.

    Returns:
        Rendered HTML template for the requested page.

    Raises:
        404: If the requested template is not found.
    """
    try:
        return render_template('example_blueprint/%s.html' % page)
    except TemplateNotFound:
        abort(404)
