from flask import Blueprint, abort, render_template
from jinja2 import TemplateNotFound

example_blueprint = Blueprint('example_blueprint', __name__, template_folder='templates')

# @example_blueprint.route('/')
# def index():
#     return "This is an example app"


@example_blueprint.route('/', defaults={'page': 'index'})
@example_blueprint.route('/<page>')
def show(page):
    try:
        return render_template('example_blueprint/%s.html' % page)
    except TemplateNotFound:
        abort(404)
