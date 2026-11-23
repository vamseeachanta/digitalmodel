"""DTF main site blueprint providing core navigation pages for digitaltwinfeed.

Defines Flask routes for the Digital Twin Feed website pages
including index, insights, services, capabilities, and contact.
"""

from flask import Blueprint, render_template
from flask_wtf import FlaskForm
from wtforms import StringField
from wtforms.validators import DataRequired

dtf = Blueprint('dtf', __name__, template_folder='templates')

context = {}
context['brand'] = 'DigitalTwinFeed Logo'
context['title'] = 'Digital Twin Feed'


class MyForm(FlaskForm):
    """Simple form with a required name field for page interactions.

    Attributes:
        name: A required string field for user input.
    """
    name = StringField('name', validators=[DataRequired()])


@dtf.route('/', methods=['GET'])
@dtf.route('/index', methods=['GET'])
def index():
    """Render the DTF index (home) page.

    Returns:
        Rendered HTML template for the DTF index page.
    """
    form = MyForm()
    return render_template('dtf/index.html', context=context, form=form)


@dtf.route('/insights', methods=['GET'])
def insights():
    """Render the DTF insights page.

    Returns:
        Rendered HTML template for the insights page.
    """
    form = MyForm()
    return render_template('dtf/insights.html', context=context, form=form)


@dtf.route('/services', methods=['GET'])
def services():
    """Render the DTF services page.

    Returns:
        Rendered HTML template for the services page.
    """
    form = MyForm()
    return render_template('dtf/services.html', context=context, form=form)


@dtf.route('/capabilities', methods=['GET'])
def capabilities():
    """Render the DTF capabilities page.

    Returns:
        Rendered HTML template for the capabilities page.
    """
    form = MyForm()
    return render_template('dtf/capabilities.html', context=context, form=form)


@dtf.route('/contact', methods=['GET'])
def contact():
    """Render the DTF contact page.

    Returns:
        Rendered HTML template for the contact page.
    """
    form = MyForm()
    return render_template('dtf/contact.html', context=context, form=form)
