# TODO Parametrize dropdown choice as part of URL
"""App Menu Dropdown blueprint with form-based tab navigation.

Provides a Flask blueprint demonstrating dropdown-based menu selection
with dynamic content rendering based on user selections.
"""

lask import (Blueprint, abort, jsonify, make_response, render_template,
                   request, url_for)
from flask_httpauth import HTTPBasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with,
                           reqparse)
from flask_wtf import FlaskForm
from wtforms import SelectField, SubmitField

auth = HTTPBasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates')
api = Api(AppBlueprint)

menu_items = [
    {
        'id': 'color',
        'href': 'color',
        'alias': 'Color Data',
    },
    {
        'id': 'property',
        'href': 'property',
        'alias': 'Property Data',
    },
    {
        'id': 'related',
        'href': 'related',
        'alias': 'Related Data',
    },
    {
        'id': 'references',
        'href': 'references',
        'alias': 'References',
    },
]
default_menu_item_id = menu_items[0]['id']
context = {}
form_sumbit_variable = 'dropDownSelection'


def set_up_app():
    """Initialize the application context with default configuration values."""
    context['AppName'] = AppName
    context['AppNameAlias'] = 'Tab Design using dropdown'
    context['title'] = 'Tab Design using dropdown'
    context['choices'] = ['', 'tomatoes', 'oranges', 'bananas']
    context['data'] = None
    context['menu_items'] = None
    context['form_submitted_value'] = None


set_up_app()


class dropDownSelectionForm(FlaskForm):
    """Form with a dropdown selection field that auto-submits on change.

    Attributes:
        dropDownSelection: SelectField for choosing from available options.
    """
    dropDownSelection = SelectField('Select a choice:',
                                    choices=context['choices'],
                                    render_kw={
                                        "onchange": "this.form.submit()",
                                        "class": "browser-default custom-select",
                                        "value": context['choices'][0]
                                    })


@AppBlueprint.route('/', methods=['GET', 'POST'])
def index():
    """Render the dropdown menu index page.

    Returns:
        Rendered HTML template with dropdown form and context data.
    """
    set_up_app()
    form = dropDownSelectionForm()
    context['form'] = form

    if form.is_submitted():
        performFormSubmissionActions()

    url_path = AppName + '/index.html'
    return render_template(url_path, form=context['form'], context=context)


@AppBlueprint.route('/<selectedMenuID>', methods=['GET', 'POST'])
def app_menuitem(selectedMenuID):
    """Render a specific menu item page.

    Args:
        selectedMenuID: The menu item identifier to render.

    Returns:
        Rendered HTML template for the selected menu item.
    """
    url_path = context['AppName'] + '/' + selectedMenuID + '.html'

    if request.method == 'GET':
        if context['data'] is not None:
            return render_template(url_path, form=context['form'], context=context)
        elif context['form_submitted_value'] is not None:
            pass
    elif request.method == 'POST':
        performFormSubmissionActions()
        return render_template(url_path, form=context['form'], context=context)


def performFormSubmissionActions():
    """Process form submission by updating context data and menu items."""
    form_submitted_value = request.form.get(form_sumbit_variable)
    if form_submitted_value is not None:
        if form_submitted_value != context['form_submitted_value']:
            context['form_submitted_value'] = form_submitted_value
            assignContextData()
            context['menu_items'] = menu_items


def assignContextData():
    """Assign result data to the application context after form submission."""
    context['data'] = {'result': "dropdown_choice is chosen now"}
