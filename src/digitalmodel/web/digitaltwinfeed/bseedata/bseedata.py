# TODO Parametrize dropdown choice as part of URL
"""BSEE Data blueprint for Bureau of Safety and Environmental Enforcement data.

Provides a Flask blueprint with dropdown-based field selection for
browsing and downloading BSEE offshore field data.
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
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates', static_folder='static')
api = Api(AppBlueprint)

menu_items = [
    {
        'id': 'fieldData',
        'href': 'fieldData',
        'alias': 'Field Data',
    },
    {
        'id': 'productionData',
        'href': 'productionData',
        'alias': 'Production',
    },
    {
        'id': 'wellData',
        'href': 'wellData',
        'alias': 'Well Data',
    },
    {
        'id': 'reservesData',
        'href': 'reservesData',
        'alias': 'Reserves',
    },
    {
        'id': 'methodology',
        'href': 'methodology',
        'alias': 'Methodology',
    },
]
default_menu_item_id = menu_items[0]['id']
context = {}
form_sumbit_variable = 'dropDownSelection'


def assignFieldList():
    """Load and assign the list of available BSEE fields from data files."""
    from common.data import ReadDataFromSystemFiles
    read_data = ReadDataFromSystemFiles()
    folder_with_file_type = os.path.join(os.getcwd(), 'services', 'bseedata', 'static', 'bseedata', 'data', '*.json')
    FieldFiles = read_data.get_file_list_from_folder(folder_with_file_type=folder_with_file_type,
                                                     with_path=False,
                                                     with_extension=False)
    context['fields'] = FieldFiles
    context['choices'] = [''] + FieldFiles


assignFieldList()


def set_up_app():
    """Initialize the application context with default BSEE data configuration."""
    context['AppName'] = AppName
    context['AppNameAlias'] = 'BSEE Data'
    context['title'] = 'BSEE Data'
    context['dropDownSelectionLabel'] = 'Select Field:'
    context['choices'] = [''] + context['fields']
    context['data'] = None
    context['menu_items'] = None
    context['form_submitted_value'] = None


set_up_app()


class dropDownSelectionForm(FlaskForm):
    """Form with a dropdown for selecting BSEE fields and a download button.

    Attributes:
        dropDownSelection: SelectField for choosing from available BSEE fields.
        downloadFieldData: Submit button to download field data as CSV.
    """
    dropDownSelection = SelectField(context['dropDownSelectionLabel'],
                                    choices=context['choices'],
                                    render_kw={
                                        "onchange": "this.form.submit()",
                                        "class": "browser-default custom-select",
                                        "value": context['choices'][0]
                                    })
    downloadFieldData = SubmitField(label='Download as CSV', render_kw={"onclick": ""})


@AppBlueprint.route('/', methods=['GET', 'POST'])
def index():
    """Render the BSEE data index page with field selection dropdown.

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
    """Render a specific BSEE data menu item page.

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
    """Load selected field data from JSON files into the application context."""
    from common.data import ReadDataFromSystemFiles
    read_data = ReadDataFromSystemFiles()
    context['FieldName'] = context['form_submitted_value']
    project = context['FieldName']
    filename = os.path.join(os.getcwd(), 'services', context['AppName'], 'static', context['AppName'], 'data',
                            context['FieldName'] + '.json')
    context['data'] = read_data.get_data_from_json(filename)
