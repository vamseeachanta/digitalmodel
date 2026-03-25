# TODO Parametrize dropdown choice as part of URL
import os

from flask import (Blueprint, abort, jsonify, make_response, render_template,
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
    set_up_app()
    form = dropDownSelectionForm()
    context['form'] = form

    if form.is_submitted():
        performFormSubmissionActions()

    url_path = AppName + '/index.html'
    return render_template(url_path, form=context['form'], context=context)


@AppBlueprint.route('/<selectedMenuID>', methods=['GET', 'POST'])
def app_menuitem(selectedMenuID):
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
    form_submitted_value = request.form.get(form_sumbit_variable)
    if form_submitted_value is not None:
        if form_submitted_value != context['form_submitted_value']:
            context['form_submitted_value'] = form_submitted_value
            assignContextData()
            context['menu_items'] = menu_items


def assignContextData():
    from common.data import ReadDataFromSystemFiles
    read_data = ReadDataFromSystemFiles()
    context['FieldName'] = context['form_submitted_value']
    project = context['FieldName']
    filename = os.path.join(os.getcwd(), 'services', context['AppName'], 'static', context['AppName'], 'data',
                            context['FieldName'] + '.json')
    context['data'] = read_data.get_data_from_json(filename)
