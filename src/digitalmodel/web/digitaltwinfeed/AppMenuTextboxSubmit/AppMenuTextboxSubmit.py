import os

from flask import (Blueprint, abort, jsonify, make_response, render_template,
                   request, url_for)
from flask_httpauth import HTTPBasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with,
                           reqparse)
from flask_wtf import FlaskForm
from wtforms import StringField, SubmitField, TextField, validators

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
form_sumbit_variable = 'textboxInput'


def set_up_app():
    context['AppName'] = AppName
    context['AppNameAlias'] = 'Tab Design using textbox'
    context['title'] = 'Tab Design using textbox'
    context['choices'] = ['', 'tomatoes', 'oranges', 'bananas']
    context['data'] = None
    context['menu_items'] = None
    context['form_submitted_value'] = None


set_up_app()


class textBoxSubmitForm(FlaskForm):
    textboxInput = StringField('Input: ', [validators.DataRequired()])
    submit = SubmitField('Submit')


@AppBlueprint.route('/', methods=['GET', 'POST'])
def index():
    set_up_app()
    form = textBoxSubmitForm(request.form)
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
    context['data'] = {'result': "form submit value is provided"}
