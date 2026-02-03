import json
import os
import traceback

import pandas as pd
from flask import (Blueprint, abort, current_app, flash, jsonify, make_response, redirect, render_template, request,
                   url_for)
from flask_httpauth import HTTPBasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with, reqparse)
from flask_wtf import FlaskForm
from wtforms import StringField, SubmitField, TextField, validators

from services.GoMFields.gom_fields_components import GoMFieldsComponents
from services.GoMFields.GoMFieldsCharts import GoMFieldsCharts

gom_fc = GoMFieldsComponents(cfg=None)
gom_charts = GoMFieldsCharts()

auth = HTTPBasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates')
api = Api(AppBlueprint)

context = {}
form_submit_variable = 'textboxInput'

app_choice_list_from_db = gom_fc.get_gom_field_list_from_db()


def set_up_app(choices=app_choice_list_from_db):
    context['AppName'] = AppName
    context['AppNameAlias'] = 'GoM Blocks'
    context['title'] = 'GoM Blocks'
    context['choices'] = choices
    context['data'] = None
    context['menu_items'] = None
    context['form_submitted_value'] = None
    context['cfg'] = {}
    return context


set_up_app()


class textBoxSubmitForm(FlaskForm):
    textboxInput = StringField('GoM Block: ', [validators.DataRequired()])
    submit = SubmitField('Submit')


@AppBlueprint.route('/', methods=['GET', 'POST'])
def index():
    current_app.logger.debug("function name: {}; http request :{}".format('index', request.method))

    if request.method in ['GET']:
        set_up_app()
        form = textBoxSubmitForm(request.form)
        template_path = AppName + '/' + 'index.html'
        return render_template(template_path, form=form, context=context)

    if request.method in ['POST']:
        user_selected_choice = get_form_submitted_value()
        url_path = url_for(AppName + '.' + 'app_menuitem',
                           user_selected_choice=user_selected_choice,
                           selectedMenuID='summary',
                           _external=True)
        return redirect(url_path, code=302)


@AppBlueprint.route('/<user_selected_choice>', methods=['GET'])
def app_user_selected_choice(user_selected_choice):
    current_app.logger.debug("function name: {}; http request :{}".format('app_ticker', request.method))

    url_path = url_for(AppName + '.' + 'app_menuitem',
                       user_selected_choice=user_selected_choice,
                       selectedMenuID='summary',
                       _external=True)
    return redirect(url_path, code=302)


@AppBlueprint.route('/<user_selected_choice>/<selectedMenuID>', methods=['GET', 'POST'])
def app_menuitem(user_selected_choice, selectedMenuID):
    current_app.logger.debug("function name: {}; http request :{}".format('app_menuitem', request.method))

    if request.method in ['POST']:
        user_selected_choice = get_form_submitted_value()
        url_path = url_for(AppName + '.' + 'app_menuitem',
                           user_selected_choice=user_selected_choice,
                           selectedMenuID=selectedMenuID,
                           _external=True)
        return redirect(url_path, code=302)

    context_cfg_choice = context['cfg'].get('gom_block', None)
    if context_cfg_choice != user_selected_choice:
        assign_data_and_update_menu_items(user_selected_choice)

    form = textBoxSubmitForm(request.form)
    template_path = AppName + '/' + selectedMenuID + '.html'
    return render_template(template_path, form=form, context=context)


def get_form_submitted_value():
    form_submitted_value = request.form.get(form_submit_variable)
    current_app.logger.debug("Form Submitted :{}".format(form_submitted_value))

    if form_submitted_value is not None:
        if form_submitted_value != context['form_submitted_value']:
            context['form_submitted_value'] = form_submitted_value

    return form_submitted_value


def assign_data_and_update_menu_items(user_selected_choice=None, test_flag=False):
    if user_selected_choice in app_choice_list_from_db:
        context['data'] = {}
        context['title'] = user_selected_choice
        context['error'] = True
        if user_selected_choice is None:
            user_selected_choice = context['form_submitted_value']
        try:
            data_df, data_dict = gom_fc.get_data_for_UI(gom_block=user_selected_choice)
            if not test_flag:
                current_app.logger.debug(
                    f"{AppName} UI, Data loading from database for: {user_selected_choice} ... SUCCESS".format(
                        AppName, user_selected_choice))
        except:
            if not test_flag:
                current_app.logger.debug(
                    f"{AppName} UI, Data loading from database for: {user_selected_choice} ... FAIL".format(
                        AppName, user_selected_choice))
        try:
            context['cfg'] = data_dict
            if not test_flag:
                current_app.logger.debug(
                    f"{AppName} UI, Data assignment for: {user_selected_choice} ... SUCCESS".format(
                        AppName, user_selected_choice))
        except:
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Data assignment for: {user_selected_choice} ... FAIL".format(
                    AppName, user_selected_choice))

        try:
            context['plot_data'] = gom_charts.get_plot_data(data_df)
            context['error'] = False
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Plot data for: {user_selected_choice} ... SUCCESS")
        except:
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Plot data for: {user_selected_choice} ... FAIL")

        if not test_flag:
            context['menu_items'] = get_menu_items(user_selected_choice)
    else:
        if not test_flag:
            current_app.logger.error(f"{AppName} UI, No data in database: {user_selected_choice} ... FAILED")


def get_menu_items(user_selected_choice):
    current_app.logger.debug("Update menu urls for :{}".format(user_selected_choice))
    menu_items = [
        {
            'id': 'summary',
            'href': "{}/summary",
            'alias': 'Summary',
        },
        {
            'id': 'production',
            'href': '{}/production',
            'alias': 'Production',
        },
        {
            'id': 'well',
            'href': '{}/well',
            'alias': 'Well',
        },
        # {
        #     'id': 'reserves',
        #     'href': '{}/reserves',
        #     'alias': 'Reserves',
        # },
        {
            'id': 'updates',
            'href': '{}/updates',
            'alias': 'Updates',
        },
    ]
    for menu_item in menu_items:
        menu_item['href'] = url_for(AppName + '.' + 'app_menuitem',
                                    user_selected_choice=user_selected_choice,
                                    selectedMenuID=menu_item['id'],
                                    _external=True)
    return menu_items
