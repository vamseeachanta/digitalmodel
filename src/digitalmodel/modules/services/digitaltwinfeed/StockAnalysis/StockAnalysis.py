import os

from flask import (Blueprint, abort, current_app, flash, jsonify, make_response, redirect, render_template, request,
                   url_for)
from flask_httpauth import HTTPBasicAuth
from flask_restful import (Api, Resource, fields, marshal, marshal_with, reqparse)
from flask_wtf import FlaskForm
from wtforms import StringField, SubmitField, TextField, validators

from common.data import AttributeDict
from common.finance_components import FinanceComponents
from services.StockAnalysis.StockCharts import StockCharts

fc = FinanceComponents(cfg=None)
stock_charts = StockCharts()

auth = HTTPBasicAuth()

AppName = os.path.basename(__file__).split('.')[0]
AppBlueprint = Blueprint(AppName, __name__, template_folder='templates')
api = Api(AppBlueprint)

context = {}
form_submit_variable = 'textboxInput'

ticker_list_from_db = fc.get_stock_ticker_list_from_db()


def set_up_app(choices=ticker_list_from_db):
    context['AppName'] = AppName
    context['AppNameAlias'] = 'Stock Analysis'
    context['title'] = 'Stock Analysis'
    context['choices'] = choices
    context['data'] = None
    context['menu_items'] = None
    context['form_submitted_value'] = None
    context['cfg'] = {}
    return context


set_up_app()


class textBoxSubmitForm(FlaskForm):
    textboxInput = StringField('Ticker: ', [validators.DataRequired()])
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
        ticker = get_form_submitted_value()
        url_path = url_for(AppName + '.' + 'app_menuitem', ticker=ticker, selectedMenuID='summary', _external=True)
        return redirect(url_path, code=302)


@AppBlueprint.route('/<ticker>', methods=['GET'])
def app_ticker(ticker):
    current_app.logger.debug("function name: {}; http request :{}".format('app_ticker', request.method))

    url_path = url_for(AppName + '.' + 'app_menuitem', ticker=ticker, selectedMenuID='summary', _external=True)
    return redirect(url_path, code=302)


@AppBlueprint.route('/<ticker>/<selectedMenuID>', methods=['GET', 'POST'])
def app_menuitem(ticker, selectedMenuID):
    current_app.logger.debug("function name: {}; http request :{}".format('app_menuitem', request.method))

    if request.method in ['POST']:
        ticker = get_form_submitted_value()
        url_path = url_for(AppName + '.' + 'app_menuitem',
                           ticker=ticker,
                           selectedMenuID=selectedMenuID,
                           _external=True)
        return redirect(url_path, code=302)

    context_cfg_ticker = context['cfg'].get('ticker', None)
    if context_cfg_ticker != ticker:
        assign_stock_data_and_update_menu_items(ticker)

    form = textBoxSubmitForm(request.form)
    template_path = AppName + '/' + selectedMenuID + '.html'
    return render_template(template_path, form=form, context=context)


def get_form_submitted_value():
    form_submitted_value = request.form.get(form_submit_variable)
    current_app.logger.debug("Form Submitted ticker :{}".format(form_submitted_value))

    if form_submitted_value is not None:
        if form_submitted_value != context['form_submitted_value']:
            context['form_submitted_value'] = form_submitted_value

    return form_submitted_value


def assign_stock_data_and_update_menu_items(ticker=None, test_flag=False):
    if ticker in ticker_list_from_db:
        context['data'] = {}
        context['title'] = ticker
        context['error'] = True
        if ticker is None:
            ticker = context['form_submitted_value']
        try:
            data_dict = fc.get_data_for_UI(ticker=ticker)
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Data from database for :{ticker} ... SUCCESS".format(
                    AppName, ticker))
        except:
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Data from database for :{ticker} ... FAIL".format(
                    AppName, ticker))

        try:
            context['cfg'] = fc.get_stock_analysis_UI_cfg(data_dict)
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Data assignment for :{ticker} ... SUCCESS".format(
                    AppName, ticker))
        except:
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Data assignment for :{ticker} ... FAIL".format(
                    AppName, ticker))

        try:
            plot_cfg = fc.get_stock_analysis_plot_cfg(data_dict)
            context['plot_data'] = stock_charts.get_plot_data(plot_cfg)
            context['error'] = False
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Plot data assignment for :{ticker} ... SUCCESS".format(
                    AppName, ticker))
        except:
            if not test_flag:
                current_app.logger.debug(f"{AppName} UI, Plot data assignment for :{ticker} ... FAIL".format(
                    AppName, ticker))

        if not test_flag:
            context['menu_items'] = get_menu_items(ticker)
    else:
        if not test_flag:
            current_app.logger.error(f"{AppName} UI, No ticker ticker in database :{ticker} ... FAIL".format(
                AppName, ticker))


def get_menu_items(ticker):
    current_app.logger.debug("Update menu urls for ticker :{}".format(ticker))
    menu_items = [
        {
            'id': 'summary',
            'href': "{}/summary",
            'alias': 'Summary',
        },
        {
            'id': 'insider',
            'href': '{}/insider',
            'alias': 'Insider Info',
        },
        {
            'id': 'institution',
            'href': '{}/institution',
            'alias': 'Institution Info',
        },
        # {
        #     'id': 'technical',
        #     'href': '{}/technical',
        #     'alias': 'Technical Analysis',
        # },
        # {
        #     'id': 'options',
        #     'href': '{}/options',
        #     'alias': 'Option Analysis',
        # },
        # {
        #     'id': 'strategies',
        #     'href': '{}/strategies',
        #     'alias': 'Strategies',
        # },
        {
            'id': 'updates',
            'href': '{}/updates',
            'alias': 'Updates',
        },
    ]
    for menu_item in menu_items:
        menu_item['href'] = url_for(AppName + '.' + 'app_menuitem',
                                    ticker=ticker,
                                    selectedMenuID=menu_item['id'],
                                    _external=True)
    return menu_items
