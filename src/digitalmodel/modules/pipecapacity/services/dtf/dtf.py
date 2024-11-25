from flask import Blueprint, render_template
from flask_wtf import FlaskForm
from wtforms import StringField
from wtforms.validators import DataRequired

dtf = Blueprint('dtf', __name__, template_folder='templates')

context = {}
context['brand'] = 'Logo'
context['title'] = 'Company Name'


class MyForm(FlaskForm):
    name = StringField('name', validators=[DataRequired()])


@dtf.route('/', methods=['GET'])
@dtf.route('/index', methods=['GET'])
def index():
    form = MyForm()
    return render_template('dtf/index.html', context=context, form=form)


@dtf.route('/insights', methods=['GET'])
def insights():
    form = MyForm()
    return render_template('dtf/insights.html', context=context, form=form)


@dtf.route('/services', methods=['GET'])
def services():
    form = MyForm()
    return render_template('dtf/services.html', context=context, form=form)


@dtf.route('/capabilities', methods=['GET'])
def capabilities():
    form = MyForm()
    return render_template('dtf/capabilities.html', context=context, form=form)


@dtf.route('/contact', methods=['GET'])
def contact():
    form = MyForm()
    return render_template('dtf/contact.html', context=context, form=form)
