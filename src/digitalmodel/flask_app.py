import logging

from flask import Flask, redirect, render_template, request, url_for
from flask_wtf import FlaskForm
from wtforms import SelectField, StringField, SubmitField
from wtforms.validators import DataRequired

# Flask app object
app = Flask(__name__)
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = True
app.secret_key = b'm\xb7\x12\x97\xb4\x98\x88~\xf4o'


def register_blueprint(mod_name, is_service=True):
    import sys

    from flask import Blueprint
    if mod_name not in sys.modules:
        EXTENSIONS_DIR = 'services'
        loaded_mod = __import__(EXTENSIONS_DIR + "." + mod_name + "." +
                                mod_name,
                                fromlist=[mod_name])
        for obj in vars(loaded_mod).values():
            if isinstance(obj, Blueprint):
                if is_service:
                    app.register_blueprint(
                        obj, url_prefix='/services/{0}'.format(mod_name))
                else:
                    app.register_blueprint(obj)


blueprint_list = ['dtf']
for blueprint_item in blueprint_list:
    register_blueprint(blueprint_item, is_service=False)

blueprint_list = ['example_blueprint', 'example_SPA']
for blueprint_item in blueprint_list:
    print("Registering blue print : {}".format(blueprint_item))
    register_blueprint(blueprint_item, is_service=True)


# Blog resides in main app. Could not transfer to services folder as unable to set configuration properties
class MyForm(FlaskForm):
    name = StringField('name', validators=[DataRequired()])


if __name__ == '__main__':
    app.run(debug=True)
else:
    gunicorn_logger = logging.getLogger('gunicorn.error')
    app.logger.handlers = gunicorn_logger.handlers
    app.logger.setLevel(gunicorn_logger.level)
