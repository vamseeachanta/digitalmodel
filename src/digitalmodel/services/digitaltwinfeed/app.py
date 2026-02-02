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
        loaded_mod = __import__(EXTENSIONS_DIR + "." + mod_name + "." + mod_name, fromlist=[mod_name])
        for obj in vars(loaded_mod).values():
            if isinstance(obj, Blueprint):
                if is_service:
                    app.register_blueprint(obj, url_prefix='/services/{0}'.format(mod_name))
                else:
                    app.register_blueprint(obj)


blueprint_list = ['dtf']
for blueprint_item in blueprint_list:
    register_blueprint(blueprint_item, is_service=False)

blueprint_list = [
    'example_blueprint', 'todorestful', 'bseedata', 'example_SPA', 'BuoySALM', 'BuoyCALM', 'BuoyCoupled',
    'FixedWindTurbine', 'FloatingWindTurbine', 'OffshorePipeline', 'OffshoreRiser', 'Floaters', 'Metocean',
    'OnshorePipeline', 'FFS', 'Pipings', 'PressureVessels', 'Foundations', 'Fatigue', 'StockAnalysis',
    'PortfolioAnalysis', 'PlotlyPlot', 'AppMenuDropdown', 'AppMenuTextboxSubmit', 'GoMFields'
]
for blueprint_item in blueprint_list:
    print("Registering blue print : {}".format(blueprint_item))
    register_blueprint(blueprint_item, is_service=True)


# Blog resides in main app. Could not transfer to services folder as unable to set configuration properties
class MyForm(FlaskForm):
    name = StringField('name', validators=[DataRequired()])


from flask_flatpages import FlatPages

FLATPAGES_AUTO_RELOAD = True
FLATPAGES_EXTENSION = '.md'
FLATPAGES_ROOT = 'blogs'
app.config.from_object(__name__)
blogs = FlatPages(app)


@app.route('/blog/<path:path>.html')
def page(path):
    context = {}
    context['brand'] = 'DigitalTwinFeed Logo'
    context['title'] = 'DTF | Blog'

    print("Page function running")
    blog = blogs.get_or_404(path)
    return render_template('blog_article.html', blog_article=blog, context=context)


@app.route('/blog', methods=['GET', 'POST'])
def blog():
    context = {}
    context['brand'] = 'DigitalTwinFeed Logo'
    context['title'] = 'DTF | Blog'
    form = MyForm()

    return render_template('blog.html', context=context, blogs=blogs, form=form)


if __name__ == '__main__':
    app.run(debug=True)
else:
    gunicorn_logger = logging.getLogger('gunicorn.error')
    app.logger.handlers = gunicorn_logger.handlers
    app.logger.setLevel(gunicorn_logger.level)
