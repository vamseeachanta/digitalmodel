from flask import Blueprint, jsonify, render_template
from jinja2 import TemplateNotFound

example_SPA = Blueprint('example_SPA', __name__, template_folder='templates')

food = {
    'fruit': ['apple', 'banana', 'cherry'],
    'vegetables': ['onion', 'cucumber'],
    'meat': ['sausage', 'beef'],
}

context = {}
context['brand'] = 'DigitalTwinFeed Logo'
context['project'] = 'example_SPA'

context['project_title'] = 'Flask SPA'
context['project_href'] = ""
# context['methodology'] = ''
# context['href_methodology'] = 'methodology'

context['sidebar_subitems_alias'] = ['Feature 1', 'Feature 2', 'Feature 3', 'Feature 4']
context['sidebar_subitems_ids'] = [item.replace(' ', "").lower() for item in context['sidebar_subitems_alias']]


@example_SPA.route('/', methods=['GET', 'POST'])
def index_Feature1():
    foodList = [i for i in list(food.keys())]
    context['data'] = {"foodList": foodList}
    return render_template('example_SPA/index.html', context=context)


@example_SPA.route('/get_food/<foodkind>')
def foodkind_Feature1(foodkind):
    if foodkind not in food:
        return jsonify([])
    else:
        return jsonify(food[foodkind])


@example_SPA.route('/get_html_content/<subItemValue>')
def contentForDiv_Feature3(subItemValue):
    return render_template('example_SPA/subitem_page.html', subItemValue=subItemValue)


@example_SPA.route('/get_html_content_menu_based/<menuItemValue>')
def contentForMenuDiv_Feature4(menuItemValue):
    return render_template('example_SPA/menuitem_page.html', menuItemValue=menuItemValue)
