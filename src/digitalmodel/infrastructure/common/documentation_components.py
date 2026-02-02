class PDFReports:

    def __init__(self, wkhtmltopdf_path):
        self.wkhtmltopdf_path = wkhtmltopdf_path

    def create_pdf_from_html_string(self, outputText, filename):
        import pdfkit
        config = pdfkit.configuration(wkhtmltopdf=self.wkhtmltopdf_path)
        # options = {
        #     'page-size': 'A4',
        #     'margin-top': '0.75in',
        #     'margin-right': '0.75in',
        #     'margin-bottom': '0.75in',
        #     'margin-left': '0.75in',
        # }
        options = None
        pdfkit.from_string(outputText, filename, configuration=config, options=options)

    def simple_example_101(self, outputText='MicroPyramid', filename='results/example.pdf'):
        import pdfkit
        config = pdfkit.configuration(wkhtmltopdf=self.wkhtmltopdf_path)
        options = {
            'page-size': 'A4',
            'margin-top': '0.75in',
            'margin-right': '0.75in',
            'margin-bottom': '0.75in',
            'margin-left': '0.75in',
        }
        pdfkit.from_string(outputText, filename, configuration=config, options=options)


class JinjaLib:

    def output_from_template_file(self, template_filename, vars, searchpath="./"):
        import jinja2

        templateLoader = jinja2.FileSystemLoader(searchpath=searchpath)
        templateEnv = jinja2.Environment(loader=templateLoader)
        TEMPLATE_FILE = template_filename
        template = templateEnv.get_template(TEMPLATE_FILE)
        outputText = template.render(vars=vars)
        return outputText

    def simple_example1(self):
        from jinja2 import Template
        t = Template("Hello {{ something }}!")
        t.render(something="World")
        print(t.render(something="World"))

        t = Template("My favorite numbers: {% for n in range(1,10) %}{{n}} " "{% endfor %}")
        t.render()
        print(t.render())

    def output_from_template_example1(self,
                                      template_filename='data_manager/jinja2/example.jinja',
                                      vars={
                                          'name': 'Mark',
                                          'my_string': "Wheeeee!",
                                          'my_list': [0, 1, 2, 3, 4, 5]
                                      },
                                      searchpath="./"):
        import jinja2

        templateLoader = jinja2.FileSystemLoader(searchpath=searchpath)
        templateEnv = jinja2.Environment(loader=templateLoader)
        TEMPLATE_FILE = template_filename
        template = templateEnv.get_template(TEMPLATE_FILE)
        outputText = template.render(vars=vars)
        print(outputText)


if __name__ == '__main__':
    # Jinja unit tests (works in pycharm and not on command prompt)
    import os
    from pathlib import Path
    jl = JinjaLib()
    BASE_DIR = os.path.dirname(__file__)
    print(BASE_DIR)

    try:
        parent_path = str(Path(BASE_DIR).parents[0])
    except Exception as e:
        print("Error: {}".format(e))
        print("Enter physical path for running")
        parent_path = str(Path('C:/Users/achantv/Documents/Utilities/aceengineer'))

    jl.output_from_template_example1(searchpath=parent_path)
    # pdfkit unit tests
    pdf_rep = PDFReports(wkhtmltopdf_path='exe\wkhtmltopdf.exe')
    filename = os.path.join(parent_path, 'results/example.pdf')
    pdf_rep.simple_example_101(filename=filename)
