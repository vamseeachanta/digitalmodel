import logging


class FEComponents():
    # https://plot.ly/python/v3/fft-filters/
    # http://scipy-lectures.org/intro/scipy/auto_examples/plot_fftpack.html
    # https://dsp.stackexchange.com/questions/724/low-pass-filter-and-fft-for-beginners-with-python

    def __init__(self, cfg):
        from digitalmodel.infrastructure.utils.data import AttributeDict
        self.cfg = cfg
        self.vars = AttributeDict({})

    def get_raw_data(self):
        if self.cfg.default['data_source'] == 'db':
            from digitalmodel.infrastructure.utils.database import Database
            db_properties = self.cfg.db
            self.dbe = Database(db_properties)
            if self.cfg.default.__contains__('input_data'):
                cfg_input = self.cfg.default['input_data'].copy()
                self.dbe.get_input_data(cfg_input)
            else:
                print("No input data in configuration")
        else:
            import sys
            print("No data source specified")
            sys.exit()

    def transform_raw_data(self):
        self.transform_dataframes()

    def transform_dataframes(self):
        from digitalmodel.infrastructure.utils.data import Transform
        trans = Transform()
        cfg_trans = self.cfg.transform['dataframe']['sets']
        for set_index in range(0, len(cfg_trans)):
            cfg_set = cfg_trans[set_index]
            set_label = cfg_set['label']
            df = getattr(self.dbe, 'input_data_' + set_label)
            if cfg_set.__contains__('transpose'):
                cfg_df_transform = cfg_set['transpose']
                try:
                    df_trans = trans.dataframe_to_dataframe(df, cfg_df_transform)
                except:
                    logging.error("The dataframe transformation can not be completed : {}".format(set_label),
                                  exc_info=True)
                    raise ("DataFrame transformation failed. Program stopping")
            else:
                df_trans = df
            setattr(self, set_label, df_trans)

    def prepare_report_data(self):
        self.add_dataframes_to_vars()
        self.add_parameters_to_vars()

    def add_dataframes_to_vars(self):
        from digitalmodel.infrastructure.utils.data import Transform
        trans = Transform()
        cfg_trans = self.cfg.transform['dataframe']['sets']
        for set_index in range(0, len(cfg_trans)):
            cfg_set = cfg_trans[set_index]
            set_label = cfg_set['label']
            df = getattr(self, set_label)
            try:
                df = trans.df_JSON_strings_values_to_dict(df, cfg_set)
            except:
                logging.error("The dataframe transformation to dict is not completed : {}".format(set_label),
                              exc_info=True)
            cfg_to_html = cfg_set.get('to_html', {})
            cfg_to_html.update({'json_transformation': cfg_set.get('json_transformation', None)})
            try:
                df_html = trans.dataframe_to_html(df.copy(), cfg_to_html)
            except:
                logging.error("The dataframe transformation to HTML is not completed : {}".format(set_label),
                              exc_info=True)
            df_dict = trans.dataframe_to_dict(df)
            df_json = trans.dataframe_to_json(df)
            self.vars.update({set_label + '_html': df_html})
            self.vars.update({set_label + '_dict': df_dict})
            self.vars.update({set_label + '_json': df_json})

    def add_parameters_to_vars(self):
        self.vars.parameters = self.cfg.parameters.copy()
        self.vars.custom_parameters = self.cfg.custom_parameters.copy()

    def prepare_web_api_data(self):
        pass

    def prepare_chart_data(self):
        import json
        cfg_temp = self.cfg.interactive_chart.copy()
        for chart_index in range(0, len(cfg_temp)):
            cfg_chart = cfg_temp[chart_index]
            for set_index in range(0, len(cfg_chart['dataSources'])):
                set_info = cfg_chart['dataSources'][set_index]
                df = getattr(self.dbe, set_info['df']['label'])
                df = self.filter_df(df, set_info)
                orient = set_info['df'].get('orient', 'records')
                set_info['data'] = df.to_json(orient=orient)
            self.vars.update({cfg_chart['label']: json.dumps(cfg_chart)})

    def filter_df(self, df, cfg_chart):
        if cfg_chart['df'].__contains__('filter'):
            from digitalmodel.infrastructure.utils.data import ReadData
            read_data = ReadData()
            cfg_filter = cfg_chart['df']
            df = read_data.df_filter_by_column_values(cfg_filter, df)

        return df

    def prepare_htmls(self):
        from digitalmodel.infrastructure.utils.documentation_components import JinjaLib
        jl = JinjaLib()
        rpt_sets = self.cfg.report['sets']
        file_prefix = self.cfg.report.get('file_suffix', self.cfg['Analysis']['file_name'])
        append_run_ID = self.cfg.report.get('append_run_ID', False)
        runID = 1
        if append_run_ID:
            file_prefix = file_prefix + str(runID)

        for rpt_index in range(0, len(rpt_sets)):
            import html2text
            rpt_set = rpt_sets[rpt_index]

            file_name_suffix = rpt_set.get('file_suffix', None)
            if file_name_suffix is not None:
                file_name = file_prefix + file_name_suffix
            else:
                file_name = file_prefix
            file_name_without_extension = self.cfg['Analysis']['result_folder'] + file_name

            template_file = rpt_set['template']
            var_dict_attribute = rpt_set['input_attribute']
            var_dict = getattr(self, var_dict_attribute)
            var_dict.update({'file_prefix': file_prefix, 'file_name_without_extension': file_name_without_extension})
            output_text = jl.output_from_template_file(template_file, var_dict)
            print(output_text)

            html2text.html2text(output_text)
            self.prepare_html(output_text, file_name_without_extension)

            if rpt_set['pdf']['flag']:
                self.prepare_pdf(output_text, file_name_without_extension)

    def prepare_pdf(self, html_string, file_name_without_extension):
        from digitalmodel.infrastructure.utils.documentation_components import PDFReports
        pdf_rep = PDFReports(self.cfg.report['wkhtmltopdf_path'])
        pdf_rep.create_pdf_from_html_string(html_string, file_name_without_extension + '.pdf')

    def prepare_html(self, html_str, file_name_without_extension):
        html_file = open(file_name_without_extension + '.html', "w")
        html_file.write(html_str)
        html_file.close()

    def save_report_data_as_json(self):
        import json
        file_name_without_extension = self.cfg['Analysis']['result_folder'] + self.cfg.custom_parameters[
            'field_nickname']
        file_name = file_name_without_extension + '.json'

        with open(file_name, 'w') as fp:
            json.dump(self.vars, fp)

    def save_report_data_as_pkl(self):
        import pickle
        file_name_without_extension = self.cfg['Analysis']['result_folder'] + self.cfg.custom_parameters[
            'field_nickname']
        file_name = file_name_without_extension + '.pkl'

        with open(file_name, 'wb') as fp:
            # pickle.dump(self.vars, fp, protocol=pickle.HIGHEST_PROTOCOL)
            pickle.dump(self.vars, fp, protocol=pickle.DEFAULT_PROTOCOL)
