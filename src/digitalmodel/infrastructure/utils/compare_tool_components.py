class CompareTools():
    """Tools for comparing data from various file formats (YAML, CSV, XLSX, ASCII).

    Provides methods to load, extract, and compare data from different file
    types and generate visualizations of the comparison results.

    Attributes:
        cfg: Configuration dictionary with file and plot settings.
        df_array: List of DataFrames loaded from input files.
    """

    def __init__(self, cfg=None):
        """Initialize CompareTools with optional configuration.

        Args:
            cfg: Configuration dictionary with 'files' and 'plot' keys.
                Defaults to None.
        """
        self.cfg = cfg
        self.df_array = []

    def dictionaries(self):
        """Compare two dictionaries using DeepDiff and print the differences."""
        from pprint import pprint

        from deepdiff import DeepDiff
        t1 = {1: 1, 2: 2, 3: 3}
        t2 = t1
        print(DeepDiff(t1, t2))

    def csvs(self):
        """Load CSV files specified in configuration into the df_array list."""
        import pandas as pd
        for file_index in range(0, len(self.cfg['files'])):
            df = pd.read_csv(self.cfg['files'][file_index]['io'])
            self.df_array.append(df)

        # from digitalmodel.infrastructure.utils.visualization.visualizations import Visualization
        # visualization = Visualization()
        # visualization.from_df_array(self.df_array, self.cfg)

    def get_df_from_yaml(self, file_data):
        """Extract data from a YAML file into a pandas DataFrame.

        Args:
            file_data: Dictionary with 'io' (file path) and 'extract_data'
                (list of key paths to extract) keys.

        Returns:
            pd.DataFrame: DataFrame with extracted data columns.
        """
        from digitalmodel.infrastructure.utils.data import DefineData, ReadData
        read_data = ReadData()
        define_data = DefineData()

        extract_data_df = define_data.empty_data_frame()
        data_as_dictionary = read_data.read_yml_file(file_data)
        if file_data['extract_data'] != None:
            file_data.update({'data_dictionary': data_as_dictionary})
            for extract_data_index in range(0, len(file_data['extract_data'])):
                data_dictionary = file_data['data_dictionary']
                map_list = file_data['extract_data'][extract_data_index]
                extract_data_df[map_list[-1]] = read_data.extract_from_dictionary(data_dictionary, map_list)

        return extract_data_df

    def get_df_from_csv(self, file_data):
        """Load a CSV file into a pandas DataFrame.

        Args:
            file_data: Dictionary with 'io' key containing the CSV file path.

        Returns:
            pd.DataFrame: DataFrame loaded from the CSV file.
        """
        import pandas as pd
        df = pd.read_csv(file_data['io'])

        return df

    def get_df_from_xlsx(self, file_data):
        """Load an Excel file into a pandas DataFrame.

        Args:
            file_data: Dictionary with 'io' (file path) and 'sheet_name' keys.

        Returns:
            pd.DataFrame: DataFrame loaded from the specified Excel sheet.
        """
        import pandas as pd
        df = pd.read_excel(io=file_data['io'], sheet_name=file_data['sheet_name'])
        return df

    def get_df_from_ascii(self, file_data):
        """Load an ASCII file into a pandas DataFrame using whitespace delimiters.

        Args:
            file_data: Dictionary with 'io' (file path), 'lines' (dict with
                'start' and 'end'), and 'data_type' keys.

        Returns:
            pd.DataFrame: DataFrame loaded from the ASCII file.
        """
        from digitalmodel.infrastructure.utils.data import ReadData
        read_data = ReadData()

        file_data.update({
            'start_line': file_data['lines']['start'],
            'end_line': file_data['lines']['end'],
            'delimiter': 'space'
        })
        if file_data['data_type'] == 'DataFrame':
            file_data.update({'DataFrame': True})
        df = read_data.from_ascii_file_get_structured_data_delimited_white_space(file_data)

        return df

    def extractData(self, fileList, cfg):
        """Extract structured data from a list of YAML files into a DataFrame.

        Args:
            fileList: List of YAML file paths to extract data from.
            cfg: Configuration dictionary with 'dataFrame' key containing
                'columns' and 'data' definitions with nested level keys (L1-L5).

        Returns:
            pd.DataFrame: DataFrame with extracted data from all files.
        """
        import pandas as pd
        import yaml
        dataDF = pd.DataFrame(columns=cfg['dataFrame']['columns'])
        for file in fileList:
            with open(file, 'r') as ymlfile:
                FileData = yaml.load(ymlfile, Loader=yaml.Loader)
            columns = cfg['dataFrame']['data']
            newDFRow = []
            newDFRow.append(file)
            for column in range(0, len(columns)):
                L1 = cfg['dataFrame']['data'][column]['L1']
                L2 = cfg['dataFrame']['data'][column]['L2']
                L3 = cfg['dataFrame']['data'][column]['L3']
                L4 = cfg['dataFrame']['data'][column]['L4']
                L5 = cfg['dataFrame']['data'][column]['L5']
                if L5 != None:
                    newDFRow.append(FileData[L1][L2][L3][L4][L5])
                elif L4 != None:
                    newDFRow.append(FileData[L1][L2][L3][L4])
                elif L3 != None:
                    newDFRow.append(FileData[L1][L2][L3])
                elif L2 != None:
                    newDFRow.append(FileData[L1][L2])
                else:
                    newDFRow.append(FileData[L1])

            dataDF.loc[len(dataDF)] = newDFRow

        return dataDF

    def extractPlotData(self, fileList, cfg):
        """Extract plot-specific data from YAML files, separated by riser type.

        Args:
            fileList: List of YAML file paths to extract data from.
            cfg: Configuration dictionary with 'plot' and 'ymlFiles' keys
                defining column mappings and riser type classifications.

        Returns:
            tuple: A pair of DataFrames (dataDF_SLWR, dataDF_SCR) for
                SLWR and SCR riser types respectively.
        """
        import pandas as pd
        import yaml
        dataDF_SLWR = pd.DataFrame(columns=cfg['plot']['columns']['SLWR'])
        dataDF_SCR = pd.DataFrame(columns=cfg['plot']['columns']['SCR'])
        for file_index in range(0, len(fileList)):
            with open(fileList[file_index], 'r') as ymlfile:
                FileData = yaml.load(ymlfile, Loader=yaml.Loader)

            columns = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']]
            newDFRow = []
            newDFRow.append(fileList[file_index])
            newDFRow.append(cfg['ymlFiles'][file_index]['riser_type'])
            newDFRow.append(cfg['ymlFiles'][file_index]['label'])
            for columnIndex in range(0, len(columns)):
                L1 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L1']
                L2 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L2']
                L3 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L3']
                L4 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L4']
                L5 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L5']
                if L5 != None:
                    newDFRow.append(FileData[L1][L2][L3][L4][L5])
                elif L4 != None:
                    newDFRow.append(FileData[L1][L2][L3][L4])
                elif L3 != None:
                    newDFRow.append(FileData[L1][L2][L3])
                elif L2 != None:
                    newDFRow.append(FileData[L1][L2])
                else:
                    newDFRow.append(FileData[L1])

            if cfg['ymlFiles'][file_index]['riser_type'] == 'SLWR':
                dataDF_SLWR.loc[len(dataDF_SLWR)] = newDFRow
            elif cfg['ymlFiles'][file_index]['riser_type'] == 'SCR':
                dataDF_SCR.loc[len(dataDF_SCR)] = newDFRow

        return dataDF_SLWR, dataDF_SCR

    def get_input_data(self):
        """Load input data from all configured files based on their file types.

        Reads data from YAML, CSV, XLSX, or ASCII files as specified in the
        configuration and stores them as instance attributes.
        """
        df_array = []
        for file_index in range(0, len(self.cfg['files'])):
            set_info = self.cfg['files'][file_index]

            if set_info['file_type'] == 'csv':
                # TODO Wire up code for reading .csvs from data.
                pass
            if set_info['file_type'] in ['yml', 'yaml']:
                file_data_df = self.get_df_from_yaml(set_info.copy())
                plt_settings = {
                    'x': [set_info['extract_data'][0][-1]],
                    'y': [set_info['extract_data'][1][-1]],
                    'label': [set_info['label']]
                }
            if set_info['file_type'] in ['csv']:
                file_data_df = self.get_df_from_csv(set_info.copy())
            if set_info['file_type'] in ['xlsx']:
                file_data_df = self.get_df_from_xlsx(set_info.copy())
            if set_info['file_type'] in ['ascii']:
                file_data_df = self.get_df_from_ascii(set_info.copy())

            setattr(self, 'input_data_' + set_info['label'], file_data_df)
            df_array.append(file_data_df)

    def prepare_visualizations(self):
        """Prepare and generate visualizations from the loaded data."""
        from digitalmodel.infrastructure.utils.visualization.visualization_components import VisualizationComponents
        vc = VisualizationComponents(self.cfg)
        vc.prepare_visualizations(self)

    def legacy_plot(self):
        """Generate legacy-style plots using the Visualization class.

        Creates plots for each configured plot specification, iterating
        over all loaded file data sets.
        """
        from digitalmodel.infrastructure.utils.visualization.visualizations import Visualization
        for plot_index in range(0, len(self.cfg['plot'])):
            plt_settings = self.cfg['plot'][plot_index]
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_suffix'] + '.png'
            })
            viz = Visualization(self.cfg['plot'][plot_index])
            for file_index in range(0, len(self.cfg['files'])):
                plt_settings.update({'label': [self.cfg.files[file_index]['label']]})
                viz.from_df_columns(self.df_array[file_index], plt_settings=plt_settings)

            viz.plt_settings = plt_settings
            viz.add_x_y_scale_formats()
            viz.add_x_y_lim_formats()
            viz.add_title_and_axis_labels()
            viz.add_legend()
            viz.add_text_fields()
            viz.save_and_close()
