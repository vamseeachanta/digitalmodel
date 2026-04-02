import logging


class LogFileAnalysisComponents():
    """Components for analyzing log files from ODA and similar systems.

    Provides methods for collecting file lists, extracting log data,
    gathering file metadata, computing statistics, and plotting results.

    Attributes:
        cfg: Configuration object containing file paths, labels, and settings.
    """

    def __init__(self, cfg):
        """Initialize LogFileAnalysisComponents with configuration.

        Args:
            cfg: Configuration object with 'files' and 'Analysis' attributes.
        """
        self.cfg = cfg
        # self.get_model_state_information()

    def get_file_list(self):
        """Populate the file_list attribute by scanning configured folders.

        Reads file lists from each folder specified in the configuration,
        filtering by the configured file type.
        """
        from digitalmodel.infrastructure.utils.data import ReadData
        read_data = ReadData()
        self.file_list = []

        for folder_index in range(0, len(self.cfg.files['folder'])):
            folder_with_file_type = self.cfg.files['folder'][folder_index] + '\*' + self.cfg.files['file_type']
            file_list_for_folder = read_data.get_file_list_from_folder(folder_with_file_type)
            self.file_list.append(file_list_for_folder)

    def get_last_2_lines(self):
        """Extract the last two lines from each log file and save to CSV.

        Processes up to 10 files per folder, reads the last 2 lines from
        each, and exports the results to a CSV file.
        """
        import pandas as pd

        from digitalmodel.infrastructure.utils.data import ReadData
        read_data = ReadData()
        # For test purposes
        # self.file_list = ['Q:\\projects\\Mole\\log_files\\30-015-45336_1_20190904_13h36m.log',
        #                   'Q:\\projects\\Mole\\log_files\\42-389-37924_1_20190904_06h11m.log']

        df_array = []
        for folder_index in range(0, len(self.cfg.files['folder'])):
            lines_array = []
            for file_index in range(0, 10):
                file = self.file_list[folder_index][file_index]
                print("processing file... {0}" .format(file))
                print("processing file... {0} of {1}" .format(len(lines_array) + 1, len(self.file_list[folder_index])))
                cfg_temp = ({'io': file, 'lines_from_end': 2})
                file_lines = read_data.from_ascii_file_get_lines_as_string_arrays(cfg_temp)
                file_lines.insert(0, file)
                lines_array.append(file_lines)

            df = pd.DataFrame(lines_array)
            df.replace(to_replace='\n', value='', inplace=True, regex=True)
            file_name =  self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '.csv'
            df.to_csv(file_name)
            df_array.append(df)

    def get_detailed_file_information(self):
        """Collect detailed metadata for each file in the file list.

        Extracts file name, label, API number, creation date, start month,
        and modification date for ODA log files. Results are stored in
        the file_detailed_information attribute.
        """
        import datetime
        import os

        import pandas as pd

        if self.cfg.files['oda_logs']:
            columns = ['file_name', 'label', 'api', 'start_date', 'start_month', 'modified_data']
            df = pd.DataFrame(columns = columns)
            for folder_index in range(0, len(self.cfg.files['folder'])):
                for file_index in range(0, len(self.file_list[folder_index])):
                    file_path = self.file_list[folder_index][file_index]
                    file_name = os.path.basename(file_path)
                    label = self.cfg.files['label'][folder_index]
                    api = file_name[0:12]
                    start_date = datetime.datetime.fromtimestamp(os.path.getctime(file_path))
                    start_month = start_date.month
                    modified_data = datetime.datetime.fromtimestamp(os.path.getmtime(file_path))

                    row_array = [file_name, label, api, start_date, start_month, modified_data]
                    df.loc[len(df)] = row_array

            self.file_detailed_information = df

    def get_file_information_statistics(self):
        """Compute high-level statistics from file metadata.

        Groups files by API number and extracts start month for each
        unique API. Results are saved to a CSV file and stored in the
        high_level_statistics attribute.
        """
        import pandas as pd
        if self.cfg.files['oda_logs']:
            columns = ['month']
            self.high_level_statistics = pd.DataFrame(columns = columns)

            api_list = self.file_detailed_information.api.unique().tolist()
            for api in api_list:
                month = self.file_detailed_information[self.file_detailed_information.api == api].iloc[0].start_month
                self.high_level_statistics.loc[len(self.high_level_statistics)] = month

            file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '.csv'
            self.high_level_statistics.to_csv(file_name)


    def plot_statistics(self):
        """Print high-level statistics and display a placeholder for plotting."""
        print(self.high_level_statistics)
        print("Perform filtering, transformation and high level plotting here")
