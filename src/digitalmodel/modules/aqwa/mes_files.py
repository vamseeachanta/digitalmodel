# Standard library imports
import logging
import os
import re
from collections import Counter, defaultdict

# Third party imports
import pandas as pd
from tabulate import tabulate


class MesFiles:
    def __init__(self) -> None:
        pass

    def read_mes_files(self, directory):
        try:
            mes_files = [f for f in os.listdir(directory) if f.endswith(".MES")]
            if not mes_files:  # checks if the directory is empty
                logging.info(f"No MES files found in directory: {directory}")
                return None, None, None
        except Exception as e:
            logging.error(f"Error reading files from directory: {directory}")
            logging.error(e)
            return None, None, None
        warnings = defaultdict(lambda: defaultdict(Counter))
        errors = defaultdict(lambda: defaultdict(Counter))
        file_status = {}

        warning_pattern = re.compile(r"\*\*\*\* (.+?) WARNING \*\*\*\* (.+)")
        error_pattern = re.compile(r"\*\*\*\* (.+?) ERROR \*\*\*\* (.+)")

        for mes_file in mes_files:
            file_warnings_errors = 0
            with open(os.path.join(directory, mes_file), "r") as file:
                for line in file:
                    warning_match = warning_pattern.search(line)
                    error_match = error_pattern.search(line)

                    if warning_match:
                        warning_type, warning_message = warning_match.groups()
                        warnings[warning_type.strip()][warning_message.strip()][
                            mes_file
                        ] += 1
                        file_warnings_errors += 1
                    elif error_match:
                        error_type, error_message = error_match.groups()
                        errors[error_type.strip()][error_message.strip()][mes_file] += 1
                        file_warnings_errors += 1
            file_status[mes_file] = "X" if file_warnings_errors > 0 else "-"

        return warnings, errors, file_status

    def assign_ids(self, warnings, errors):
        warning_id_map = {}
        error_id_map = {}
        warning_id_counter = 1
        error_id_counter = 1

        for warning_type, warning_messages in warnings.items():
            for message in warning_messages.keys():
                warning_id_map[(warning_type, message)] = f"WAR{warning_id_counter}"
                warning_id_counter += 1

        for error_type, error_messages in errors.items():
            for message in error_messages.keys():
                error_id_map[(error_type, message)] = f"ERR{error_id_counter}"
                error_id_counter += 1

        return warning_id_map, error_id_map

    def summarize_warnings_and_errors(
        self, warnings, errors, warning_id_map, error_id_map
    ):
        warning_summary = []
        error_summary = []

        for warning_type, warning_messages in warnings.items():
            for message, file_counts in warning_messages.items():
                warning_id = warning_id_map[(warning_type, message)]
                files = ", ".join(
                    [os.path.splitext(file)[0] for file in file_counts.keys()]
                )
                total_count = sum(file_counts.values())
                warning_summary.append(
                    [warning_id, warning_type, message, total_count, files]
                )

        for error_type, error_messages in errors.items():
            for message, file_counts in error_messages.items():
                error_id = error_id_map[(error_type, message)]
                files = ", ".join(
                    [os.path.splitext(file)[0] for file in file_counts.keys()]
                )
                total_count = sum(file_counts.values())
                error_summary.append(
                    [error_id, error_type, message, total_count, files]
                )

        return warning_summary, error_summary

    def generate_file_status_table(self, file_status):
        file_names = ["Filename"] + [
            os.path.splitext(filename)[0] for filename in file_status.keys()
        ]
        statuses = ["Status"] + list(file_status.values())
        status_summary = [file_names, statuses]
        status_table = tabulate(status_summary, tablefmt="grid")
        return status_summary, status_table

    def to_dataframe(self, summary, output_file, columns):
        df = pd.DataFrame(summary, columns=columns)
        df.to_csv(output_file, index=False)
        return df

    def merge_cells(self, df):
        df = df.copy()
        for col in ["ID", "Type", "Description"]:
            df[col] = df[col].mask(df.duplicated(subset=[col]))

        col = "Type"
        for row_idx in range(0, len(df)):
            if pd.isna(df.loc[row_idx, col]):
                df.loc[row_idx, col] = df.loc[row_idx - 1, col]

        # df.fillna({'col': col}, inplace=True)
        # df['Type'].fillna(method='ffill', inplace=True)
        return df

    def generate_id_file_matrix(
        self, warnings, errors, warning_id_map, error_id_map, file_status
    ):
        all_ids = list(warning_id_map.values()) + list(error_id_map.values())
        file_names = [os.path.splitext(filename)[0] for filename in file_status.keys()]

        matrix = []
        matrix.append(["File"] + file_names)

        for id in all_ids:
            row = [id]
            for file in file_names:
                if id in warning_id_map.values():
                    warning_key = list(warning_id_map.keys())[
                        list(warning_id_map.values()).index(id)
                    ]
                    present = file in {
                        os.path.splitext(f)[0]
                        for f in warnings[warning_key[0]][warning_key[1]].keys()
                    }
                else:
                    error_key = list(error_id_map.keys())[
                        list(error_id_map.values()).index(id)
                    ]
                    present = file in {
                        os.path.splitext(f)[0]
                        for f in errors[error_key[0]][error_key[1]].keys()
                    }
                row.append("X" if present else "-")
            matrix.append(row)

        return matrix

    def router(self, cfg=None):

        if cfg is None:
            directory = r"src\digitalmodel\tests\test_data\aqwa\output\mes"
        else:
            directory = cfg["Analysis"]["file_management_input_directory"]

        warnings, errors, file_status = self.read_mes_files(directory)
        warning_id_map, error_id_map = self.assign_ids(warnings, errors)
        warning_summary, error_summary = self.summarize_warnings_and_errors(
            warnings, errors, warning_id_map, error_id_map
        )
        status_summary, status_table = self.generate_file_status_table(file_status)

        warnings_filename = os.path.join(directory, "mes_warnings.csv")
        warning_df = self.to_dataframe(
            warning_summary,
            warnings_filename,
            ["ID", "Type", "Description", "Frequency", "Filename"],
        )

        error_filename = os.path.join(directory, "mes_errors.csv")
        error_df = self.to_dataframe(
            error_summary,
            error_filename,
            ["ID", "Type", "Description", "Frequency", "Filename"],
        )

        status_df = pd.DataFrame(status_summary[1:], columns=status_summary[0])
        status_filename = os.path.join(directory, "mes_status.csv")
        status_df.to_csv(status_filename, index=False)

        merged_warning_df = self.merge_cells(warning_df)
        merged_error_df = self.merge_cells(error_df)

        merged_warning_df.index += 1
        merged_error_df.index += 1

        warnings_filename = os.path.join(directory, "mes_warnings.csv")
        merged_warning_df.to_csv(warnings_filename, index=False)

        error_filename = os.path.join(directory, "mes_errors.csv")
        merged_error_df.to_csv(error_filename, index=False)

        logging.info("Summary of Warnings:")
        logging.info(merged_warning_df)
        logging.info("\nSummary of Errors:")
        logging.info(merged_error_df)
        logging.info("\nFile Status:")
        logging.info(status_df)
        logging.info(status_table)

        id_file_matrix = self.generate_id_file_matrix(
            warnings, errors, warning_id_map, error_id_map, file_status
        )
        id_file_df = pd.DataFrame(id_file_matrix[1:], columns=id_file_matrix[0])
        id_file_df.index += 1

        map_filename = os.path.join(directory, "mes_warning_error_map.csv")
        id_file_df.to_csv(map_filename, index=False)

        logging.info("\nID File Matrix:")
        logging.info(id_file_df)


if __name__ == "__main__":
    mes_files = MesFiles()
    mes_files.router()
