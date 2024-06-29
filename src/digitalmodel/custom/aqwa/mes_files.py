# Standard library imports
import os
import re
from collections import Counter, defaultdict

# Third party imports
import pandas as pd  # noqa
from assetutilities.common.utilities import is_dir_valid_func
from tabulate import tabulate


def read_mes_files(directory):
    mes_files = [f for f in os.listdir(directory) if f.endswith('.MES')]
    warnings = defaultdict(lambda: defaultdict(Counter))
    errors = defaultdict(lambda: defaultdict(Counter))
    file_status = {}
    
    warning_pattern = re.compile(r'\*\*\*\* (.+?) WARNING \*\*\*\* (.+)')
    error_pattern = re.compile(r'\*\*\*\* (.+?) ERROR \*\*\*\* (.+)')
    
    for mes_file in mes_files:
        file_warnings_errors = 0
        with open(os.path.join(directory, mes_file), 'r') as file:
            for line in file:
                warning_match = warning_pattern.search(line)
                error_match = error_pattern.search(line)
                
                if warning_match:
                    warning_type, warning_message = warning_match.groups()
                    warnings[warning_type.strip()][warning_message.strip()][mes_file] += 1
                    file_warnings_errors += 1
                elif error_match:
                    error_type, error_message = error_match.groups()
                    errors[error_type.strip()][error_message.strip()][mes_file] += 1
                    file_warnings_errors += 1
        
        file_status[mes_file] = "X" if file_warnings_errors > 0 else "-"
    
    return warnings, errors, file_status

def summarize_warnings_and_errors(warnings, errors):
    warning_summary = []
    error_summary = []

    for warning_type, warning_messages in warnings.items():
        for message, file_counts in warning_messages.items():
            for file, count in file_counts.items():
                warning_summary.append([warning_type, message, count, os.path.splitext(file)[0]])
    
    for error_type, error_messages in errors.items():
        for message, file_counts in error_messages.items():
            for file, count in file_counts.items():
                error_summary.append([error_type, message, count, os.path.splitext(file)[0]])
    
    return warning_summary, error_summary

def to_dataframe(summary, output_file,columns):
    df = pd.DataFrame(summary, columns=columns)
    df.to_csv(output_file, index=False)
    return df

def generate_file_status_table(file_status):
    file_names = ["File"] + [os.path.splitext(filename)[0] for filename in file_status.keys()]
    statuses = ["Status"] + list(file_status.values())
    status_summary = [file_names, statuses]
    status_table = tabulate(status_summary, tablefmt="grid")
    return status_summary, status_table

if __name__ == "__main__":
    directory = r'src\digitalmodel\tests\test_data\aqwa\mes_files' 
    dir_is_valid, dir = is_dir_valid_func(directory)
    if not dir_is_valid:
        raise FileNotFoundError(f"Directory not found: {dir}")
        
    warnings, errors,file_status = read_mes_files(dir)
    warning_summary, error_summary = summarize_warnings_and_errors(warnings, errors)
    status_summary,status_table = generate_file_status_table(file_status)

    warning_df = to_dataframe(warning_summary, 'warnings.csv', ['Type', 'Description', 'Frequency', 'File'])
    error_df = to_dataframe(error_summary, 'errors.csv', ['Type', 'Description', 'Frequency', 'File'])
    status_df = pd.DataFrame(status_summary[1:], columns=status_summary[0])
    status_df.to_csv('mes_files_status.csv', index=False)

    print("Warnings.csv :")
    print(warning_df)
    print("\nErrors.csv :")
    print(error_df)
    print("\nMES Files Comparison:")
    print(status_df)