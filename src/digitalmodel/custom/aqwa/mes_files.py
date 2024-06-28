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
    warnings = defaultdict(Counter)
    errors = defaultdict(Counter)
    
    warning_pattern = re.compile(r'\*\*\*\* (.+?) WARNING \*\*\*\* (.+)')
    error_pattern = re.compile(r'\*\*\*\* (.+?) ERROR \*\*\*\* (.+)')
    
    for mes_file in mes_files:
        with open(os.path.join(directory, mes_file), 'r') as file:
            for line in file:
                warning_match = warning_pattern.search(line)
                error_match = error_pattern.search(line)
                
                if warning_match:
                    warning_type, warning_message = warning_match.groups()
                    warnings[warning_type.strip()][warning_message.strip()] += 1
                elif error_match:
                    error_type, error_message = error_match.groups()
                    errors[error_type.strip()][error_message.strip()] += 1
    
    return warnings, errors

def summarize_warnings_and_errors(warnings, errors):
    warning_summary = []
    error_summary = []

    for warning_type, warning_messages in warnings.items():
        messages = []
        for message, count in warning_messages.items():
            messages.append(f"{message}: -- {count} time(s)")
        warning_summary.append([warning_type, "\n".join(messages)])

    for error_type, error_messages in errors.items():
        messages = []
        for message, count in error_messages.items():
            messages.append(f"{message}: -- {count} time(s)")
        error_summary.append([error_type, "\n".join(messages)])
    
    warning_table = tabulate(warning_summary, headers=["Type", "Messages"], tablefmt="grid")
    error_table = tabulate(error_summary, headers=["Type", "Messages"], tablefmt="grid")
    
    return warning_table, error_table

def to_dataframe():
    df_columns = ['Type', 'Description', 'Frequency', 'Files']
    df = pd.DataFrame(columns=df_columns)
    Type = 'INPUT DATA'
    Description = 'Requested number of cores, 12, is reduced to the number of available licenses'
    Frequency = 5
    Files = 'File1, File2, File3, ...'
    
    df.loc[len(df)] = [Type, Description, Frequency, Files]
    
    df.head()
    df.to_csv('warnings.csv', index=False)
    df.to_csv('errors.csv', index=False)

    return df

if __name__ == "__main__":
    directory = r'src\digitalmodel\tests\test_data\aqwa\mes_files'  # Replace with the actual path to the .MES files directory
    dir_is_valid, dir = is_dir_valid_func(directory)
    if not dir_is_valid:
        raise FileNotFoundError(f"Directory not found: {dir}")
        
    warnings, errors = read_mes_files(dir)
    warning_table, error_table = summarize_warnings_and_errors(warnings, errors)
    
    print("Summary of Warnings:")
    print(warning_table)
    print("\nSummary of Errors:")
    print(error_table)
    print("\nSummary of Errors:")
    print(error_table)
    print(error_table)
    df = to_dataframe()