import os
import re
from collections import Counter, defaultdict
from tabulate import tabulate
import pandas as pd

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
        file_status[mes_file] = "✘" if file_warnings_errors > 0 else "-"
    
    return warnings, errors, file_status

def assign_ids(warnings, errors):
    warning_id_map = {}
    error_id_map = {}
    warning_id_counter = 1
    error_id_counter = 1

    for warning_type, warning_messages in warnings.items():
        for message in warning_messages.keys():
            warning_id_map[(warning_type, message)] = f'WAR{warning_id_counter}'
            warning_id_counter += 1
    
    for error_type, error_messages in errors.items():
        for message in error_messages.keys():
            error_id_map[(error_type, message)] = f'ERR{error_id_counter}'
            error_id_counter += 1
    
    return warning_id_map, error_id_map

def summarize_warnings_and_errors(warnings, errors, warning_id_map, error_id_map):
    warning_summary = []
    error_summary = []

    for warning_type, warning_messages in warnings.items():
        for message, file_counts in warning_messages.items():
            warning_id = warning_id_map[(warning_type, message)]
            for idx, (file, count) in enumerate(file_counts.items()):
                if idx == 0:
                    warning_summary.append([warning_id, warning_type, message, count, os.path.splitext(file)[0]])
                else:
                    warning_summary.append(["", "", "", count, os.path.splitext(file)[0]])
    
    for error_type, error_messages in errors.items():
        for message, file_counts in error_messages.items():
            error_id = error_id_map[(error_type, message)]
            for idx, (file, count) in enumerate(file_counts.items()):
                if idx == 0:
                    error_summary.append([error_id, error_type, message, count, os.path.splitext(file)[0]])
                else:
                    error_summary.append(["", "", "", count, os.path.splitext(file)[0]])
    
    return warning_summary, error_summary

def generate_file_status_table(file_status):
    file_names = ["Filename"] + [os.path.splitext(filename)[0] for filename in file_status.keys()]
    statuses = ["Status"] + list(file_status.values())
    status_summary = [file_names, statuses]
    status_table = tabulate(status_summary, tablefmt="grid")
    return status_summary, status_table

def to_dataframe(summary, output_file, columns):
    df = pd.DataFrame(summary, columns=columns)
    df.to_csv(output_file, index=False)
    return df

if __name__ == "__main__":
    directory =  r'src\digitalmodel\tests\test_data\aqwa\mes_files' 
    warnings, errors, file_status = read_mes_files(directory)
    warning_id_map, error_id_map = assign_ids(warnings, errors)
    warning_summary, error_summary = summarize_warnings_and_errors(warnings, errors, warning_id_map, error_id_map)
    status_summary, status_table = generate_file_status_table(file_status)
    
    warning_df = to_dataframe(warning_summary, 'warnings.csv', ['ID', 'Type', 'Description', 'Frequency', 'Filename'])
    error_df = to_dataframe(error_summary, 'errors.csv', ['ID', 'Type', 'Description', 'Frequency', 'Filename'])
    status_df = pd.DataFrame(status_summary[1:], columns=status_summary[0])
    status_df.to_csv('file_status.csv', index=False)
    
    print("Summary of Warnings:")
    print(warning_df)
    print("\nSummary of Errors:")
    print(error_df)
    print("\nFile Status:")
    print(status_df)
    print(status_table)
