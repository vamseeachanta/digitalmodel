import os
import re
from collections import Counter, defaultdict
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

if __name__ == "__main__":
    directory = r'C:\Users\Sk Samdan\Desktop\digitalmodel-1\src\digitalmodel\tests\test_data\aqwa\mes_files'  # Replace with the actual path to the .MES files directory
    warnings, errors = read_mes_files(directory)
    warning_table, error_table = summarize_warnings_and_errors(warnings, errors)
    
    print("Summary of Warnings:")
    print(warning_table)
    print("\nSummary of Errors:")
    print(error_table)
