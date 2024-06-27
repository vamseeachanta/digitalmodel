import os
import re
from collections import defaultdict

def summarize_mes_files(directory):
    warnings_count = defaultdict(int)
    
    # Iterate over files in the directory
    for filename in os.listdir(directory):
        if filename.endswith(".MES"):
            file_path = os.path.join(directory, filename)
            with open(file_path, 'r') as file:
                content = file.read()
                # Using regex to find all warning patterns
                warning_patterns = re.findall(r'\*{4,}.*?(?=\*{4,}|$)', content, re.DOTALL)
                # Count occurrences of each warning pattern
                for pattern in warning_patterns:
                    warnings_count[pattern.strip()] += 1
    
    # Print or return the summarized warnings
    for warning, count in warnings_count.items():
        print(f"{warning.strip()}: {count} occurrences")

# Example usage:
directory_path = r'C:\Users\Sk Samdan\Desktop\digitalmodel-1\src\digitalmodel\tests\test_data\aqwa\mes_files'
summarize_mes_files(directory_path)
