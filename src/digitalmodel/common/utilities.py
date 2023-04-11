import os

# Determine if file is valid    
def is_file_valid(file_name):
    if not os.path.isfile(file_name):
        print("File does not exist")
        return False
    else:
        return True
