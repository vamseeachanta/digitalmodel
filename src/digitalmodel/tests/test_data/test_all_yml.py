# Standard library imports
import os
import subprocess

# Third party imports
import pandas as pd
from colorama import Fore, Style
from colorama import init as colorama_init

colorama_init()

def run_yaml_files(root_directory,sub_folders):
    
    folders = []
    filenames = []
    statuses = []

    for subfolder in sub_folders:
        subfolder_path = os.path.join(root_directory, subfolder)
        for root, _, files in os.walk(subfolder_path):
            for filename in files:
                if filename.endswith(('.yml', '.yaml')) and not filename.lower().startswith('app'):
                    file_path = os.path.join(root, filename)
                    try:
                        result = subprocess.run(['python', '-m', library, file_path], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                        print(result.stdout.decode())  
                        folders.append(os.path.relpath(root, root_directory))  
                        filenames.append(filename)
                        statuses.append('Success')
                    except subprocess.CalledProcessError as e:
                        print(e.stderr.decode())  
                        folders.append(os.path.relpath(root, root_directory))  
                        filenames.append(filename)
                        statuses.append('Failed')

            df = pd.DataFrame({'Folder': folders, 'Filename': filenames, 'Status': statuses})

            df = df.sort_values(by=['Status', 'Folder'], ascending=[False, True])

            output_csv = os.path.join(root_directory, 'yml_status.csv')
            df.to_csv(output_csv, index=False)


    print(f'Detailed output: {df}')
    print(f'No. of files processed: {len(filenames)}')
    print(f"Tests passed: {Fore.GREEN}{len(df[df['Status'] == 'Success'])}{Style.RESET_ALL}")
    print(f"Tests Failed: {Fore.RED}{len(df[df['Status'] == 'Failed'])}{Style.RESET_ALL}")
    print('Done!')


if __name__ == '__main__':
    library = 'digitalmodel-1'
    root_directory = f'src/{library}/tests/test_data'
    sub_folders = ['6d_buoy','aqwa','catenary_riser','fatigue_analysis','fea_model','installation','orcaflex','orcaflex_analysis',
                   'orcaflex_file_preparation','orcaflex_post_process','pipeline','raos','rigging','shapes','ship_design',
                   'umbilical_analysis','vertical_riser','vessels','viv_analysis'] 
    run_yaml_files(root_directory,sub_folders)
