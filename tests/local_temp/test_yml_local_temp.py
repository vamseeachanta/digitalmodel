import os
import subprocess
import pandas as pd

def run_yaml_files(directory):
    
    filenames = []
    statuses = []

    
    for filename in os.listdir(directory):
        if filename.endswith(('.yml', '.yaml')) and not filename.lower().startswith('app'):
            file_path = os.path.join(directory, filename)
            try:
                
                result = subprocess.run(['python', '-m', 'digitalmodel', file_path], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                print(result.stdout.decode())  
                filenames.append(filename)
                statuses.append('Success')
            except subprocess.CalledProcessError as e:
                print(e.stderr.decode())  
                filenames.append(filename)
                statuses.append('Failed')

    
    df = pd.DataFrame({'Filename': filenames, 'Status': statuses})
    output_csv = os.path.join(directory, 'file_status.csv')
    df.to_csv(output_csv, index=False)
    print(f'Results saved to {output_csv}')


directory = r'src\digitalmodel\tests\local_temp'
run_yaml_files(directory)