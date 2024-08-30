import os

def test_pass_pipeline():

    library = 'digitalmodel'
    # root_directory = f'src/{library}/tests/test_data'
    summary_file = f'src/{library}/tests/test_data/yml_summary.txt'

    
    with open(summary_file, 'r') as file:
        content = file.read()

    tests_passed = None
    for line in content.splitlines():
        if "Tests passed:" in line:
            tests_passed = int(line.split(":")[1].strip())
            break

    expected_result = 15

    assert tests_passed == expected_result

