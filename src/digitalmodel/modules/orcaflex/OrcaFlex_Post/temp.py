import os

try:
    user_paths = os.environ["PYTHONPATH"]
except KeyError:
    user_paths = []
