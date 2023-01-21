import logging
import os
import sys

try:
    user_paths = os.environ['PYTHONPATH']
except KeyError:
    user_paths = []

