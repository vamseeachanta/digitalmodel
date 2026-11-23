import os

# PYTHONPATH is always a string (os.pathsep-joined); split into a list so
# the result type is uniform whether or not the variable is set.
user_paths = os.environ.get("PYTHONPATH", "").split(os.pathsep)
