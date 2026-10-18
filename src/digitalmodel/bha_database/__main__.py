"""Print SI summaries of the three synthetic teaching assemblies."""
import json

from .loader import load_database


def main():
    database = load_database()
    print(json.dumps([database.summary(key) for key in database.assemblies], indent=2, allow_nan=False))


if __name__ == '__main__':
    main()
