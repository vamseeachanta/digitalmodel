"""Attribute-accessible dictionary for DNV OS F101 configuration."""

class AttributeDict(dict):
    def __init__(self, *args, **kwargs):
        super(AttributeDict, self).__init__(*args, **kwargs)
        self.__dict__ = self
