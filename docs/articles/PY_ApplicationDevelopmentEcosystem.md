title: Python Application Development Ecosystem
date: 2020-09-25
category: Python

## Introduction
When developing python applications, there are a host of programs (IDEs, frameworks etc.) that can be used for development

A broad category of typical development projects are:

* Backend
* Data interface
* ascii files (Such as YAML, TXT, JSON, .csv delimited etc.)
* Databases


We will address the following in this article based on personal experience at our organization.

## Summary

For a non-programmer who is getting started and have no no personal preferences (from prior experience), below are starter recommendations:

* Python Release : MiniConda
* Python IDE: PyCharm
* Database: SQL Express (for Microsoft SQL) or PostgreSQL
* Database Manager: DBeaver

## Python Release
There are python releases which can be used for development.
Directly get python from [python org](https://www.python.org/doc/versions/){:target="_blank"}
A miniconda python is recommended as it is easy to maintain the bare minimum base packages.
All projects are executed using a dedicated (or single) virtual environment.
For detailed virtual environment and miniconda vs. anaconda, see guidelines provided in [Conda versions article](/blog/PY_allAboutCondaEnvs.html){:target="_blank"}

## Python IDE
Pycharm Community Edition is a good start See [pycharm](https://www.jetbrains.com/pycharm/download/#section=windows){:target="_blank"}.
Pycharm is a good choice if starting new and with less to no development experience, use this to get familiar with intricacies of python coding, git version control and other coding aspects

If one used VSCode in the past, working can be seamless in VSCode

Pycharm and VSCode are neck to neck when compared with each other. Usage comparison is given below:

## Database
SQL or NoSQL of your choice and comfort. If an absolute database novice, utilize ascii files to start with to have decrease time to finish concept.

If programmer is a database starter and database is a must, utilize the following:
If using microsoft sql, utilize Microsoft sql express (#TODO link??). Alternatively PostgreSQL (local or heorku cloud) can also be used.


## Database Manager
If using microsoft sql, utilize Microsoft SQL Server Management Studio (also typically known as MSSMS)
If PostgreSQL or any other cloud database server, utilize DBeaver community to access data and perform simple database operations.
See [DBeaver](https://dbeaver.io/download/){:target="_blank"}


## Version Control
Utilize git in conjunction with an online repository.
Install ([Git](https://git-scm.com/downloads) git and set up configuration.
Recommend [GitHub](https://github.com/) to save files online (i.e. cloud). Can keep code repository private or public per preference.
Can utilize command line (bash) or utilize a popular IDE to handle version control code revision cycles.
The purpose and motive of Git and code lifecycle will be fully understood when 2 or more team members work together on at least partial common code base.

## References
1. <https://hottakeoftheday.com>
1. <>
