title: Virtualization
date: 2020-07-25
category: python

## Introduction
Virtualization helps create isolated environments and help in transferring the application to another computer or scaling applications to a larger scale such as a cluster or cloud computers.
Dockers is a system level virtualization while python virtual environment is program level virtualization. A high level rough estimate comparison is given in Table 12.1.

Feature	Virtual Machine	Docker/ Container	Program Environment
Level	System	System	Program
Installation Time	>5  mins	<5 mins	1-2 mins
Start-up Time	>5  mins	5-10 s	2 - 3 s
Configuration	OS, programs	OS, programs	program
Operating System		Linux preferred. Unstable in windows
Table 12.1 – Virtualization of Environments

Virtual environments help isolate the development and (also) deployment of code. The virtual environments help with a stable and consistent environment.

A virtual environment helps in support and maintenance of reliable code when working on multiple projects:
•	Updating or downgrading python modules is easy to do but dependencies can be rendered incompatible.
•	Upgrading some dependencies will sometimes remove or uninstall other required basic dependencies. Therefore, it is recommended to work in virtual environments to avoid this problem. See example in Section
•	If the programmer is required to work on multiple projects which require different modules then it is highly recommended to develop, run and deploy in each application in a dedicated virtual environment.




Virtual environments also help run legacy codes with ease:
•	Legacy codes may not be supported in latest python installations. Therefore, it may be difficult to maintain a reliable code with 1 python installation.

## Python Virtualization

A virtual environment is an isolated working copy of Python.

•	Define environment.yml file with an environment name and dependencies.
o	Example contents of environment.yml:
name: <environment_name>
channels:
  - defaults
  - conda-forge
dependencies:
  - <list any additional conda packages here (you can include =, or >= version numbers also)>
  - pip:
   		 - <list any additional pip packages here (you can include =, or >= version numbers also)>

o	Example with pip packages
			name: API579
channels:
  - defaults
  - conda-forge
dependencies:
  - python=3.6
  - pyyaml
  - yaml
  - oyaml
  - matplotlib
  - scipy
  - pandas
  - python-docx
  - pip:
    - imgkit
    - openpyxl
    - xlrd

•	Create Environment:
	conda env create -f environment.yml
        environment.yml can be replaced with any anyfilename.yaml and it works.

or use below and Program will search for default environment.yml in current folder and then create the environment.
	conda env create

•	Removing Environment:
	conda env remove -n FOO

•	Update Environment:
	conda env update -n statistical_assessment –f statistical_assessment.yml

•	Example 1 : environment.yml:
o	name: customenv
o	channels:
o	  - defaults
o	dependencies:
o	  - cx_oracle=6.3.1=py35h2fa13f4_0
o	  - numpy=1.14.5=py35h9fa60d3_0
o	  - pandas=0.23.1=py35h830ac7b_0
o	  - pyodbc=4.0.22=py35h6538335_0
o	  - python=3.5.5=h0c2934d_2
o	  - sqlalchemy=1.2.8=py35hfa6e2cd_0
•	Example 2 : environment.yml:
o	name : finance
o	channels:
o	  - defaults
o	dependencies:
o	  - python=3.5
o	  - pandas=0.22.0
o	  - pandas-datareader=0.6.0



Linking virtual environments in Jupyter
https://janakiev.com/blog/jupyter-virtual-envs/


## References
1. <https://medium.com/knerd/best-practices-for-python-dependency-management-cc8d1913db82>
1. <https://medium.com/knerd/best-practices-for-python-dependency-management-cc8d1913db82>
