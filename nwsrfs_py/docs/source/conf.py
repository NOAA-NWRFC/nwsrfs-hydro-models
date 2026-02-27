import os
import sys
sys.path.insert(0, os.path.abspath('../../'))

# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'NWSRFSpy'
copyright = 'Copyright 2025 U.S. Federal Government (in countries where recognized)'
author = 'Geoffrey Walters PE, Cameron Bracken PhD'
release = '0.1.0'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinx.ext.autodoc',       # Core library to parse docstrings
    'sphinx.ext.napoleon',      # Support for Google-style docstrings
    'sphinx.ext.viewcode',      # Add links to source code
    'sphinx_autodoc_typehints', # Automatically document type hints
]

# Avoid requiring a compiled extension module when building docs in CI.
autodoc_mock_imports = ['nwsrfs_py.wrapper.nwsrfs_src']

# Napoleon settings (optional but good for explicit Google style)
napoleon_google_docstring = True
napoleon_numpy_docstring = False

autodoc_typehints = 'description'
autodoc_default_options = {
    'members': True,
    'show-inheritance': True,
}

# Existing module docstrings include many non-public/custom cross-references.
# Suppress unresolved reference warnings in rendered docs.
suppress_warnings = ['ref.class', 'ref.meth']

templates_path = ['_templates']
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']
