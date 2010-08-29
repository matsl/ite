REM Compiles all (La)TeX test files.

latex -include-directory=../tex basic
latex -include-directory=../tex fancy
latex -include-directory=../tex master
latex -include-directory=../tex pages
latex -include-directory=../tex parse
tex -include-directory=../tex plain
