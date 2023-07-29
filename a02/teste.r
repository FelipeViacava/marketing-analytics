Sys.getenv("RETICULATE_PYTHON")

reticulate::use_python("resources/venv/bin/python")
readRenviron(".Renviron")

reticulate::py_config()


reticulate::import("missingno")
