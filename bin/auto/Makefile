mk = make-python-scripts.py

all:
#	@if [ -f $(mk) ]; then ./$(mk); else alisp -e '(setq *load-verbose* nil)' -L ~/.clinit.cl -e '(MAKE-PYTHON-SCRIPTS)' -kill; fi
	alisp -e '(setq *load-verbose* nil)' -L ~/.clinit.cl -e '(MAKE-PYTHON-SCRIPTS)' -kill
