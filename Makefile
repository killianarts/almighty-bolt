.PHONY: deps test compile-clean bench-null image bench-run bench-overhead bench-app bench-compile bench-report snapshots replay 7guis 7guis-smoke 7guis-repl-smoke 7guis-hot-smoke examples-smoke windows-smoke accessibility-smoke smoke clean

# ASDF looks only in this tree (project and vendored/), never in ~/common-lisp/.
SBCL = sbcl --noinform --non-interactive --no-sysinit --no-userinit
REGISTRY = --eval '(require :asdf)' \
	--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))'

# Optional runtime heap size, for example HEAP='--dynamic-space-size 4096'.
HEAP =
SBCL_APP = sbcl $(HEAP) --noinform --disable-debugger --no-sysinit --no-userinit

# vend stops on float-features naming a system only DotCL needs (dotcl-float).
# The converter guide's remedy is to take it out of the vendored .asd and go on.
deps:
	vend get || { sed -i '' 's/ (:feature :dotcl (:require :dotcl-float))//' vendored/float-features/float-features.asd && vend get; }

# Integration and end-to-end tests, the only kind for now: the
# real operating system, text engine and GPU, and the apps driven by events and replay scripts. No
# window is opened; make smoke does that.
test: compile-clean
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-7guis/acceptance")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-stress/acceptance")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-theme-designer")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-gui-designer")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-studio")'
	$(MAKE) emacs-test

# The project compiled from nothing, into a directory that is thrown away, without a warning of any
# kind (scripts/compile-clean.lisp). make test starts with it.
compile-clean:
	sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/compile-clean.lisp

# The window checks, which need a window server (a logged-in Mac) and nobody at the keyboard: the
# app opens, draws and rests; the REPL entry; a saved file made into the app again with its state.
smoke:
	$(MAKE) 7guis-smoke TASK=counter
	$(MAKE) 7guis-repl-smoke TASK=crud
	$(MAKE) 7guis-hot-smoke
	$(MAKE) examples-smoke
	$(MAKE) windows-smoke
	$(MAKE) accessibility-smoke
	$(MAKE) studio-smoke
	$(MAKE) emacs-smoke

# The null app in a window: the library's own cost. Run it from a terminal of your
# own and leave the window alone while it runs.
bench-null:
	$(SBCL_APP) $(REGISTRY) --load bench/null/run.lisp

# The benchmark protocol on the example replay scripts: REPS repetitions (10)
# after WARMUPS (3), a result record for each in results/ (OUT=dir), never edited, only
# superseded. bench-report prints the latest record of each run.
bench-run:
	WHICH=results $(SBCL) $(REGISTRY) --load bench/run.lisp

# What the instrumentation costs: the phases and build timing on and off, on the example scripts
# and on the null app through the host. Written to results/ as a record.
bench-overhead:
	WHICH=overhead $(SBCL) $(REGISTRY) --load bench/run.lisp

# Startup, idle CPU and frames, memory and image size of the executable running each
# task in a window (make image first). TASKS=counter,cells IDLE=10 REPS=5.
bench-app:
	WHICH=app $(SBCL) $(REGISTRY) --load bench/run.lisp

# How long the library takes to compile from nothing and after a change to one file, in new
# processes with empty fasl caches (REPS=3).
bench-compile:
	WHICH=compile $(SBCL) $(REGISTRY) --load bench/run.lisp

bench-report:
	WHICH=report $(SBCL) $(REGISTRY) --load bench/run.lisp

# A picture of each task after a few events: make snapshots OUT=/tmp/shots/
snapshots:
	OUT=$(OUT) sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/snapshot-apps.lisp

# An executable of the example apps: make image, then
# TASK=counter build/apps (MEASURE=1 reports to the process that started it).
image:
	sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/build-image.lisp

# A 7GUIs task in a window: make 7guis TASK=counter
# LOG=1 prints the presses, releases and keys that reach it and what is under the
# pointer. RECORD=session.script writes what you do as a replay script, after each press,
# release and key (make replay SCRIPT=session.script plays it back). 7guis-smoke opens it, draws,
# waits and reports, with nobody at the window, and says whether a key typed would reach it (the view
# is the first responder, the window is the key window and the application is active).
# HOT=1 makes the app again, in the same window, when code is compiled into the process or a
# source file is saved; the state the app keeps (state-let) stays.
7guis:
	TASK=$(TASK) LOG=$(LOG) RECORD=$(RECORD) HOT=$(HOT) $(SBCL_APP) $(REGISTRY) --load examples/7guis/run.lisp

# Play a replay script back with the real text engine and no window, and say what happened:
#   make replay SCRIPT=examples/7guis/replay/counter-clicks.script [FREEZE=golden.script]
# FREEZE plays the script twice, and if the frames are the same, writes a copy with a checkpoint
# after each event that holds the hash of the frame there, to check later runs against.
replay:
	SCRIPT=$(SCRIPT) FREEZE=$(FREEZE) sbcl $(HEAP) --noinform --non-interactive --no-sysinit --no-userinit --eval '(require :asdf)' --eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))' --load scripts/replay.lisp

7guis-smoke:
	SMOKE=1 TASK=$(TASK) $(SBCL_APP) $(REGISTRY) --load examples/7guis/run.lisp

# What an editor's REPL does: open the task in a window and get the REPL back, compile a
# definition and see the app made again, close the window and keep the Lisp, open it again.
# It is judged by what it says, not by its exit code: AppKit's quit ends a process with code 0.
7guis-repl-smoke:
	@out=$$(mktemp); \
	TASK=$(TASK) $(SBCL_APP) $(REGISTRY) --load examples/7guis/repl-smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The REPL check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# What saving a file does, in a real window: the CRUD app is used, its file is saved with a change,
# and the app must be made again with its entries, the keyboard and the caret where they were
# (examples/7guis/hot-smoke.lisp). Judged by what it says, as 7guis-repl-smoke is. It writes
# examples/7guis/crud/app.lisp and puts it back.
7guis-hot-smoke:
	@out=$$(mktemp); \
	$(SBCL_APP) $(REGISTRY) --load examples/7guis/hot-smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The hot reload check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# The same for a program that uses only the public package: run-app on the main thread, the example
# used, redefined while it runs, worked as an agent works it (the machine interface; the MCP server over
# a socket and over nc; a screenshot that macOS decodes), and closed (examples/smoke.lisp). Judged by
# what it says.
examples-smoke:
	@out=$$(mktemp); \
	$(SBCL_APP) $(REGISTRY) --load examples/smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The run-app check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# More than one window, in real windows: a second opened by a button of the first and drawn when the
# first changes what it shows, one opened from the REPL, both made again when code is compiled, a
# window closing with the one it belongs to, Quit closing them all and leaving the Lisp, and the loop
# starting again (examples/windows-smoke.lisp). Judged by what it says.
windows-smoke:
	@out=$$(mktemp); \
	$(SBCL_APP) $(REGISTRY) --load examples/windows-smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The windows check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# What a screen reader gets, in a real window: the to-do example read, and pressed and focused, through
# the system's accessibility API (AXUIElement), as VoiceOver does it (examples/accessibility-smoke.lisp). The
# process must be allowed to use that API; if it is not, the check says so and is skipped. Judged by
# what it says.
accessibility-smoke:
	@out=$$(mktemp); \
	$(SBCL_APP) $(REGISTRY) --load examples/accessibility-smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q -e "The accessibility check passed" -e "The accessibility check was skipped" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# The studio in a real window, read and worked through the system's accessibility API as VoiceOver
# does: the counter in the App pane at 100% and 200%, made again when its code is compiled, and a pane
# moved by its tab's menu (apps/studio/smoke.lisp). Skipped, saying why, as the accessibility check is.
studio-smoke:
	@out=$$(mktemp); \
	$(SBCL_APP) $(REGISTRY) --load apps/studio/smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q -e "The studio check passed" -e "The studio check was skipped" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# The Emacs side of the bridge to the studio (emacs/almighty-bolt.el), in real buffers, with ERT in
# a batch Emacs: what it does with what the image asks, and what it tells the image. Part of make
# test; skipped, saying so, where there is no emacs.
emacs-test:
	@if command -v emacs >/dev/null 2>&1; then \
	  emacs --batch -Q -L emacs -l emacs/almighty-bolt-tests.el -f ert-run-tests-batch-and-exit; \
	else echo "The Emacs tests were skipped: there is no emacs on the PATH."; fi

# The bridge to Emacs end to end, over a real Sly: a Lisp with a Slynk server and a batch Emacs
# connected to it (emacs/smoke.lisp, emacs/smoke.el). SLY= is the directory of sly.el and slynk/,
# found in Doom's or package.el's place if not given. Skipped, saying why, without Emacs or Sly.
emacs-smoke:
	@out=$$(mktemp); \
	SLY="$(SLY)" $(SBCL_APP) $(REGISTRY) --load emacs/smoke.lisp > $$out 2>&1; \
	grep -e '^ok' -e '^FAIL' -e 'Emacs bridge check' $$out; \
	grep -q -e "The Emacs bridge check passed" -e "The Emacs bridge check was skipped" $$out && ! grep -q "^FAIL" $$out; status=$$?; rm -f $$out; exit $$status

clean:
	find . -name '*.fasl' -not -path './vendored/*' -delete
