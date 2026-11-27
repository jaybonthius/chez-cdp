SCHEME ?= scheme
CAP ?=
CHROMIUM ?= chromium
JSON_DIR ?= ../chez-json
PROCESS_DIR ?= ../chez-process
TEST_DIR ?= ../chez-test
OBJ := build/obj
DEPENDENCIES := $(JSON_DIR)/build/obj/src:$(PROCESS_DIR)/build/obj/src:$(TEST_DIR)/build/obj/src
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib:$(DEPENDENCIES)
# Browsers the tests start keep their profiles here, so the check that none
# is left running looks only at the tests' own. Chromium puts a socket in
# the temporary directory and dies at start when its path is too long for
# one, so this cannot live under build/.
TEST_TMP ?= /tmp/chez-cdp-test-$(shell id -u)
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 600
TESTS := $(sort $(wildcard tests/*-test.sps))

.PHONY: deps build test clean $(TESTS:%=run/%)

deps:
	$(MAKE) -C $(JSON_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build
	$(MAKE) -C $(PROCESS_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build
	$(MAKE) -C $(TEST_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build:
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --libdirs $(DEPENDENCIES) --script build.ss

# One process per test file, so a crash or leaked global state stays inside
# its file. A file fails when it exits non-zero or writes to standard error.
# The bracket keeps pgrep from matching the shell that runs it.
test: $(TESTS:%=run/%)
	@if pgrep -f -- '[-]-user-data-dir=$(TEST_TMP)/' > /dev/null; then \
	  echo "test: a browser started by the tests is still running" >&2; exit 1; fi
	@rm -rf $(TEST_TMP)

$(TESTS:%=run/%): run/%: build
	@rm -rf $(TEST_TMP)/$(*F:.sps=) && mkdir -p $(TEST_TMP)/$(*F:.sps=)
	@err=$$(mktemp); \
	$(CAP) timeout $(TEST_TIMEOUT) env TMPDIR=$(TEST_TMP)/$(*F:.sps=) CHROMIUM=$(CHROMIUM) SCHEME=$(SCHEME) \
	  LIBDIRS='$(LIBDIRS)' $(SCHEME) --libdirs '$(LIBDIRS)' --program ./$(OBJ)/$(*:.sps=.so) 2> $$err; \
	status=$$?; cat $$err >&2; \
	if [ $$status -ne 0 ] || [ -s $$err ]; then rm -f $$err; echo "FAIL $*" >&2; exit 1; fi; \
	rm -f $$err

clean:
	rm -rf build
