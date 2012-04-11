# Make sure to have ocp-get-server running before launchin the tests
LOCALHOST       ?= 127.0.0.1

# never use /tmp
# should be absolute
TEST_DIR=/tmp
OPAM_ROOT        = $(TEST_DIR)/OPAM.TEST
OPAM_SERVER_ROOT = $(TEST_DIR)/OPAM.SERVER.TEST
BIN              = $(TEST_DIR)/bin

# ocp-get in the path should not be a requirement
ENV           = OCAMLRUNPARAM=b OPAM_ROOT=$(OPAM_ROOT) PATH=$(BIN):$(PATH)
OCPGET        = $(ENV) ocp-get --debug --root $(OPAM_ROOT)
OCPGET_SERVER = $(ENV) ocp-get-server --debug --root $(OPAM_SERVER_ROOT)

PACKAGES = P1-1 P1-2 P2-1 P2-1 P3-1~weird.version P4-1

ARCHIVES = $(PACKAGES:%=packages/%.tar.gz)

.PHONY: all upload

all: fresh init upload list install upload-new upgrade downgrade ocpbuild remove
	@

$(BIN)/ocp-get: ../ocp-get
	mkdir -p $(BIN)
	cp ../ocp-get $(BIN)/ocp-get

$(BIN)/ocp-get-server: ../ocp-get-server
	mkdir -p $(BIN)
	cp ../ocp-get-server $(BIN)/ocp-get-server

$(BIN)/ocp-build: ../_obuild/unixrun
	cp ../_obuild/unixrun $(BIN)/unixrun
	cp ../boot/ocp-build.boot $(BIN)/ocp-build.boot
	echo 'exec $(BIN)/unixrun $(BIN)/ocp-build.boot "$$@"' > $(BIN)/ocp-build
	chmod +x $(BIN)/ocp-build

runserver: fresh $(BIN)/ocp-get-server
	$(OCPGET_SERVER)

init: fresh $(BIN)/ocp-get
	$(OCPGET) init $(LOCALHOST)

ocpbuild: $(BIN)/ocp-build

upload: $(ARCHIVES) init
	cd packages && $(OCPGET) upload P1
	cd packages && $(OCPGET) upload P2
	cd packages && $(OCPGET) upload P3
	cd packages && $(OCPGET) upload P4

list: upload
	$(OCPGET) list

remove: upload
	cd packages && $(OCPGET) install P1
	cd packages && $(OCPGET) remove P1

install: remove upload
	$(OCPGET) install P1
	$(OCPGET) install P2
	$(OCPGET) install P3
	$(OCPGET) install P4

upload-new: install
	cd packages && $(OCPGET) upload P1-2.spec
	cd packages && $(OCPGET) upload P4-2.spec
	cd packages && $(OCPGET) upload P4-3.spec

upgrade: upload-new
	$(OCPGET) list
	$(OCPGET) upgrade
	$(OCPGET) list

downgrade: upgrade
	$(OCPGET) install P4-2
	$(OCPGET) list

ocpget_upload: fresh init ocpbuild
	cd packages/bootstrap && for i in *spec ; do $(OCPGET) upload $$i ; done

ocpget: ocpget_upload
	$(OCPGET) install ocpget

packages/%.tar.gz: packages/% packages/%/*
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf $(OPAM_ROOT) $(OPAM_SERVER_ROOT) $(BIN)
