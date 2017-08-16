INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	jbuilder build @install --dev

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	rm -rf _build

test:
	jbuilder runtest --dev

.PHONY: default install uninstall reinstall clean test
