.PHONY: all install

# Customisable
SCHEDULE=0 */4 * * * # any valid cron expression
CONFIG_FILE=$(XDG_CONFIG_HOME)/mirror-elpa.sh

# Non-customisable
path=$(PWD)/mirror-elpa
job:=$(SCHEDULE) $(path) $(CONFIG_FILE)
tmp:=$(shell mktemp)

all:
	# no-op
	# specified so that `make && make install` won't accidentally install twice

install:
	-crontab -l 2> /dev/null > $(tmp)
	@if grep -Eq "/mirror-elpa $(CONFIG_FILE)$$" $(tmp); then \
		echo "\033[1;33mWARN: crontab already contains a reference to mirror-elpa with $(CONFIG_FILE)."; \
		echo "This is likely to result in the job being run more often than" \
			"expected.\033[0m"; \
	fi
	echo "$(job)" >> $(tmp)
	crontab $(tmp)
