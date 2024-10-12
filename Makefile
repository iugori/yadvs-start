NODE_VERSION := 20.18.0


help:
## help: Shows this list of commands
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' | sed -e 's/^/-/'


setup-dev-common:
	sudo apt update
	sudo apt install -y curl
	

setup-dev-fe: # setup-dev-common 
## setup-dev-fe: Installs the front-end development utilities (for WSL)
	bash ./ops/setup/install-nodejs.sh $(NODE_VERSION)


