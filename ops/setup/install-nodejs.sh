#! /bin/bash


INSTALL_DIR=/tmp/yadvs-install
INSTALL_FILE=${INSTALL_DIR}/install-nvm.sh
NODE_VERSION=$1


mkdir -p ${INSTALL_DIR}
curl -o ${INSTALL_FILE} https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh
cat ${INSTALL_FILE} | bash
rm -rf ${INSTALL_DIR}
source ~/.nvm/nvm.sh
nvm install ${NODE_VERSION}
nvm use ${NODE_VERSION}
echo node.js $(node -v) installed in $(which node)
echo npm $(npm -v) installed in $(which npm)


# nvm unload
# rm -rf ~/.nvm
