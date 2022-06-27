#!/bin/bash
set -e


# Reference from https://github.com/foundry-rs/foundry/blob/master/foundryup/install
need_cmd() {
  if ! check_cmd "$1"; then
    err "need '$1' (command not found)"
  fi
}

check_cmd() {
  command -v "$1" > /dev/null 2>&1
}

echo "Installing fe compiler to your system..."

need_cmd jq

FE_DIR=${FE_DIR-"$HOME/.fe"}
FE_BIN_DIR="$FE_DIR/bin"
FE_BIN="$FE_BIN_DIR/fe"

mkdir -p $FE_BIN_DIR
echo $FE_DIR
echo $FE_BIN_DIR
echo $FE_BIN

OS=$(uname -s)

echo "Your OS is $OS"

if [ $(uname -s) = "Linux" ]; then
    ARCHITECTURE="amd64" 
else
    ARCHITECTURE="mac" 
fi


VERSION=$(curl --silent  https://api.github.com/repos/ethereum/fe/releases | jq -r 'map(select(.prerelease)) | first | .tag_name')
echo "Installing latest version $VERSION..."


FE_LINK=https://github.com/ethereum/fe/releases/download/${VERSION}/fe_$ARCHITECTURE

echo $FE_LINK
curl -L $FE_LINK -o $FE_BIN
chmod +x $FE_BIN

case $SHELL in
*/zsh)
    PROFILE=$HOME/.zshrc
    ;;
*/bash)
    PROFILE=$HOME/.bashrc
    ;;
*/fish)
    PROFILE=$HOME/.config/fish/config.fish
    ;;
*)
    echo "Cannot detect shell in your system. Please manually add ${FE_BIN_DIR} to your PATH!"
    exit 1
esac

echo "Add fe dir to you PATH"
if [[ ":$PATH:" != *":${FE_BIN_DIR}:"* ]]; then
    echo >> $PROFILE && echo "export PATH=\"\$PATH:$FE_BIN_DIR\"" >> $PROFILE
fi

if [[ "$OSTYPE" =~ ^darwin && ! -f /usr/local/opt/libusb/lib/libusb-1.0.0.dylib ]]; then
    echo && echo "warning: libusb not found. You may need to install it manually on MacOS via Homebrew (brew install libusb)."
fi

echo "Install complete... Please start new terminal or run 'source $PROFILE' to reset shell env"



