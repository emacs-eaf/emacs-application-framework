#!/bin/bash

set -eu

if apt-get -v &> /dev/null; then
    sudo apt-get install git nodejs aria2 libreoffice wmctrl xdotool
    sudo apt-get install libglib2.0-dev libdbus-1-3 libdbus-1-dev
    # Missing in Ubuntu: filebrowser-bin
elif type pacman &> /dev/null; then
    sudo pacman -Sy --needed yay nodejs aria2 libreoffice wmctrl xdotool
    yay -S filebrowser-bin
else
    echo "Unsupported distribution."
    exit 1
fi

# Use .emacs.d if not present
export EMACS_ROOT="$XDG_CONFIG_HOME"/emacs

if [ ! -d "$EMACS_ROOT" ]; then
    export EMACS_ROOT="$HOME"/.emacs.d

    if [ ! -d "$EMACS_ROOT" ]; then
        echo "Could not determine the location of your Emacs config directory."
        exit 1
    fi
fi

export EAF_ROOT="$EMACS_ROOT"/eaf
mkdir -p "$EAF_ROOT"/git "$EAF_ROOT"/venv "$EAF_ROOT"/bin

if [ ! -d "$EAF_ROOT"/git/emacs-application-framework ]; then
    git clone https://github.com/manateelazycat/emacs-application-framework.git --depth=1 "$EAF_ROOT"/git/emacs-application-framework
fi

if type virtualenv &>/dev/null; then
    virtualenv "$EAF_ROOT"/venv/emacs-application-framework
else
    echo "Cannot find virtualenv. Please install it."
    exit 1
fi

source "$EAF_ROOT"/venv/emacs-application-framework/bin/activate

if type pip &>/dev/null; then
    pip install -r requirements.txt
elif type pip &>/dev/null; then
    pip3 install -r requirements.txt
else
    echo "Cannot find pip. Please install it !"
    exit 1
fi

LAUNCH_SCRIPT="$EAF_ROOT"/bin/emacs-eaf.sh
echo "Creating Emacs-EAF shortcut"
{
    echo "#!/bin/bash" ;
    echo "" ;
    echo "" ;
    echo "source $EAF_ROOT/venv/emacs-application-framework/bin/activate"
    echo "$HOME/.local/emacs/current/bin/emacs"
} > "$LAUNCH_SCRIPT"
chmod u+x "$LAUNCH_SCRIPT"

echo "EAF is now installed !"
echo "Remaining steps:"
echo "- Set EAF load-path to: $EAF_ROOT/git/emacs-application-framework"
echo "- Add $EAF_ROOT/bin to your PATH variable, e.g. export PATH=$EAF_ROOT/bin:\$PATH"
echo "- Launch EAF-enabled Emacs via emacs-eaf.sh"
