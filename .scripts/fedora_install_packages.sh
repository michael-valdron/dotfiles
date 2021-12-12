#!/bin/sh

# Check if running as root
if [ "$EUID" -ne 0 ]
then 
    echo "Please run as root"
    exit 1
fi

# Enable fusion repositorties
dnf -y install "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm"
dnf -y install "https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm"

# Add Visual Studio Code repository
rpm --import "https://packages.microsoft.com/keys/microsoft.asc"
sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'

# Add Docker repository
dnf -y install dnf-plugins-core
dnf config-manager \
    --add-repo \
    "https://download.docker.com/linux/fedora/docker-ce.repo"

# Update packages
dnf -y update

# Install packages
dnf -y install neofetch cmatrix tmux htop ufw zsh gcc gcc-c++ curl make cmake go clojure code docker-ce docker-ce-cli containerd.io chromium firefox flatpak \
    keepassxc barrier gimp libreoffice calibre xournal clamav clamtk vlc sqlitebrowser p7zip p7zip-gui p7zip-plugins cheese @virtualization

# Install Docker Compose
curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose
ln -s /usr/local/bin/docker-compose /usr/bin/docker-compose

# Install Mailspring
curl -L "https://updates.getmailspring.com/download?platform=linuxRpm" -o /tmp/mailspring.rpm
rpm -i --quiet /tmp/mailspring.rpm

# Install Etcher
mkdir -p /opt/balena-etcher
curl -L "https://github.com/balena-io/etcher/releases/download/v1.7.1/balenaEtcher-1.7.1-x64.AppImage" -o /opt/balena-etcher/balena-etcher.AppImage
ln -s /opt/balena-etcher/balena-etcher.AppImage /usr/bin/etcher

# Install Zotero
mkdir -p /opt/zotero
curl -L "https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64&version=5.0.96.3" -o /tmp/zotero.tar.bz2
tar -xf /tmp/zotero.tar.bz2 -C /opt/zotero
ln -s /opt/zotero/zotero.desktop /usr/local/share/applications/zotero.desktop

# Add flatpak remotes
flatpak remote-add --if-not-exists flathub "https://flathub.org/repo/flathub.flatpakrepo"

# Install flatpaks
## Install Postman
flatpak install -y flathub com.getpostman.Postman
## Install MakeMKV
flatpak install -y flathub com.makemkv.MakeMKV
## Install OBS Studio
flatpak install -y flathub com.obsproject.Studio
## Install HandBrake
flatpak install -y flathub fr.handbrake.ghb
## Install Skype
flatpak install -y flathub com.skype.Client
## Install AnyDesk
flatpak install -y flathub com.anydesk.Anydesk
