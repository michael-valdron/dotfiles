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

# Add GCP SDK repository
sudo tee -a /etc/yum.repos.d/google-cloud-sdk.repo << EOM
[google-cloud-sdk]
name=Google Cloud SDK
baseurl=https://packages.cloud.google.com/yum/repos/cloud-sdk-el8-x86_64
enabled=1
gpgcheck=1
repo_gpgcheck=0
gpgkey=https://packages.cloud.google.com/yum/doc/yum-key.gpg
       https://packages.cloud.google.com/yum/doc/rpm-package-key.gpg
EOM

# Update packages
dnf -y update

# Install packages
dnf -y install neofetch cmatrix tmux htop ufw zsh gcc gcc-c++ curl make cmake go java-11-openjdk clojure code chromium podman podman-docker podman-compose \
    firefox flatpak keepassxc gimp libreoffice calibre xournalpp clamav clamtk vlc sqlitebrowser p7zip p7zip-gui p7zip-plugins cheese @virtualization \
    unzip wget libappindicator redhat-lsb-core google-cloud-sdk bridge-utils openssl

# Install Minikube
curl -L "https://storage.googleapis.com/minikube/releases/latest/minikube-latest.x86_64.rpm" -o /tmp/minikube-latest.x86_64.rpm
rpm -Uvh /tmp/minikube-latest.x86_64.rpm

# Install odo
curl -L "https://mirror.openshift.com/pub/openshift-v4/clients/odo/latest/odo-linux-amd64" -o /usr/local/bin/odo
chmod +x /usr/local/bin/odo

# Install Waterfox
curl -L "https://github.com/WaterfoxCo/Waterfox/releases/download/G4.0.4/waterfox-G4.0.4.en-US.linux-x86_64.tar.bz2" -o /tmp/waterfox.tar.bz2
tar -xf /tmp/waterfox.tar.bz2 -C /tmp
mv /tmp/waterfox /opt/waterfox
chmod +x /opt/waterfox/waterfox
ln -s /opt/waterfox/waterfox /usr/bin/waterfox

# Install Mailspring
curl -L "https://updates.getmailspring.com/download?platform=linuxRpm" -o /tmp/mailspring.rpm
rpm -i --quiet /tmp/mailspring.rpm

# Install Etcher
mkdir -p /opt/balena-etcher
curl -L "https://github.com/balena-io/etcher/releases/download/v1.7.1/balenaEtcher-1.7.1-x64.AppImage" -o /opt/balena-etcher/balena-etcher.AppImage
chmod +x /opt/balena-etcher/balena-etcher.AppImage
ln -s /opt/balena-etcher/balena-etcher.AppImage /usr/bin/etcher

# Install Zotero
mkdir -p /opt/zotero
curl -L "https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64&version=5.0.96.3" -o /tmp/zotero.tar.bz2
tar -xf /tmp/zotero.tar.bz2 -C /tmp
mv /tmp/Zotero_linux-x86_64 /opt/zotero
ln -s /opt/zotero/zotero.desktop /usr/local/share/applications/zotero.desktop

# Install Gradle
mkdir -p /opt/gradle
curl -L "https://downloads.gradle-dn.com/distributions/gradle-7.0-bin.zip" -o /tmp/gradle.zip
unzip /tmp/gradle.zip -d /opt/gradle
mv /opt/gradle/gradle-7.0/* /opt/gradle
rm -rf /opt/gradle/gradle-7.0
chmod +x /opt/gradle/bin/gradle
ln -s /opt/gradle/bin/gradle /usr/bin/gradle

# Install Leiningen
curl -L "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein" -o /usr/local/bin/lein
chmod +x /usr/local/bin/lein
ln -s /usr/local/bin/lein /usr/bin/lein

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
## Install Telegram
flatpak install -y flathub org.telegram.desktop
