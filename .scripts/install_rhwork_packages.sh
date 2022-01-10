#!/bin/sh

# Check if running as root
if [ "$EUID" -ne 0 ]
then 
    echo "Please run as root"
    exit 1
fi

# Add Visual Studio Code repository
rpm --import "https://packages.microsoft.com/keys/microsoft.asc"
sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'

# Add Docker repository
dnf -y install dnf-plugins-core
dnf config-manager \
    --add-repo \
    "https://download.docker.com/linux/centos/docker-ce.repo"

# Update packages
dnf -y update

# Install packages
dnf -y install neofetch cmatrix tmux htop zsh gcc gcc-c++ curl make cmake go java-11-openjdk code docker-ce docker-ce-cli containerd.io chromium \
    firefox flatpak keepassxc barrier gimp libreoffice calibre xournal clamav clamtk vlc sqlitebrowser p7zip p7zip-gui p7zip-plugins cheese \
    unzip wget libappindicator redhat-lsb-core bridge-utils openssl

# Install package groups
dnf -y groupinstall "Virtualization Host"

# Install Docker Compose
curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose
ln -s /usr/local/bin/docker-compose /usr/bin/docker-compose

# Install Minikube
curl -L "https://storage.googleapis.com/minikube/releases/latest/minikube-latest.x86_64.rpm" -o /tmp/minikube-latest.x86_64.rpm
rpm -Uvh /tmp/minikube-latest.x86_64.rpm

# Install odo
curl -L "https://mirror.openshift.com/pub/openshift-v4/clients/odo/latest/odo-linux-amd64" -o /usr/local/bin/odo
chmod +x /usr/local/bin/odo

# Install Mailspring
curl -L "https://updates.getmailspring.com/download?platform=linuxRpm" -o /tmp/mailspring.rpm
rpm -i --quiet /tmp/mailspring.rpm

# Install Etcher
mkdir -p /opt/balena-etcher
curl -L "https://github.com/balena-io/etcher/releases/download/v1.7.3/balena-etcher-electron-1.7.3.x86_64.rpm" -o /tmp/balena-etcher.rpm
rpm -i --quiet /tmp/balena-etcher.rpm

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

# Add flatpak remotes
flatpak remote-add --if-not-exists flathub "https://flathub.org/repo/flathub.flatpakrepo"

# Install flatpaks
## Install Postman
flatpak install -y flathub com.getpostman.Postman
## Install Skype
flatpak install -y flathub com.skype.Client
