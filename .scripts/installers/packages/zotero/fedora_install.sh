#!/bin/sh

# Check if running as root
if [ "$EUID" -ne 0 ]
then 
    echo "Please run as root"
    exit 1
fi

# Variables
VERSION="5.0.96.3"

# Create required directories if they don't exist
mkdir -p /tmp /opt/zotero

# Install Dependencies
dnf -y install curl which bzip2

# Download Zotero
curl -L "https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64&version=${VERSION}" -o /tmp/zotero.tar.bz2

# Install Zotero
tar -xf /tmp/zotero.tar.bz2 -C /tmp
mv /tmp/Zotero_linux-x86_64 /opt/zotero
cp $(dirname $0)/run.sh /opt/zotero/run.sh
chmod +x /opt/zotero/run.sh
ln -s /opt/zotero/run.sh /usr/bin/zotero
ln -s /opt/zotero/zotero.desktop /usr/local/share/applications/zotero.desktop

# Installation Check
ZOTERO_PATH=$(which zotero)
if [ -z "${ZOTERO_PATH}" ]
then
    echo "Zotero was not installed correctly."
    exit 206
fi