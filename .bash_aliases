#!/bin/bash
# helper values
TZ=$(timedatectl status | grep -Po "(?<=Time zone: )(.+)(?= \()")

# aliases

## tmux ##
# if tmux colour is not working..
# alias tmux='tmux -2'

## openssl ##
# alias encrypt='bash ~/.scripts/enc.sh'
# alias decrypt='bash ~/.scripts/dec.sh'

## nix ##
alias nix-search='nix --extra-experimental-features nix-command --extra-experimental-features flakes search nixpkgs'

## dconf ##
alias dconf-save-all='dconf-save-gterm && dconf-save-ginterface'
alias dconf-load-all='dconf-load-gterm && dconf-load-ginterface'

# Terminal
alias dconf-save-gterm='dconf dump /org/gnome/terminal/ > ~/.gterm.conf'
alias dconf-load-gterm='cat ~/.gterm.conf | dconf load /org/gnome/terminal/'

# Interface
alias dconf-save-ginterface='dconf dump /org/gnome/desktop/interface/ > ~/.ginterface.conf'
alias dconf-load-ginterface='cat ~/.ginterface.conf | dconf load /org/gnome/desktop/interface/'

## rclone ##
# GDrive
alias gdrive-mount='rclone mount gdrive:/ "${HOME}/GDrives/Google Drive Stream/" &'
alias gdrive-umount='fusermount -u "${HOME}/GDrives/Google Drive Stream"'
alias gdrive-push='rclone sync "${PWD}" "gdrive:${1}" -v -u --checkers 1 --transfers 1'
alias gdrive-pull='rclone sync "gdrive:${1}" "${PWD}" -v -u --checkers 1 --transfers 1'

# UOIT Drive
alias udrive-mount='rclone mount udrive:/ "${HOME}/GDrives/UOIT Drive Stream/" &'
alias udrive-umount='fusermount -u "${HOME}/GDrives/UOIT Drive Stream"'
alias udrive-push='rclone sync "${PWD}" "udrive:${1}" -v -u --checkers 1 --transfers 1'
alias udrive-pull='rclone sync "udrive:${1}" "${PWD}" -v -u --checkers 1 --transfers 1'

# OneDrive
alias odrive-mount='rclone mount onedrive:/ "${HOME}/GDrives/OneDrive Stream/" &'
alias odrive-umount='fusermount -u "${HOME}/GDrives/OneDrive Stream"'
alias odrive-push='rclone sync "${PWD}" "onedrive:${1}" -v -u --checkers 1 --transfers 1'
alias odrive-pull='rclone sync "onedrive:${1}" "${PWD}" -v -u --checkers 1 --transfers 1'

## VPN ##
# alias home-vpn-connect='sudo openvpn --config ~/client.ovpn --daemon'
# alias home-vpn-disconnect='sudo killall openvpn'

## SSH ##
# Edit SSH Config
alias ssh-config='${EDITOR} ~/.ssh/config'

# Home NAS
alias ssh-nas='ssh home-nas'

# dbsci
alias ssh-dbsci-sat='ssh dbsci-sat'
alias ssh-dbsci-twitter='ssh dbsci-twitter'

# rtrsci
alias ssh-rtrsci-dev='ssh rtrsci-dev'

# GCP
alias gce='gcloud compute'
# alias od-dev-start='gcloud compute instances start $OD_DEV_HOST --zone=us-central1-c'
# alias od-dev-ssh='gcloud compute ssh $OD_DEV_USER@$OD_DEV_HOST --zone=us-central1-c'
# alias od-dev-stop='gcloud compute instances stop $OD_DEV_HOST --zone=us-central1-c'

## SSHFS ##

# Home NAS
## Mount
alias sshfs-nas-home='sshfs home-nas:/data/home/${USER} /mnt/${USER}/home_nas/home'
alias sshfs-nas-apps='sshfs home-nas:/data/software /mnt/${USER}/home_nas/software'
alias sshfs-nas-videos='sshfs home-nas:/data/videos /mnt/${USER}/home_nas/videos'
alias sshfs-nas-shares='sshfs home-nas:/data/shares /mnt/${USER}/home_nas/shares'
alias sshfs-nas='sshfs-nas-home && sshfs-nas-apps && sshfs-nas-videos && sshfs-nas-shares'

## Unmount
alias um-nas-home='fusermount -u /mnt/${USER}/home_nas/home'
alias um-nas-apps='fusermount -u /mnt/${USER}/home_nas/software'
alias um-nas-videos='fusermount -u /mnt/${USER}/home_nas/videos'
alias um-nas-shares='fusermount -u /mnt/${USER}/home_nas/shares'
alias um-nas='um-nas-home && um-nas-apps && um-nas-videos && um-nas-shares'

## Anaconda ##
# envs
#alias conda='~/anaconda3/bin/conda'
#alias activate='~/anaconda3/bin/activate'
#alias deactivate='~/anaconda3/bin/deactivate'

## Clojure ##
# alias clj='clojure'

## QEMU ##
alias qemu-mount='sudo sh ~/.scripts/qemu/qemu-mount.sh'
alias qemu-umount='sudo sh ~/.scripts/qemu/qemu-umount.sh'

## Podman ##
PODMAN_PATH=$(which podman 2> /dev/null)
if [ -f "${PODMAN_PATH}" ];
then
	alias docker='podman'
fi
