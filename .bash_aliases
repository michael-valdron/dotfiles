#!/bin/bash
# helper values
TZ=$(timedatectl status | grep -Po "(?<=Time zone: )(.+)(?= \()")

# aliases
alias clear-recent='rm ~/.local/share/recently-used.xbel'

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

# OneDrive
alias odrive-mount='rclone mount onedrive:/ "${HOME}/GDrives/OneDrive Stream/" &'
alias odrive-umount='fusermount -u "${HOME}/GDrives/OneDrive Stream"'
alias odrive-push='rclone sync "${PWD}" "onedrive:${1}" -v -u --checkers 1 --transfers 1'
alias odrive-pull='rclone sync "onedrive:${1}" "${PWD}" -v -u --checkers 1 --transfers 1'

## Duplicity ##
alias backup='DUPLICITY_ENC_PASSWORD=$(cat ~/.passwords/backup.key) sh ~/.scripts/duplicity/backup.sh'
alias restore='DUPLICITY_ENC_PASSWORD=$(cat ~/.passwords/backup.key) sh ~/.scripts/duplicity/restore.sh'

## pCloud ##
alias pcloud='/opt/pcloud/pcloud'
alias install-pcloud='sh ~/.scripts/installers/packages/pcloud/fedora_install.sh'
alias remove-pcloud='sh ~/.scripts/installers/packages/pcloud/remove.sh'

## VPN ##
# alias home-vpn-connect='sudo openvpn --config ~/client.ovpn --daemon'
# alias home-vpn-disconnect='sudo killall openvpn'

## SSH ##
# Edit SSH Config
alias ssh-config='${EDITOR} ~/.ssh/config'

## GCP ##
alias gce='gcloud compute'

## SSHFS ##

## Anaconda ##
# envs
#alias conda='~/anaconda3/bin/conda'
#alias activate='~/anaconda3/bin/activate'
#alias deactivate='~/anaconda3/bin/deactivate'

## Clojure ##
alias clj='clojure'

## Go ##
alias gom='go mod'
alias go-init='go mod init'
alias go-tidy='go mod tidy'
alias go-vendor='go mod vendor'
alias go-tests='go test ./...'

## QEMU ##
alias qemu-mount='sudo sh ~/.scripts/qemu/qemu-mount.sh'
alias qemu-umount='sudo sh ~/.scripts/qemu/qemu-umount.sh'

## Podman ##
alias podman-clean='podman rm --all --force && podman image rm --all --force && podman network rm $(podman network ls -q | grep -vE "podman")'
alias pm-clean='podman-clean'
alias pm='podman'

## Kubernetes ##
alias minikube-start='sh ~/.scripts/minikube/start.sh'
alias minikube-pm-start='MINIKUBE_DRIVER=podman minikube-start'
alias minikube-stop='sh ~/.scripts/minikube/stop.sh'
alias install-kubectl='sh ~/.scripts/installers/packages/kubectl/install.sh'
alias update-kubectl='sh ~/.scripts/installers/packages/kubectl/update.sh'
alias remove-kubectl='sh ~/.scripts/installers/packages/kubectl/remove.sh'
alias kv='kubectl version --short'
alias kgp='kubectl get pods'
alias kgr='kubectl get routes'
alias kn='kubectl config view --minify | grep namespace: | sed "s/namespace\://" | tr -d "[:space:]"'
alias kgn='kubectl get namespace'
alias kan='kubectl create namespace'
alias kcn='kubectl config set-context --current --namespace'
alias klogs='sh ~/.scripts/kubectl/logs.sh'

## Che ##
alias install-chectl='sh ~/.scripts/installers/packages/chectl/install.sh'
alias update-chectl='sh ~/.scripts/installers/packages/chectl/update.sh'
alias remove-chectl='sh ~/.scripts/installers/packages/chectl/remove.sh'
alias minikube-chestart='minikube start --cpus=4 --memory=10240 --vm=true --disk-size=50GB --kubernetes-version=v1.23.9 --addons=dashboard,ingress'
alias minikube-pm-chestart='minikube-chestart --driver=podman'
alias chestart='minikube-chestart && chectl server:deploy --platform minikube'
alias chestop='chectl server:stop && chectl server:delete -y && minikube-stop'
alias chestatus='chectl server:status'

## Podman ##
if [[ -z $(command -v docker 2> /dev/null) ]] && [[ ! -z $(command -v podman 2> /dev/null) ]];
then
	alias docker='podman'
fi
