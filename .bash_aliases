#!/bin/bash
# helper values
TZ=$(timedatectl status | grep -Po "(?<=Time zone: )(.+)(?= \()")

# aliases

## Private Aliases ##
if [ -f '~/.bash_private_aliases' ]; then
	source ~/.bash_private_aliases
fi

## GNOME ##
alias clear-recent='rm ~/.local/share/recently-used.xbel'

## tmux ##
# if tmux colour is not working..
# alias tmux='tmux -2'

## dnf ##
alias dnf-install='sh ~/.scripts/dnf/install.sh'
alias dnf-update='sh ~/.scripts/dnf/update.sh'
alias dnf-update-nokernel='sh ~/.scripts/dnf/update-nokernel.sh'
alias dnf-remove='sh ~/.scripts/dnf/remove.sh'

## flatpak ##
alias flatpak-version='flatpak --version'
alias flatpak-arch='flatpak --default-arch'
alias flatpak-archs='flatpak --supported-archs'
alias flatpak-gl='flatpak --gl-driver'
alias flatpak-installations='flatpak --installations'
alias flatpak-updated-env='flatpak --print-updated-env'
alias flatpak-sys-updated-env='flatpak --print-system-only'
alias flatpak-remotes='flatpak remotes'
alias flatpak-remote-add='flatpak remote-add'
alias flatpak-remote-modify='flatpak remote-modify'
alias flatpak-remote-del='flatpak remote-delete'
alias flatpak-remote-contents='flatpak remote-ls'
alias flatpak-remote-info='flatpak remote-info'
alias flatpak-list='flatpak list'
alias flatpak-list-user='flatpak list --user'
alias flatpak-list-sys='flatpak list --system'
alias flatpak-info='flatpak info'
alias flatpak-ps='flatpak ps'
alias flatpak-search='flatpak search'
alias flatpak-install='flatpak install --user ${FLATPAK_REPO:-flathub}'
alias flatpak-sys-install='sudo flatpak install ${FLATPAK_REPO:-flathub}'
alias flatpak-update='flatpak update --user'
alias flatpak-sys-update='sudo flatpak update'
alias flatpak-remove='flatpak uninstall --user'
alias flatpak-sys-remove='sudo flatpak uninstall'

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

## SSH ##
# Edit SSH Config
alias ssh-config='${EDITOR} ~/.ssh/config'

## GCP ##
alias gce='gcloud compute'

## Clojure ##
alias clj='clojure'

## Go ##
alias gom='go mod'
alias go-init='go mod init'
alias go-tidy='go mod tidy'
alias go-vendor='go mod vendor'
alias go-tests='go test ./...'

## git ##
alias git-push-all='for remote in `git remote`; do git push $remote; done'
alias git-pull='git pull --recurse-submodules'

## yadm ##
alias yadm-push-all='for remote in `yadm remote`; do yadm push $remote; done'
alias yadm-pull='yadm pull --recurse-submodules'

## QEMU ##
alias qemu-mount='sudo sh ~/.scripts/qemu/qemu-mount.sh'
alias qemu-umount='sudo sh ~/.scripts/qemu/qemu-umount.sh'

## Podman ##
alias podman-clean='podman rm --all --force && podman image rm --all --force && podman network rm $(podman network ls -q | grep -vE "podman")'
alias pm-clean='podman-clean'
alias pm='podman'
if [[ -z $(command -v docker 2> /dev/null) ]] && [[ ! -z $(command -v podman 2> /dev/null) ]];
then
	alias docker='podman'
fi

## Kubernetes ##
alias minikube-start='sh ~/.scripts/minikube/start.sh'
alias minikube-pm-start='MINIKUBE_DRIVER=podman minikube-start'
alias minikube-stop='sh ~/.scripts/minikube/stop.sh'
alias minikube-dash='minikube dashboard'
alias install-kubectl='sh ~/.scripts/installers/packages/kubectl/install.sh'
alias update-kubectl='sh ~/.scripts/installers/packages/kubectl/update.sh'
alias remove-kubectl='sh ~/.scripts/installers/packages/kubectl/remove.sh'
alias kversion='kubectl version 2> /dev/null'
alias kdeployments='kubectl get deploy'
alias kconfigmaps='kubectl get configmap'
alias kpods='kubectl get pods'
alias kservices='kubectl get service'
alias kingress='kubectl get ingress'
alias knamespace='kubectl config view --minify | grep namespace: | sed "s/namespace\://" | tr -d "[:space:]"'
alias knamespaces='kubectl get namespace'
alias knamespaceadd='kubectl create namespace'
alias kchnamespace='kubectl config set-context --current --namespace'
alias klogs='sh ~/.scripts/kubectl/logs.sh'

## OpenShift Console CLI (oc) ##
alias oversion='oc version'
alias odeployments='oc get deploy'
alias oconfigmaps='oc get configmap'
alias opods='oc get pods'
alias oservices='oc get service'
alias oroutes='oc get route'
alias onamespace='sh ~/.scripts/oc/project.sh'
alias onamespaces='oc projects'
alias onamespaceadd='oc create namespace'
alias ochnamespace='sh ~/.scripts/oc/project.sh --change'

## OpenShift Installer (openshift-install) ##
alias oinstall-version='openshift-install version'
alias oinstall-create-cluster='sh ~/.scripts/openshift-install/create.sh cluster'
alias oinstall-destroy-cluster='sh ~/.scripts/openshift-install/destroy.sh cluster'
alias oinstall-create-install-config='sh ~/.scripts/openshift-install/create.sh install-config'
alias oinstall-config-edit='sh ~/.scripts/openshift-install/config_edit.sh'

## Che ##
alias install-chectl='sh ~/.scripts/installers/packages/chectl/install.sh'
alias update-chectl='chectl update --channel="${CHECTL_CHANNEL:-stable}"'
alias remove-chectl='sh ~/.scripts/installers/packages/chectl/remove.sh'
alias minikube-chestart='minikube start --cpus=4 --memory=10240 --vm=true --disk-size=50GB --kubernetes-version=v1.23.9 --addons=dashboard,ingress'
alias minikube-pm-chestart='minikube-chestart --driver=podman'
alias chestart='sh ~/.scripts/chectl/start.sh'
alias chestop='sh ~/.scripts/chectl/stop.sh'
alias chestatus='chectl server:status'

## yq ##
alias install-yq='go install github.com/mikefarah/yq/v4@"${YQ_VERSION:-latest}"'

## sdkman ##
alias sdkman-install-check='bash ~/.scripts/installers/packages/sdkman/check_install.sh'
alias sdkman-install='bash ~/.scripts/installers/packages/sdkman/install.sh'
alias sdkman-remove='bash ~/.scripts/installers/packages/sdkman/remove.sh'

## gradle ##
alias install-gradle='sdk install gradle'
alias remove-gradle='sdk uninstall gradle'
