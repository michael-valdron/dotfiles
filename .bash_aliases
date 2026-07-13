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

## apt ##
alias apt-install='sh ~/.scripts/util/apt/install.sh'
alias apt-update='sh ~/.scripts/util/apt/update.sh'
alias apt-update-nokernel='sh ~/.scripts/util/apt/update-nokernel.sh'
alias apt-remove='sh ~/.scripts/util/apt/remove.sh'

## dnf ##
alias dnf-install='sh ~/.scripts/util/dnf/install.sh'
alias dnf-update='sh ~/.scripts/util/dnf/update.sh'
alias dnf-update-nokernel='sh ~/.scripts/util/dnf/update-nokernel.sh'
alias dnf-remove='sh ~/.scripts/util/dnf/remove.sh'

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
alias backup='DUPLICITY_ENC_PASSWORD=$(cat ~/.passwords/backup.key) sh ~/.scripts/util/duplicity/backup.sh'
alias restore='DUPLICITY_ENC_PASSWORD=$(cat ~/.passwords/backup.key) sh ~/.scripts/util/duplicity/restore.sh'

## pCloud ##
alias pcloud='/opt/pcloud/pcloud'
alias install-pcloud='sh ~/.scripts/installers/packages/pcloud/fedora_install.sh'
alias remove-pcloud='sh ~/.scripts/installers/packages/pcloud/remove.sh'

## GrayJay ##
alias install-grayjay='sh ~/.scripts/installers/packages/grayjay/install.sh'
alias remove-grayjay='sh ~/.scripts/installers/packages/grayjay/remove.sh'

## SSH ##
# Edit SSH Config
alias ssh-config='${EDITOR} ~/.ssh/config'

## GCP ##
alias gce='gcloud compute'

## DevContainers ##
alias dev-shell='podman-compose run --rm dev'
alias dev-node='dev-shell node'
alias dev-npm='dev-shell npm'
alias dev-npx='dev-shell npx'
alias dev-yarn='dev-shell yarn'
alias dev-nvm='dev-shell nvm'

## Clojure ##
alias clj='clojure'

## Go ##
alias gom='go mod'
alias go-init='go mod init'
alias go-tidy='go mod tidy'
alias go-vendor='go mod vendor'
alias go-tests='go test ./...'

## Node.js ##
alias node='${CONTAINER_ENGINE} compose run --rm node node'
alias node-24='${CONTAINER_ENGINE} compose run --rm node-24 node'
alias node-22='${CONTAINER_ENGINE} compose run --rm node-22 node'
alias node-sh='${CONTAINER_ENGINE} compose run --rm node sh'
alias node-24-sh='${CONTAINER_ENGINE} compose run --rm node-24 sh'
alias node-22-sh='${CONTAINER_ENGINE} compose run --rm node-22 sh'
alias node-yarn='${CONTAINER_ENGINE} compose run --rm node yarn'
alias node-24-yarn='${CONTAINER_ENGINE} compose run --rm node-24 yarn'
alias node-22-yarn='${CONTAINER_ENGINE} compose run --rm node-22 yarn'
alias node-npx='${CONTAINER_ENGINE} compose run --rm node npx'
alias node-24-npx='${CONTAINER_ENGINE} compose run --rm node-24 npx'
alias node-22-npx='${CONTAINER_ENGINE} compose run --rm node-22 npx'
alias node-tsc='${CONTAINER_ENGINE} compose run --rm node tsc'
alias node-24-tsc='${CONTAINER_ENGINE} compose run --rm node-24 tsc'
alias node-22-tsc='${CONTAINER_ENGINE} compose run --rm node-22 tsc'

## git ##
alias git-push-all='for remote in `git remote`; do git push $remote; done'
alias git-pull='git pull --recurse-submodules'
alias git-rebase='bash ~/.scripts/util/git/rebase.sh'
alias git-download='sh ~/.scripts/util/git/download.sh'

## yadm ##
alias yadm-push-all='for remote in `yadm remote`; do yadm push $remote; done'
alias yadm-pull='yadm pull --recurse-submodules'
alias yadm-rebase='bash ~/.scripts/util/git/rebase.sh --yadm'

## QEMU ##
alias qemu-mount='sudo sh ~/.scripts/util/qemu/qemu-mount.sh'
alias qemu-umount='sudo sh ~/.scripts/util/qemu/qemu-umount.sh'

## Podman ##
alias podman-clean='podman rm --all --force && podman image rm --all --force && podman network rm $(podman network ls -q | grep -vE "podman")'
alias pm-clean='podman-clean'
alias pm='podman'
if [[ -z $(command -v docker 2> /dev/null) ]] && [[ ! -z $(command -v podman 2> /dev/null) ]];
then
	alias docker='podman'
fi

## Kubernetes ##
alias minikube-start='sh ~/.scripts/util/minikube/start.sh'
alias minikube-pm-start='MINIKUBE_DRIVER=podman minikube-start'
alias minikube-stop='sh ~/.scripts/util/minikube/stop.sh'
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
alias klogs='sh ~/.scripts/util/kubectl/logs.sh'

## OpenShift Console CLI (oc) ##
alias oversion='oc version'
alias odeployments='oc get deploy'
alias oconfigmaps='oc get configmap'
alias opods='oc get pods'
alias oservices='oc get service'
alias oroutes='oc get route'
alias onamespace='sh ~/.scripts/util/oc/project.sh'
alias onamespaces='oc projects'
alias onamespaceadd='oc create namespace'
alias ochnamespace='sh ~/.scripts/util/oc/project.sh --change'

## OpenShift Installer (openshift-install) ##
alias oinstall-version='openshift-install version'
alias oinstall-create-cluster='sh ~/.scripts/util/openshift-install/create.sh cluster'
alias oinstall-destroy-cluster='sh ~/.scripts/util/openshift-install/destroy.sh cluster'
alias oinstall-create-install-config='sh ~/.scripts/util/openshift-install/create.sh install-config'
alias oinstall-config-edit='sh ~/.scripts/util/openshift-install/config_edit.sh'

## Che ##
alias install-chectl='sh ~/.scripts/installers/packages/chectl/install.sh'
alias update-chectl='chectl update --channel="${CHECTL_CHANNEL:-stable}"'
alias remove-chectl='sh ~/.scripts/installers/packages/chectl/remove.sh'
alias minikube-chestart='minikube start --cpus=4 --memory=10240 --vm=true --disk-size=50GB --kubernetes-version=v1.23.9 --addons=dashboard,ingress'
alias minikube-pm-chestart='minikube-chestart --driver=podman'
alias chestart='sh ~/.scripts/util/chectl/start.sh'
alias chestop='sh ~/.scripts/util/chectl/stop.sh'
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

## leiningen ##
alias install-leiningen='sdk install leiningen'
alias remove-leiningen='sdk uninstall leiningen'

## maven ##
alias install-maven='sdk install maven'
alias remove-maven='sdk uninstall maven'

## sbt ##
alias install-sbt='sdk install sbt'
alias remove-sbt='sdk uninstall sbt'

## renovate ##
alias run-renovate='RENOVATE_USE_CONTAINER=true bash ~/.scripts/util/renovate/run.sh'

## llm ##
alias skills-sync='sh ~/.scripts/util/llm/skills/sync.sh'
alias skills-sync-cursor='skills-sync .cursor/skills'
alias skills-sync-cursor-user='skills-sync ~/.cursor/skills'
