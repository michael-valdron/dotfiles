# .profile

## Alias
## Podman ##
PODMAN_PATH=$(which podman 2> /dev/null)
if [ -f "${PODMAN_PATH}" ];
then
	alias docker='podman'
fi
