## Run EAF with docker

If you prefer to run linux in a container, you can use below command to build an EAF container.

### Build
```Shell
docker build -t eaf  .
```

### Run
```Shell
xhost +local:root # WARN: this comes with security issues

docker run -e DISPLAY -e DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus --rm -it -v /tmp/.X11-unix/:/tmp/.X11-unix -v /run/user/$(id -u)/:/run/user/1000/ -v ~/.Xauthority:/home/eaf/.Xauthority   eaf
```
You can also reuse your own Emacs configuration in the container:

```Shell
xhost +local:root # WARN: this comes with security issues

# mount the Emacs configuration into the container
docker run -e DISPLAY -e DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus --rm -it -v /tmp/.X11-unix/:/tmp/.X11-unix -v /run/user/$(id -u)/:/run/user/1000/ -v ~/.Xauthority:/home/eaf/.Xauthority -v ~/.emacs.d:/home/eaf/.emacs.d  eaf
```
