## Run EAF with docker

If you prefer to run linux in a container, you can use below command to build an EAF container.

### Build
```Shell
docker build -t eaf -f Dockerfile.arch .
```

### Run
```Shell
xhost +local:root # WARN: this comes with security issues

docker run -e DISPLAY -e DBUS_SESSION_BUS_ADDRESS --rm -it -v /tmp/.X11-unix/:/tmp/.X11-unix -v /run/user/$(id -u)/:/run/user/1000/ -v ~/.Xauthority:/home/eaf/.Xauthority   eaf
```
