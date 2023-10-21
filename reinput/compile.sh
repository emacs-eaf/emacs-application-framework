#/bin/sh

gcc main.c -o reinput `pkg-config --cflags --libs libinput libevdev libudev`
