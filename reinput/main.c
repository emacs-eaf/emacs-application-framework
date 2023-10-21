#include <ctype.h>
#include <fcntl.h>
#include <inttypes.h>
#include <libinput.h>
#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <libudev.h>
#include <libevdev/libevdev.h>
#include <libevdev/libevdev-uinput.h>
#include <pthread.h>
#include <poll.h>
#include <stdbool.h>

#define MAX_KEY_CODE 248

char command[50];
unsigned short focus_on_eaf;

static int open_restricted(const char *path, int flags, void *user_data)
{
	bool *grab = user_data;
	int fd = open(path, flags);

	grab && *grab && ioctl(fd, EVIOCGRAB, (void*) 1);

	return fd < 0 ? -errno : fd;
}

static void close_restricted(int fd, void *user_data)
{
	close(fd);
}

struct libinput *li;
struct libinput_event *ev;
struct libinput_event_keyboard *kev;
const static struct libinput_interface interface = {
	.open_restricted = open_restricted,
	.close_restricted = close_restricted,
};
char kbd_devnode[5][19] = {0};
int8_t kbd_devnode_n = 0;

static void get_kbd_device(void)
{
	struct udev *udev = udev_new();
	struct udev_device *udev_device;
	struct libinput_device *dev;
	const char *devnode;

	li = libinput_udev_create_context(&interface, NULL, udev);
	libinput_udev_assign_seat(li, "seat0");

	libinput_dispatch(li);
	while ((ev = libinput_get_event(li))) {
		dev = libinput_event_get_device(ev);

		if (libinput_device_has_capability(dev, LIBINPUT_DEVICE_CAP_KEYBOARD)
		    && libinput_device_keyboard_has_key(dev, KEY_A)
		    && libinput_event_get_type(ev) == LIBINPUT_EVENT_DEVICE_ADDED
		    && kbd_devnode_n < 5) {
			udev_device = libinput_device_get_udev_device(dev);
			devnode = udev_device_get_devnode(udev_device);

			strcpy(kbd_devnode[kbd_devnode_n], devnode);
			kbd_devnode_n++;

			udev_device_unref(udev_device);
		}

		libinput_device_unref(dev);
		libinput_event_destroy(ev);
		libinput_dispatch(li);
	}

	udev_unref(udev);
	libinput_unref(li);
}

static void init_libinput(void)
{
	bool grab = 1;

	get_kbd_device();

	li = libinput_path_create_context(&interface, &grab);
	while (kbd_devnode_n--) {
		libinput_path_add_device(li, kbd_devnode[kbd_devnode_n]);
	}
}

struct libevdev *dev;
struct libevdev_uinput *uidev;

static void init_libevdev(void)
{
	dev = libevdev_new();
	libevdev_set_name(dev, "reinput");
	libevdev_enable_event_type(dev, EV_KEY);

	for (uint8_t code = 0; code <= MAX_KEY_CODE; code++) {
		libevdev_enable_event_code(dev, EV_KEY, code, NULL);
	}

	libevdev_uinput_create_from_device(dev, LIBEVDEV_UINPUT_OPEN_MANAGED, &uidev);
}

static void focus_emacs(void)
{
	system(command);
}

static void reinput_key(void)
{
	if (focus_on_eaf) {
		focus_emacs();
		focus_on_eaf = 0;
	}

	uint32_t code = libinput_event_keyboard_get_key(kev);
	enum libinput_key_state state = libinput_event_keyboard_get_key_state(kev);

	if (state == LIBINPUT_KEY_STATE_PRESSED) {
		libevdev_uinput_write_event(uidev, EV_KEY, code, 1);
	} else {
		libevdev_uinput_write_event(uidev, EV_KEY, code, 0);
	}

	libevdev_uinput_write_event(uidev, EV_SYN, SYN_REPORT, 0);
}

static void handle_input(void)
{
	libinput_dispatch(li);
	while ((ev = libinput_get_event(li))) {
		if (libinput_event_get_type(ev) == LIBINPUT_EVENT_KEYBOARD_KEY) {
			kev = libinput_event_get_keyboard_event(ev);
			reinput_key();
		}

		libinput_event_destroy(ev);
		libinput_dispatch(li);
	}
}

static void *listen(void *_)
{
	struct pollfd fds;

	fds.fd = libinput_get_fd(li);
	fds.events = POLLIN;
	fds.revents = 0;

	do {
		handle_input();
	} while (poll(&fds, 1, -1) > -1);

	return NULL;
}

static void init_focus_command(pid_t pid)
{
	char desktop[20];

	if (getenv("XDG_CURRENT_DESKTOP")) {
		strcpy(desktop, getenv("XDG_CURRENT_DESKTOP"));
	} else {
		strcpy(desktop, getenv("XDG_SESSION_DESKTOP"));
	}

	if (!strcmp(desktop, "sway")) {
		sprintf(command, "swaymsg '[pid=%d] focus'", pid);
	} else if (!strcmp(desktop, "Hyprland")) {
		sprintf(command, "hyprctl dispatch focuswindow pid:%d", pid);
	}
}

int main(int argc, const char **argv)
{
	pid_t pid;
	pthread_t thread;

	pid = atoi(argv[1]);

	init_focus_command(pid);
	init_libinput();
	init_libevdev();

	pthread_create(&thread, NULL, listen, NULL);

	focus_on_eaf = 0;
	while (1) {
		scanf("%hu", &focus_on_eaf);
	}
}
