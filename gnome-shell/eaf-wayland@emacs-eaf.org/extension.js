import Gio from 'gi://Gio';

const EAFWaylandInterface = `
<node>
  <interface name="org.eaf.wayland">
    <method name="get_active_window">
      <arg name="active_window" type="s" direction="out" />
    </method>
    <method name="get_emacs_window_coordinate">
      <arg name="window_coordinate" type="s" direction="out" />
    </method>
  </interface>
</node>
`;

 export default class wayland {

  dbus;

  enable() {
    this.dbus = Gio.DBusExportedObject.wrapJSObject(
      EAFWaylandInterface,
      this,
    );
    this.dbus.export(
      Gio.DBus.session,
      '/org/eaf/wayland',
    );
  }

  disable() {
    this.dbus.unexport_from_connection(
      Gio.DBus.session,
    );
    this.dbus = undefined;
  }

  get_windows() {
    return global.get_window_actors()
           .map(w => ({id: w.toString(),
                       ref: w,
                       title: w.get_meta_window().get_wm_class()}))
           .filter(w => !w.title.includes('Gnome-shell'));
  }

  get_active_window() {
    return this.get_windows().slice(-1)[0].title
  }

  get_emacs_window_coordinate() {
    const match_windows = global.get_window_actors().filter(w => w.get_meta_window().get_wm_class() == "emacs")
    if (match_windows[0] == undefined) {
      return "0,0"
    } else {
      const emacs_window = match_windows[0]
      const rect = emacs_window.get_meta_window().get_frame_rect()
      return rect.x + "," + rect.y
    }
  }
}
