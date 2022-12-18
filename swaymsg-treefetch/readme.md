# Swaymsg-treefetch: Simple bash scripts to fetch wanted info using swaymsg

### Requirements
- Window Manager: sway
- Dependencies: swaymsg, jshon
- Execution permission. To acquire this, use following
```bash
sudo chmod +x <path to script>
```

## Swaymsg-rectfetcher

Fetch rect info of window with app_id.

This bash script could get the rect (i.e. absolute coordinates and size) info of the window with specified app_id (the first argument) in SwayWM.

 If multiple window matches, the focused window will be selected. 
 If no one is focused, the first one will be selected.

 Remove every '#' before "$pecho" in the script file to get verbose output.

### Usage
```bash
 $ ./swaymsg-rectfetcher.sh <app_id>
```

### Output format
```bash
 <x> <y>
```

## Swaymsg-rectfetcher

Fetch the app_id of focused window.

Remove every '#' before "$pecho" in the script file to get verbose output.

### Usage
```bash
 $ ./swaymsg-focusfetcher.sh
```

### Output format
```bash
 <app_id>
```

## Examples
```bash
$ ./swaymsg-rectfetcher.sh emacs
1080 1546
$ ./swaymsg-focusfetcher.sh
emacs
```
## LICENSE
MIT license
