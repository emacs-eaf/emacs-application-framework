#!/usr/bin/env python3

import argparse
import os
import sys
import subprocess
from shutil import which
import json

script_path = os.path.dirname(os.path.realpath(__file__))

parser = argparse.ArgumentParser()
parser.add_argument("--install-all-apps", action="store_true",
                    help='install all available applications')
parser.add_argument("--install-core-deps", action="store_true",
                    help='only install core dependencies')
parser.add_argument("--ignore-core-deps", action="store_true",
                    help='ignore core dependencies')
parser.add_argument("--install-app", nargs='+', default=[],
                    help='only install apps listed here')
parser.add_argument("--ignore-sys-deps", action="store_true",
                    help='ignore system dependencies')
parser.add_argument("--ignore-py-deps", action="store_true",
                    help='ignore python dependencies')
parser.add_argument("--ignore-node-deps", action="store_true",
                    help='ignore node dependencies')
parser.add_argument("--app-git-full-clone", action="store_true",
                    help='apps to conduct a full clone to preserve git logs')
parser.add_argument("--use-gitee", action="store_true",
                    help='use gitee mirror instead of github')
args = parser.parse_args()

def run_command(command, path=script_path, ensure_pass=True):
    print("[EAF] Running", ' '.join(command), "@", path)
    process = subprocess.Popen(command, stdin = subprocess.PIPE, text=True, cwd=path)
    process.wait()
    if process.returncode != 0 and ensure_pass:
        print(process.stderr)
        sys.exit(process.returncode)
    return process

def install_sys_deps(distro: str, deps_list):
    command = []
    if distro == 'pacman':
        command = ['yay', '-Sy', '--noconfirm', '--needed']
    elif distro == 'apt':
        command = ['sudo', 'apt', '-y', 'install']
    elif which("dnf"):
        command = ['sudo', 'dnf', '-y', 'install']
    elif which("pkg"):
        command = ['doas', 'pkg', '-y', 'install']
    command.extend(deps_list)
    return run_command(command)

def install_py_deps(deps_list):
    command = ['pip', 'install', '--user']
    command.extend(deps_list)
    return run_command(command)

def install_npm_install(app_path_list):
    for app_path in app_path_list:
        command = ["npm", "install"]
        run_command(command, path=app_path)

def install_npm_rebuild(app_path_list):
    for app_path in app_path_list:
        command = ["npm", 'rebuild']
        run_command(command, path=app_path)

def install_vue_install(app_path_list):
    for app_path in app_path_list:
        command = ["npm", 'install']
        run_command(command, path=app_path)
        command = ["npm", 'run', 'build']
        run_command(command, path=app_path)

def add_or_update_app(app: str, app_spec_dict):
    url = ""
    path = os.path.join("app", app)
    if args.use_gitee:
        url = app_spec_dict['gitee']
    else:
        url = app_spec_dict['github']

    if os.path.exists(path):
        print("\n[EAF] Updating", app, "to newest version...")
    else:
        print("\n[EAF] Adding", app, "application to EAF...")

    if os.path.exists(path):
        run_command(["git", "pull", "origin", "master"], path=path, ensure_pass=False)
    elif args.app_git_full_clone:
        run_command(["git", "clone", "--branch", "master", url, path])
    else:
        run_command(["git", "clone", "--depth", "1", "--branch", "master", url, path])

def get_distro():
    if which("pacman"):
        distro = "pacman"
        if (not args.ignore_core_deps and not args.ignore_sys_deps and len(args.install_app) == 0) or args.install_core_deps:
            run_command(['sudo', 'pacman', '-Sy', '--noconfirm', '--needed', 'yay'])
    elif which("apt"):
        distro = "apt"
    elif which("dnf"):
        distro = "dnf"
    elif which("pkg"):
        distro = "pkg"
    elif sys.platform == "linux":
        print("[EAF] Unsupported Linux distribution/package manager.")
        print(" Please see dependencies.json for list of dependencies.")
        sys.exit(1)
    return distro

def install_core_deps(distro, deps_dict):
    print("[EAF] Installing core dependencies")
    core_deps = []
    if not args.ignore_sys_deps and sys.platform == "linux":
        core_deps.extend(deps_dict[distro])
        if len(core_deps) > 0:
            install_sys_deps(distro, core_deps)
    if not args.ignore_py_deps or sys.platform != "linux":
        install_py_deps(deps_dict["pip"][sys.platform])
    if not args.ignore_node_deps:
        run_command(["npm", "install"])
    print("[EAF] Finished installing core dependencies")

def yes_no(question, default_yes=False, default_no=False):
    key = input(question)
    if default_yes:
        return key.lower() == 'y' or key == ""
    elif default_no:
        return key.lower() == 'y' or not (key == "" or key.lower() == 'n')
    else:
        return key.lower() == 'y'

def install_app_deps(distro, deps_dict):
    print("[EAF] Installing application dependencies")
    with open(os.path.join(script_path, 'applications.json')) as f:
        app_dict = json.load(f)

    prev_app_choices = []
    prev_app_choices_file = os.path.join(script_path, '.eaf-installed-apps.json')
    if os.path.exists(prev_app_choices_file) and os.stat(prev_app_choices_file).st_size > 0:
        with open(prev_app_choices_file) as f:
            prev_app_choices = json.load(f)

    use_prev_choices = False
    if not args.install_all_apps and len(args.install_app) == 0:
        if len(prev_app_choices) > 0:
            print("[EAF] Found these existing EAF applications:")
            for app in prev_app_choices:
                print("[EAF]", app)
            use_prev_choices = not yes_no("[EAF] Want something new? (y/N): ", default_no=True)
        if not use_prev_choices:
            args.install_all_apps = yes_no("[EAF] Install all available EAF applications? (Y/n): ", default_yes=True)

    if not args.install_all_apps and use_prev_choices:
        app_dict = {k: app_dict[k] for k in prev_app_choices}

    sys_deps = []
    py_deps = []
    npm_install_apps = []
    vue_install_apps = []
    npm_rebuild_apps = []
    for app_name, app_spec_dict in app_dict.items():
        install_this_app = False
        if not args.install_all_apps:
            if len(args.install_app) > 0 and app_name in args.install_app:
                install_this_app = True
            elif len(args.install_app) == 0 and not (use_prev_choices and app_name in prev_app_choices):
                install_this_app = yes_no("[EAF] " + app_spec_dict["name"] + ". Install? (y/N): ", default_no=True)
            elif use_prev_choices and app_name in prev_app_choices:
                install_this_app = True

        if args.install_all_apps or install_this_app:
            prev_app_choices.append(app_name)
            add_or_update_app(app_name, app_spec_dict)
            app_path = os.path.join(script_path, "app", app_name)
            app_dep_path = os.path.join(app_path, 'dependencies.json')
            if os.path.exists(app_dep_path):
                with open(os.path.join(app_dep_path)) as f:
                    deps_dict = json.load(f)
                if not args.ignore_sys_deps and sys.platform == "linux" and distro in deps_dict:
                    sys_deps.extend(deps_dict[distro])
                if not args.ignore_py_deps and 'pip' in deps_dict and sys.platform in deps_dict['pip']:
                    py_deps.extend(deps_dict['pip'][sys.platform])
                if not args.ignore_node_deps:
                    if 'npm_install' in deps_dict and deps_dict['npm_install']:
                        npm_install_apps.append(app_path)
                    if 'vue_install' in deps_dict and deps_dict['vue_install']:
                        vue_install_apps.append(app_path)
                    if 'npm_rebuild' in deps_dict and deps_dict['npm_rebuild']:
                        npm_rebuild_apps.append(app_path)

    print("[EAF] Installing dependencies for chosen applications")
    if not args.ignore_sys_deps and sys.platform == "linux" and len(sys_deps) > 0:
        print("[EAF] Installing system dependencies for installed applications")
        install_sys_deps(distro, sys_deps)
    if not args.ignore_py_deps and len(py_deps) > 0:
        print("[EAF] Installing python dependencies for installed applications")
        install_py_deps(py_deps)
    if not args.ignore_node_deps:
        if len(npm_install_apps) > 0:
            install_npm_install(npm_install_apps)
        if len(npm_rebuild_apps) > 0:
            install_npm_rebuild(npm_rebuild_apps)
        if len(vue_install_apps) > 0:
            install_vue_install(vue_install_apps)

    with open(prev_app_choices_file, 'w') as f:
        json.dump(list(set(prev_app_choices)), f)

    print("[EAF] Finished installing application dependencies")

def main():
    distro = get_distro()
    with open(os.path.join(script_path, 'dependencies.json')) as f:
        deps_dict = json.load(f)

    if (not args.ignore_core_deps and len(args.install_app) == 0) or args.install_core_deps:
        print("[EAF] ------------------------------------------")
        install_core_deps(distro, deps_dict)
        print("[EAF] ------------------------------------------")

    if not args.install_core_deps:
        print("[EAF] ------------------------------------------")
        install_app_deps(distro, deps_dict)
        print("[EAF] ------------------------------------------")

    print("[EAF] install-eaf.py finished.")


if __name__ == '__main__':
    main()
