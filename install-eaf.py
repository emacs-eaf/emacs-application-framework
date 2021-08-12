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

def git_add_app(app: str, app_spec_dict):
    url = ""
    path = os.path.join("app", app)
    if args.use_gitee:
        url = app_spec_dict['gitee']
    else:
        url = app_spec_dict['github']

    if os.path.exists(path):
        run_command(["git", "pull", "origin", "master"], path=path)
    elif args.app_git_full_clone:
        run_command(["git", "clone", "--branch", "master", url, path])
    else:
        run_command(["git", "clone", "--depth", "1", "--branch", "master", url, path])

def main():


    distro = ""
    if which("pacman"):
        distro = "pacman"
        if not args.ignore_sys_deps:
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

    with open(os.path.join(script_path, 'dependencies.json')) as f:
        deps_dict = json.load(f)
    with open(os.path.join(script_path, 'applications.json')) as f:
        app_dict = json.load(f)

    core_deps = []
    if not args.ignore_sys_deps and sys.platform == "linux":
        core_deps.extend(deps_dict[distro])
        print("[EAF] Installing system dependencies")
        if len(core_deps) > 0:
            install_sys_deps(distro, core_deps)
        print("[EAF] Finished installing system dependencies")
    if not args.ignore_py_deps or sys.platform != "linux":
        install_py_deps(deps_dict["pip"][sys.platform])
    if not args.ignore_node_deps:
        run_command(["npm", "install"])

    if not args.install_all_apps:
        key = input("[EAF] Install all available EAF applications? (Y/n): ")
        args.install_all_apps = key.lower() == 'y' or key == ""

    sys_deps = []
    py_deps = []
    npm_install_apps = []
    vue_install_apps = []
    npm_rebuild_apps = []
    for app_name, app_spec_dict in app_dict.items():
        install_this_app = False
        if not args.install_all_apps:
            key = input("[EAF] " + app_spec_dict["name"] + ". Install? (y/N): ")
            install_this_app = key.lower() == 'y' or not (key == "" or key.lower() == 'n')
        if args.install_all_apps or install_this_app:
            print("[EAF] Adding", app_name, "application to EAF")
            git_add_app(app_name, app_spec_dict)
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


    print("[EAF] Installing dependencies for installed applications")
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


    print("[EAF] install-eaf.py finished.")


if __name__ == '__main__':
    main()
