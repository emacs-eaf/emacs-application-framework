#!/usr/bin/env python3

from shutil import which, rmtree
import argparse
import datetime
import json
import os
import site
import subprocess
import sys

parser = argparse.ArgumentParser()
parser.add_argument("--install-all-apps", action="store_true",
                    help='install/update all available applications')
parser.add_argument("--install-core-deps", action="store_true",
                    help='only install/update core dependencies')
parser.add_argument("--install", nargs='+', default=[],
                    help='only install/update apps listed here')
parser.add_argument("--install-new-apps", action="store_true",
                    help='also install previously uninstalled or new applications')
parser.add_argument("--force", action="store_true",
                    help="force install/update app dependencies even if apps are already up-to-date")
parser.add_argument("--ignore-core-deps", action="store_true",
                    help='ignore core dependencies')
parser.add_argument("--ignore-sys-deps", action="store_true",
                    help='ignore system dependencies')
parser.add_argument("--ignore-py-deps", action="store_true",
                    help='ignore python dependencies')
parser.add_argument("--ignore-node-deps", action="store_true",
                    help='ignore node dependencies')
parser.add_argument("--git-full-clone", action="store_true",
                    help='during installation, conduct a full clone to preserve git logs')
parser.add_argument("--app-drop-local-edit", action="store_true",
                    help='during installation, local changes to app repos will be hard reset')
parser.add_argument("--app-save-local-edit", action="store_true",
                    help='compared with --app-drop-local-edit, this option will stash your changes')
parser.add_argument("--use-mirror", action="store_true",
                    help='use mirror url instead of default url')
parser.add_argument("--use-gitee", action="store_true",
                    help='alias of --use-mirror')
args = parser.parse_args()

NPM_CMD = "npm.cmd" if sys.platform == "win32" else "npm"
PIP_CMD = "pip3" if which("pip3") else "pip" # mac only have pip3, so we need use pip3 instead pip

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

script_path = os.path.dirname(os.path.realpath(__file__))
def get_available_apps_dict():
    with open(os.path.join(script_path, 'applications.json')) as f:
        info = json.load(f)
    apps_dict = {}
    for app_name, app_spec_dict in info.items():
        if app_spec_dict["type"] == "app":
            apps_dict[app_name] = app_spec_dict
    return apps_dict

available_apps_dict = get_available_apps_dict()

install_failed_sys = []
install_failed_pys = []
install_failed_apps = []

important_messages = [
    "[EAF] Please run 'git pull ; ./install-eaf.py' (M-x eaf-install-and-update) to update EAF, applications and their dependencies."
]

def run_command(command, path=script_path, ensure_pass=True, get_result=False):
    print("[EAF] Running", ' '.join(command), "@", path)

    # Use LC_ALL=C to make sure command output use English.
    # Then we can use English keyword to check command output.
    english_env = os.environ.copy()
    english_env['LC_ALL'] = 'C'

    if get_result:
        process = subprocess.Popen(command, env = english_env, stdin = subprocess.PIPE,
                                   universal_newlines=True, text=True, cwd=path,
                                   stdout = subprocess.PIPE)
    else:
        process = subprocess.Popen(command, env = english_env, stdin = subprocess.PIPE,
                                   universal_newlines=True, text=True, cwd=path)
    process.wait()
    if process.returncode != 0 and ensure_pass:
        raise Exception(process.returncode)
    if get_result:
        return process.stdout.readlines()
    else:
        return None

def prune_existing_sys_deps(deps_list):
    remove_deps = []
    for dep in deps_list:
        if "node" in dep and which("node"):
            remove_deps.append(dep)
        elif "npm" in dep and which("npm"):
            remove_deps.append(dep)
    return list(set(deps_list) - set(remove_deps))

def get_archlinux_aur_helper():
    command = None
    for helper in ["pacaur", "yay", "yaourt", "paru"]:
        if which(helper):
            command = helper
            break
    if command:
        return command
    else:
        print("Please install one of AUR's helper, such as 'pacaur', 'yay', 'yaourt', 'paru', etc.", file=sys.stderr)
        sys.exit(1)

def install_sys_deps(distro: str, deps_list):
    deps_list = prune_existing_sys_deps(deps_list)
    command = []
    if distro == 'dnf':
        command = ['sudo', 'dnf', '-y', 'install']
    elif distro == 'emerge':
        command = ['sudo', 'emerge']
    elif distro == 'apt':
        command = ['sudo', 'apt', '-y', 'install']
    elif distro == 'pacman':
        aur_helper = get_archlinux_aur_helper()
        command = [aur_helper, '-Sy', '--noconfirm', '--needed']
    elif which("pkg"):
        command = ['doas', 'pkg', '-y', 'install']
    elif which("zypper"):
        command = ['sudo', 'zypper', 'install','-y']
    command.extend(deps_list)
    try:
        run_command(command)
    except Exception as e:
        print("Error: e")
        install_failed_sys.append(' '.join(command))

def install_py_deps(deps_list):
    if sys.prefix == sys.base_prefix:
        command = [PIP_CMD, 'install', '--user', '-U']
    else:
        # if running on a virtual env, --user option is not valid.
        command = [PIP_CMD, 'install', '-U']
    command.extend(deps_list)
    try:
        run_command(command)
    except Exception as e:
        print("Error:", e)
        install_failed_pys.append(' '.join(command))

def remove_node_modules_path(app_path_list):
    for app_path in app_path_list:
        node_modules_path = os.path.join(app_path, "node_modules")
        if os.path.isdir(node_modules_path):
            rmtree(node_modules_path)
            print("[EAF] WARN: removing {}".format(node_modules_path))

def install_npm_install(app_path_list):
    for app_path in app_path_list:
        command = [NPM_CMD, "install", "--force"]
        try:
            run_command(command, path=app_path)
        except Exception as e:
            print("Error:", e)
            install_failed_apps.append(app_path)

def install_npm_rebuild(app_path_list):
    for app_path in app_path_list:
        command = [NPM_CMD, "rebuild"]
        try:
            run_command(command, path=app_path)
        except Exception as e:
            print("Error:", e)
            install_failed_apps.append(app_path)

def install_vue_install(app_path_list):
    for app_path in app_path_list:
        command = [NPM_CMD, "install", "--force"]
        try:
            run_command(command, path=app_path)
        except Exception as e:
            print("Error:", e)
            install_failed_apps.append(app_path)
        command = [NPM_CMD, "run", "build"]
        try:
            run_command(command, path=app_path)
        except Exception as e:
            print("Error:", e)
            install_failed_apps.append(app_path)

def add_or_update_app(app: str, app_spec_dict):
    url = ""
    path = os.path.join("app", app)

    if os.path.exists(path):
        print("\n[EAF] Updating", app, "to newest version...")
    else:
        print("\n[EAF] Adding", app, "application to EAF...")

    if args.use_mirror or args.use_gitee: # use_gitee is alias of use_mirror.
        if 'mirror_url' not in app_spec_dict:
            print("[EAF] There is no mirror URL set in applications.json for", app)
            print("[EAF] Using default URL...")
            url = app_spec_dict['url']
        else:
            url = app_spec_dict['mirror_url']
    else:
        url = app_spec_dict['url']

    branch = app_spec_dict['branch']
    time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')


    updated = True
    if os.path.exists(path):
        if args.app_drop_local_edit:
            print("[EAF] Clean {}'s local changed for pull code automatically.".format(app))
            run_command(["git", "clean", "-df"], path=path, ensure_pass=False)
            run_command(["git", "reset", "--hard", "origin"], path=path, ensure_pass=False)
        elif args.app_save_local_edit:
            print("[EAF] Clean {}'s local changed for pull code automatically.".format(app))
            run_command(["git", "clean", "-df"], path=path, ensure_pass=False)
            run_command(["git", "stash", "save", "[{}] Auto stashed by install-eaf.py".format(time)], path=path, ensure_pass=False)
            run_command(["git", "reset", "--hard"], path=path, ensure_pass=False)
            run_command(["git", "checkout", branch], path=path, ensure_pass=False)
            run_command(["git", "reset", "--hard", "origin"], path=path, ensure_pass=False)

        branch_outputs = run_command(["git", "branch"], path=path, get_result=True)
        if branch_outputs is None:
            raise Exception("Not in git app!")
        exist_branch = False
        for b in branch_outputs:
            if branch in b:
                exist_branch = True
                break
        if not exist_branch:
            run_command(["git", "config", "remote.origin.fetch", "+refs/heads/"+branch+":refs/remotes/origin/"+branch], path=path)
            if args.git_full_clone:
                run_command(["git", "fetch", "origin", branch], path=path)
            else:
                run_command(["git", "fetch", "origin", branch, "--depth", "1"], path=path)

        current_branch_outputs = run_command(["git", "symbolic-ref", "HEAD"], path=path, get_result=True)
        if current_branch_outputs is None:
            raise Exception("git symbolic-ref failed!")
        current_branch = current_branch_outputs[0]
        if branch not in current_branch:
            run_command(["git", "checkout", branch], path=path)

        output_lines = run_command(["git", "pull", "origin", branch], path=path, get_result=True)
        if output_lines is None:
            raise Exception("git pull failed!")
        for output in output_lines:
            print(output.rstrip())
            if "Already up to date." in output:
                updated = False if branch in current_branch else True

    elif args.git_full_clone:
        run_command(["git", "clone", "-b", branch, url, path])
    else:
        run_command(["git", "clone", "-b", branch, "--depth", "1", url, path])
    return updated

def get_distro():
    distro = ""
    if sys.platform != "linux":
        pass
    elif which("dnf"):
        distro = "dnf"
    elif which("emerge"):
        distro = "emerge"
    elif which("apt"):
        distro = "apt"
    elif which("pacman"):
        distro = "pacman"
        aur_helper = get_archlinux_aur_helper()
        if (not args.ignore_core_deps and not args.ignore_sys_deps and len(args.install) == 0) or args.install_core_deps:
            run_command([aur_helper, '-Sy', '--noconfirm', '--needed'])

    elif which("pkg"):
        distro = "pkg"
    elif which("zypper"):
        distro = "zypper"
    elif which("brew"):
        distro = "brew"
    elif sys.platform == "linux":
        print("[EAF] Unsupported Linux distribution/package manager.")
        print(" Please see dependencies.json for list of dependencies.")
        if not (args.ignore_core_deps or args.ignore_sys_deps):
            sys.exit(1)

    return distro

def install_core_deps(distro, deps_dict):
    print("[EAF] Installing core dependencies")
    core_deps = []
    if not args.ignore_sys_deps and sys.platform == "linux":
        core_deps.extend(deps_dict[distro])
        if len(core_deps) > 0:
            install_sys_deps(distro, core_deps)
    if (not args.ignore_py_deps or sys.platform != "linux") and sys.platform in deps_dict["pip"]:
        install_py_deps(deps_dict["pip"][sys.platform])

    print("[EAF] Finished installing core dependencies")

def yes_no(question, default_yes=False, default_no=False):
    key = input(question)
    if default_yes:
        return key.lower() == 'y' or key == ""
    elif default_no:
        return key.lower() == 'y' or not (key == "" or key.lower() == 'n')
    else:
        return key.lower() == 'y'

def get_installed_apps(app_dir):
    apps_installed = [
        f
        for f in os.listdir(app_dir)
        if os.path.isdir(os.path.join(app_dir, f))
        # directories not managed, or not part of, EAF
        if f not in ("__pycache__",)
    ]
    for app in apps_installed:
        git_dir = os.path.join(app_dir, app, ".git")
        if app not in get_available_apps_dict().keys():
            apps_installed.remove(app)
        if not os.path.isdir(git_dir):
            important_messages.append("[EAF] *WARN* 'app/{}' is not a git repo installed by install-eaf.py!".format(app))
            apps_installed.remove(app)

    return apps_installed

def get_installed_apps_dict(apps_installed):
    return {app_name: available_apps_dict[app_name] for app_name in apps_installed}

def get_new_apps_dict(apps_installed):
    not_installed_apps_dict = {}
    new_apps_dict = {}
    num = 1
    for app_name, app_spec_dict in available_apps_dict.items():
        if app_name not in apps_installed:
            not_installed_apps_dict[app_name] = app_spec_dict
    for app_name, app_spec_dict in not_installed_apps_dict.items():
        indicator = "({}/{})".format(num, len(not_installed_apps_dict))
        prompt = "[EAF] " + indicator + " " + app_spec_dict['name'] + ". " + app_spec_dict['desc'] + " - Install?"
        install_p = yes_no(prompt + " (Y/n): ", default_yes=True) if app_spec_dict['default_install'] == 'true' else yes_no(prompt + " (y/N): ", default_no=True)
        if install_p:
            new_apps_dict[app_name] = app_spec_dict
        num = num + 1
    return new_apps_dict

def get_specific_install_apps_dict(apps_need_install):
    need_install_apps_dict = {}
    for app_name, app_spec_dict in available_apps_dict.items():
        if app_name in apps_need_install:
            need_install_apps_dict[app_name] = app_spec_dict
    return need_install_apps_dict

def print_sample_config(app_dir):
    for app in get_installed_apps(app_dir):
        print("(require 'eaf-{})".format(app))

def get_install_apps(apps_installed):
    if args.install_all_apps:
        return [get_available_apps_dict()]
    if len(args.install) > 0:
        return [get_specific_install_apps_dict(args.install)]

    pending_apps_dict_list = [get_installed_apps_dict(apps_installed)]
    if args.install_new_apps or len(apps_installed) == 0:
        pending_apps_dict_list.append(get_new_apps_dict(apps_installed))
    elif not args.install_new_apps:
        important_messages.append("[EAF] Use the flag '--install-new-apps' to install previously uninstalled or new apps.")

    return pending_apps_dict_list

def install_app_deps(distro, deps_dict):
    print("[EAF] Installing application dependencies")

    app_dir = os.path.join(script_path, "app")
    if not os.path.exists(app_dir):
        os.makedirs(app_dir)

    apps_installed = get_installed_apps(app_dir)
    pending_apps_dict_list = get_install_apps(apps_installed)

    sys_deps = []
    py_deps = []
    npm_install_apps = []
    vue_install_apps = []
    npm_rebuild_apps = []
    for pending_apps_dict in pending_apps_dict_list:
        for app_name, app_spec_dict in pending_apps_dict.items():
            updated = True
            try:
                updated = add_or_update_app(app_name, app_spec_dict)
            except Exception as e:
                raise Exception("[EAF] There are unsaved changes in EAF " + app_name + " application. Please re-run command with --app-drop-local-edit or --app-save-local-edit")
            app_path = os.path.join(app_dir, app_name)
            app_dep_path = os.path.join(app_path, 'dependencies.json')
            if (updated or args.force) and os.path.exists(app_dep_path):
                with open(app_dep_path) as f:
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

    print("\n[EAF] Installing dependencies for the selected applications")
    if not args.ignore_sys_deps and sys.platform == "linux" and len(sys_deps) > 0:
        print("[EAF] Installing system dependencies")
        install_sys_deps(distro, sys_deps)
    if not args.ignore_py_deps and len(py_deps) > 0:
        print("[EAF] Installing python dependencies")
        install_py_deps(py_deps)
    if not args.ignore_node_deps:
        if args.force:
            if len(npm_install_apps) > 0:
                remove_node_modules_path(npm_install_apps)
            if len(vue_install_apps) > 0:
                remove_node_modules_path(vue_install_apps)
        if len(npm_install_apps) > 0:
            install_npm_install(npm_install_apps)
        if len(npm_rebuild_apps) > 0:
            install_npm_rebuild(npm_rebuild_apps)
        if len(vue_install_apps) > 0:
            install_vue_install(vue_install_apps)

    print("\n[EAF] Please always ensure the following config are added to your init.el:")
    print_sample_config(app_dir)

    global install_failed_sys
    global install_failed_pys
    global install_failed_apps
    if len(install_failed_sys) > 0:
        install_failed_sys = list(set(install_failed_sys))
        print(bcolors.WARNING + "\n[EAF] Installation FAILED for the following system dependencies:" + bcolors.ENDC)
        for dep in install_failed_sys: print(bcolors.WARNING + dep + bcolors.ENDC)
    if len(install_failed_pys) > 0:
        install_failed_pys = list(set(install_failed_pys))
        print(bcolors.WARNING + "\n[EAF] Installation FAILED for the following Python dependencies:" + bcolors.ENDC)
        for dep in install_failed_pys: print(bcolors.WARNING + dep + bcolors.ENDC)
    if len(install_failed_apps) > 0:
        install_failed_apps = list(set(install_failed_apps))
        print(bcolors.WARNING + "\n[EAF] Installation FAILED for following applications:" + bcolors.ENDC)
        for app in install_failed_apps: print(bcolors.WARNING + app + bcolors.ENDC)
    if len(install_failed_sys) + len(install_failed_pys) + len(install_failed_apps) == 0:
        print("[EAF] Installation SUCCESS!")
    else:
        print("[EAF] Please rerun ./install-eaf.py with `--force`, or install them manually!")


def main():
    try:
        distro = get_distro()
        with open(os.path.join(script_path, 'dependencies.json')) as f:
            deps_dict = json.load(f)

        if (not args.ignore_core_deps and len(args.install) == 0) or args.install_core_deps:
            print("[EAF] ------------------------------------------")
            install_core_deps(distro, deps_dict)
            print("[EAF] ------------------------------------------")

        if not args.install_core_deps:
            print("[EAF] ------------------------------------------")
            install_app_deps(distro, deps_dict)
            print("[EAF] ------------------------------------------")

        print("[EAF] install-eaf.py finished.\n")

        for msg in important_messages:
            print(bcolors.WARNING + msg + bcolors.ENDC)
    except KeyboardInterrupt:
        print("[EAF] install-eaf.py aborted!")
        sys.exit()


if __name__ == '__main__':
    main()
