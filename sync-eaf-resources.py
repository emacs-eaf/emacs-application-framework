#!/usr/bin/env python3

import argparse
import os
import platform
import sys
import subprocess
import json
import tempfile

script_path = os.path.dirname(os.path.realpath(__file__))

parser = argparse.ArgumentParser()
parser.add_argument("--force", action="store_true",
                    help='force sync even when there is no updates')
parser.add_argument("--really-run", action="store_true",
                    help='Really run this script.')
parser.add_argument("--mirror-username", type=str,
                    help='The username of mirror.')
parser.add_argument("--mirror-password", type=str,
                    help='The password or token of mirror.')
parser.add_argument("--mirror-use-ssh", action="store_true",
                    help='push to mirror by ssh url, which can be run without password.')
args = parser.parse_args()

def run_command(command, path=script_path, ensure_pass=True, get_result=False, print_command=True):
    if print_command:
        print("[EAF] Running", ' '.join(command), "@", path)

    # Use LC_ALL=C to make sure command output use English.
    # Then we can use English keyword to check command output.
    english_env = os.environ.copy()
    english_env['LC_ALL'] = 'C'

    if get_result:
        process = subprocess.Popen(command, env = english_env,stdin = subprocess.PIPE,
                                   stdout = subprocess.PIPE, universal_newlines=True,
                                   text=True, cwd=path)
    else:
        process = subprocess.Popen(command, env = english_env, stdin = subprocess.PIPE,
                                   universal_newlines=True, text=True, cwd=path)
    process.wait()
    if process.returncode != 0 and ensure_pass:
        sys.exit(process.returncode)
    if get_result:
        return process.stdout.readlines()
    else:
        return None

def yes_no(question, default_yes=False, default_no=False):
    key = input(question)
    if default_yes:
        return key.lower() == 'y' or key == ""
    elif default_no:
        return key.lower() == 'y' or not (key == "" or key.lower() == 'n')
    else:
        return key.lower() == 'y'

def add_auth_info_to_url(url, username, password):
    url = url or ""
    new_url = str.replace(url, "https://gitee.com", "https://{0}:{1}@gitee.com".format(username, password))
    if url == new_url: # Fail to insert auth info into url.
        return False
    else:
        return new_url

def convert_https_url_to_ssh(url):
    url = url or ""
    new_url = str.replace(url, "https://gitee.com/", "git@gitee.com:")
    if url == new_url: # Fail to convert https url to ssh url.
        return False
    else:
        return new_url

def git_repos_sync(mirror_username, mirror_password, mirror_use_ssh):
    with open(os.path.join(script_path, 'applications.json')) as f:
        app_dict = json.load(f)
    for app_name, app_spec_dict in app_dict.items():
        path = os.path.join(tempfile.gettempdir(), "sync-eaf-resourcs", app_name)
        branch = app_spec_dict["branch"]
        url = app_spec_dict["url"]
        mirror_url = app_spec_dict["mirror_url"]
        if mirror_use_ssh:
            mirror_url_with_auth_info = convert_https_url_to_ssh(mirror_url)
        else:
            mirror_url_with_auth_info = add_auth_info_to_url(mirror_url, mirror_username, mirror_password)
        updated = True
        print("[EAF] * Sync EAF {0} repo.".format(app_name))
        if url and mirror_url_with_auth_info:
            print("[EAF] ** Upstream -> Local-dir")
            if os.path.exists(path):
                run_command(["git", "clean", "-df"], path=path)
                run_command(["git", "remote", "rm", "origin"], path=path)
                run_command(["git", "remote", "add", "origin", url], path=path)
                run_command(["git", "reset", "--hard"], path=path)
                output_lines = run_command(["git", "pull", "origin", branch], path=path, ensure_pass=False, get_result=True)
                for output in output_lines:
                    print(output)
                    if "Already up to date." in output:
                        updated = False
            else:
                run_command(["git", "clone", "--branch", branch, url, path])

            if updated or args.force:
                print("[EAF] ** Local-dir -> Mirror")
                print("[EAF] Running git push -f <push url of mirror>")
                run_command(["git", "push", "-f", mirror_url_with_auth_info], path=path, print_command=False, get_result=True)
        else:
            print("[EAF] WARN: url or mirror_url of EAF {} may have some problem, please check them!".format(app_name))

def main():
    try:
        if args.really_run:
            ## Before push to mirror url, username and password of mirror
            ## will be inserted into this url.
            ##
            ## Username and password of mirror come from arguments or
            ## environment variable:
            ##
            ## 1. EAF_MIRROR_USERNAME
            ## 2. EAF_MIRROR_PASSWORD
            ##
            ## In github-action platform, they can be configed in:
            ## "https://github.com" -> "Settings" -> "Secrets" -> "New repository secret"
            result = True
            try:
                mirror_username = args.mirror_username or os.environ["EAF_MIRROR_USERNAME"]
                mirror_password = args.mirror_password or os.environ["EAF_MIRROR_PASSWORD"]
            except KeyError:
                mirror_username = False
                mirror_password = False
        else:
            print("[EAF] Do nothing, exiting...")
            sys.exit()

        if args.mirror_use_ssh:
            result = True
            mirror_use_ssh = True
        else:
            mirror_use_ssh = False
            if mirror_username and mirror_password and len(mirror_username) > 0 and len(mirror_password) > 0:
                result = True
            else:
                result = False
                print("[EAF] No username or password of mirror, exiting...")
                sys.exit()

        if result:
            print("[EAF] sync-eaf-resources.py started")
            print("[EAF] -----------------------------\n")
            git_repos_sync(mirror_username, mirror_password, mirror_use_ssh)
            print("\n[EAF] -----------------------------")
        else:
            sys.exit()
        print("[EAF] sync-eaf-resources.py finished!")
    except KeyboardInterrupt:
        print("[EAF] sync-eaf-resources.py aborted!")
        sys.exit()


if __name__ == '__main__':
    main()
