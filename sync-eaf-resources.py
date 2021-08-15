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
parser.add_argument("--really_run", action="store_true",
                    help='Really run this script.')
parser.add_argument("--mirror-username", type=str,
                    help='The username of mirror.')
parser.add_argument("--mirror-password", type=str,
                    help='The password of mirror.')
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

def git_repos_sync(mirror_username, mirror_password):
    with open(os.path.join(script_path, 'applications.json')) as f:
        app_dict = json.load(f)
    for app_name, app_spec_dict in app_dict.items():
        path = os.path.join(tempfile.gettempdir(), "sync-eaf-resourcs", app_name)
        branch = app_spec_dict["branch"]
        url = app_spec_dict["url"].format(username = mirror_username, password = mirror_password)
        mirror_url = app_spec_dict["mirror_url"].format(username = mirror_username, password = mirror_password)
        updated = True
        if url and mirror_url:
            print("[EAF] * Sync EAF {0} repo.".format(app_name))
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
                ## When run script in github action, do not print command and output,
                ## for command and output may include mirror username and password.
                print("[EAF] Running git push -f <mirror_url>")
                run_command(["git", "push", "-f", mirror_url], path=path, print_command=False, get_result=True)
        else:
            print("[EAF] No url or mirror_url can be found. do nothing!")

def main():
    try:
        if args.really_run:
            ## The url of mirror must include string: "{username}" and "{password}",
            ## Which will be replaced to real values when run this script in github-action.
            ##
            ## The values of username and password come from environment variable:
            ## "EAF_MIRROR_USERNAME" and "EAF_MIRROR_PASSWORD", which can be configed in:
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

        if mirror_username and mirror_password and len(mirror_username) > 0 and len(mirror_password) > 0:
            result = True
        else:
            result = False
            print("[EAF] No username or password of mirror, exiting...")
            sys.exit()

        if result:
            print("[EAF] sync-eaf-resources.py started")
            print("[EAF] -----------------------------\n")
            git_repos_sync(mirror_username, mirror_password)
            print("\n[EAF] -----------------------------")
        else:
            sys.exit()
        print("[EAF] sync-eaf-resources.py finished ?!")
    except KeyboardInterrupt:
        print("[EAF] sync-eaf-resources.py aborted!")
        sys.exit()


if __name__ == '__main__':
    main()
