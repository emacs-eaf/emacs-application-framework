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
parser.add_argument("--github-action", action="store_true",
                    help='Use it when run script in github action.')
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

def git_repos_sync(gitee_info):
    with open(os.path.join(script_path, 'applications.json')) as f:
        app_dict = json.load(f)
    for app_name, app_spec_dict in app_dict.items():
        path = os.path.join(tempfile.gettempdir(), "sync-eaf-resourcs", app_name)
        git_branch = app_spec_dict["git_branch"]
        github_url = app_spec_dict["github"]
        if gitee_info:
            gitee_url = str.replace(app_spec_dict["gitee"], "https://gitee.com", gitee_info)
        else:
            gitee_url = app_spec_dict["gitee_ssh"]
        updated = True
        print("[EAF] * Sync EAF {0} repo.".format(app_name))
        print("[EAF] ** Github -> local")
        if os.path.exists(path):
            run_command(["git", "clean", "-df"], path=path)
            run_command(["git", "remote", "rm", "origin"], path=path)
            run_command(["git", "remote", "add", "origin", github_url], path=path)
            run_command(["git", "reset", "--hard"], path=path)
            output_lines = run_command(["git", "pull", "origin", git_branch], path=path, ensure_pass=False, get_result=True)
            for output in output_lines:
                print(output)
            if "Already up to date." in output:
                updated = False
        else:
            run_command(["git", "clone", "--branch", git_branch, github_url, path])

        if updated or args.force:
            print("[EAF] ** Local -> gitee")
            if gitee_info:
                ## When run script in github action, do not print command and output,
                ## for command and output may include gitee username and password.
                print("[EAF] Running git push -f <gitee_url>")
                run_command(["git", "push", "-f", gitee_url], path=path, print_command=False, get_result=True)
            else:
                run_command(["git", "push", "-f", gitee_url], path=path)

def main():
    gitee_info = False
    try:
        if args.github_action:
            ## When run this script in git action, https will be used, to deal with
            ## the problem of gitee username and password inputing, string "https://gitee.com"
            ## in git-url will be replaced with:
            ##
            ##    https://<gitee-username>:<gitee-token>@gitee.com
            ##
            ## Variable gitee_info record gitee username and password, its value come from
            ## environment variable "GITEE_INFO", which can be configed in:
            ## "https://github.com" -> "Settings" -> "Secrets" -> "New repository secret"
            ##
            ## 1. NAME: GITEE_INFO
            ## 2. Value:
            ##
            ##    https://<gitee-username>:<gitee-token>@gitee.com
            ##
            ## NOTE:
            ## 1. <gitee-username> is the username of gitee.com
            ## 2. <gitee-token> can create in: "https://gitee.com" -> "设置" -> "私人令牌".
            try:
                gitee_info = os.environ["GITEE_INFO"]
            except KeyError:
                gitee_info = False
            if gitee_info and len(gitee_info) > 0:
                result = True
            else:
                print("[EAF] Gitee info is not found, exiting...")
                sys.exit()
        else:
            print("[EAF] If you don't know what you're doing with this script, don't do it!")
            result = yes_no("[EAF] Continue? y/N ", default_no=True)
            print("[EAF] Sure. This will probably fail anyways...")

        if result:
            print("[EAF] sync-eaf-resources.py started")
            print("[EAF] -----------------------------\n")
            git_repos_sync(gitee_info)
            print("\n[EAF] -----------------------------")
        else:
            print("[EAF] Wise decision. Exiting...")
            sys.exit()
        print("[EAF] sync-eaf-resources.py finished ?!")
    except KeyboardInterrupt:
        print("[EAF] sync-eaf-resources.py aborted!")
        sys.exit()


if __name__ == '__main__':
    main()
