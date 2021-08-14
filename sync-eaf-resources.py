#!/usr/bin/env python3

import argparse
import os
import platform
import sys
import subprocess
import json
import tempfile


script_path = os.path.dirname(os.path.realpath(__file__))

def run_command(command, path=script_path, ensure_pass=True, get_result=False):
    print("[EAF] Running", ' '.join(command), "@", path)
    if get_result:
        process = subprocess.Popen(command, stdin = subprocess.PIPE, stderr = subprocess.PIPE,
                                   stdout = subprocess.PIPE, universal_newlines=True, text=True, cwd=path)
    else:
        process = subprocess.Popen(command, stdin = subprocess.PIPE, stderr = subprocess.PIPE,
                                   universal_newlines=True, text=True, cwd=path)
    process.wait()
    if process.returncode != 0 and ensure_pass:
        print(process.stderr)
        sys.exit(process.returncode)
    if get_result:
        return process.stdout.readlines()
    else:
        return None

def git_repos_sync():
    with open(os.path.join(script_path, 'eaf-sources.json')) as f:
        app_dict = json.load(f)
    for app_name, app_spec_dict in app_dict.items():
        path = os.path.join(tempfile.gettempdir(), "sync-eaf-resourcs", app_name)
        github_url = app_spec_dict["github"]
        gitee_ssh_url = app_spec_dict["gitee_ssh"]
        updated = True
        print("[EAF] * Sync EAF {0} repo.".format(app_name))
        print("[EAF] ** Github to local: {0} -> {1}".format(github_url, path))
        if os.path.exists(path):
            run_command(["git", "clean", "-df"], path=path, ensure_pass=False)
            run_command(["git", "remote", "rm", "origin"], path=path, ensure_pass=False)
            run_command(["git", "remote", "add", "origin", github_url], path=path, ensure_pass=False)
            run_command(["git", "reset", "--hard", "origin"], path=path, ensure_pass=False)
            output_lines = run_command(["git", "fetch", "--dry-run"], path=path, ensure_pass=False, get_result=True)
            if (len(output_lines) == 0):
                updated = False
                print("[EAF] eaf-{} already up to data.".format(app_name))
            else:
                run_command(["git", "pull", "origin", "master"], path=path, ensure_pass=False)
        else:
            run_command(["git", "clone", "--branch", "master", github_url, path])

        if updated:
            print("[EAF] ** Local to gitee ({0} -> {1})".format(path, gitee_ssh_url))
            run_command(["git", "push", "-f", gitee_ssh_url], path=path, ensure_pass=False)
        print("")

def main():
    try:
        print("[EAF] sync-eaf-resources.py start.")
        git_repos_sync()
        print("[EAF] sync-eaf-resources.py finished.")
    except KeyboardInterrupt:
        print("[EAF] sync-eaf-resources.py aborted!")
        sys.exit()


if __name__ == '__main__':
    main()
