# Release Instructions

1. Build packages

  ```bash
  $ npm run build-all
  ```

1. Add release notes to CHANGELOG.md

1. Change version number in package.json

1. Change version number in bower.json

1. Update CDN urls in README.md

1. Commit changes and tag code

  ```bash
  $ git add . --all
  $ git commit -a -m "bumped version number"
  $ git push origin master
  $ git tag <version-number>
  $ git push --tags
  ```

1. Push changes to NPM

  ```bash
  $ npm publish
  ```
