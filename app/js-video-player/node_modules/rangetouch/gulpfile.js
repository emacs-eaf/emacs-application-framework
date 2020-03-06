// ==========================================================================
// Gulp build script
// ==========================================================================

const path = require('path');
const gulp = require('gulp');

// CSS
const less = require('gulp-less');
const clean = require('gulp-clean-css');
const prefix = require('gulp-autoprefixer');

// JavaScript
const terser = require('gulp-terser');
const rollup = require('gulp-better-rollup');
const babel = require('rollup-plugin-babel');
const commonjs = require('rollup-plugin-commonjs');
const resolve = require('rollup-plugin-node-resolve');
const sourcemaps = require('gulp-sourcemaps');

// Images
const svgstore = require('gulp-svgstore');
const imagemin = require('gulp-imagemin');

// Utils
const plumber = require('gulp-plumber');
const rename = require('gulp-rename');
const replace = require('gulp-replace');
const size = require('gulp-size');
const log = require('fancy-log');

// Deployment
const aws = require('aws-sdk');
const publish = require('gulp-awspublish');

const pkg = require('./package.json');
const bundles = require('./bundles.json');
const deploy = require('./deploy.json');

const { browserslist, version } = pkg;

// Get AWS config
Object.values(deploy).forEach(target => {
    Object.assign(target, {
        publisher: publish.create({
            region: target.region,
            params: {
                Bucket: target.bucket,
            },
            credentials: new aws.SharedIniFileCredentials({ profile: 'rangetouch' }),
        }),
    });
});

const root = __dirname;
const paths = {
    rangetouch: {
        // Source paths
        src: {
            js: path.join(root, 'src/js/**/*'),
        },
    },
    docs: {
        // Source paths
        src: {
            less: path.join(root, 'docs/src/less/**/*'),
            js: path.join(root, 'docs/src/js/**/*'),
            sprite: path.join(root, 'docs/src/sprite/**/*'),
        },
        // Output paths
        dist: path.join(root, 'docs/dist/'),
        // Docs
        root: path.join(root, 'docs/'),
    },
    upload: [path.join(root, 'dist/**'), path.join(root, 'docs/dist/**')],
};

// Task arrays
const tasks = {
    less: [],
    sass: [],
    js: [],
    sprite: [],
};

// Babel config
const babelrc = {
    babelrc: false,
    presets: [
        '@babel/env',
        [
            'minify',
            {
                builtIns: false, // Temporary fix for https://github.com/babel/minify/issues/904
            },
        ],
    ],
};

// Size plugin
const sizeOptions = { showFiles: true, gzip: true };

// JavaScript
const namespace = 'RangeTouch';

Object.entries(bundles.js).forEach(([filename, entry]) => {
    entry.formats.forEach(format => {
        const name = `js:${filename}:${format}`;
        tasks.js.push(name);

        gulp.task(name, () => {
            return gulp
                .src(entry.src)
                .pipe(plumber())
                .pipe(sourcemaps.init())
                .pipe(
                    rollup(
                        {
                            plugins: [resolve(), commonjs(), babel(babelrc)],
                        },
                        {
                            name: namespace,
                            // exports: 'named',
                            format,
                        },
                    ),
                )
                .pipe(terser())
                .pipe(
                    rename({
                        extname: `.${format === 'es' ? 'mjs' : 'js'}`,
                    }),
                )
                .pipe(size(sizeOptions))
                .pipe(gulp.dest(entry.dist));
        });
    });
});

// CSS

Object.entries(bundles.css).forEach(([filename, entry]) => {
    const name = `less:${filename}`;
    tasks.less.push(name);

    gulp.task(name, () => {
        return gulp
            .src(entry.src)
            .pipe(less())
            .pipe(
                prefix(browserslist, {
                    cascade: false,
                }),
            )
            .pipe(clean())
            .pipe(size(sizeOptions))
            .pipe(gulp.dest(entry.dist));
    });
});

// SVG Sprite

const name = 'sprite';
tasks.sprite.push(name);

gulp.task(name, () => {
    return gulp
        .src(paths.docs.src.sprite)
        .pipe(imagemin())
        .pipe(svgstore())
        .pipe(rename({ basename: 'docs' }))
        .pipe(size(sizeOptions))
        .pipe(gulp.dest(paths.docs.dist));
});

// Build all JS
gulp.task('js', gulp.parallel(...tasks.js));

// Watch for file changes
gulp.task('watch', () => {
    // Core
    gulp.watch(paths.rangetouch.src.js, gulp.parallel(...tasks.js));

    // Docs
    gulp.watch(paths.docs.src.js, gulp.parallel(...tasks.js));
    gulp.watch(paths.docs.src.less, gulp.parallel(...tasks.less));
    gulp.watch(paths.docs.src.sprite, gulp.parallel(...tasks.sprite));
});

// Default gulp task
gulp.task('default', gulp.parallel(...tasks.js, ...tasks.less, ...tasks.sprite, 'watch'));

// Publish a version to CDN and docs
// --------------------------------------------

// Some options
const maxAge = 31536000; // seconds 1 year
const headers = {
    cdn: {
        'Cache-Control': `max-age=${maxAge}`,
    },
    docs: {
        'Cache-Control': 'no-cache, no-store, must-revalidate, max-age=0',
    },
};

const regex =
    '(?:0|[1-9][0-9]*)\\.(?:0|[1-9][0-9]*).(?:0|[1-9][0-9]*)(?:-[\\da-z\\-]+(?:.[\\da-z\\-]+)*)?(?:\\+[\\da-z\\-]+(?:.[\\da-z\\-]+)*)?';
const semver = new RegExp(`v${regex}`, 'gi');
const cdnpath = new RegExp(`${deploy.cdn.domain}/${regex}`, 'gi');
const localpath = new RegExp('(../)?dist', 'gi');

// Publish version to CDN bucket
gulp.task('cdn', () => {
    const { bucket, publisher } = deploy.cdn;

    if (!publisher) {
        throw new Error('No publisher instance. Check AWS configuration.');
    }

    log(`Uploading ${version} to ${bucket}`);

    // Upload to CDN
    return gulp
        .src(paths.upload)
        .pipe(
            size({
                showFiles: true,
                gzip: true,
            }),
        )
        .pipe(
            rename(path => {
                path.dirname = path.dirname.replace('.', version);
            }),
        )
        .pipe(publisher.publish(headers.cdn))
        .pipe(publish.reporter());
});

// Replace versioned files in readme.md
gulp.task('docs:readme', () => {
    const { domain } = deploy.docs;

    return gulp
        .src([`${root}/readme.md`])
        .pipe(replace(cdnpath, `${domain}/${version}`))
        .pipe(gulp.dest(root));
});

// Replace versions in rangetouch.js
gulp.task('docs:src', () =>
    gulp
        .src(path.join(root, 'src/js/rangetouch.js'))
        .pipe(replace(semver, `v${version}`))
        .pipe(gulp.dest(path.join(root, 'src/js/'))),
);

// Replace versions in rangetouch.js
gulp.task('docs:svg', () => {
    const { domain, publisher } = deploy.cdn;

    if (!publisher) {
        throw new Error('No publisher instance. Check AWS configuration.');
    }

    return gulp
        .src(path.join(root, 'docs/dist/docs.js'))
        .pipe(replace(localpath, `https://${domain}/${version}`))
        .pipe(
            rename(path => {
                path.dirname = path.dirname.replace('.', version);
            }),
        )
        .pipe(publisher.publish(headers.cdn))
        .pipe(publish.reporter());
});

// Replace local file paths with remote paths in docs
// e.g. "../dist/rangetouch.js" to "https://cdn.rangetouch.com/x.x.x/rangetouch.js"
gulp.task('docs:paths', () => {
    const { publisher } = deploy.docs;
    const { domain } = deploy.cdn;

    if (!publisher) {
        throw new Error('No publisher instance. Check AWS configuration.');
    }

    return gulp
        .src([`${paths.docs.root}*.html`])
        .pipe(replace(localpath, `https://${domain}/${version}`))
        .pipe(publisher.publish(headers.docs))
        .pipe(publish.reporter());
});

// Upload error.html to cdn (as well as docs site)
gulp.task('docs:error', () => {
    const { publisher } = deploy.docs;
    const { domain } = deploy.cdn;

    if (!publisher) {
        throw new Error('No publisher instance. Check AWS configuration.');
    }

    return gulp
        .src([`${paths.docs.root}error.html`])
        .pipe(replace(localpath, `https://${domain}/${version}`))
        .pipe(publisher.publish(headers.docs))
        .pipe(publish.reporter());
});

// Publish to Docs bucket
gulp.task('docs', gulp.parallel('docs:readme', 'docs:src', 'docs:svg', 'docs:paths', 'docs:error'));

// Do everything
gulp.task('deploy', gulp.series(gulp.parallel(...tasks.js, ...tasks.less, ...tasks.sprite), 'cdn', 'docs'));
