This bundles a polyfill available on [github](https://github.com/js-temporal/temporal-polyfill).

Import and bundle generation code is located in `import`.

If you need to re-bundle, cd into that dir and run the `build.sh` script:

```bash
cd lib/core/src/temporal-stubs/import
sh build.sh
sh upload-build-data.sh
```
