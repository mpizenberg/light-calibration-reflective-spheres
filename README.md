# Select-ball : calibration for stereophotometric reconstitution

Here are some instruction to make everything work as well as possible.

## What you need to do for wasm part to work

> Warning: this has not been tested on Windows and Mac, only Linux.

### Compile the rust code

First, you need to build the `select-ball` executables.
For that, you will need [Cargo, and all the rustc stuff][cargo].

```sh
$ cargo build --release
```

These executables will be located at `target/release/select-ball`.

### Build the javascript glue code for web-assembly

You will need `wasm-pack-unknown-unknown`, it is strongly suggested to use `npm` to install it.
For example :
```sh
$ sudo npm install -g wasm-pack
```

Then you can build the web-assembly part in the `select-ball-wasm/` directory :

```sh
$ wasm-pack build --target web -- --features console_error_panic_hook
```

This will generate a `pkg/` directory with two important files inside:

- `pkg/select_ball.js`: the "glue" JavaScript module to be imported.
- `pkg/select_ball_bg.wasm`: the compiled WebAssembly module corresponding to the rust code in `src/lib.rs`.

## What you need to do to get the web part working

If you want to see the nice web interface in your favorite browser, you need to compile the elm code and connect it to its javascript ports properly.

### Sharing the `pkg/` dir

We will first link the `pkg/` directory that we created with a symlink :
```sh
$ ln -s <path to pkg/> <path to web-elm/static>/pkg
```

### Building the worker

The worker is a thread, making the computations we need.
It takes the form of a javascript module (`worker.mjs`).
In order to get it to talk with the elm UI, we will transform it into a `.js` file with [esbuild][esbuild].

```sh
$ sudo npm install -g esbuild
```
```text
changed 1 package, and audited 2 packages in 2s

found 0 vulnerabilities
```
```sh
$ esbuild worker.mjs --bundle --preserve-symlinks --outfile=worker.js
```

Now we have in `web-elm/static/` the `worker.js` file that we wanted.

### Compiling the elm code itself

Finaly, we need the elm compiler for the elm language, the core of this project :

```sh
$ sudo npm install -g elm
```
```text
<Some warnings>
changed 1 package, and audited 2 packages in 2s

found 0 vulnerabilities
```
```sh
$ elm make src/Main.elm --optimize --output=static/Elm.js
```

## See the results

When all of this is done, you can the the results by launching the python server :

```sh
python -m http.server 8080
```

If you need to do this again, don't worry, I have prepared two scripts that you can check quickly in `web-elm/static` : `launch.sh` and `launchElm.sh` (you might have to make some ajustments, depending for example on the name of the python executables, either `python` or `python3` between other choices).




[cargo]: https://doc.rust-lang.org/rustc/what-is-rustc.html
[esbuild]: https://github.com/evanw/esbuild
