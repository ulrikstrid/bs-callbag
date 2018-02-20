# BsCallbag

This is my try at implementing the [callbag spec](https://github.com/callbag/callbag) in reasonml.

## Example

```ocaml
Callbag.interval(1000) |> Callbag.take(3) |> Callbag.observe(d => Js.log(d));
```

Should log 0, 1, 2

# Build

```
npm run build
```

# Build + Watch

```
npm run watch
```

# Editor

If you use `vscode`, Press `Windows + Shift + B` it will build automatically
