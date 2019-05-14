# parsax

See intro.pdf in this directory for explanation.

See `examples/` directory for example parsers.


## Motivational examples

Here are some bad error messages from stack's yaml parser:

From `stack.yaml` and me not remembering its schema:

``` haskell
Error in $['extra-deps'][1]: failed to parse field 'extra-deps':
failed to parse field 'github': key "github" not present
```

``` haskell
Warning: /home/chris/Work/fpco/streaming-parsers/stack.yaml:
Unrecognized field in PLRepo: extra-dep
```

``` haskell
Warning: /home/chris/Work/fpco/streaming-parsers/stack.yaml:
Unrecognized field in PLArchive:github: extra-dep
```
