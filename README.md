# streaming-parsers

Streaming parsers for file formats like JSON and YAML

## Milestones

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Test suite|  :heavy_check_mark: | Easy |
|High-level GADT for parser|  :heavy_check_mark: | Fairly straight-forward |
|Avoiding collision exploits|  :heavy_check_mark: | Difficult |
|Resumable parsing with backtracking| :heavy_check_mark: | Difficult |
|Output warnings of ignored object keys| :heavy_check_mark: | Easy |
|Custom errors| :heavy_check_mark: | Straight-forward |
|Running m (IO/RIO) actions in a user parser| :heavy_check_mark: | Fairly straight-forward |
|YAML backend| :heavy_check_mark: | Straight-forward |
|JSON backend| :heavy_check_mark: |Fairly straight-forward|
|Documentation generator| :heavy_check_mark: | Straight-forward |
|Deal with utf8 strictly|  :heavy_check_mark: | Straight-forward |
|Include line/col info in errors| - | Fairly easy |
|Supplying extra limits (array length, etc.)| - | Easy |
|Get code coverage to near 100%| - | Detailed |
|Memory/allocation tests| - | Fairly straight-forward |
|Deal with duplicate keys| - | TBD |


## Motivational examples

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
