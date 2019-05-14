# streaming-parsers

Streaming parsers for file formats like JSON and YAML

## Introduction

* `reparsec` is a resumable, backtracking parser (like attoparsec),
  that works on any token and can produce any type of error message
  (like megaparsec). It use used as an auxilliary package for
  `parsax`.
* `parsax` is the main package that can parse either JSON or YAML in a
  streaming fashion, protects against security attacks, it can explain
  what it can parse (like optparse-applicative), and is written in
  applicative style.

Go to the `parsax` directory for more explanation.

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
|Deal with duplicate keys| :heavy_check_mark: | Straight-forward |
|Supplying extra limits (array length, etc.)| :heavy_check_mark: | Easy |
|Set hard limit on number of warnings generated| - | Fairly easy |
|Cleaned up, robust documentation generator| - | Fairly straight-forward |
|Include line/col info in errors| - | Fairly easy |
|Get code coverage to near 100%| - | Detailed |
|Memory/allocation tests| - | Fairly straight-forward |
|Update copyright (esp. from aeson, yaml) before publication| - | Easy |
