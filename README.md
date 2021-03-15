# unsecurity

## Versions

|Unsecurity| Status    | Scala 2.12 | Scala 2.13 | Https      | Cats Effect | FS2      |
| ---------| ----------|:----------:|:----------:|:----------:|:-----------:|:--------:|
| 1.0      | stable    |    X       |      -     | 0.20.11    | 1.4.0       | 1.0.5    |
| 2.0.2    | stable    |    -       |      X     | 0.21.1     | 2.1.1       | 2.2.2    |
| 2.0.6    | stable    |    -       |      X     | 0.21.4     | 2.1.3       | 2.3.0    |
| 2.0.7    | stable    |    -       |      X     | 0.21.6     | 2.1.3       | 2.4.2    |
| 3.0      | stable    |    -       |      X     | 0.21.7     | 2.1.4       | 2.4.4    |
| 3.0.2    | stable    |    -       |      X     | 0.21.20    | 2.3.3       | 2.5.3    |

## Changelog

### 3.0.2 (2021-03-15)

* Added Produces..response to be able to return any Http4s Response directly from run
* Updated http4s, fs2 and cats effetcs versions
* Improved somewhat error messages when errors occurs during authentication 
