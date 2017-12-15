# Obmapp

Obmapp is a library for parsing beatmap files for the
[free rhythm game called osu!][osu]. It is written in Haskell, and it aims to
support all versions of the game's beatmap files (.osu) with ease of use. Obmapp
is currently work in progress, and supports only version 3, which is the oldest
version seen in use.

## Features

- Strong typing for different beatmap file versions - each version has its own
  type
- A common interface for all beatmap versions for ease of use
- Conversion of difficult-to-understand values to different values that are
  easier to understand

## Supported Versions

The following versions are currently supported:

 3 | 4–14
---|------
 ✓ |  ✗

Here's something unrelated that's going to go away, probably soon-ish:
```
0289657314
```

[osu]: https://osu.ppy.sh/
