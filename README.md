# eatpi

6502 code for [Ben Eater's 6502 kit](https://eater.net/6502) that calculates
pi.

## Prerequisites

* [xa65 assembler](https://www.floodgap.com/retrotech/xa/).  Note that this
  is a different assembler to the one used by Ben in the series
  ([vasm](http://sun.hasenbraten.de/vasm/)).
  * On Debian, `sudo apt install xa65` will do the trick
* An assembled 6502 kit from eater.net, EEPROM programmer, etc.
* Alternatively, or additionally, a 6502 emulator. I made a
  [fork of x6502](https://github.com/Jon-Bright/x6502) that I found useful
  during development.

## Usage

```
./build.sh
```

The output will appear as two 32kiB files, `pi6502` and `mathtest`. The
first is the actual pi calculation code. The second is a bunch of
duct-tape-and-string unit tests for the various multi-byte math operations
found in `math.s`.

The pi calculation here is essentially a 6502 translation of
[this C code](https://github.com/rafaello7/pi), which in turn is based on
Frabrice Bellard's algorithm. The code uses (my own) math routines that are
capable of operating on integers with an arbitrary number of bytes. In the
case of the code here, that means 32-bit or 64-bit, but if you've ever
wanted to do 24-bit or 72-bit mathematical operations on a 6502, you're...
well, it's hard to say you're in the right place because clearly something
has gone deeply wrong with your life choices, but the code in `math.s` will
nevertheless assist in achieving that goal.

## Weaknesses

* The last calculated digit of pi is wrong. If the number of digits to
  calculate is extended, then that digit will be right and the new last
  digit will be wrong. This is probably easy to fix, I haven't spent time on
  it.

## Author

* [Jon Bright](https://github.com/Jon-Bright)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
