## About PromptFont
This is a font designed for button prompts in games. It includes the base alphabet, as well as icons for modifier and control keys, and gamepad buttons. All the icons included in the font are custom made and available under the same [SIL Open Font Licence](LICENSE.txt). Included trademarks however of course still belong to their respective owners.

PromptFont is based on the Xolonium font by Severin Meyer.

## Attribution
If you use this font in your project please leave an attribution notice in your credits like this:

> PromptFont by Yukari "Shinmera" Hafner, available at https://shinmera.com/promptfont

## Release Files
The [PromptFont release](https://github.com/Shinmera/promptfont/archive/refs/heads/gh-pages.zip) includes a couple of files. Here's what they're for:

- ``LICENSE.txt``
  A copy of the SIL Open Font license
- ``README.md``
  A copy of this readme
- ``index.html``
  A copy of the [website](https://shinmera.github.io/promptfont) so you can use it offline as well
- ``glyphs.json``
  This is a JSON file with an array of the glyphs the font provides. Each glyph is an object with the following attributes:
  - ``character`` The actual character as a one-character string
  - ``code`` The unicode codepoint name (``U+XXXX``)
  - ``codepoint`` The actual codepoint as an integer
  - ``category`` The category the glyph belongs to
  - ``name`` The unique human-readable name of the glyph
  - ``code-name`` The unique code-readable name of the glyph. It only contains lowercase characters a-z, numbers 0-9, and dashes. This should make it easy to turn into a code symbol
  - ``tags`` A list of tags that apply to the glyph. The following tags are known:
    - ``xbox`` Applies to Xbox style gamepads
    - ``nintendo`` Applies to Nintendo style gamepads
    - ``playstation`` Applies to Playstation style gamepads
    - ``generic`` Applies to any gamepad
- ``chars.txt``
  A plaintext UTF-8 file that contains all the characters that the font provides.
- ``promptfont.ttf`` and ``promptfont.otf``
  TrueType and OpenType versions of the font, which you should be able to use directly in-engine or other programs.
- ``promptfont.css``
  A CSS file that includes CSS classes for every special glyph, so you can easily embed it in HTML pages and JS games.
- ``atlas-*.png``
  Texture atlases of the various glyphs. Each glyph is 64x64 pixels and has a 1 pixel margin around itself. They are ordered left to right top to bottom according to their filename in the ``glyphs/`` directory of this repository. Since these rasterised versions don't scale well we heavily recommend you to use the fonts directly.

## Engine Specifics
Since there's far too many engines out there and their methods vary a lot, we can't include guides in this repository. For specifics on how to use this font in your engine of choice, please consult their documentation on importing custom fonts and custom glyph ranges, and how to write text with specific unicode codepoints. Since this is literally just a font, it should not prove too difficult.

## Contributing Glyphs
For a brief guide on what to watch out for if you'd like to contribute to this font, please see the [CONTRIBUTING.md](CONTRIBUTING.md) file. If you'd like to request new glyphs to be added, please comment on the [issue ticket]().

## Support
If you'd like to support the continued development of PromptFont, please consider becoming a backer on Patreon:

[![Patreon](https://filebox.tymoon.eu//file/TWpjeU9RPT0=)](https://patreon.com/shinmera)
