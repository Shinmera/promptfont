# Contributing Glyphs
Before contributing additional glyphs to this font, please discuss your proposal in an issue.

When creating new glyphs, make sure you save every variant except for pure rotations as an `svg` file in the `glyphs` directory, with some useful name that follows the scheme you see in the directory. Additionally, ensure that:

- The document has a size of 500x500 pixels
- The glyph is centred in the document
- The glyph has some margins around it
- The glyph has been merged down to a single object
- The glyph has no outline and has an opaque black fill
- Any text within the glyph is written with Xolonium / PromptFont itself
- The file name fits into the existing naming scheme. Particularly:
  - You use the abbreviations L R U D for left right up down, etc.
  - Everything is lowercase except for abbreviations like L R U D LD UD S1 ZR etc.
  - The files are hyphenated

Once you've added the glyphs, please open a pull request and we will handle importing the glyph into the font and updating the metadata files.
