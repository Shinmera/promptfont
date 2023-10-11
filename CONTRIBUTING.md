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

# Allocating Glyphs
If you want to actually allocate new glyphs into the font yourself, you'll need FontForge to import the SVG files and SBCL to fix up the json file and generate the ancillary files.

1. Open the promptfont.sfd file with FontForge
2. Find suitable spaces for your glyphs and import the SVGs. Preferably group the glyphs together with the existing ones, and for the generic icons use the appropriate Unicode codepoint if there is one
3. Save the SFD file
4. Update the `glyphs.json` file with your new entries. Add your new entries to the bottom of the file as follows:
   ```json
   {
     "code": "U+BEEF",
     "name": "my character",
     "category": "gamepad"
   }
   ```
   The available categories are: alphabet, android, device, gamepad, icon, keyboard, logo, mouse
5. Run the `compile.lisp` script to fix up the `glyphs.json` file
6. If any of the names of your new glyphs are already taken, the script will warn you about it. Please update the names if that's the case, they should be globally unique
7. Commit the sfd and json files
