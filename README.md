## About PromptFont
This is a font designed for button prompts in games. It includes the base alphabet, as well as icons for modifier and control keys, and gamepad buttons.

PromptFont is based on the Xolonium font by Severin Meyer.

## Special Glyphs
Please see the included ``glyphs.json`` file for all the relevant glyphs in the font.


## Import and use the font in Unity (TextMeshPro)

### Import

Import the font in your project (place it in the *resources* folder specified in your Text Mesh Pro project settings).

Create the Text Mesh Pro font asset by clicking right on the font then **Create -> Text Mesh Pro -> Font Asset**.

Click on the font asset created and in the inspector click **Update Atlas Texture**.

Specify the Custom Range (Corresponding to codepoint in glyph.json).

Click Generate Font Atlas.

**Don't forget to save.**

### Use

In a Text Mesh Pro text component, you can now use the font with the font tag.

```
<font="PromptFont SDF">\uE001</font> 
```

*\uE001* is a unicode example, see glyph.json to find the one you need).
