
FMarkup is a tool for creating HTML/CSS documents in pure F#.

## Simple Example

```fsharp

open FMarkup
open FMarkup.Css

let accountsPage accounts =
  %%[
    Doctype.html5
    html [
      body [
        ul [
          rule "li" [               // Inline rules are scoped to the parent element
            font.weight.bold
          ]
          [ for a in accounts ->
            li [
              img [
                src a.photo
                width 100           // Inline styles work, too
                height 100
              ]
              a.name
            ]
          ]
        ]
      ]
    ]
  ]

let accountsStyle =
  style [
    rule "body" [
      color "silver"
      background.color "black"
    ]
    rule "a" [
      text.decoration.none
      rule ":hover" [               // Rules may be nested
        text.decoration.underline
      ]
    ]
  ]

```

Because your views are simple functions, composing templates/layouts/master pages is as simple as creating functions with arguments where markup holes are filled. To render the markup, create an instance of the `HtmlPrinter` record. It has two fields: `Format`, which specifies whether you want to render HTML or XHTML, and `CssUnit`, which specifies what CSS unit value (such as `px`) you would like `int` and `float` values to be rendered with.

## Notes

Because FMarkup works with `obj list` to avoid noise, sometimes it may be necessary to upcast symbols, so the operators `%` and `%%` are provided to cast to `obj` and `obj list`, respectively.

Virtually all HTML5 and CSS3 symbols are supported. Bindings for some more obscure CSS properties which support multiple values may need to be improved, let me know if you run into any issues.

## Compact Style

A module called `FMarkup.Compact` is provided with succinct shortcuts to many common artifacts. It contains some experimental operators which may be included later into the main module.

## License

Public Domain.

## Dependencies

- Futility: https://github.com/danfinch/futility (submodule)

## Maintainer

- Dan Finch (d@nfin.ch)
