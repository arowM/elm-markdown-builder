module MarkdownBuilderSuite exposing (myMarkdown, suite)

import Expect
import MarkdownBuilder as MB
import MarkdownAst as Ast
import Test exposing (..)


suite : Test
suite =
    test "render sample markdown as expected." <|
        \_ ->
            myMarkdown
                |> Ast.render
                |> Expect.equal """# Markdown Builder

Markdown Builder builds *Markdown* programmatically.

## Section

* List Item **1**
    1. Child item
* List Item 2

    Child paragraph. \\[Link like\\]\\(./src\\) in plain text. 1. foo  
    2\\. bar

    1\\. dummy ordered list item

    ```elm
    type Builder parent elem =
        ...
        ...
    ```
* List Item 3

    > [Child paragraph](./bar "title here").
    >
    > ```elm
    > type Builder parent elem =
    >     ...
    >     ...
    > ```

    ```text
    foo
    bar
    ```

    ![dummy image](./foo)
* List Item 4"""


myMarkdown : Ast.Section
myMarkdown =
    MB.root
        { title = "Markdown Builder"
        }
        |> MB.editBody
        |> MB.appendParagraph
            [ Ast.PlainText "Markdown Builder builds "
            , Ast.Emphasis "Markdown"
            , Ast.PlainText " programmatically."
            ]
        |> MB.endAppendMode
        |> MB.appendChildSection
            { title = "Section"
            }
        |> MB.editBody
        |> MB.appendUnorderedList
        |> MB.appendListItem
            [ Ast.PlainText "List Item "
            , Ast.StrongEmphasis "1"
            ]
        |> MB.appendOrderedList
        |> MB.appendListItem
            [ Ast.PlainText "Child item"
            ]
        |> MB.break
        |> MB.break
        |> MB.break
        |> MB.appendListItem
            [ Ast.PlainText "List Item 2"
            ]
        |> MB.appendParagraph
            [ Ast.PlainText "Child paragraph."
            , Ast.PlainText " [Link like](./src) in plain text.\n1. foo"
            , Ast.LineBreak
            , Ast.PlainText "2. bar"
            ]
        |> MB.appendParagraph
            [ Ast.PlainText "1. dummy ordered list item"
            ]
        |> MB.appendCodeBlock
            """elm
            type Builder parent elem =
                ...
                ...
            """
        |> MB.break
        |> MB.appendListItem
            [ Ast.PlainText "List Item 3"
            ]
        |> MB.appendQuoteBlock
        |> MB.appendParagraph
            [ Ast.Link
              { text = "\tChild paragraph\n\n"
              , href = "./bar"
              , title = Just "\n  title   \there "
              }
            , Ast.PlainText "."
            ]
        |> MB.appendCodeBlock
            """elm
            type Builder parent elem =
                ...
                ...
            """
        |> MB.break
        |> MB.appendBlocks
            [ Ast.CodeBlock
                """text
                foo
                bar
                """
            , Ast.ParagraphBlock
                [ Ast.Image
                  { src = "./foo"
                  , alt = "  dummy  image\n"
                  , title = Nothing
                  }
                ]
            ]
        |> MB.break
        |> MB.appendListItem
            [ Ast.PlainText "List Item 4"
            ]
        |> MB.run
