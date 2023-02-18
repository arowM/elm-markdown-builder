module MarkdownBuilderSuite exposing (myMarkdown, suite)

import Expect
import MarkdownBuilder as MB
import Test exposing (..)


suite : Test
suite =
    test "render sample markdown as expected." <|
        \_ ->
            myMarkdown
                |> MB.toString
                |> Expect.equal """Markdown Builder

Markdown Builder builds *Markdown* programmatically.

## Builder

* List Item 1
    1. Child item
* List Item 2

    Child paragraph.

    ```elm
    type Builder parent elem =
        ...
        ...
    ```
* List Item 3"""


myMarkdown : MB.Root
myMarkdown =
    MB.root
        { title = "Markdown Builder"
        }
        |> MB.editBody
        |> MB.appendParagraph
        |> MB.pushText
            "Markdown Builder builds"
        |> MB.pushEmphasis
            "Markdown"
        |> MB.pushText
            "programmatically."
        |> MB.break
        |> MB.endAppendMode
        |> MB.appendChildSection
            { title = "Builder"
            }
        |> MB.editBody
        |> MB.appendUnorderedList
        |> MB.appendListItem
        |> MB.editListItemContent
        |> MB.pushText "List Item 1"
        |> MB.endPushMode
        |> MB.editListItemChildren
        |> MB.appendOrderedList
        |> MB.appendListItem
        |> MB.editListItemContent
        |> MB.pushText "Child item"
        |> MB.break
        |> MB.break
        |> MB.break
        |> MB.appendListItem
        |> MB.editListItemContent
        |> MB.pushText "List Item 2"
        |> MB.endPushMode
        |> MB.editListItemChildren
        |> MB.appendParagraph
        |> MB.pushText
            "Child paragraph."
        |> MB.break
        |> MB.appendCodeBlock
            """elm
            type Builder parent elem =
                ...
                ...
            """
        |> MB.break
        |> MB.appendListItem
        |> MB.editListItemContent
        |> MB.pushText "List Item 3"
        |> MB.run
