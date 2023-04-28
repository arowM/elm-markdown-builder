module MarkdownBuilder exposing
    ( Builder
    , break
    , Root
    , root
    , run
    , Section
    , editBody
    , appendChildSection
    , ListBlock, ListItem
    , appendListItem
    , AppendMode
    , endAppendMode
    , appendParagraph
    , appendQuoteBlock
    , QuoteBlock
    , appendOrderedList
    , appendUnorderedList
    , appendCodeBlock
    , appendBlocks
    )

{-| This library helps your library or application to generate valid markdown document programmatically.


# Builder

@docs Builder
@docs break


# Root

@docs Root
@docs root
@docs run


# Section

@docs Section
@docs editBody
@docs appendChildSection


# List Block

@docs ListBlock, ListItem
@docs appendListItem


# General Modifiers


## Append Mode

@docs AppendMode
@docs endAppendMode
@docs appendParagraph
@docs appendQuoteBlock
@docs QuoteBlock
@docs appendOrderedList
@docs appendUnorderedList
@docs appendCodeBlock
@docs appendBlocks

-}

import MarkdownAst as Ast



-- Builder


{-| Markdown builder.
You can build a valid markdown structure programmatically in your program:

    import MarkdownAst as Ast

    myMarkdown : Ast.Section
    myMarkdown =
        root
            { title = "Markdown Builder"
            }
            |> editBody
            |> appendParagraph
                [ Ast.PlainText "Markdown Builder builds "
                , Ast.Emphasis "Markdown"
                , Ast.PlainText " programmatically."
                ]
            |> endAppendMode
            |> appendChildSection
                { title = "Builder"
                }
            |> editBody
            |> appendUnorderedList
            |> appendListItem
                [ Ast.PlainText "List Item "
                , Ast.StrongEmphasis "1"
                ]
            |> appendOrderedList
            |> appendListItem
                [ Ast.PlainText "Child item"
                ]
            |> break
            |> break
            |> break
            |> appendListItem
                [ Ast.PlainText "List Item 2"
                ]
            |> appendParagraph
                [ Ast.PlainText "Child paragraph."
                ]
            |> appendCodeBlock
                """elm
                type Builder parent elem =
                    ...
                    ...
                """
            |> break
            |> appendListItem
                [ Ast.PlainText "List Item 3"
                ]
            |> run

    Ast.render myMarkdown
        |> String.lines
    --> [ "# Markdown Builder"
    --> , ""
    --> , "Markdown Builder builds *Markdown* programmatically."
    --> , ""
    --> , "## Builder"
    --> , ""
    --> , "* List Item **1**"
    --> , "    1. Child item"
    --> , "* List Item 2"
    --> , ""
    --> , "    Child paragraph."
    --> , ""
    --> , "    ```elm"
    --> , "    type Builder parent elem ="
    --> , "        ..."
    --> , "        ..."
    --> , "    ```"
    --> , "* List Item 3"
    --> ]

-}
type Builder parent elem
    = Builder
        { current : elem
        , parent : elem -> parent
        , root : parent -> Root
        }


{-| Quit editing current child, and return to edit its parent.
-}
break : Builder (Builder parent child) a -> Builder parent child
break (Builder builder) =
    builder.parent builder.current


current : Builder parent a -> a
current (Builder builder) =
    builder.current


{-| Represents markdown root.
-}
type Root
    = Root Section


{-| Entry point for building markdown.
-}
root : { title : String } -> Builder Root Section
root { title } =
    Builder
        { current = initSection title
        , parent = Root
        , root = identity
        }


{-| Quit modifying markdown, and compile it to the valid markdown structure.
-}
run : Builder parent a -> Ast.Section
run (Builder builder) =
    builder.current
        |> builder.parent
        |> builder.root
        |> (\(Root section) -> runSection section)


runSection : Section -> Ast.Section
runSection (Section section) =
    Ast.Section
        { title = section.title
        , body = List.reverse section.body
        , children =
            List.reverse section.children
                |> List.map runSection
        }



-- Section


{-| Builder for section.
-}
type Section
    = Section
        { title : String
        , body : List Ast.BlockElement -- reversed
        , children : List Section -- reversed
        }


appendSectionBody : List Ast.BlockElement -> Section -> Section
appendSectionBody elems (Section sec) =
    Section
        { sec
            | body = elems ++ sec.body
        }


initSection : String -> Section
initSection title =
    Section
        { title = title
        , body = []
        , children = []
        }


{-| Start [Append Mode](#append-mode) for editing section body.
-}
editBody :
    Builder parent Section
    -> Builder parent (AppendMode Section)
editBody (Builder builder) =
    Builder
        { current =
            AppendMode
                { appendBlocks = appendSectionBody
                , value = builder.current
                }
        , parent =
            \(AppendMode appendable) ->
                builder.parent appendable.value
        , root = builder.root
        }


{-| Append new child section to the current section, and change focus to the new one.
-}
appendChildSection :
    { title : String }
    -> Builder p Section
    -> Builder (Builder p Section) Section
appendChildSection { title } builder =
    Builder
        { current =
            initSection title
        , parent =
            \child ->
                modify
                    (\(Section sec) ->
                        Section { sec | children = child :: sec.children }
                    )
                    builder
        , root = childRoot builder
        }



-- List Block


{-| Builder for a list block.
-}
type ListBlock
    = ListBlock_
        { ordered : Bool
        , items : List ListItem -- reversed
        }


{-| Builder for items in a list block.
-}
type ListItem
    = ListItem
        { content : List Ast.InlineElement
        , children : List Ast.BlockElement -- reversed
        }


{-| Append a list item, and change focus to it.
-}
appendListItem :
    List Ast.InlineElement
    -> Builder p ListBlock
    -> Builder (Builder p ListBlock) (AppendMode ListItem)
appendListItem content builder =
    Builder
        { current =
            ListItem
                { content = content
                , children = []
                }
        , parent =
            \listItem ->
                modify
                    (\(ListBlock_ listBlock_) ->
                        ListBlock_
                            { listBlock_
                                | items = listItem :: listBlock_.items
                            }
                    )
                    builder
        , root = childRoot builder
        }
        |> editListItemChildren


{-| Start [Append Mode](#append-mode) for editing list item children.
-}
editListItemChildren :
    Builder parent ListItem
    -> Builder parent (AppendMode ListItem)
editListItemChildren (Builder builder) =
    Builder
        { current =
            AppendMode
                { appendBlocks = appendListItemChild
                , value = builder.current
                }
        , parent =
            \(AppendMode appendable) ->
                builder.parent appendable.value
        , root = builder.root
        }


appendListItemChild : List Ast.BlockElement -> ListItem -> ListItem
appendListItemChild elems (ListItem item) =
    ListItem
        { item | children = elems ++ item.children }



-- General Modifiers
-- -- Append Mode


{-| Represents that `a` is in the _Append Mode_, which enalbes you to append some blocks to it.
-}
type AppendMode a
    = AppendMode
        { appendBlocks : List Ast.BlockElement -> a -> a
        , value : a
        }


append : List Ast.BlockElement -> AppendMode a -> AppendMode a
append blocks (AppendMode appendable) =
    AppendMode
        { appendable
            | value =
                appendable.appendBlocks blocks appendable.value
        }


{-| End append mode.
-}
endAppendMode : Builder parent (AppendMode a) -> Builder parent a
endAppendMode (Builder builder) =
    let
        (AppendMode appendable) =
            builder.current
    in
    Builder
        { current = appendable.value
        , parent =
            \value ->
                builder.parent
                    (AppendMode { appendable | value = value })
        , root = builder.root
        }


{-| Append a paragraph block.
-}
appendParagraph :
    List Ast.InlineElement
    -> Builder parent (AppendMode a)
    -> Builder parent (AppendMode a)
appendParagraph content builder =
    modify
        (append [ Ast.ParagraphBlock content ])
        builder


{-| Append an ordered list block, and change focus to it.
-}
appendOrderedList :
    Builder parent (AppendMode a)
    -> Builder (Builder parent (AppendMode a)) ListBlock
appendOrderedList builder =
    Builder
        { current =
            ListBlock_
                { ordered = True
                , items = []
                }
        , parent =
            \(ListBlock_ context) ->
                modify
                    (append
                        [ Ast.ListBlock
                            { ordered = context.ordered
                            , items =
                                List.reverse context.items
                                    |> List.map
                                        (\(ListItem item) ->
                                            { content = List.reverse item.content
                                            , children = List.reverse item.children
                                            }
                                        )
                            }
                        ]
                    )
                    builder
        , root = childRoot builder
        }


{-| Append an unordered list block, and change focus to it.
-}
appendUnorderedList :
    Builder parent (AppendMode a)
    -> Builder (Builder parent (AppendMode a)) ListBlock
appendUnorderedList builder =
    Builder
        { current =
            ListBlock_
                { ordered = False
                , items = []
                }
        , parent =
            \(ListBlock_ context) ->
                modify
                    (append
                        [ Ast.ListBlock
                            { ordered = context.ordered
                            , items =
                                List.reverse context.items
                                    |> List.map
                                        (\(ListItem item) ->
                                            { content = item.content
                                            , children = List.reverse item.children
                                            }
                                        )
                            }
                        ]
                    )
                    builder
        , root = childRoot builder
        }


{-| Append a code block.

Elm code:

    appendCodeBlock
        """elm
        appendCodeBlock
            = Debug.todo ""
        """

Generated markdown:

    ```elm
    appendCodeBlock
        = Debug.todo ""
    ```

-}
appendCodeBlock :
    String
    -> Builder parent (AppendMode a)
    -> Builder parent (AppendMode a)
appendCodeBlock r =
    modify
        (append [ Ast.CodeBlock r ])


{-| Append a quote block, and focus it.
-}
appendQuoteBlock :
    Builder parent (AppendMode a)
    -> Builder (Builder parent (AppendMode a)) (AppendMode QuoteBlock)
appendQuoteBlock builder =
    Builder
        { current =
            AppendMode
                { appendBlocks =
                    \elems (QuoteBlock context) ->
                        QuoteBlock
                            { context
                                | content = elems ++ context.content
                            }
                , value =
                    QuoteBlock
                        { content = []
                        }
                }
        , parent =
            \(AppendMode appendable) ->
                (\(QuoteBlock block) ->
                    modify
                        (append
                            [ Ast.QuoteBlock
                                (List.reverse block.content)
                            ]
                        )
                        builder
                )
                    appendable.value
        , root = childRoot builder
        }


{-| -}
type QuoteBlock
    = QuoteBlock
        { content : List Ast.BlockElement --reversed
        }


childRoot : Builder p b -> Builder p b -> Root
childRoot (Builder builder) =
    current >> builder.parent >> builder.root


modify : (a -> a) -> Builder parent a -> Builder parent a
modify f (Builder builder) =
    Builder
        { builder
            | current = f builder.current
        }


{-| Lower level API to append `BlockElement`.
-}
appendBlocks :
    List Ast.BlockElement
    -> Builder parent (AppendMode a)
    -> Builder parent (AppendMode a)
appendBlocks blocks builder =
    modify (append blocks) builder
