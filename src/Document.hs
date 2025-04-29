{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Document
-}

module Document where

data Document = Document {
    header :: Header,
    body :: [Content]
}

newtype Date = Date String

data Header = Header {
    title :: String,
    author :: Maybe String,
    date :: Maybe Date
}

data FormatType = Italic | Bold | Code

data Link = Link {
    label :: Content,
    link :: String
}

data Image = Image {
    altText :: String,
    url :: String
}

newtype Paragraph = Paragraph [Content]

data Title = Title {
    titleText :: String,
    titleLevel :: Int
}

data Section = Section {
    sectionTitle :: Maybe Title,
    sectionBody :: [Content]
}

data CodeBlock = CodeBlock {
    code :: String,
    language :: Maybe String
}

newtype Item = Item [Content]

newtype List = List [Item]

data Content =
    Text String
    | TitleContent Title
    | Formatted FormatType Content
    | LinkContent Link
    | ImageContent Image
    | ParagraphContent Paragraph
    | SectionContent Section
    | CodeBlockContent CodeBlock
    | ListContent List
