{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Document
-}

module Document (
    Document (..),
    Date (..),
    Header (..),
    FormatType (..),
    Link (..),
    Image (..),
    Paragraph (..),
    Title (..),
    Section (..),
    CodeBlock (..),
    Item (..),
    List (..),
    Content (..)
) where

data Document = Document {
    header :: Header,
    body :: [Content]
} deriving (Show)

newtype Date = Date String deriving (Show)

data Header = Header {
    title :: String,
    author :: Maybe String,
    date :: Maybe Date
} deriving (Show)

data FormatType = Italic | Bold | Code
    deriving (Show)

data Link = Link {
    label :: Content,
    link :: String
} deriving (Show)

data Image = Image {
    altText :: String,
    url :: String
} deriving (Show)

newtype Paragraph = Paragraph [Content] deriving (Show)

data Title = Title {
    titleText :: String,
    titleLevel :: Int
} deriving (Show)

data Section = Section {
    sectionTitle :: Maybe Title,
    sectionBody :: [Content]
} deriving (Show)

data CodeBlock = CodeBlock {
    code :: String,
    language :: Maybe String
} deriving (Show)

newtype Item = Item [Content] deriving (Show)

newtype List = List [Item] deriving (Show)

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
    deriving (Show)
